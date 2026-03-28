// Copyright © 2024 The ELPS authors

package lsp

import (
	"errors"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/parser/token"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

const debounceDelay = 300 * time.Millisecond

// textDocumentDidOpen handles the textDocument/didOpen notification.
func (s *Server) textDocumentDidOpen(ctx *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	s.captureNotify(ctx)
	doc := s.docs.Open(
		params.TextDocument.URI,
		int32(params.TextDocument.Version),
		params.TextDocument.Text,
	)
	s.analyzeAndPublish(doc)
	return nil
}

// textDocumentDidChange handles the textDocument/didChange notification.
func (s *Server) textDocumentDidChange(ctx *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	s.captureNotify(ctx)
	// With full sync, the last content change is the complete document.
	var content string
	for _, change := range params.ContentChanges {
		switch c := change.(type) {
		case protocol.TextDocumentContentChangeEventWhole:
			content = c.Text
		case protocol.TextDocumentContentChangeEvent:
			content = c.Text
		}
	}

	doc := s.docs.Change(
		params.TextDocument.URI,
		int32(params.TextDocument.Version),
		content,
	)

	// Debounce: delay analysis to avoid thrashing during rapid edits.
	s.debounceMu.Lock()
	if t, ok := s.debounce[doc.URI]; ok {
		t.Stop()
	}
	s.debounce[doc.URI] = time.AfterFunc(debounceDelay, func() {
		defer func() { _ = recover() }() // don't crash the server on analysis panic
		d := s.docs.Get(doc.URI)
		if d != nil {
			s.analyzeAndPublish(d)
		}
	})
	s.debounceMu.Unlock()
	return nil
}

// textDocumentDidSave handles the textDocument/didSave notification.
func (s *Server) textDocumentDidSave(ctx *glsp.Context, params *protocol.DidSaveTextDocumentParams) error {
	s.captureNotify(ctx)
	// Cancel any pending debounce and publish immediately.
	s.debounceMu.Lock()
	if t, ok := s.debounce[params.TextDocument.URI]; ok {
		t.Stop()
		delete(s.debounce, params.TextDocument.URI)
	}
	s.debounceMu.Unlock()

	// Update the definition index synchronously before analysis so that
	// the current save's diagnostics reflect the latest ExtraGlobals.
	s.updateFileDefinitions(params.TextDocument.URI)

	doc := s.docs.Get(params.TextDocument.URI)
	if doc != nil {
		s.analyzeAndPublish(doc)
	}

	// Incrementally update the workspace reference index in the background.
	go func() {
		defer func() { _ = recover() }() // don't crash on update panic
		s.updateFileRefs(params.TextDocument.URI)
	}()

	return nil
}

// textDocumentDidClose handles the textDocument/didClose notification.
func (s *Server) textDocumentDidClose(_ *glsp.Context, params *protocol.DidCloseTextDocumentParams) error {
	// Cancel pending debounce.
	s.debounceMu.Lock()
	if t, ok := s.debounce[params.TextDocument.URI]; ok {
		t.Stop()
		delete(s.debounce, params.TextDocument.URI)
	}
	s.debounceMu.Unlock()

	// Clear diagnostics for the closed file.
	s.sendNotification(protocol.ServerTextDocumentPublishDiagnostics, &protocol.PublishDiagnosticsParams{
		URI:         params.TextDocument.URI,
		Diagnostics: []protocol.Diagnostic{},
	})

	s.docs.Close(params.TextDocument.URI)
	return nil
}

// analyzeAndPublish runs analysis and lint on a document and publishes
// the resulting diagnostics to the client.
func (s *Server) analyzeAndPublish(doc *Document) {
	// Check document size before running analysis. Use a cheap lock-and-read
	// rather than a full Snapshot() to avoid unnecessary slice cloning.
	if s.maxDocumentBytes > 0 {
		doc.mu.Lock()
		contentLen := len(doc.Content)
		uri := doc.URI
		doc.mu.Unlock()

		if contentLen > s.maxDocumentBytes {
			sev := protocol.DiagnosticSeverityInformation
			s.sendNotification(protocol.ServerTextDocumentPublishDiagnostics, &protocol.PublishDiagnosticsParams{
				URI: uri,
				Diagnostics: []protocol.Diagnostic{{
					Range:    protocol.Range{},
					Severity: &sev,
					Source:   strPtr("elps"),
					Message:  "File exceeds size limit for semantic analysis",
				}},
			})
			return
		}
	}

	s.ensureAnalysis(doc)

	// Re-snapshot after analysis to capture the result and the current
	// version atomically. We use doc.Version (not snap.Version) so the
	// version guard compares the version that matches the content/analysis
	// we actually read here — avoiding a TOCTOU window where the doc
	// could have been updated between the initial snapshot and now.
	doc.mu.Lock()
	parseErrors := doc.parseErrors
	content := doc.Content
	docAnalysis := doc.analysis
	uri := doc.URI
	currentVersion := doc.Version
	doc.mu.Unlock()

	var diags []protocol.Diagnostic

	// Report parse errors as diagnostics.
	for _, parseErr := range parseErrors {
		diags = append(diags, protocol.Diagnostic{
			Range:    parseErrorRange(parseErr),
			Severity: severity(protocol.DiagnosticSeverityError),
			Source:   strPtr("elps"),
			Message:  parseErr.Error(),
		})
	}

	// Run linter with cached analysis.
	lintDiags, err := s.linter.LintFileWithContext(
		[]byte(content),
		uriToPath(uri),
		docAnalysis,
	)
	if err == nil {
		for _, d := range lintDiags {
			diags = append(diags, convertLintDiagnostic(d))
		}
	}

	// Version guard: discard stale results from debounced analysis.
	doc.mu.Lock()
	if currentVersion < doc.Version {
		// Document changed since we read it — discard these stale results.
		doc.mu.Unlock()
		return
	}
	if currentVersion < doc.publishedVersion {
		// A strictly newer version was already published — skip.
		doc.mu.Unlock()
		return
	}
	doc.publishedVersion = currentVersion
	doc.publishedDiagnostics = diags
	doc.mu.Unlock()

	s.sendNotification(protocol.ServerTextDocumentPublishDiagnostics, &protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diags,
	})
}

// convertLintDiagnostic converts a lint.Diagnostic to an LSP Diagnostic.
func convertLintDiagnostic(d lint.Diagnostic) protocol.Diagnostic {
	line := d.Pos.Line
	col := d.Pos.Col
	if line > 0 {
		line--
	}
	if col > 0 {
		col--
	}
	start := protocol.Position{Line: safeUint(line), Character: safeUint(col)}
	end := start // Default: zero-width range.
	if d.EndPos.Line > 0 {
		endLine := d.EndPos.Line - 1
		endCol := d.EndPos.Col
		if endCol > 0 {
			endCol--
		}
		end = protocol.Position{Line: safeUint(endLine), Character: safeUint(endCol)}
	}
	sev := mapLintSeverity(d.Severity)
	diag := protocol.Diagnostic{
		Range:    protocol.Range{Start: start, End: end},
		Severity: &sev,
		Source:   strPtr("elps-lint"),
		Code:     &protocol.IntegerOrString{Value: d.Analyzer},
		Message:  d.Message,
	}
	if d.Unnecessary {
		diag.Tags = []protocol.DiagnosticTag{protocol.DiagnosticTagUnnecessary}
	}
	return diag
}

// mapLintSeverity converts a lint.Severity to a protocol.DiagnosticSeverity.
func mapLintSeverity(sev lint.Severity) protocol.DiagnosticSeverity {
	switch sev {
	case lint.SeverityError:
		return protocol.DiagnosticSeverityError
	case lint.SeverityWarning:
		return protocol.DiagnosticSeverityWarning
	case lint.SeverityInfo:
		return protocol.DiagnosticSeverityInformation
	default:
		return protocol.DiagnosticSeverityWarning
	}
}

func severity(s protocol.DiagnosticSeverity) *protocol.DiagnosticSeverity {
	return &s
}

// parseErrorRange extracts source position from a parse error, returning
// a non-zero LSP range when possible. It tries *lisp.ErrorVal (parser
// errors) and *token.LocationError (scanner errors) in that order.
func parseErrorRange(err error) protocol.Range {
	// Try *lisp.ErrorVal (parser-level errors).
	var errVal *lisp.ErrorVal
	if errors.As(err, &errVal) && errVal.Source != nil && errVal.Source.Line > 0 {
		return elpsToLSPRange(errVal.Source, 1)
	}
	// Try *token.LocationError (scanner-level errors).
	var locErr *token.LocationError
	if errors.As(err, &locErr) && locErr.Source != nil && locErr.Source.Line > 0 {
		return elpsToLSPRange(locErr.Source, 1)
	}
	return protocol.Range{}
}

func strPtr(s string) *string {
	return &s
}
