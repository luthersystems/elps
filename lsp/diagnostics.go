// Copyright © 2024 The ELPS authors

package lsp

import (
	"time"

	"github.com/luthersystems/elps/lint"
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

	doc := s.docs.Get(params.TextDocument.URI)
	if doc != nil {
		s.analyzeAndPublish(doc)
	}
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
	s.ensureAnalysis(doc)

	// Snapshot document fields under the lock.
	doc.mu.Lock()
	parseErr := doc.parseErr
	content := doc.Content
	docAnalysis := doc.analysis
	uri := doc.URI
	doc.mu.Unlock()

	var diags []protocol.Diagnostic

	// Report parse errors as diagnostics.
	if parseErr != nil {
		diags = append(diags, protocol.Diagnostic{
			Range:    protocol.Range{},
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
	sev := mapLintSeverity(d.Severity)
	return protocol.Diagnostic{
		Range: protocol.Range{
			Start: protocol.Position{Line: safeUint(line), Character: safeUint(col)},
			End:   protocol.Position{Line: safeUint(line), Character: safeUint(col)},
		},
		Severity: &sev,
		Source:   strPtr("elps-lint"),
		Code:     &protocol.IntegerOrString{Value: d.Analyzer},
		Message:  d.Message,
	}
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

func strPtr(s string) *string {
	return &s
}
