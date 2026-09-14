// Copyright © 2024 The ELPS authors

package cmd

import (
	"context"
	"io"
	"os"

	"github.com/luthersystems/elps/diagnostic"
	lintpkg "github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
)

func colorMode() diagnostic.ColorMode {
	switch colorFlag {
	case "always":
		return diagnostic.ColorAlways
	case "never":
		return diagnostic.ColorNever
	default:
		return diagnostic.ColorAuto
	}
}

func newRenderer() *diagnostic.Renderer {
	return &diagnostic.Renderer{Color: colorMode()}
}

// lintDiagToDiagnostic converts a lint.Diagnostic to a diagnostic.Diagnostic.
func lintDiagToDiagnostic(ld lintpkg.Diagnostic) diagnostic.Diagnostic {
	sev := diagnostic.SeverityWarning // fallback
	switch ld.Severity {
	case lintpkg.SeverityError:
		sev = diagnostic.SeverityError
	case lintpkg.SeverityWarning:
		sev = diagnostic.SeverityWarning
	case lintpkg.SeverityInfo:
		sev = diagnostic.SeverityNote
	}
	d := diagnostic.Diagnostic{
		Severity: sev,
		Message:  ld.Message + " (" + ld.Analyzer + ")",
	}
	if ld.Pos.Line > 0 {
		d.Spans = append(d.Spans, diagnostic.Span{
			File: ld.Pos.File,
			Line: ld.Pos.Line,
			Col:  ld.Pos.Col,
		})
	}
	d.Notes = append(d.Notes, ld.Notes...)
	d.Notes = append(d.Notes, "to suppress: add \"; nolint:"+ld.Analyzer+"\" as a comment on this line")
	return d
}

// renderLispError renders a lisp error with diagnostic formatting to stderr.
// If sourceFile is non-empty, a hint to run elps lint is appended.
func renderLispError(lerr *lisp.LVal, sourceFiles ...string) {
	renderLispErrorContext(nil, lerr, sourceFiles...)
}

func renderLispErrorContext(ctx context.Context, lerr *lisp.LVal, sourceFiles ...string) {
	renderLispErrorTo(ctx, os.Stderr, lerr, sourceFiles...)
}

func renderLispErrorTo(ctx context.Context, w io.Writer, lerr *lisp.LVal, sourceFiles ...string) {
	var hint []string
	if len(sourceFiles) > 0 && sourceFiles[0] != "" {
		hint = []string{"try: elps lint ", sourceFiles[0]}
	}
	_, _ = (*lisp.ErrorVal)(lerr).WriteDiagnosticContext(ctx, w, newRenderer(), hint...)
}

// renderLintDiagnostics renders lint diagnostics with diagnostic formatting to stderr.
func renderLintDiagnostics(diags []lintpkg.Diagnostic) {
	var ds []diagnostic.Diagnostic
	for _, ld := range diags {
		ds = append(ds, lintDiagToDiagnostic(ld))
	}
	r := newRenderer()
	_ = r.RenderAll(os.Stderr, ds)
}
