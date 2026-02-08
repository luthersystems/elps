// Copyright © 2024 The ELPS authors

package cmd

import (
	"os"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/lisp"
	lintpkg "github.com/luthersystems/elps/lint"
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

// lispErrorToDiagnostic converts an LError value to a Diagnostic for display.
func lispErrorToDiagnostic(lerr *lisp.LVal) diagnostic.Diagnostic {
	ev := (*lisp.ErrorVal)(lerr)
	d := diagnostic.Diagnostic{
		Severity: diagnostic.SeverityError,
		Message:  ev.ErrorMessage(),
	}

	// Add the function context to the message if available
	fname := ev.FunName()
	if fname != "" {
		d.Message = fname + ": " + d.Message
	}
	if lerr.Str != "" && lerr.Str != "error" {
		d.Message = lerr.Str + ": " + d.Message
	}

	// Add source span if available
	if lerr.Source != nil && lerr.Source.Pos >= 0 {
		span := diagnostic.Span{
			File: lerr.Source.File,
			Line: lerr.Source.Line,
			Col:  lerr.Source.Col,
		}
		// Prefer physical path for reading source
		if lerr.Source.Path != "" {
			span.File = lerr.Source.Path
		}
		d.Spans = append(d.Spans, span)
	}

	// Add stack trace frames as notes
	stack := lerr.CallStack()
	if stack != nil {
		for i := len(stack.Frames) - 1; i >= 0; i-- {
			frame := &stack.Frames[i]
			name := frame.QualifiedFunName(lisp.DefaultUserPackage)
			if name == "" {
				continue
			}
			loc := "unknown"
			if frame.Source != nil {
				loc = frame.Source.String()
			}
			d.Notes = append(d.Notes, "in "+name+" at "+loc)
		}
	}

	return d
}

// lintDiagToDiagnostic converts a lint.Diagnostic to a diagnostic.Diagnostic.
func lintDiagToDiagnostic(ld lintpkg.Diagnostic) diagnostic.Diagnostic {
	d := diagnostic.Diagnostic{
		Severity: diagnostic.SeverityWarning,
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
	d := lispErrorToDiagnostic(lerr)
	if len(sourceFiles) > 0 && sourceFiles[0] != "" {
		d.Notes = append(d.Notes, "try: elps lint "+sourceFiles[0])
	}
	r := newRenderer()
	_ = r.Render(os.Stderr, d)
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
