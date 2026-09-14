// Copyright © 2024 The ELPS authors

package repl

import (
	"context"
	"io"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/lisp"
)

// renderError renders a lisp error using the diagnostic renderer for
// Rust-style annotated output. For REPL errors, source snippets may not
// be available (input comes from stdin, not files), but the renderer
// degrades gracefully to show just the location and error message.
func renderError(w io.Writer, lerr *lisp.LVal) {
	renderErrorContext(nil, w, lerr)
}

func renderErrorContext(ctx context.Context, w io.Writer, lerr *lisp.LVal) {
	d := lispErrorToDiagContext(ctx, lerr)
	d.Notes = append(d.Notes, "use (help 'symbol) to browse available symbols")
	r := &diagnostic.Renderer{Color: diagnostic.ColorAuto}
	_ = r.Render(w, d)
}

// lispErrorToDiag converts an LError value to a Diagnostic for display.
func lispErrorToDiag(lerr *lisp.LVal) diagnostic.Diagnostic {
	return lispErrorToDiagContext(nil, lerr)
}

func lispErrorToDiagContext(ctx context.Context, lerr *lisp.LVal) diagnostic.Diagnostic {
	ev := (*lisp.ErrorVal)(lerr)
	d := diagnostic.Diagnostic{
		Severity: diagnostic.SeverityError,
		Message:  ev.ErrorMessageContext(ctx),
	}

	if ctx != nil && ctx.Err() != nil {
		return d
	}

	fname := ev.FunName()
	if fname != "" {
		d.Message = fname + ": " + d.Message
	}
	if lerr.Str != "" && lerr.Str != "error" {
		d.Message = lerr.Str + ": " + d.Message
	}

	if loc, ok := lerr.Source(); ok && loc.Pos >= 0 {
		span := diagnostic.Span{
			File: loc.File,
			Line: loc.Line,
			Col:  loc.Col,
		}
		if loc.Path != "" {
			span.File = loc.Path
		}
		d.Spans = append(d.Spans, span)
	}

	stack := lerr.CallStack()
	if stack != nil {
		for i := len(stack.Frames) - 1; i >= 0; i-- {
			if ctx != nil && ctx.Err() != nil {
				d.Notes = append(d.Notes, "#<truncated>")
				break
			}
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
