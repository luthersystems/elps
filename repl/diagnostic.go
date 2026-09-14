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
	r := &diagnostic.Renderer{Color: diagnostic.ColorAuto}
	_, _ = (*lisp.ErrorVal)(lerr).WriteDiagnosticContext(ctx, w, r, "use (help 'symbol) to browse available symbols")
}
