// Copyright © 2026 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// Cell index the body of a definition form starts at:
// (defun name (formals) body...) and (lambda (formals) body...).
const (
	defunBodyStart  = 3
	lambdaBodyStart = 2
)

// DefunDocstring returns the docstring of a defun or defmacro form, read
// exactly the way the interpreter reads it in lisp.(*LVal).Docstring.
//
// The docstring is the whole run of consecutive leading string literals in the
// body, joined by lisp.JoinDocStrings: non-empty parts with single spaces, an
// empty string opening a new paragraph. An ELPS string literal cannot hold a
// raw line break, so a separate "" is how source spells a paragraph break --
// which is exactly how the guide teaches a "Deprecated:" paragraph is written.
// Reading only the first string would drop every paragraph after it, and the
// tooling would then disagree with `elps doc` about the same definition.
//
// A body that is nothing but strings is a constant function returning a string
// rather than a documented one, so it has no docstring.
//
// expr is only read positionally; validating that it is a definition form at
// all is the caller's job. Indexing is guarded rather than assumed: analysis
// runs over fault-tolerant parses of arbitrary bytes, where a definition can
// be truncated to any shape.
func DefunDocstring(expr *lisp.LVal) string {
	n := docstringRun(expr, defunBodyStart)
	if n == 0 {
		return ""
	}
	parts := make([]string, n)
	for i := range parts {
		parts[i] = expr.Cells[defunBodyStart+i].Str
	}
	return lisp.JoinDocStrings(parts)
}

// docstringRun returns the number of leading string literals at bodyStart that
// make up the form's docstring, and 0 when those strings are the body rather
// than documentation.
func docstringRun(expr *lisp.LVal, bodyStart int) int {
	// astutil.ArgCount is 0 for a nil or headless form, so this single guard
	// covers every truncated shape: a docstring needs a string at bodyStart
	// and at least one form after it, hence bodyStart arguments at minimum.
	if expr == nil || expr.Type != lisp.LSExpr || astutil.ArgCount(expr) < bodyStart {
		return 0
	}
	n := 0
	for i := bodyStart; i < len(expr.Cells); i++ {
		cell := expr.Cells[i]
		if cell == nil || cell.Type != lisp.LString {
			break
		}
		n++
	}
	if bodyStart+n >= len(expr.Cells) {
		// Every body form is a string: a constant function, not documentation.
		return 0
	}
	return n
}
