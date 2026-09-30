// Copyright © 2026 The ELPS authors

package astutil

import (
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
)

// PackageForms returns top-level forms plus nested defun, defmacro, set and
// export forms in source order. These forms affect package bindings even when
// enclosed by lexical scopes. Quoted data and quasiquote templates are omitted.
// The returned nodes belong to the input tree; no syntax is moved or copied.
func PackageForms(exprs []*lisp.LVal) []*lisp.LVal {
	return codewalk.PackageForms(exprs)
}
