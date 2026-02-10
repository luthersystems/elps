// Copyright © 2024 The ELPS authors

package lint

import (
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// Walk calls fn for every node in the tree, depth-first.
// parent is nil for top-level expressions.
func Walk(exprs []*lisp.LVal, fn func(node *lisp.LVal, parent *lisp.LVal, depth int)) {
	astutil.Walk(exprs, fn)
}

// WalkSExprs calls fn for every unquoted s-expression (potential function
// call or special form) in the tree.
func WalkSExprs(exprs []*lisp.LVal, fn func(sexpr *lisp.LVal, depth int)) {
	astutil.WalkSExprs(exprs, fn)
}

// HeadSymbol returns the symbol name at the head of an s-expression, or "".
func HeadSymbol(sexpr *lisp.LVal) string {
	return astutil.HeadSymbol(sexpr)
}

// ArgCount returns the number of arguments in an s-expression (excluding the head).
func ArgCount(sexpr *lisp.LVal) int {
	return astutil.ArgCount(sexpr)
}

// UserDefined returns the set of names defined or bound in the source that
// shadow builtins. This includes:
//   - Function/macro names from defun/defmacro
//   - Parameter names from defun/defmacro/lambda formals lists
//
// The result is file-global (not scope-aware), which is conservative: it may
// suppress a valid finding but will never produce a false positive.
func UserDefined(exprs []*lisp.LVal) map[string]bool {
	return astutil.UserDefined(exprs)
}

// SourceOf returns the best source location for a node.
// Prefers the node's own source, falls back to first child's source.
func SourceOf(v *lisp.LVal) *lisp.LVal {
	return astutil.SourceOf(v)
}
