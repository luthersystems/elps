// Copyright © 2024 The ELPS authors

// Package astutil provides shared AST walking utilities for ELPS lisp values.
//
// These helpers are used by both the lint and analysis packages for
// traversing parsed ELPS expressions.
package astutil

import "github.com/luthersystems/elps/lisp"

// Walk calls fn for every node in the tree, depth-first.
// parent is nil for top-level expressions.
func Walk(exprs []*lisp.LVal, fn func(node *lisp.LVal, parent *lisp.LVal, depth int)) {
	for _, expr := range exprs {
		walkNode(expr, nil, 0, fn)
	}
}

func walkNode(node *lisp.LVal, parent *lisp.LVal, depth int, fn func(*lisp.LVal, *lisp.LVal, int)) {
	if node == nil {
		return
	}
	fn(node, parent, depth)
	// Don't recurse into quasiquote bodies — they are code-generation
	// templates where forms like (defun (unquote name) ...) are data,
	// not actual function definitions or calls.
	if node.Type == lisp.LSExpr && len(node.Cells) > 0 &&
		node.Cells[0].Type == lisp.LSymbol && node.Cells[0].Str == "quasiquote" {
		return
	}
	for _, child := range node.Cells {
		walkNode(child, node, depth+1, fn)
	}
}

// WalkSExprs calls fn for every unquoted s-expression (potential function
// call or special form) in the tree.
func WalkSExprs(exprs []*lisp.LVal, fn func(sexpr *lisp.LVal, depth int)) {
	Walk(exprs, func(node *lisp.LVal, _ *lisp.LVal, depth int) {
		if node.Type == lisp.LSExpr && !node.Quoted && len(node.Cells) > 0 {
			fn(node, depth)
		}
	})
}

// HeadSymbol returns the symbol name at the head of an s-expression, or "".
func HeadSymbol(sexpr *lisp.LVal) string {
	if sexpr.Type != lisp.LSExpr || len(sexpr.Cells) == 0 {
		return ""
	}
	head := sexpr.Cells[0]
	if head.Type == lisp.LSymbol {
		return head.Str
	}
	return ""
}

// ArgCount returns the number of arguments in an s-expression (excluding the head).
func ArgCount(sexpr *lisp.LVal) int {
	if len(sexpr.Cells) <= 1 {
		return 0
	}
	return len(sexpr.Cells) - 1
}

// UserDefined returns the set of names defined or bound in the source that
// shadow builtins. This includes:
//   - Function/macro names from defun/defmacro
//   - Parameter names from defun/defmacro/lambda formals lists
//
// The result is file-global (not scope-aware), which is conservative: it may
// suppress a valid finding but will never produce a false positive.
func UserDefined(exprs []*lisp.LVal) map[string]bool {
	defs := make(map[string]bool)
	WalkSExprs(exprs, func(sexpr *lisp.LVal, depth int) {
		head := HeadSymbol(sexpr)
		switch head {
		case "defun", "defmacro":
			if ArgCount(sexpr) >= 1 && sexpr.Cells[1].Type == lisp.LSymbol {
				defs[sexpr.Cells[1].Str] = true
			}
			// Collect parameter names from the formals list
			if ArgCount(sexpr) >= 2 {
				CollectFormals(sexpr.Cells[2], defs)
			}
		case "lambda":
			if ArgCount(sexpr) >= 1 {
				CollectFormals(sexpr.Cells[1], defs)
			}
		}
	})
	return defs
}

// CollectFormals extracts symbol names from a formals list, skipping
// &rest, &optional, and &key markers.
func CollectFormals(formals *lisp.LVal, defs map[string]bool) {
	if formals == nil || formals.Type != lisp.LSExpr {
		return
	}
	for _, sym := range formals.Cells {
		if sym.Type != lisp.LSymbol {
			continue
		}
		switch sym.Str {
		case "&rest", "&optional", "&key":
			// skip markers
		default:
			defs[sym.Str] = true
		}
	}
}

// SourceOf returns the best source location for a node.
// Prefers the node's own source, falls back to first child's source.
func SourceOf(v *lisp.LVal) *lisp.LVal {
	if v.Source != nil && v.Source.Line > 0 {
		return v
	}
	if len(v.Cells) > 0 && v.Cells[0].Source != nil {
		return v.Cells[0]
	}
	return v
}
