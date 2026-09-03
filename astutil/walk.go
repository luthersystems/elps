// Copyright © 2024 The ELPS authors

// Package astutil provides shared AST walking utilities for ELPS lisp values.
//
// These helpers are used by both the lint and analysis packages for
// traversing parsed ELPS expressions.
package astutil

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

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
		if node.Type == lisp.LSExpr && !node.IsQuoted() && len(node.Cells) > 0 {
			fn(node, depth)
		}
	})
}

// HeadSymbol returns the symbol name at the head of an s-expression, or "".
// A nil sexpr yields "", so it is safe to call on the nil parent Walk passes
// for top-level expressions.
func HeadSymbol(sexpr *lisp.LVal) string {
	if sexpr == nil {
		return ""
	}
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
// A nil sexpr yields 0, so it is safe to call on the nil parent Walk passes for
// top-level expressions.
func ArgCount(sexpr *lisp.LVal) int {
	if sexpr == nil {
		return 0
	}
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

// PackageNameArg extracts a package name from a use-package or in-package
// argument. Handles quoted symbols ('testing), bare symbols (testing), and
// strings ("testing").
func PackageNameArg(arg *lisp.LVal) string {
	if arg == nil {
		return ""
	}
	if arg.Type == lisp.LString || arg.Type == lisp.LSymbol {
		return arg.Str
	}
	if arg.Type == lisp.LSExpr && arg.IsQuoted() && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

// SourceLoc returns v's source location as a pointer, or nil when v is nil
// or carries no location.  The pointer refers to a private copy — mutating
// it never affects v or any other LVal (lisp.LVal exposes locations by value
// only; see issue #362).
func SourceLoc(v *lisp.LVal) *token.Location {
	if v == nil {
		return nil
	}
	if loc, ok := v.Source(); ok {
		return &loc
	}
	return nil
}

// SymbolLoc returns the location of the NAME a node is written with, which is
// not always the node's own span.
//
// Two shapes carry a name inside a wider form.  The first is a quoted symbol:
//
// rdparser gives the whole 'x form a single node: lisp.Quote copies the symbol
// and sets its quoted flag rather than wrapping it, so no node stands for the
// quote, and applyPrefixLocation then moves the surviving node's start back
// onto the ' so that the form reports the position a reader would point at.
// That start is right for the FORM and wrong for the NAME, and a consumer that
// wants to point at, highlight, or REPLACE the identifier needs the latter:
// textDocument/rename built its edit ranges from the form span and so replaced
// the quote along with the name, turning (set 'x 1) into (set new 1) -- a
// different program, applied to the user's file unread (elps#577).
//
// The end of the span is the name's end already (ParseQuote inherits it from
// the operand), so the name is recovered by measuring len(v.Str) BACK from it
// rather than by counting ' characters forward.  That is exact whatever sits
// in the gap -- "' x", a newline, a preserved comment -- and it is a no-op on
// an unquoted symbol, whose span is its name.
//
// It never widens a span and never moves one it cannot account for: a node
// whose recorded end is missing, or whose name does not fit inside its own
// span, is returned untouched.
//
// This is NOT elps#463.  That was a WIDTH in the wrong unit (token.TokenEnd
// counted EndCol one per rune onto a byte-valued Col); this is a start that is
// one reader-prefix too far left, and it is wrong by the same byte for "'x" as
// for "'é".
//
// The second shape is a STRING LITERAL used as a name, which some def-like
// forms take: (s:deftype "myint" ...) binds a global called myint, and the
// node analysis records for it is the literal.  Its span covers the quotes, so
// a rename built from it replaced them too and produced (s:deftype NEW ...) --
// a bare symbol where the form requires a string.  Here the name is the
// literal's INTERIOR, so both ends move in by one delimiter.
//
// A string is only handled when its raw span is exactly the decoded value plus
// two delimiter bytes on one line.  Anything else -- an escape, a raw-string
// form, a line break inside -- means the interior is not recoverable by
// arithmetic on the length, and the span is returned untouched rather than
// guessed at.
func SymbolLoc(v *lisp.LVal) *token.Location {
	loc := SourceLoc(v)
	if loc == nil {
		return loc
	}
	switch {
	case v.Type == lisp.LSymbol && v.IsQuoted():
		return quotedSymbolNameLoc(loc, len(v.Str))
	case v.Type == lisp.LString:
		return stringLiteralNameLoc(loc, len(v.Str))
	}
	return loc
}

// quotedSymbolNameLoc narrows a quoted symbol's span onto its name by
// measuring the name back from the end.
func quotedSymbolNameLoc(loc *token.Location, n int) *token.Location {
	if n == 0 || loc.EndLine <= 0 || loc.EndCol <= 0 {
		return loc
	}
	if loc.EndCol-n < 1 || loc.EndPos-n < loc.Pos {
		return loc
	}
	loc.Line = loc.EndLine
	loc.Col = loc.EndCol - n
	loc.Pos = loc.EndPos - n
	return loc
}

// stringLiteralNameLoc narrows a string literal's span onto the text between
// its delimiters, and only when the arithmetic is exact.
func stringLiteralNameLoc(loc *token.Location, n int) *token.Location {
	if loc.EndLine != loc.Line || loc.Col < 1 || loc.Pos < 0 {
		return loc
	}
	// The span covers the raw literal; n counts the DECODED value.  They
	// differ by exactly the two delimiters only for a plain one-line literal
	// with nothing escaped, which is the only case this can narrow safely.
	if loc.EndPos-loc.Pos != n+2 || loc.EndCol-loc.Col != n+2 {
		return loc
	}
	loc.Col++
	loc.Pos++
	loc.EndCol--
	loc.EndPos--
	return loc
}

// SourceOf returns the best source location for a node.
// Prefers the node's own source, falls back to first child's source.
// Returns nil for a nil node, so it is safe to call on the nil parent Walk
// passes for top-level expressions.
func SourceOf(v *lisp.LVal) *lisp.LVal {
	if v == nil {
		return nil
	}
	if loc, ok := v.Source(); ok && loc.Line > 0 {
		return v
	}
	if len(v.Cells) > 0 {
		if _, ok := v.Cells[0].Source(); ok {
			return v.Cells[0]
		}
	}
	return v
}
