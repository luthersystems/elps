// Copyright © 2026 The ELPS authors

package codewalk

import (
	"github.com/luthersystems/elps/internal/codewalk/hook"
	"github.com/luthersystems/elps/lisp"
)

// Operator names carried by walker events. These constants let consumers
// select events without maintaining a separate special-form spelling table.
const (
	OpQuote           = "quote"
	OpQuasiquote      = "quasiquote"
	OpLambda          = "lambda"
	OpExpr            = "expr"
	OpDefun           = "defun"
	OpDefmacro        = "defmacro"
	OpDeftype         = "deftype"
	OpLet             = "let"
	OpLetSeq          = "let*"
	OpFlet            = "flet"
	OpLabels          = "labels"
	OpMacrolet        = "macrolet"
	OpHandlerBind     = "handler-bind"
	OpCond            = "cond"
	OpSetBang         = "set!"
	OpThreadFirst     = "thread-first"
	OpThreadLast      = "thread-last"
	OpUnquote         = "unquote"
	OpUnquoteSplicing = "unquote-splicing"
)

// SyntaxVisitor receives a raw node, its parent, the canonical operator name
// of a list's head (including template-hole markers), and its syntax depth.
// Returning false skips its children. Unlike evaluated-code events, syntax
// events include quoted data, structural lists, empty lists and every leaf.
type SyntaxVisitor = func(node, parent *lisp.LVal, op string, depth int) bool

// Syntax visits raw syntax without expansion, lexical tracking, memoization
// or a depth cap. It uses CodeWalker's operator registry, but leaves traversal
// policy to its visitor. Tools that intentionally inspect malformed forms or
// quoted structure can preserve that policy without decoding operator names.
// A visitor may recursively call Syntax to select particular children.
func Syntax(node, parent *lisp.LVal, depth int, visit SyntaxVisitor) {
	if node == nil {
		return
	}
	op := ""
	if node.Type == lisp.LSExpr && len(node.Cells) > 0 {
		op = syntaxOp(node.Cells[0])
	}
	if !visit(node, parent, op, depth) {
		return
	}
	for _, child := range node.Cells {
		Syntax(child, node, depth+1, visit)
	}
}

var syntaxOp func(*lisp.LVal) string

func init() {
	var ok bool
	syntaxOp, ok = hook.SyntaxOp.(func(*lisp.LVal) string)
	if !ok {
		panic("codewalk: lisp did not inject the syntax visitor")
	}
}
