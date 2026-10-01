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
// An optional stop flag ends the walk when the visitor sets it to true.
func Syntax(node, parent *lisp.LVal, depth int, visit SyntaxVisitor, stop ...*bool) {
	var stopped *bool
	if len(stop) > 0 {
		stopped = stop[0]
	}
	syntax(node, parent, depth, visit, stopped, nil)
}

func syntax(node, parent *lisp.LVal, depth int, visit SyntaxVisitor, stop *bool, calls *callVisitor) {
	if node == nil || (stop != nil && *stop) {
		return
	}
	if calls != nil {
		v := node
		visit, formals := calls.visit, calls.formals
		if v.IsQuoted() || v.Type != lisp.LSExpr || len(v.Cells) == 0 {
			return
		}
		op, policy := syntaxCall(v.Cells[0])
		if op == OpQuote || op == OpQuasiquote {
			return
		}
		if visit != nil {
			visit(v)
		}
		start := 0
		if policy != nil {
			if formals != nil {
				if i := policy.FormalsIndex; i > 0 && len(v.Cells) > i {
					emitCallFormals(formals, v, v.Cells[i], nil, op, depth+1, policy.Role)
				}
				if policy.BindingFormals && len(v.Cells) > 1 && v.Cells[1].Type == lisp.LSExpr {
					for _, binding := range v.Cells[1].Cells {
						if binding.Type == lisp.LSExpr && len(binding.Cells) > 1 {
							emitCallFormals(formals, v, binding.Cells[1], binding, op, depth+2, policy.Role)
						}
					}
				}
			}
			if policy.BindingStart > 0 && len(v.Cells) > 1 && v.Cells[1].Type == lisp.LSExpr {
				for _, binding := range v.Cells[1].Cells {
					if binding.Type == lisp.LSExpr {
						for _, child := range binding.Cells[min(policy.BindingStart, len(binding.Cells)):] {
							syntax(child, v, depth+3, nil, nil, calls)
						}
					}
				}
			}
			if policy.Clauses {
				for _, clause := range v.Cells[1:] {
					if clause.Type == lisp.LSExpr {
						for _, child := range clause.Cells {
							syntax(child, v, depth+2, nil, nil, calls)
						}
					}
				}
				return
			}
			start = policy.CallsStart
		}
		for _, child := range v.Cells[min(start, len(v.Cells)):] {
			syntax(child, v, depth+1, nil, nil, calls)
		}
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
		if stop != nil && *stop {
			return
		}
		syntax(child, node, depth+1, visit, stop, nil)
	}
}

// Operator returns the canonical operator name of form's head, as Syntax
// reports it, or "" for anything that is not a classified form.
func Operator(form *lisp.LVal) string {
	if form == nil || form.Type != lisp.LSExpr || len(form.Cells) == 0 {
		return ""
	}
	return syntaxOp(form.Cells[0])
}

// IsOp reports whether form's head is the operator op, spelled bare or
// lisp:-qualified. op must be one of the Op constants, which init checks
// against CodeWalker's registry; this is the cheap test for hot walks that
// only ask about one operator.
func IsOp(form *lisp.LVal, op string) bool {
	if form == nil || len(form.Cells) == 0 || form.Type != lisp.LSExpr {
		return false
	}
	head := form.Cells[0]
	if head.Type != lisp.LSymbol {
		return false
	}
	s := head.Str
	return s == op || (len(s) == len(langPrefix)+len(op) && s[len(langPrefix):] == op && s[:len(langPrefix)] == langPrefix && op != OpUnquote && op != OpUnquoteSplicing)
}

const langPrefix = lisp.DefaultLangPackage + ":"

var syntaxOp func(*lisp.LVal) string

func init() {
	var ok bool
	syntaxOp, ok = hook.SyntaxOp.(func(*lisp.LVal) string)
	if !ok {
		panic("codewalk: lisp did not inject the syntax visitor")
	}
	for _, op := range []string{OpQuote, OpQuasiquote, OpLambda, OpExpr, OpDefun, OpDefmacro, OpDeftype,
		OpLet, OpLetSeq, OpFlet, OpLabels, OpMacrolet, OpHandlerBind, OpCond, OpSetBang,
		OpThreadFirst, OpThreadLast, OpUnquote, OpUnquoteSplicing} {
		if syntaxOp(lisp.Symbol(op)) != op {
			panic("codewalk: " + op + " is not a CodeWalker syntax operator")
		}
	}
}
