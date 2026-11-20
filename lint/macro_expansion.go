// Copyright © 2026 The ELPS authors

package lint

import (
	"github.com/luthersystems/elps/lisp"
)

// AnalyzerMacroQuotedHead warns when a macro builds its expansion with list
// or cons and a quoted symbol as the head, as in (list '+ a b).  A symbol
// written 'x keeps its quote, so the expansion evaluates to data or fails
// with "first element of expression is not a function" (elps#999).
var AnalyzerMacroQuotedHead = &Analyzer{
	Name:     "macro-quoted-head",
	Severity: SeverityWarning,
	Doc: "Warn when a macro builds a form of its expansion with list or cons and a quoted symbol head.\n\n" +
		"A symbol written 'x or (quote x) stays quoted inside a list made by list or cons. " +
		"When a macro returns (list '+ a b), the expansion head is the quoted symbol '+, " +
		"so evaluation fails with \"first element of expression is not a function: '+\". " +
		"(cons 'progn body) inserted with unquote evaluates to data with no error. " +
		"Build the expansion with quasiquote instead: (quasiquote (+ (unquote a) (unquote b))). " +
		"The check follows the macro's result through let, progn, if, cond, when and unless, " +
		"the arguments of list, and unquote inside quasiquote. It cannot see a head passed in " +
		"a variable or built by a helper function. Suppress with ; nolint:macro-quoted-head.",
	Run: func(pass *Pass) error {
		userDefs := UserDefined(pass.Exprs)
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, _ int) {
			if sexpr.IsQuoted() || unqualifiedLispName(HeadSymbol(sexpr)) != "defmacro" || userDefs["defmacro"] {
				return
			}
			body := macroBody(sexpr)
			if len(body) == 0 {
				return
			}
			w := macroResultWalker{pass: pass, userDefs: userDefs}
			w.result(body[len(body)-1])
		})
		return nil
	},
}

// macroBody returns the body forms of a defmacro: the cells after the name,
// the formals and an optional docstring.
func macroBody(sexpr *lisp.LVal) []*lisp.LVal {
	if len(sexpr.Cells) < 4 {
		return nil
	}
	body := sexpr.Cells[3:]
	if len(body) > 1 && body[0].Type == lisp.LString {
		body = body[1:]
	}
	return body
}

type macroResultWalker struct {
	pass     *Pass
	userDefs map[string]bool
}

// head returns the core form a call names, or "" when the call is quoted,
// not a call, or names a function the file defines itself.
func (w macroResultWalker) head(v *lisp.LVal) string {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() || len(v.Cells) == 0 {
		return ""
	}
	name := HeadSymbol(v)
	if name == "" || w.userDefs[name] {
		return ""
	}
	return unqualifiedLispName(name)
}

// result walks a form whose value becomes a form of the expansion.
func (w macroResultWalker) result(v *lisp.LVal) {
	switch w.head(v) {
	case "progn", "let", "let*", "when", "unless":
		if len(v.Cells) > 1 {
			w.result(v.Cells[len(v.Cells)-1])
		}
	case "if":
		for _, branch := range v.Cells[min(2, len(v.Cells)):] {
			w.result(branch)
		}
	case "cond":
		for _, clause := range v.Cells[1:] {
			if clause.Type == lisp.LSExpr && len(clause.Cells) > 1 {
				w.result(clause.Cells[len(clause.Cells)-1])
			}
		}
	case "list":
		if len(v.Cells) > 1 {
			w.checkHead(v, v.Cells[1])
		}
		for _, arg := range v.Cells[min(2, len(v.Cells)):] {
			w.result(arg)
		}
	case "cons":
		if len(v.Cells) > 1 {
			w.checkHead(v, v.Cells[1])
		}
	case "quasiquote":
		if len(v.Cells) > 1 {
			w.template(v.Cells[1])
		}
	}
}

// template walks a quasiquote template for the forms unquote inserts.
// A nested quasiquote is left alone: its unquotes belong to a later
// expansion.
func (w macroResultWalker) template(v *lisp.LVal) {
	if v == nil || v.Type != lisp.LSExpr || len(v.Cells) == 0 {
		return
	}
	switch unqualifiedLispName(HeadSymbol(v)) {
	case "unquote":
		if len(v.Cells) > 1 {
			w.result(v.Cells[1])
		}
		return
	case "unquote-splicing":
		// The spliced value is a list of forms, so the forms are the
		// elements of a (list ...) argument, not the list itself.
		if len(v.Cells) > 1 && w.head(v.Cells[1]) == "list" {
			for _, arg := range v.Cells[1].Cells[1:] {
				w.result(arg)
			}
		}
		return
	case "quasiquote":
		return
	}
	for _, c := range v.Cells {
		w.template(c)
	}
}

// checkHead reports call when head, the first element it builds, is a
// quoted symbol.
func (w macroResultWalker) checkHead(call, head *lisp.LVal) {
	name, ok := quotedSymbol(head)
	if !ok {
		return
	}
	op := HeadSymbol(call)
	w.pass.ReportNode(head,
		"macro expansion built with %s has the quoted head '%s, so it evaluates as data, not a call to %s; build it with quasiquote",
		op, name, name)
}

// quotedSymbol reports whether v is 'sym or (quote sym), and returns sym.
func quotedSymbol(v *lisp.LVal) (string, bool) {
	if v == nil {
		return "", false
	}
	if v.Type == lisp.LSymbol && v.IsQuoted() {
		return v.Str, true
	}
	if v.Type == lisp.LSExpr && !v.IsQuoted() && len(v.Cells) == 2 &&
		unqualifiedLispName(HeadSymbol(v)) == "quote" &&
		v.Cells[1].Type == lisp.LSymbol && !v.Cells[1].IsQuoted() {
		return v.Cells[1].Str, true
	}
	return "", false
}
