// Copyright © 2026 The ELPS authors

package astutil

import "github.com/luthersystems/elps/lisp"

// PackageForms returns top-level forms plus nested defun, defmacro, set and
// export forms in source order. These forms affect package bindings even when
// enclosed by lexical scopes. Quoted data and quasiquote templates are omitted.
// The returned nodes belong to the input tree; no syntax is moved or copied.
func PackageForms(exprs []*lisp.LVal) []*lisp.LVal {
	var forms []*lisp.LVal
	var walk func(*lisp.LVal, bool)
	walk = func(node *lisp.LVal, top bool) {
		if node == nil || node.Type != lisp.LSExpr || node.IsQuoted() || len(node.Cells) == 0 {
			return
		}
		head := HeadSymbol(node)
		if head == "quote" || head == "lisp:quote" || head == "quasiquote" || head == "lisp:quasiquote" {
			return
		}
		if top || head == "defun" || head == "defmacro" || head == "set" || head == "export" || head == "lisp:export" {
			forms = append(forms, node)
		}
		// Binding specifications are syntax even when written with [].
		// Their initializer/body expressions can create package bindings.
		switch head {
		case "let", "let*", "flet", "labels", "macrolet", "handler-bind":
			if len(node.Cells) > 1 {
				start := 1
				if head == "flet" || head == "labels" || head == "macrolet" {
					start = 2
				}
				for _, binding := range node.Cells[1].Cells {
					for i := start; i < len(binding.Cells); i++ {
						walk(binding.Cells[i], false)
					}
				}
			}
			for i := 2; i < len(node.Cells); i++ {
				walk(node.Cells[i], false)
			}
			return
		}
		start := 0
		switch head {
		case "defun", "defmacro", "deftype":
			start = 3
		case "lambda":
			start = 2
		}
		for i := start; i < len(node.Cells); i++ {
			walk(node.Cells[i], false)
		}
	}
	for _, expr := range exprs {
		walk(expr, true)
	}
	return forms
}
