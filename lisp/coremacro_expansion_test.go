// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// dumpForm prints v with each node's type and quoting, and marks the ()
// singleton and functions, so two forms that print the same but differ in
// structure dump differently.
func dumpForm(v *lisp.LVal) string {
	var b strings.Builder
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		b.WriteString(v.Type.String())
		if v.IsQuoted() {
			b.WriteString("'")
		}
		if v == lisp.Nil() {
			b.WriteString("!")
		}
		switch v.Type {
		case lisp.LSExpr, lisp.LQuote:
			b.WriteString("(")
			for i, c := range v.Cells {
				if i > 0 {
					b.WriteString(" ")
				}
				walk(c)
			}
			b.WriteString(")")
		case lisp.LFun:
			b.WriteString("<fun>")
		default:
			b.WriteString(":" + v.String())
		}
	}
	walk(v)
	return b.String()
}

// TestCoreMacroExpansions pins the expansions of the core Go macros, node by
// node, as the hand-built Go made them before they were written as form
// templates.
func TestCoreMacroExpansions(t *testing.T) {
	for _, tt := range []struct{ name, form, dump string }{
		{"defun", `(defun f (x) (+ x 1))`, `list'(symbol:lisp:progn list(symbol:lisp:set symbol':'f function<fun>) list!())`},
		{"defmacro", `(defmacro m (x) x)`, `list'(symbol:lisp:progn list(symbol:lisp:set symbol':'m function<fun>) list!())`},
		{"defconst", `(defconst c 1)`, `list'(symbol:lisp:progn list(symbol:lisp:set symbol':'c int:1) list(symbol:lisp:export symbol':'c) list!())`},
		{"defconst-doc", `(defconst c 1 "a" "b")`, `list'(symbol:lisp:progn list(symbol:lisp:set symbol':'c int:1 string:"a" string:"b") list(symbol:lisp:export symbol':'c) list!())`},
		{"curry-function", `(curry-function f 1 2)`, `list'(symbol:lambda list(symbol:&rest symbol:gen00000001) list(symbol:lisp:apply symbol:f int:1 int:2 symbol:gen00000001))`},
		{"curry-function-noargs", `(curry-function f)`, `list'(symbol:lambda list(symbol:&rest symbol:gen00000001) list(symbol:lisp:apply symbol:f symbol:gen00000001))`},
		{"get-default", `(get-default m k d)`, `list'(symbol:lisp:let list(list(symbol:map@1@1 symbol:m) list(symbol:key@1@2 symbol:k)) list(symbol:lisp:if list(symbol:lisp:if list(symbol:lisp:nil? symbol:map@1@1) symbol:lisp:false list(symbol:lisp:key? symbol:map@1@1 symbol:key@1@2)) list(symbol:lisp:get symbol:map@1@1 symbol:key@1@2) symbol:d))`},
		{"trace", `(trace (+ 1 2))`, `list'(symbol:lisp:let list(list(symbol:gen00000001 list(symbol:+ int:1 int:2))) list(symbol:lisp:debug-print string:"TRACE" symbol:gen00000001) symbol:gen00000001)`},
		{"trace-message", `(trace (+ 1 2) "here")`, `list'(symbol:lisp:let list(list(symbol:gen00000001 list(symbol:+ int:1 int:2))) list(symbol:lisp:debug-print string:"here" symbol:gen00000001) symbol:gen00000001)`},
	} {
		t.Run(tt.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
			got := env.LoadString("expand.lisp", "(macroexpand-1 '"+tt.form+")")
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, tt.dump, dumpForm(got))
		})
	}
}
