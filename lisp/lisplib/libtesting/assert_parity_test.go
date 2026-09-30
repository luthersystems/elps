// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestAssertMacroParity pins the assert macros' expansions, results, error
// messages and step counts.  The goldens were recorded from the hand-built
// expansions before the macros moved to elpsutil templates; they must not
// change.  "ERR:" marks an error result.
func TestAssertMacroParity(t *testing.T) {
	for _, tt := range []struct {
		program, result string
		steps           int64
	}{
		{"(macroexpand-1 '(testing:assert= (f 1) (g 2)))", "'(let ((gen00000001 (f 1)) (gen00000002 (g 2))) (assert (lisp:number? gen00000001) \"expression did not evaluate to a number\\n\\texpression: {}\\n\\t    result: {}\" \"(f 1)\" gen00000001) (assert (lisp:number? gen00000002) \"expression did not evaluate to a number\\n\\texpression: {}\\n\\t    result: {}\" \"(g 2)\" gen00000002) (assert (lisp:= gen00000001 gen00000002) \"the numeric expressions are not equal\\n\\texpression: {}\\n\\t    result: {}\\n\\t  expected: {}\" \"(g 2)\" gen00000002 gen00000001))", 3},
		{"(macroexpand-1 '(testing:assert-string= (f 1) (g 2)))", "'(let ((gen00000001 (f 1)) (gen00000002 (g 2))) (assert (lisp:string? gen00000001) \"expression did not evaluate to a string\\n\\texpression: {}\\n\\t    result: {}\" \"(f 1)\" gen00000001) (assert (lisp:string? gen00000002) \"expression did not evaluate to a string\\n\\texpression: {}\\n\\t    result: {}\" \"(g 2)\" gen00000002) (assert (lisp:string= gen00000001 gen00000002) \"the string expressions are not equal\\n\\texpression: {}\\n\\t    result: {}\\n\\t  expected: {}\" \"(g 2)\" gen00000002 gen00000001))", 3},
		{"(macroexpand-1 '(testing:assert-equal (f 1) (g 2)))", "'(let ((gen00000001 (f 1)) (gen00000002 (g 2))) (assert (lisp:equal? gen00000001 gen00000002) \"the expressions are not ``equal?''\\n\\texpression: {}\\n\\t    result: {}\\n\\t  expected: {}\" \"(g 2)\" gen00000002 gen00000001))", 3},
		{"(macroexpand-1 '(testing:assert-nil (f 1)))", "'(let ((gen00000001 (f 1))) (assert (lisp:nil? gen00000001) \"the expressions is not nil\\n\\texpression: {}\\n\\t    result: {}\" \"(f 1)\" gen00000001))", 3},
		{"(macroexpand-1 '(testing:assert-not-nil (f 1)))", "'(let ((gen00000001 (f 1))) (assert (not (lisp:nil? gen00000001)) \"the expressions is nil\\n\\texpression: {}\" \"(f 1)\"))", 3},
		{"(macroexpand-1 '(testing:assert-not (f 1)))", "'(let ((gen00000001 (f 1))) (assert (not gen00000001) \"the expressions is not falsey\\n\\texpression: {}\\n\\t    result: {}\" \"(f 1)\" gen00000001))", 3},
		{"(testing:assert= 3 (+ 1 2))", "()", 25},
		{"(testing:assert= 3 (+ 1 1))", "ERR:test:1:1: lisp:assert: the numeric expressions are not equal\n\texpression: (+ 1 1)\n\t    result: 2\n\t  expected: 3", 29},
		{"(testing:assert= \"a\" 1)", "ERR:test:1:1: lisp:assert: expression did not evaluate to a number\n\texpression: \"a\"\n\t    result: a", 14},
		{"(testing:assert-string= \"ab\" (lisp:concat 'string \"a\" \"b\"))", "()", 26},
		{"(testing:assert-string= \"ab\" \"a\")", "ERR:test:1:1: lisp:assert: the string expressions are not equal\n\texpression: \"a\"\n\t    result: a\n\t  expected: ab", 26},
		{"(testing:assert-equal '(1 2) (list 1 2))", "()", 15},
		{"(testing:assert-equal '(1 2) (list 1))", "ERR:test:1:1: lisp:assert: the expressions are not ``equal?''\n\texpression: (list 1)\n\t    result: '(1)\n\t  expected: '(1 2)", 18},
		{"(testing:assert-nil ())", "()", 10},
		{"(testing:assert-nil 1)", "ERR:test:1:1: lisp:assert: the expressions is not nil\n\texpression: 1\n\t    result: 1", 13},
		{"(testing:assert-not-nil 1)", "()", 12},
		{"(testing:assert-not-nil ())", "ERR:test:1:1: lisp:assert: the expressions is nil\n\texpression: ()", 14},
		{"(testing:assert-not false)", "()", 10},
		{"(testing:assert-not 1)", "ERR:test:1:1: lisp:assert: the expressions is not falsey\n\texpression: 1\n\t    result: 1", 13},
	} {
		t.Run(tt.program, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))))
			require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
			require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
			before := env.Runtime.TotalSteps()
			got := env.LoadString("test", tt.program)
			steps := env.Runtime.TotalSteps() - before
			result := got.String()
			if got.Type == lisp.LError {
				result = "ERR:" + lisp.GoError(got).Error()
			}
			assert.Equal(t, tt.result, result)
			assert.Equal(t, tt.steps, steps)
		})
	}
}
