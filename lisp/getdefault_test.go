// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"regexp"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestGetDefaultParity pins the result and the evaluation cost of
// get-default.  The results were recorded while get-default was a macro and
// must not change.  The steps are those of the special operator
// (luthersystems/elps#944): the form, its map and key expressions, and the
// default expression when it runs.  The macro charged the steps in the
// comments, for its expansion's let, two ifs, three calls and their
// arguments.  Substrate counts steps, so a step count here may only go down.
func TestGetDefaultParity(t *testing.T) {
	for _, tt := range []struct {
		name, program, result string
		steps                 int64
	}{
		{"hit", `(get-default (sorted-map "k" 7) "k" 42)`, `7`, 7},                                                                                   // macro: 24
		{"miss", `(get-default (sorted-map "other" 7) "k" 42)`, `42`, 8},                                                                             // macro: 21
		{"nil map", `(get-default () "k" 42)`, `42`, 5},                                                                                              // macro: 15
		{"non-map", `(handler-bind ((condition (lambda (&rest _) 'caught))) (get-default 1 "k" 42))`, `'caught`, 10},                                 // macro: 23
		{"nested default", `(get-default (sorted-map) "k" (get-default () "inner" 42))`, `42`, 10},                                                   // macro: 33
		{"default side effects", `(let ((calls 0)) (list (get-default (sorted-map) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(42 1)`, 20}, // macro: 33
		{"unused default", `(let ((calls 0)) (list (get-default (sorted-map "k" 7) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(7 0)`, 13},  // macro: 30
		// A special operator has no expansion, as when and default have none.
		{"macroexpand", `(macroexpand-1 '(get-default m k d))`, `'(get-default m k d)`, 3},
	} {
		t.Run(tt.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithMaxSteps(1000000))))
			before := env.Runtime.TotalSteps()
			got := env.LoadString("get-default.lisp", tt.program)
			steps := env.Runtime.TotalSteps() - before
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, tt.result, got.String())
			assert.Equal(t, tt.steps, steps)
		})
	}
}

// getDefaultExpansion is the form the get-default macro expanded to before
// it became a special operator (luthersystems/elps#944), with fixed names in
// place of the generated ones.
func getDefaultExpansion(m, k, d string) string {
	return `(lisp:let ((gd-map ` + m + `) (gd-key ` + k + `))
		(lisp:if (lisp:if (lisp:nil? gd-map) lisp:false (lisp:key? gd-map gd-key))
			(lisp:get gd-map gd-key)
			` + d + `))`
}

// TestGetDefaultMatchesMacroExpansion runs the operator and the old macro's
// expansion in two fresh environments and compares what a program can see:
// the result, the order of the side effects, the error (condition, message
// and the function name in front of the message), and the name of a lambda
// made afterwards.  That name draws from the environment ID counter, which
// the expansion's let advanced.
func TestGetDefaultMatchesMacroExpansion(t *testing.T) {
	const prelude = `
		(set 'log ())
		(defun note (tag v) (set! log (cons tag log)) v)
		(set 'm (sorted-map "k" 7 'sym 8 3 9 "nil" () "false" false))`
	for _, args := range [][3]string{
		{`m`, `"k"`, `(note 'd 42)`},
		{`m`, `"absent"`, `(note 'd 42)`},
		{`m`, `'k`, `42`},
		{`m`, `"sym"`, `42`},
		{`m`, `:sym`, `42`},
		{`m`, `3`, `42`},
		{`m`, `"3"`, `42`},
		{`m`, `"nil"`, `42`},
		{`m`, `"false"`, `42`},
		{`(note 'm m)`, `(note 'k "k")`, `(note 'd 42)`},
		{`(note 'm m)`, `(note 'k "x")`, `(note 'd 42)`},
		{`(note 'm ())`, `(note 'k "x")`, `(note 'd 42)`},
		{`()`, `1.5`, `42`},
		{`()`, `(list 1)`, `42`},
		{`(sorted-map)`, `"k"`, `(error 'custom "from default")`},
		{`(sorted-map)`, `"k"`, `(get-default m "k" 0)`},
		{`m`, `1.5`, `(note 'd 42)`},
		{`m`, `(list 1)`, `(note 'd 42)`},
		{`m`, `()`, `(note 'd 42)`},
		{`m`, `true`, `(note 'd 42)`},
		{`5`, `"k"`, `(note 'd 42)`},
		{`"text"`, `0`, `(note 'd 42)`},
		{`(vector 1 2)`, `0`, `(note 'd 42)`},
		{`(list 1 2)`, `0`, `(note 'd 42)`},
		{`(lambda () 1)`, `0`, `(note 'd 42)`},
		{`(note 'm (error 'custom "from map"))`, `(note 'k 1)`, `(note 'd 42)`},
		{`(note 'm m)`, `(note 'k (error 'custom "from key"))`, `(note 'd 42)`},
		{`(progn (lambda () 1) m)`, `"k"`, `42`},
	} {
		t.Run(args[0]+" "+args[1]+" "+args[2], func(t *testing.T) {
			// run returns the result, the Go error text, the side-effect
			// log and the name of a lambda made afterwards.
			run := func(form string) [4]string {
				env := newGenSymsEnv(t)
				require.NoError(t, lisp.GoError(env.LoadString("prelude.lisp", prelude)))
				got := env.LoadString("form.lisp", form)
				goErr := ""
				if err := lisp.GoError(got); err != nil {
					goErr = err.Error()
				}
				fn := env.LoadString("after.lisp", `(lambda () 1)`)
				require.NoError(t, lisp.GoError(fn))
				// The hand-written expansion sits at other positions than
				// the call form, so drop the position from the error.
				position := regexp.MustCompile(`^form\.lisp:\d+:\d+: `)
				return [4]string{
					position.ReplaceAllString(got.String(), ""), position.ReplaceAllString(goErr, ""),
					env.LoadString("log.lisp", `log`).String(), fn.FID(),
				}
			}
			call := `(get-default ` + args[0] + ` ` + args[1] + ` ` + args[2] + `)`
			want := run(getDefaultExpansion(args[0], args[1], args[2]))
			got := run(call)
			// The lowering (macroexpand-all :lower true) must evaluate as
			// the macro's expansion did, so it is the third column.
			low := run(`(eval (macroexpand-all '` + call + ` :lower true))`)
			for i, what := range []string{"result", "error text", "side effects", "name of the next lambda"} {
				assert.Equal(t, want[i], got[i], what)
				assert.Equal(t, want[i], low[i], "lowered: "+what)
			}
		})
	}
}
