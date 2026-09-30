// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These goldens were recorded before macroGetDefault used NewGenSyms. Keep
// the result and evaluation cost unchanged; the port changes only temporary
// names in the printed expansion.
func TestGetDefaultParity(t *testing.T) {
	for _, tt := range []struct {
		name, program, result string
		steps                 int64
	}{
		{"hit", `(get-default (sorted-map "k" 7) "k" 42)`, `7`, 24},
		{"miss", `(get-default (sorted-map "other" 7) "k" 42)`, `42`, 21},
		{"nil map", `(get-default () "k" 42)`, `42`, 15},
		{"non-map", `(handler-bind ((condition (lambda (&rest _) 'caught))) (get-default 1 "k" 42))`, `'caught`, 23},
		{"nested default", `(get-default (sorted-map) "k" (get-default () "inner" 42))`, `42`, 33},
		{"default side effects", `(let ((calls 0)) (list (get-default (sorted-map) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(42 1)`, 33},
		{"unused default", `(let ((calls 0)) (list (get-default (sorted-map "k" 7) "k" (progn (set! calls (+ calls 1)) 42)) calls))`, `'(7 0)`, 30},
		{"macroexpand", `(macroexpand-1 '(get-default m k d))`, `'(lisp:let ((map@1@1 m) (key@1@2 k)) (lisp:if (lisp:if (lisp:nil? map@1@1) lisp:false (lisp:key? map@1@1 key@1@2)) (lisp:get map@1@1 key@1@2) d))`, 3},
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
