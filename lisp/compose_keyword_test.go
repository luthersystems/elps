// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: composition must preserve g's keyword calling convention,
// including nil defaults and inert list/symbol values.
func TestComposeKeywordArguments(t *testing.T) {
	for _, tc := range []struct{ name, code, want string }{
		{"one keyword", `((compose identity (lambda (&key x) x)) :x 42)`, `42`},
		{"omitted keyword", `((compose identity (lambda (&key x) x)))`, `()`},
		{"reordered keywords", `((compose identity (lambda (&key x y) (list x y))) :y 2 :x 1)`, `'(1 2)`},
		{"required optional keyword", `((compose identity (lambda (x &optional y &key z) (list x y z))) 1 2 :z 3)`, `'(1 2 3)`},
		{"optional defaults", `((compose identity (lambda (x &optional y &key z) (list x y z))) 1)`, `'(1 () ())`},
		{"data stays inert", `(let ((writes 0)) (list ((compose identity (lambda (&key x y) (list x y))) :x (car '((set! writes 99))) :y (car '(unbound-data))) writes))`, `'('((set! writes 99) unbound-data) 0)`},
		{"both functions applied once", `(let ((calls (vector))) (list ((compose (lambda (x) (append! calls 'f) (+ x 1)) (lambda (&key x) (append! calls 'g) (* x 2))) :x 3) calls))`, `'(7 (vector 'g 'f))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			got := env.LoadString("compose-keyword.lisp", tc.code)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			require.Equal(t, tc.want, got.String())
		})
	}
}
