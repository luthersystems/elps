// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Issue #657: metatype values can be built and their data can be mutated
// by Lisp programs. A typedef tag alone is not proof of a valid descriptor.
func TestMalformedTypedefReturnsOrdinaryError(t *testing.T) {
	for _, data := range []string{
		`7`, `()`, `(list 'user:box)`, `(list 'user:box identity 3)`,
		`(list 7 identity)`, `(list 'user:box 7)`, `(list 'user:box if)`,
	} {
		for _, use := range []string{`(new bad 9)`, `(type? bad 9)`} {
			t.Run(data+use, func(t *testing.T) {
				env, err := (&elpstest.Runner{}).NewEnv(t)
				require.NoError(t, err)
				code := `(let ((bad (new (new lisp:typedef 'lisp:typedef identity) ` + data + `))) ` + use + `)`
				got := env.LoadString("typedef-data.lisp", code)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				require.Equal(t, lisp.LError, got.Type, "%v", got)
				require.Contains(t, got.String(), "typedef")
			})
		}
	}
}

func TestMalformedTypedefRejectsBeforeConstructor(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	got := env.LoadString("typedef-before-call.lisp", `
		(let ((writes 0))
			(let ((bad (new (new lisp:typedef 'lisp:typedef identity)
				(list 7 (lambda (&rest _) (set! writes 99))))))
				(handler-bind ((condition (lambda (&rest _) ()))) (new bad)))
			writes)`)
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	require.Equal(t, lisp.LInt, got.Type, "%v", got)
	require.Equal(t, 0, got.Int)
}
