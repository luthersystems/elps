// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestComposePropagatesLookupErrors(t *testing.T) {
	for _, expr := range []string{
		`(compose identity 'missing-function)`,
		`(compose identity 'missing-package:function)`,
		`(compose 'missing-function identity)`,
	} {
		t.Run(expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			got := env.LoadString("compose-error.lisp", expr)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			require.Contains(t, got.String(), "missing-")
		})
	}
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"valid composition", elpstest.TestSequence{
			{`(funcall (compose (lambda (x) (* x 2)) (lambda (x) (+ x 3))) 4)`, `14`, ``},
			{`(handler-bind ((condition (lambda (&rest _) 'caught))) (compose identity 'missing-function))`, `'caught`, ``},
		}},
	})
}
