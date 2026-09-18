// Copyright © 2026 The ELPS authors

package analysis

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestNestedPackageDefinitions(t *testing.T) {
	result := parseAndAnalyze(t, `(in-package 'lib)
 (let ((k 1))
  (export 'helper 'macro 'counter)
  (defun helper (x) (+ x k))
  (defmacro macro (x) x)
  (set 'counter k))
 (helper counter)`)
	for _, name := range []string{"helper", "macro", "counter"} {
		sym := result.RootScope.LookupLocalInPackage(name, "lib")
		require.NotNil(t, sym, name)
		require.Equal(t, ScopeGlobal, sym.Scope.Kind, name)
		require.True(t, sym.Exported, name)
	}
	require.Empty(t, result.Unresolved)
}
