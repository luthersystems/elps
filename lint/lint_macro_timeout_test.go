// Copyright © 2026 The ELPS authors

package lint

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/require"
)

// TestBuildAnalysisConfigMacroLoadTimeout checks that MacroLoadTimeout
// bounds the load of the workspace macros: a workspace whose preamble
// never finishes still gives a config. elps lint sets the bound since it
// started loading macros (luthersystems/elps#952); a lint run in CI must
// end even when a workspace file computes a value forever at load time.
func TestBuildAnalysisConfigMacroLoadTimeout(t *testing.T) {
	dir := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(dir, "spin.lisp"),
		[]byte("(set 'never (dotimes (i 1000000000) (dotimes (j 1000000000) (+ i j))))\n(defmacro after () 1)\n"), 0o600))
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)

	done := make(chan error, 1)
	go func() {
		_, err := BuildAnalysisConfig(&LintConfig{
			Workspace:        dir,
			Env:              env,
			MacroLoadTimeout: 5 * time.Second,
		})
		done <- err
	}()
	select {
	case err := <-done:
		require.NoError(t, err)
	case <-time.After(60 * time.Second):
		t.Fatal("the bounded macro load did not end")
	}
}
