// Copyright © 2026 The ELPS authors

package analysis

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGeneratedSharedOriginDuringDeepWalk(t *testing.T) {
	t.Parallel()
	env := newTestEnv(t)
	evalSource(t, env, `(defmacro fixed () '(progn (set 'x 1)))
(defmacro via-fixed () '(user:fixed))`)
	result := parseAndAnalyzeWithConfig(t, `(defun first () (user:via-fixed))
(in-package 'q)
(defun second () (user:via-fixed))`, &Config{MacroExpander: &EnvMacroExpander{Env: env}})
	byPkg := map[string]*Symbol{}
	for _, sym := range result.Symbols {
		if sym.Name == "x" {
			byPkg[sym.Package] = sym
		}
	}
	for pkg, line := range map[string]int{"user": 1, "q": 3} {
		sym := byPkg[pkg]
		require.NotNil(t, sym, pkg)
		require.NotNil(t, sym.GeneratedBy, pkg)
		assert.Equal(t, "user:via-fixed", sym.GeneratedBy.Macro)
		assert.Equal(t, pkg, sym.GeneratedBy.Package)
		require.NotNil(t, sym.GeneratedBy.CallSite)
		assert.Equal(t, line, sym.GeneratedBy.CallSite.Line)
	}
}
