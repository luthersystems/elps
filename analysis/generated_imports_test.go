// Copyright © 2026 The ELPS authors

package analysis

import (
	"testing"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGeneratedQualifiedUsePackageReachesTheExpander(t *testing.T) {
	t.Parallel()
	env := newTestEnv(t)
	evalSource(t, env, `
(in-package 'q)
(defmacro qconst (name value)
  (quasiquote (set (quote (unquote name)) (unquote value))))
(in-package 'user)
(defmacro import-q () '(progn (lisp:use-package 'q)))`)
	expander := &headCountingExpander{inner: &EnvMacroExpander{Env: env}}
	cfg := &Config{
		MacroExpander:  expander,
		PackageExports: map[string][]ExternalSymbol{"q": {{Name: "qconst", Kind: SymMacro, Package: "q"}}},
	}
	result := parseAndAnalyzeWithConfig(t, `(import-q)
(qconst qa 1)
qa`, cfg)
	assert.Empty(t, result.Unresolved)
	qa := findSymbol(result, "qa")
	require.NotNil(t, qa)
	assert.Equal(t, "user", qa.Package)
	require.NotNil(t, qa.GeneratedBy)
	assert.Equal(t, "qconst", qa.GeneratedBy.Macro)
	assert.Equal(t, 1, expander.calls["q:qconst"], "prescan and deep walk share the qualified expansion")
	assert.Equal(t, 0, expander.calls["qconst"])
	assert.Equal(t, 0, expander.calls["lisp:use-package"])
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
	assert.Equal(t, lisp.LError, env.Get(lisp.Symbol("qconst")).Type, "analysis must not import into the shared environment")

	// Reusing the expander for a file without the generated import must not
	// inherit the preceding analysis's use-package.
	result = parseAndAnalyzeWithConfig(t, `(qconst qb 2) qb`, cfg)
	assert.Nil(t, findSymbol(result, "qb"))
	assert.NotEmpty(t, result.Unresolved)
}

func TestExpansionCacheIncludesImportedHead(t *testing.T) {
	t.Parallel()
	env := newTestEnv(t)
	evalSource(t, env, `(in-package 'q) (defmacro value () '"q")
(in-package 'r) (defmacro value () '"r")
(in-package 'user)`)
	expander := &headCountingExpander{inner: &EnvMacroExpander{Env: env}}
	a := &analyzer{cfg: &Config{MacroExpander: expander}}
	scope := NewScope(ScopeGlobal, nil, nil)
	call := parsePreamble(t, `(value)`)[0]
	head := call.Cells[0]
	loc := astutil.SymbolLoc(head)
	assert.Nil(t, a.expand(call, scope, "user"))
	for _, pkg := range []string{"q", "r"} {
		scope.DefineImported(&Symbol{Name: "value", Kind: SymMacro, Package: pkg, External: true}, "user")
		expanded := a.expand(call, scope, "user")
		require.NotNil(t, expanded)
		assert.Equal(t, pkg, expanded.Str)
		assert.Same(t, expanded, a.expand(call, scope, "user"))
		assert.Equal(t, 1, expander.calls[pkg+":value"])
	}
	assert.Equal(t, 1, expander.calls["value"], "a failed unqualified expansion must not hide a later import")
	assert.Same(t, head, call.Cells[0])
	assert.Equal(t, "value", head.Str)
	assert.Equal(t, loc, astutil.SymbolLoc(head))
}
