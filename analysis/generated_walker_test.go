// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGeneratedWalkerDefLikeExpansionPriority(t *testing.T) {
	t.Parallel()
	for _, custom := range []bool{false, true} {
		t.Run(map[bool]string{false: "expansion", true: "custom grammar"}[custom], func(t *testing.T) {
			env := newTestEnv(t)
			evalSource(t, env, `(defmacro defvariable (name params body)
  (quasiquote (set (quote (unquote name)) (unquote body))))`)
			expander := &headCountingExpander{inner: &EnvMacroExpander{Env: env}}
			cfg := &Config{MacroExpander: expander}
			if custom {
				cfg.DefForms = []DefFormSpec{{Head: "defvariable", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction}}
			}
			// Inside a function, only the deep resolver sees the macro call.
			result := parseAndAnalyzeWithConfig(t, `(defun setup () (defvariable actual () 7))`, cfg)
			sym := findSymbol(result, "actual")
			require.NotNil(t, sym)
			if custom {
				assert.Equal(t, SymFunction, sym.Kind)
				assert.Equal(t, ScopeFunction, sym.Scope.Kind)
				assert.Nil(t, sym.GeneratedBy)
				assert.Zero(t, expander.calls["defvariable"])
			} else {
				assert.Equal(t, SymVariable, sym.Kind)
				assert.Same(t, result.RootScope, sym.Scope)
				require.NotNil(t, sym.GeneratedBy)
				assert.Equal(t, "defvariable", sym.GeneratedBy.Macro)
				assert.Equal(t, 1, expander.calls["defvariable"])
			}
		})
	}
}

func TestGeneratedWalkerCachesFailedExpansionPerCall(t *testing.T) {
	t.Parallel()
	expander := &headCountingExpander{inner: &EnvMacroExpander{Env: newTestEnv(t)}}
	result := parseAndAnalyzeWithConfig(t, `(defunknown first-generated (x) x) (defunknown second-generated (y) y)`, &Config{MacroExpander: expander})
	assert.Equal(t, 2, expander.calls["defunknown"], "each distinct call is tried once across prescan and the deep walk")
	for _, name := range []string{"first-generated", "second-generated"} {
		sym := findSymbol(result, name)
		require.NotNil(t, sym)
		assert.Equal(t, SymFunction, sym.Kind, "failed expansion retains the definition heuristic")
		assert.Nil(t, sym.GeneratedBy)
	}
}

func TestGeneratedWalkerRespectsFileVariable(t *testing.T) {
	t.Parallel()
	result, expander := analyzeGenerated(t, `(constant limit 10) (set 'constant (lambda (a b) (list a b)))`)
	assert.Nil(t, findSymbol(result, "limit"))
	assert.Empty(t, result.GeneratedDefinitions())
	assert.Zero(t, expander.calls["constant"])
}

func TestGeneratedWalkerExplicitQuoteSetLocations(t *testing.T) {
	t.Parallel()
	const source = "(set (quote value) 1)\n(defun touch () (set (lisp:quote value) 2))"
	result := parseAndAnalyzeWithConfig(t, source, &Config{})
	sym := findSymbol(result, "value")
	require.NotNil(t, sym)
	require.NotNil(t, sym.Source)
	assert.Equal(t, 1, sym.Source.Line)
	assert.Equal(t, 13, sym.Source.Col)
	require.NotNil(t, sym.Node)
	assert.Equal(t, lisp.LSymbol, sym.Node.Type)
	assert.Equal(t, "value", sym.Node.Str)

	var refs []*Reference
	for _, ref := range result.References {
		if ref.Symbol == sym {
			refs = append(refs, ref)
		}
	}
	require.Len(t, refs, 1)
	require.NotNil(t, refs[0].Source)
	assert.Equal(t, 2, refs[0].Source.Line)
	assert.Equal(t, strings.Index(strings.Split(source, "\n")[1], "value")+1, refs[0].Source.Col)
	assert.Equal(t, "value", refs[0].Node.Str)
	assert.Empty(t, result.Unresolved)
}
