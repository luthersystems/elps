// Copyright © 2026 The ELPS authors

package analysis

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// headCountingExpander wraps an expander and counts ExpandMacro calls per head.
type headCountingExpander struct {
	inner MacroExpander
	calls map[string]int
}

func (c *headCountingExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	if c.calls == nil {
		c.calls = map[string]int{}
	}
	if len(form.Cells) > 0 {
		c.calls[form.Cells[0].Str]++
	}
	return c.inner.ExpandMacro(form, pkg)
}

// Definition-generating macros unrelated to any embedder: a named constant,
// and a pair of accessor functions for a counter.
const generatingMacros = `
(defmacro constant (name value)
  (quasiquote (set (quote (unquote name)) (unquote value))))
(defmacro defcounter (name getter bumper)
  (quasiquote
    (progn
      (set (quote (unquote name)) 0)
      (defun (unquote getter) () (unquote name))
      (defun (unquote bumper) () (set! (unquote name) (+ 1 (unquote name)))))))
(defmacro via-constant (name value)
  (quasiquote (constant (unquote name) (unquote value))))
`

func analyzeGenerated(t *testing.T, source string) (*Result, *headCountingExpander) {
	t.Helper()
	env := newTestEnv(t)
	evalSource(t, env, generatingMacros)
	expander := &headCountingExpander{inner: &EnvMacroExpander{Env: env}}
	return parseAndAnalyzeWithConfig(t, source, &Config{MacroExpander: expander}), expander
}

func findSymbol(r *Result, name string) *Symbol {
	for _, sym := range r.Symbols {
		if sym.Name == name {
			return sym
		}
	}
	return nil
}

func TestGeneratedDefinitionsAreForwardReferenceable(t *testing.T) {
	t.Parallel()
	result, _ := analyzeGenerated(t, `(defun report () (list (hits-value) limit))
(bump-hits)
(defcounter hits hits-value bump-hits)
(constant limit 10)`)
	assert.Empty(t, result.Unresolved, "names generated later in the file must resolve")
}

func TestGeneratedDefinitionsRecordTheirMacro(t *testing.T) {
	t.Parallel()
	result, expander := analyzeGenerated(t, "(defun plain () 1)\n(defcounter hits hits-value bump-hits)\n(constant limit 10)\n")

	for name, macro := range map[string]string{"hits": "defcounter", "hits-value": "defcounter", "bump-hits": "defcounter", "limit": "constant"} {
		sym := findSymbol(result, name)
		require.NotNil(t, sym, name)
		require.NotNil(t, sym.GeneratedBy, name)
		assert.Equal(t, macro, sym.GeneratedBy.Macro, name)
		assert.Equal(t, lisp.DefaultUserPackage, sym.GeneratedBy.Package, name)
		require.NotNil(t, sym.GeneratedBy.CallSite, name)
	}
	assert.Equal(t, 2, findSymbol(result, "hits").GeneratedBy.CallSite.Line)
	assert.Equal(t, 3, findSymbol(result, "limit").GeneratedBy.CallSite.Line)
	assert.Nil(t, findSymbol(result, "plain").GeneratedBy, "an ordinary defun is not generated")
	// Parameters and other local bindings are never facts.
	for _, sym := range result.Symbols {
		if sym.Scope != result.RootScope {
			assert.Nil(t, sym.GeneratedBy, sym.Name)
		}
	}
	// Prescan and the deep walk share one expansion per call site.
	assert.Equal(t, 1, expander.calls["defcounter"])
	assert.Equal(t, 1, expander.calls["constant"])

	var names []string
	for _, def := range result.GeneratedDefinitions() {
		names = append(names, def.Name)
		assert.NotNil(t, def.GeneratedBy)
	}
	assert.ElementsMatch(t, []string{"hits", "hits-value", "bump-hits", "limit"}, names)
}

func TestGeneratedDefinitionsRecordTheOutermostCall(t *testing.T) {
	t.Parallel()
	result, _ := analyzeGenerated(t, "(via-constant answer 42)\n")
	sym := findSymbol(result, "answer")
	require.NotNil(t, sym)
	require.NotNil(t, sym.GeneratedBy)
	assert.Equal(t, "via-constant", sym.GeneratedBy.Macro)
}

func TestGeneratedDefinitionsInsideFunctionBodies(t *testing.T) {
	t.Parallel()
	result, _ := analyzeGenerated(t, "(defun setup () (constant late 1))\n")
	sym := findSymbol(result, "late")
	require.NotNil(t, sym)
	require.NotNil(t, sym.GeneratedBy)
	assert.Equal(t, "constant", sym.GeneratedBy.Macro)
}

// TestGeneratedDefinitionsFlowBetweenFiles uses the facts the way
// go/analysis passes facts between packages: one file's generated
// definitions become another file's ExtraGlobals.
func TestGeneratedDefinitionsFlowBetweenFiles(t *testing.T) {
	t.Parallel()
	lib, _ := analyzeGenerated(t, "(defcounter hits hits-value bump-hits)\n")
	facts := lib.GeneratedDefinitions()
	require.NotEmpty(t, facts)

	user := parseAndAnalyzeWithConfig(t, "(bump-hits)\n(hits-value)\n", &Config{ExtraGlobals: facts})
	assert.Empty(t, user.Unresolved)
	for _, ref := range user.References {
		if ref.Symbol.Name == "bump-hits" {
			require.NotNil(t, ref.Symbol.GeneratedBy)
			assert.Equal(t, "defcounter", ref.Symbol.GeneratedBy.Macro)
		}
	}
	assert.Empty(t, user.GeneratedDefinitions(), "external facts are not re-exported")
}

func TestGeneratedDefinitionsWithoutExpander(t *testing.T) {
	t.Parallel()
	result := parseAndAnalyzeWithConfig(t, "(defcounter hits hits-value bump-hits)\n(defun plain () 1)\n", &Config{})
	assert.Empty(t, result.GeneratedDefinitions())
	var nilResult *Result
	assert.Empty(t, nilResult.GeneratedDefinitions())
}

// TestGeneratedDefinitionsRespectFileDefinitions pins that prescan agrees
// with the deep walk: a head the file itself defines as a function is that
// function, even when the expander's environment has a macro of that name.
func TestGeneratedDefinitionsRespectFileDefinitions(t *testing.T) {
	t.Parallel()
	result, expander := analyzeGenerated(t, "(constant limit 10)\n(defun constant (a b) (list a b))\n")
	assert.Nil(t, findSymbol(result, "limit"), "a file-local function named constant generates nothing")
	assert.Empty(t, result.GeneratedDefinitions())
	assert.Zero(t, expander.calls["constant"])
}

// TestGeneratedDefinitionSourceIsTheCallSite pins that a generated variable
// is located at its name in the macro call, not in the macro's template, so
// go-to-definition and rename land in the analyzed file.
func TestGeneratedDefinitionSourceIsTheCallSite(t *testing.T) {
	t.Parallel()
	result, _ := analyzeGenerated(t, "(defun plain () 1)\n(defcounter hits hits-value bump-hits)\n")
	for _, name := range []string{"hits", "hits-value"} {
		sym := findSymbol(result, name)
		require.NotNil(t, sym, name)
		require.NotNil(t, sym.Source, name)
		assert.Equal(t, "test.lisp", sym.Source.File, name)
		assert.Equal(t, 2, sym.Source.Line, name)
	}
	assert.Equal(t, 13, findSymbol(result, "hits").Source.Col)
}

// counterFile is the docs/embed.md example: the macro and its use in one file.
const counterFile = `(defmacro defcounter (name getter bumper)
  (quasiquote
    (progn
      (set (quote (unquote name)) 0)
      (defun (unquote getter) () (unquote name))
      (defun (unquote bumper) () (set! (unquote name) (+ 1 (unquote name)))))))

(defcounter hits hits-value bump-hits)
`

// TestGeneratedDefinitionsDocExample follows docs/embed.md: the expander
// knows the file's macro only after LoadWorkspaceMacros.
func TestGeneratedDefinitionsDocExample(t *testing.T) {
	t.Parallel()
	expander := &EnvMacroExpander{Env: newTestEnv(t)}
	require.Empty(t, expander.LoadWorkspaceMacros(parsePreamble(t, counterFile)))
	result := parseAndAnalyzeWithConfig(t, counterFile, &Config{MacroExpander: expander})
	var names []string
	for _, def := range result.GeneratedDefinitions() {
		names = append(names, def.Name)
		assert.Equal(t, "defcounter", def.GeneratedBy.Macro)
	}
	assert.ElementsMatch(t, []string{"hits", "hits-value", "bump-hits"}, names)
}

// TestGeneratedDefinitionsFallbackWithoutLoadedMacro pins the documented
// fallback: an expander whose env lacks the macro expands nothing, and no
// definition claims to be generated.
func TestGeneratedDefinitionsFallbackWithoutLoadedMacro(t *testing.T) {
	t.Parallel()
	result := parseAndAnalyzeWithConfig(t, counterFile, &Config{MacroExpander: &EnvMacroExpander{Env: newTestEnv(t)}})
	assert.Empty(t, result.GeneratedDefinitions())
	for _, sym := range result.Symbols {
		assert.Nil(t, sym.GeneratedBy, sym.Name)
	}
}
