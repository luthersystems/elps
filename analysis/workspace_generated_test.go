// Copyright © 2026 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// generatedWorkspace writes files under a new directory, scans it, loads
// its macros into a new env and returns the scan and a Config with the
// tables of the scan and an expander of that env.
func generatedWorkspace(t *testing.T, files map[string]string) (*WorkspaceScan, *Config, string) {
	t.Helper()
	root := t.TempDir()
	for name, src := range files {
		require.NoError(t, os.WriteFile(filepath.Join(root, name), []byte(src), 0o600))
	}
	scan, err := ScanWorkspaceFiles(root, nil)
	require.NoError(t, err)
	exp := &EnvMacroExpander{Env: newTestEnv(t)}
	prescan := scan.Prescan()
	require.Empty(t, exp.LoadWorkspaceMacros(prescan.Preamble))
	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		PackageSymbols: prescan.PkgAllSymbols,
		PackageImports: prescan.PackageImports,
		DefaultPackage: prescan.DefaultPackage,
		FilePackages:   prescan.FilePackages,
		MacroExpander:  exp,
	}
	return scan, cfg, root
}

// TestWithGeneratedDefinitions checks that the tables of a scan hold what
// the macro calls of each file define, in the package of the call, and
// exported when the file exports the name (luthersystems/elps#953).
func TestWithGeneratedDefinitions(t *testing.T) {
	scan, cfg, root := generatedWorkspace(t, map[string]string{
		"macros.lisp": "(in-package 'lib)\n(export 'defhelper)\n" +
			"(defmacro defhelper (name) (quasiquote (defun (unquote name) (x) \"Helper.\" x)))\n",
		"app.lisp":  "(in-package 'lib)\n(export 'foo)\n(defhelper foo)\n(defhelper hidden)\n",
		"user.lisp": "(in-package 'app)\n(lib:foo 1)\n",
	})

	before := scan.Prescan()
	assert.Empty(t, namesOf(before.PkgAllSymbols["lib"], "foo", "hidden"))

	gen, changed := scan.WithGeneratedDefinitions(cfg, nil, nil)
	require.True(t, changed)
	after := gen.Prescan()
	assert.ElementsMatch(t, []string{"foo", "hidden"}, namesOf(after.PkgAllSymbols["lib"], "foo", "hidden"))
	assert.Equal(t, []string{"foo"}, namesOf(after.PkgExports["lib"], "foo", "hidden"),
		"only the name the file exports is an export")
	var foo *ExternalSymbol
	for i := range after.AllDefs {
		if after.AllDefs[i].Name == "foo" {
			foo = &after.AllDefs[i]
		}
	}
	require.NotNil(t, foo)
	assert.Equal(t, "lib", foo.Package)
	assert.Equal(t, SymFunction, foo.Kind)
	assert.Equal(t, "Helper.", foo.DocString)
	assert.Equal(t, 1, foo.Signature.MinArity())
	require.NotNil(t, foo.GeneratedBy)
	assert.Equal(t, "defhelper", foo.GeneratedBy.Macro)
	require.NotNil(t, foo.Source)
	assert.Equal(t, filepath.Join(root, "app.lisp"), foo.Source.File, "the definition is in the file of the call")

	// The same expansion again changes nothing.
	_, changed = gen.WithGeneratedDefinitions(cfg, nil, nil)
	assert.False(t, changed)

	// A new scan of the file keeps what its calls defined until they are
	// expanded again, and the expansion reports the change against them:
	// a save that removes the call must reach the other files.
	app := filepath.Join(root, "app.lisp")
	src, err := os.ReadFile(app) //nolint:gosec // G304: a file this test wrote
	require.NoError(t, err)
	rescanned, _ := gen.WithFile(app, src)
	assert.Len(t, namesOf(rescanned.Prescan().PkgAllSymbols["lib"], "foo", "hidden"), 2)
	assert.True(t, rescanned.MayGenerateDefinitions(app))
	_, changed = rescanned.WithGeneratedDefinitions(cfg, []string{app}, nil)
	assert.False(t, changed, "the same text defines the same names")

	noCalls := []byte("(in-package 'lib)\n(defun other () 1)\n")
	rescanned, _ = gen.WithFile(app, noCalls)
	assert.True(t, rescanned.MayGenerateDefinitions(app), "it has definitions to drop")
	dropped, changed := rescanned.WithGeneratedDefinitions(cfg, []string{app},
		func(string) ([]byte, bool) { return noCalls, true })
	assert.True(t, changed)
	assert.Empty(t, namesOf(dropped.Prescan().PkgAllSymbols["lib"], "foo", "hidden"))
	assert.False(t, dropped.MayGenerateDefinitions(app), "a file of defun forms is not read again")
}

// TestWithGeneratedDefinitions_NameFromMacroBody checks that a name the
// macro writes in its own body is placed at the call, in the file that
// calls the macro.
func TestWithGeneratedDefinitions_NameFromMacroBody(t *testing.T) {
	scan, cfg, root := generatedWorkspace(t, map[string]string{
		"macros.lisp": "(defmacro defbar () '(defun bar () 1))\n",
		"app.lisp":    "(defbar)\n",
	})
	gen, changed := scan.WithGeneratedDefinitions(cfg, nil, nil)
	require.True(t, changed)
	defs := gen.Prescan().AllDefs
	var bar *ExternalSymbol
	for i := range defs {
		if defs[i].Name == "bar" {
			bar = &defs[i]
		}
	}
	require.NotNil(t, bar)
	require.NotNil(t, bar.Source)
	assert.Equal(t, filepath.Join(root, "app.lisp"), bar.Source.File)
	assert.Equal(t, 1, bar.Source.Line)
}

// TestWithGeneratedDefinitions_TextDefinitionOnce checks that a defun
// written in the arguments of a macro call, which the scan of the text
// finds too, is in the tables once.
func TestWithGeneratedDefinitions_TextDefinitionOnce(t *testing.T) {
	scan, cfg, _ := generatedWorkspace(t, map[string]string{
		"macros.lisp": "(defmacro with-logging (&rest body) (quasiquote (progn (unquote-splicing body))))\n",
		"app.lisp":    "(with-logging (defun handler () 1))\n",
	})
	gen, _ := scan.WithGeneratedDefinitions(cfg, nil, nil)
	assert.Len(t, namesOf(gen.Prescan().AllDefs, "handler"), 1)
}

// TestWithGeneratedDefinitions_NoExpander checks that a scan is returned
// as it is without an expander.
func TestWithGeneratedDefinitions_NoExpander(t *testing.T) {
	scan, cfg, _ := generatedWorkspace(t, map[string]string{"app.lisp": "(foo)\n"})
	cfg.MacroExpander = nil
	got, changed := scan.WithGeneratedDefinitions(cfg, nil, nil)
	assert.Same(t, scan, got)
	assert.False(t, changed)
}

// TestHasMacroCandidates checks which files WithGeneratedDefinitions reads.
func TestHasMacroCandidates(t *testing.T) {
	for src, want := range map[string]bool{
		"(defun f () 1)\n(set 'x 1)\n(in-package 'p)\n(export 'f)\n": false,
		"(lisp:defun f () 1)\n":                  false,
		"(progn (defun f () 1))\n":               false,
		"'(defhelper foo)\n":                     false,
		"(defhelper foo)\n":                      true,
		"(progn (defun f () 1) (defhelper g))\n": true,
	} {
		assert.Equal(t, want, scanFileFull([]byte(src), "f.lisp").macroCandidates, src)
	}
}

// namesOf returns the names of syms that are one of names.
func namesOf(syms []ExternalSymbol, names ...string) []string {
	var out []string
	for _, sym := range syms {
		for _, n := range names {
			if sym.Name == n {
				out = append(out, n)
			}
		}
	}
	return out
}

// TestWithGeneratedDefinitions_InferredDefForm checks that a call of a
// workspace macro whose name starts with "def" gives its definition to
// the tables, with the DefForms the workspace infers in the config, as
// the lint command and the editor pass them (luthersystems/elps#984).
// Before, the inferred spec stopped the expansion of the call, so the
// definition had no GeneratedBy and was dropped.
func TestWithGeneratedDefinitions_InferredDefForm(t *testing.T) {
	scan, cfg, _ := generatedWorkspace(t, map[string]string{
		"main.lisp": "(in-package 'repro)\n" +
			"(defmacro defun-traced (name formals &rest body)\n" +
			"  (quasiquote (defun (unquote name) (unquote formals) (unquote-splicing body))))\n" +
			"(defun-traced macro-fn (x) (+ x 1))\n",
		"other.lisp": "(in-package 'repro)\n(macro-fn 1)\n",
	})
	cfg.DefForms = scan.Prescan().DefForms
	require.NotEmpty(t, cfg.DefForms, "the workspace infers a spec for defun-traced")

	gen, changed := scan.WithGeneratedDefinitions(cfg, nil, nil)
	require.True(t, changed)
	assert.Equal(t, []string{"macro-fn"}, namesOf(gen.Prescan().PkgAllSymbols["repro"], "macro-fn"))
}

// defunTracedMacro is a workspace def* macro: the scan infers a
// DefFormSpec for it from its name.
const defunTracedMacro = "(in-package 'repro)\n" +
	"(defmacro defun-traced (name formals &rest body)\n" +
	"  (quasiquote (defun (unquote name) (unquote formals) (unquote-splicing body))))\n"

// rootSymbol returns the symbol name the analysis defined in its root
// scope, or nil.
func rootSymbol(r *Result, name string) *Symbol {
	for _, sym := range r.Symbols {
		if sym.Name == name && sym.Scope == r.RootScope {
			return sym
		}
	}
	return nil
}

// TestExplicitDefFormWinsOverExpansion checks that a DefFormSpec an
// embedder supplies still takes priority over the expander: it is the
// embedder's grammar, not a guess by name (luthersystems/elps#984). The
// call is not expanded, so its name comes from the spec and has no
// GeneratedBy.
func TestExplicitDefFormWinsOverExpansion(t *testing.T) {
	src := defunTracedMacro + "(defun-traced macro-fn (x) (+ x 1))\n"
	_, cfg, _ := generatedWorkspace(t, map[string]string{"main.lisp": src})
	cfg.DefForms = []DefFormSpec{{Head: "defun-traced", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: SymFunction}}
	r := AnalyzeFile([]byte(src), "main.lisp", cfg)
	sym := rootSymbol(r, "macro-fn")
	require.NotNil(t, sym, "the explicit spec defines macro-fn")
	assert.Nil(t, sym.GeneratedBy, "an explicit spec stops the expansion")
}

// TestInferredDefFormUsesExpansion checks that the analysis of a file
// follows the expansion of a call of an inferred head, not the guess
// (luthersystems/elps#984). Here the macro defines a variable named in
// its own body; the guess would define a function named by the first
// argument.
func TestInferredDefFormUsesExpansion(t *testing.T) {
	src := "(in-package 'repro)\n" +
		"(defmacro defvar-of (name formals &rest body)\n" +
		"  (quasiquote (set 'var-thing 1)))\n" +
		"(defvar-of thing (x) x)\n" +
		"var-thing\n"
	scan, cfg, _ := generatedWorkspace(t, map[string]string{"main.lisp": src})
	cfg.DefForms = scan.Prescan().DefForms
	require.NotEmpty(t, cfg.DefForms)
	require.True(t, cfg.DefForms[0].Inferred)

	r := AnalyzeFile([]byte(src), "main.lisp", cfg)
	v := rootSymbol(r, "var-thing")
	require.NotNil(t, v, "the expansion defines var-thing")
	assert.NotNil(t, v.GeneratedBy)
	assert.Nil(t, rootSymbol(r, "thing"), "the guess from the name is not used")
}

// TestExplicitDefFormBeforeInferred checks that, when an embedder adds
// its own spec for a head the workspace also inferred, the embedder's
// spec gives the grammar as well as stopping the expansion
// (luthersystems/elps#984 review).
func TestExplicitDefFormBeforeInferred(t *testing.T) {
	src := defunTracedMacro + "(defun-traced macro-fn (x) (+ x 1))\n"
	scan, cfg, _ := generatedWorkspace(t, map[string]string{"main.lisp": src})
	// The inferred spec comes first and binds the name; the embedder's
	// spec, later in the list, says the call defines no name.
	cfg.DefForms = append(scan.Prescan().DefForms,
		DefFormSpec{Head: "defun-traced", FormalsIndex: 2, BindsName: false})
	r := AnalyzeFile([]byte(src), "main.lisp", cfg)
	assert.Nil(t, rootSymbol(r, "macro-fn"), "the embedder's grammar defines no name")
}
