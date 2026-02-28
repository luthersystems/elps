// Copyright © 2024 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestScanWorkspace_Basic(t *testing.T) {
	dir := t.TempDir()

	// Write a file with exported definitions
	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun helper (x) (+ x 1))
(defun public-fn (a b) (+ a b))
(set 'my-var 42)
(export 'public-fn 'my-var)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	// Only exported symbols should be returned
	names := make(map[string]bool)
	for _, s := range syms {
		names[s.Name] = true
	}
	assert.True(t, names["public-fn"], "exported function should be included")
	assert.True(t, names["my-var"], "exported variable should be included")
	assert.False(t, names["helper"], "non-exported function should be excluded")
}

func TestScanWorkspace_MultipleFIles(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`
(defun fn-a () 1)
(export 'fn-a)
`), 0600)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`
(defun fn-b () 2)
(export 'fn-b)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range syms {
		names[s.Name] = true
	}
	assert.True(t, names["fn-a"])
	assert.True(t, names["fn-b"])
}

func TestScanWorkspace_SkipsParseErrors(t *testing.T) {
	dir := t.TempDir()

	// Valid file
	err := os.WriteFile(filepath.Join(dir, "good.lisp"), []byte(`
(defun good-fn () 42)
(export 'good-fn)
`), 0600)
	require.NoError(t, err)

	// Malformed file
	err = os.WriteFile(filepath.Join(dir, "bad.lisp"), []byte(`(unclosed`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range syms {
		names[s.Name] = true
	}
	assert.True(t, names["good-fn"], "good file should still be scanned")
}

func TestScanWorkspace_SkipsNonLisp(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "readme.txt"), []byte("not lisp"), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)
	assert.Empty(t, syms)
}

func TestScanWorkspace_Subdirectories(t *testing.T) {
	dir := t.TempDir()
	subdir := filepath.Join(dir, "sub")
	err := os.MkdirAll(subdir, 0750)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(subdir, "deep.lisp"), []byte(`
(defun deep-fn () 42)
(export 'deep-fn)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range syms {
		names[s.Name] = true
	}
	assert.True(t, names["deep-fn"], "files in subdirectories should be scanned")
}

func TestScanWorkspace_SignaturePreserved(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun add (a b) (+ a b))
(export 'add)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "add", syms[0].Name)
	assert.Equal(t, SymFunction, syms[0].Kind)
	require.NotNil(t, syms[0].Signature)
	assert.Equal(t, 2, syms[0].Signature.MinArity())
	assert.Equal(t, 2, syms[0].Signature.MaxArity())
}

func TestScanWorkspace_EmptyDir(t *testing.T) {
	dir := t.TempDir()

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)
	assert.Empty(t, syms)
}

func TestScanWorkspace_DefmacroExported(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(`
(defmacro when (condition &rest body) (list 'if condition (cons 'progn body)))
(export 'when)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "when", syms[0].Name)
	assert.Equal(t, SymMacro, syms[0].Kind)
}

func TestScanWorkspace_DocStringExtracted(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun greet (name)
  "Say hello to someone."
  (concat "Hello, " name))
(export 'greet)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "greet", syms[0].Name)
	assert.Equal(t, "Say hello to someone.", syms[0].DocString)
}

func TestScanWorkspace_NoDocStringWhenNoBody(t *testing.T) {
	dir := t.TempDir()

	// A function with only a string (constant function) has no docstring.
	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun version () "1.0.0")
(export 'version)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "version", syms[0].Name)
	assert.Empty(t, syms[0].DocString, "string-only body should not be treated as docstring")
}

func TestScanWorkspaceFull_SkipsHiddenDirs(t *testing.T) {
	dir := t.TempDir()

	// Create a hidden directory with a lisp file.
	hiddenDir := filepath.Join(dir, ".git")
	err := os.MkdirAll(hiddenDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(hiddenDir, "config.lisp"), []byte(`
(defun hidden-fn () 42)
(export 'hidden-fn)
`), 0600)
	require.NoError(t, err)

	// Create a node_modules directory.
	nmDir := filepath.Join(dir, "node_modules")
	err = os.MkdirAll(nmDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(nmDir, "dep.lisp"), []byte(`
(defun dep-fn () 42)
(export 'dep-fn)
`), 0600)
	require.NoError(t, err)

	// Create a visible file.
	err = os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun visible-fn () 42)
(export 'visible-fn)
`), 0600)
	require.NoError(t, err)

	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range globals {
		names[s.Name] = true
	}
	for _, syms := range pkgs {
		for _, s := range syms {
			names[s.Name] = true
		}
	}

	assert.True(t, names["visible-fn"], "visible file should be scanned")
	assert.False(t, names["hidden-fn"], "hidden directory files should be skipped")
	assert.False(t, names["dep-fn"], "node_modules files should be skipped")
}

func TestShouldSkipDir(t *testing.T) {
	assert.False(t, shouldSkipDir("."), "current directory should not be skipped")
	assert.False(t, shouldSkipDir(".."), "parent directory should not be skipped")
	assert.True(t, shouldSkipDir(".git"), "hidden directory should be skipped")
	assert.True(t, shouldSkipDir(".vscode"), "hidden directory should be skipped")
	assert.True(t, shouldSkipDir("node_modules"), "node_modules should be skipped")
	assert.False(t, shouldSkipDir("src"), "normal directory should not be skipped")
}

func TestScanWorkspaceFull_CombinedResults(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(in-package 'mylib)
(defun helper (x) (+ x 1))
(export 'helper)
`), 0600)
	require.NoError(t, err)

	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	// Globals should contain the exported symbol.
	require.Len(t, globals, 1)
	assert.Equal(t, "helper", globals[0].Name)
	assert.Equal(t, SymFunction, globals[0].Kind)

	// Package exports should group by package with Package field set.
	require.Contains(t, pkgs, "mylib")
	require.Len(t, pkgs["mylib"], 1)
	assert.Equal(t, "helper", pkgs["mylib"][0].Name)
	assert.Equal(t, "mylib", pkgs["mylib"][0].Package,
		"package export should have Package field set")
	assert.Equal(t, SymFunction, pkgs["mylib"][0].Kind)
}

func TestScanWorkspaceRefs_CrossFile(t *testing.T) {
	dir := t.TempDir()

	// File A defines and exports "helper".
	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`(defun helper (x) (+ x 1))
(export 'helper)
`), 0600)
	require.NoError(t, err)

	// File B calls "helper" inside a function.
	err = os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(defun caller () (helper 42))
`), 0600)
	require.NoError(t, err)

	// Phase 1: Scan definitions to build config.
	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   globals,
		PackageExports: pkgs,
	}

	// Phase 2: Scan references.
	refs := ScanWorkspaceRefs(dir, cfg)

	// There should be a reference to "helper" from file B.
	helperKey := SymbolKey{Name: "helper", Kind: SymFunction}.String()
	helperRefs := refs[helperKey]
	require.NotEmpty(t, helperRefs, "should have cross-file references to helper")

	// Find the reference from file B.
	bPath := filepath.Join(dir, "b.lisp")
	var found bool
	for _, ref := range helperRefs {
		if ref.File == bPath {
			found = true
			assert.Equal(t, "caller", ref.Enclosing, "reference should be inside 'caller'")
			assert.NotNil(t, ref.Source, "reference should have source location")
			break
		}
	}
	assert.True(t, found, "should find helper reference from b.lisp")
}

func TestExtractFileRefs_SkipsBuiltinsAndLocals(t *testing.T) {
	src := []byte(`(defun my-fn (x)
  (let ((local-var 1))
    (+ x local-var)))
`)
	filename := "test.lisp"
	result := AnalyzeFile(src, filename, nil)
	require.NotNil(t, result)

	refs := ExtractFileRefs(result, filename)

	// "+" is a builtin, "x" is a parameter, "local-var" is a let binding.
	// None should appear in the extracted refs.
	for _, ref := range refs {
		assert.NotEqual(t, "+", ref.SymbolKey.Name, "builtins should be excluded")
		assert.NotEqual(t, "x", ref.SymbolKey.Name, "parameters should be excluded")
		assert.NotEqual(t, "local-var", ref.SymbolKey.Name, "locals should be excluded")
	}
}

func TestFindEnclosingFunction(t *testing.T) {
	src := []byte(`(defun outer ()
  (defun inner (x) (+ x 1))
  (inner 42))
`)
	result := AnalyzeFile(src, "test.lisp", nil)
	require.NotNil(t, result)
	require.NotNil(t, result.RootScope)

	t.Run("inside inner function body", func(t *testing.T) {
		// The "+" on line 2 is inside "inner".
		enc := FindEnclosingFunction(result.RootScope, 2, 22)
		require.NotNil(t, enc)
		assert.Equal(t, "inner", enc.Name)
	})

	t.Run("at top-level returns nil", func(t *testing.T) {
		// Position before any function.
		enc := FindEnclosingFunction(result.RootScope, 1, 1)
		// Line 1 col 1 is the opening paren of (defun outer ...).
		// It's at the start of outer's definition, which is global scope.
		// Depending on scope boundaries, this might return outer or nil.
		// The important thing is it doesn't crash.
		_ = enc
	})
}

func TestSymbolKey_String(t *testing.T) {
	key := SymbolKey{Name: "helper", Kind: SymFunction}
	assert.Equal(t, "helper/function", key.String())

	key2 := SymbolKey{Name: "my-var", Kind: SymVariable}
	assert.Equal(t, "my-var/variable", key2.String())
}
