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
