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
`), 0644)
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
`), 0644)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`
(defun fn-b () 2)
(export 'fn-b)
`), 0644)
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
`), 0644)
	require.NoError(t, err)

	// Malformed file
	err = os.WriteFile(filepath.Join(dir, "bad.lisp"), []byte(`(unclosed`), 0644)
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

	err := os.WriteFile(filepath.Join(dir, "readme.txt"), []byte("not lisp"), 0644)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)
	assert.Empty(t, syms)
}

func TestScanWorkspace_Subdirectories(t *testing.T) {
	dir := t.TempDir()
	subdir := filepath.Join(dir, "sub")
	err := os.MkdirAll(subdir, 0755)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(subdir, "deep.lisp"), []byte(`
(defun deep-fn () 42)
(export 'deep-fn)
`), 0644)
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
`), 0644)
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
`), 0644)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "when", syms[0].Name)
	assert.Equal(t, SymMacro, syms[0].Kind)
}
