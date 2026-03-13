// Copyright © 2026 The ELPS authors

package minifier

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMinifySource_DeterministicAndScopeAware(t *testing.T) {
	src := []byte(`(defun outer (value)
  (let ((value value))
    value))
`)

	out1, map1, err := MinifySource(src, "test.lisp", nil)
	require.NoError(t, err)

	out2, map2, err := MinifySource(src, "test.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, string(out1), string(out2))
	assert.Equal(t, map1.Entries, map2.Entries)
	assert.Equal(t, "(defun x1 (x2) (let ((x3 x2)) x3))\n", string(out1))
	require.Len(t, map1.Entries, 3)
	assert.Equal(t, "outer", map1.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", map1.MinifiedToOriginal["x2"])
	assert.Equal(t, "value", map1.MinifiedToOriginal["x3"])
}

func TestMinifySource_PreservesQuotedDataAndQualifiedSymbols(t *testing.T) {
	src := []byte(`(defun demo (value)
  '(value pkg:qualified :kw)
  pkg:qualified
  value)
`)

	out, symMap, err := MinifySource(src, "quoted.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) '(value pkg:qualified :kw) pkg:qualified x2)\n", string(out))
	assert.Len(t, symMap.Entries, 2)
}

func TestMinifySource_ExportedSymbolsPreservedByDefault(t *testing.T) {
	src := []byte(`(export 'public)
(defun public (arg)
  arg)
`)

	out, symMap, err := MinifySource(src, "exports.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(export 'public)\n(defun public (x1) x1)\n", string(out))
	assert.Len(t, symMap.Entries, 1)
	assert.Equal(t, "arg", symMap.MinifiedToOriginal["x1"])
}

func TestMinifySource_RenameExportsOptionRewritesExportForms(t *testing.T) {
	src := []byte(`(export 'public)
(defun public (arg)
  arg)
`)

	out, symMap, err := MinifySource(src, "exports.lisp", &Config{RenameExports: true})
	require.NoError(t, err)

	assert.Equal(t, "(export 'x1)\n(defun x1 (x2) x2)\n", string(out))
	assert.Len(t, symMap.Entries, 2)
	assert.Equal(t, "public", symMap.MinifiedToOriginal["x1"])
}

func TestMinify_ExclusionsAndMultiFileSession(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "a.lisp",
			Source: []byte(`(defun keep-name (first)
  first)
`),
		},
		{
			Path: "b.lisp",
			Source: []byte(`(defun helper (second)
  second)
`),
		},
	}

	result, err := Minify(inputs, &Config{
		Exclusions: map[string]bool{"keep-name": true},
	})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Equal(t, "(defun keep-name (x1) x1)\n", string(result.Files[0].Output))
	assert.Equal(t, "(defun x2 (x3) x3)\n", string(result.Files[1].Output))
	assert.Equal(t, "first", result.SymbolMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "helper", result.SymbolMap.MinifiedToOriginal["x2"])
	assert.Equal(t, "second", result.SymbolMap.MinifiedToOriginal["x3"])
}

func TestMinifySource_StripsCommentsByDefault(t *testing.T) {
	src := []byte(`;; comment
(defun demo (value)
  ; lead
  (let ((value (+ value 1))) ; trailing
    value))
`)

	out, _, err := MinifySource(src, "comments.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) (let ((x3 (+ x2 1))) x3))\n", string(out))
}

func TestMinifySource_QuasiquoteOnlyRenamesUnquotedCode(t *testing.T) {
	src := []byte(`(set 'x 1)
(quasiquote (list x (unquote x) (unquote-splicing [x])))
`)

	out, symMap, err := MinifySource(src, "qq.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(set 'x1 1)\n(quasiquote (list x (unquote x1) (unquote-splicing [x])))\n", string(out))
	require.Len(t, symMap.Entries, 1)
	assert.Equal(t, "x", symMap.MinifiedToOriginal["x1"])
}

func TestMinifySource_WithWorkspacePreservesImportedSymbols(t *testing.T) {
	dir := t.TempDir()
	libPath := filepath.Join(dir, "lib.lisp")
	mainPath := filepath.Join(dir, "main.lisp")
	require.NoError(t, os.WriteFile(libPath, []byte("(in-package 'lib)\n(export 'helper)\n(defun helper (value) value)\n"), 0o600))
	require.NoError(t, os.WriteFile(mainPath, []byte("(use-package 'lib)\n(defun outer (value) (helper value))\n"), 0o600))

	wsGlobals, wsPkgs, err := analysis.ScanWorkspaceFull(dir)
	require.NoError(t, err)

	out, symMap, err := MinifySource([]byte("(use-package 'lib)\n(defun outer (value) (helper value))\n"), mainPath, &Config{
		Analysis: &analysis.Config{
			ExtraGlobals:   wsGlobals,
			PackageExports: wsPkgs,
		},
	})
	require.NoError(t, err)

	assert.Equal(t, "(use-package 'lib)\n(defun x1 (x2) (helper x2))\n", string(out))
	require.Len(t, symMap.Entries, 2)
	assert.Equal(t, "outer", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x2"])
}

func TestReadExcludeFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "symbols.spec")
	require.NoError(t, os.WriteFile(path, []byte("; comment\nkeep\n\nstay\n"), 0o600))

	names, err := ReadExcludeFile(path)
	require.NoError(t, err)
	assert.Equal(t, []string{"keep", "stay"}, names)
}

func TestSymbolMapJSON(t *testing.T) {
	symMap := SymbolMap{
		Entries:            []SymbolMapEntry{{Minified: "x1", Original: "foo", Kind: "function"}},
		MinifiedToOriginal: map[string]string{"x1": "foo"},
		OriginalToMinified: map[string][]string{"foo": []string{"x1"}},
	}

	data, err := symMap.JSON()
	require.NoError(t, err)

	var parsed SymbolMap
	require.NoError(t, json.Unmarshal(data, &parsed))
	assert.Equal(t, symMap.Entries, parsed.Entries)
	assert.Equal(t, symMap.MinifiedToOriginal, parsed.MinifiedToOriginal)
	assert.Equal(t, symMap.OriginalToMinified, parsed.OriginalToMinified)
}
