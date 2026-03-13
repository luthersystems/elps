// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/minifier"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMinifyCommand_DefaultFlags(t *testing.T) {
	assert.Equal(t, "minify [flags] [files...]", minifyCmd.Use)

	for _, name := range []string{"write", "map", "exclude", "exclude-file", "workspace", "rename-exports"} {
		assert.NotNil(t, minifyCmd.Flags().Lookup(name), "missing flag: %s", name)
	}
}

func TestRunMinify_StdoutSingleFile(t *testing.T) {
	resetMinifyFlags()
	dir := t.TempDir()
	path := filepath.Join(dir, "demo.lisp")
	require.NoError(t, os.WriteFile(path, []byte("(defun outer (value) value)\n"), 0o600))

	var out bytes.Buffer
	err := runMinify([]string{path}, bytes.NewBuffer(nil), &out)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) x2)\n", out.String())
	src, err := os.ReadFile(path) //nolint:gosec // test reads temp file created in this test
	require.NoError(t, err)
	assert.Equal(t, "(defun outer (value) value)\n", string(src))
}

func TestRunMinify_WriteAndMap(t *testing.T) {
	resetMinifyFlags()
	minifyWrite = true
	dir := t.TempDir()
	path := filepath.Join(dir, "demo.lisp")
	mapPath := filepath.Join(dir, "map.json")
	minifyMapPath = mapPath
	require.NoError(t, os.WriteFile(path, []byte("(defun outer (value) value)\n"), 0o600))

	var out bytes.Buffer
	err := runMinify([]string{path}, bytes.NewBuffer(nil), &out)
	require.NoError(t, err)
	assert.Empty(t, out.String())

	src, err := os.ReadFile(path) //nolint:gosec // test reads temp file created in this test
	require.NoError(t, err)
	assert.Equal(t, "(defun x1 (x2) x2)\n", string(src))

	mapBytes, err := os.ReadFile(mapPath) //nolint:gosec // test reads temp file created in this test
	require.NoError(t, err)
	var symMap minifier.SymbolMap
	require.NoError(t, json.Unmarshal(mapBytes, &symMap))
	assert.Equal(t, "outer", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x2"])
}

func TestRunMinify_Stdin(t *testing.T) {
	resetMinifyFlags()

	var out bytes.Buffer
	err := runMinify(nil, bytes.NewBufferString("(defun outer (value) value)\n"), &out)
	require.NoError(t, err)
	assert.Equal(t, "(defun x1 (x2) x2)\n", out.String())
}

func TestRunMinify_MultipleFilesRequireWrite(t *testing.T) {
	resetMinifyFlags()
	err := runMinify([]string{"a.lisp", "b.lisp"}, bytes.NewBuffer(nil), &bytes.Buffer{})
	require.Error(t, err)
	assert.Contains(t, err.Error(), "multiple files require --write")
}

func TestRunMinify_RenameExports(t *testing.T) {
	resetMinifyFlags()
	minifyRenameExports = true
	dir := t.TempDir()
	path := filepath.Join(dir, "exports.lisp")
	require.NoError(t, os.WriteFile(path, []byte("(export 'public)\n(defun public (arg) arg)\n"), 0o600))

	var out bytes.Buffer
	err := runMinify([]string{path}, bytes.NewBuffer(nil), &out)
	require.NoError(t, err)
	assert.Equal(t, "(export 'x1)\n(defun x1 (x2) x2)\n", out.String())
}

func resetMinifyFlags() {
	minifyWrite = false
	minifyMapPath = ""
	minifyExcludeFiles = nil
	minifyExcludes = nil
	minifyWorkspace = ""
	minifyRenameExports = false
}
