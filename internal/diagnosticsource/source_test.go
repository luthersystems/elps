// Copyright © 2026 The ELPS authors

package diagnosticsource

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/internal/rootlibrary"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSourceReaderPhysicalSource(t *testing.T) {
	root := t.TempDir()
	file := filepath.Join(root, "main.lisp")
	require.NoError(t, os.WriteFile(file, []byte("missing-symbol ; physical source"), 0o600))
	lib, err := rootlibrary.Open(root)
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, lib.Close()) })
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = NewReader(parser.NewReader())
	env.Runtime.Library = lib
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	// An inline label colliding with a physical path must not replace file text.
	require.Equal(t, lisp.LError, env.LoadString(file, "other-missing-symbol").Type)
	result := env.LoadFile("main.lisp")
	require.Equal(t, lisp.LError, result.Type)
	read := SourceReader(env.Runtime, result)
	source, err := read(file)
	require.NoError(t, err)
	assert.Equal(t, "missing-symbol ; physical source", string(source))
	// Even an existing file is unavailable without the active library.
	env.Runtime.Library = nil
	_, err = read(file)
	require.ErrorIs(t, err, os.ErrNotExist)
}
