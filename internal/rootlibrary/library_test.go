// Copyright © 2026 The ELPS authors

package rootlibrary

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoadSourceSymlinkBeforeParent(t *testing.T) {
	root := t.TempDir()
	require.NoError(t, os.MkdirAll(filepath.Join(root, "a", "b"), 0o700))
	for name, source := range map[string]string{
		"main.lisp":    `(load-file "value.lisp")`,
		"a/main.lisp":  `(load-file "value.lisp")`,
		"value.lisp":   "99",
		"a/value.lisp": "42",
		"nested.lisp":  `(load-file "link/../main.lisp")`,
	} {
		require.NoError(t, os.WriteFile(filepath.Join(root, name), []byte(source), 0o600))
	}
	require.NoError(t, os.Symlink("a/b", filepath.Join(root, "link")))
	require.NoError(t, os.Symlink("link/../main.lisp", filepath.Join(root, "target.lisp")))
	for _, tc := range []struct{ name, path string }{
		{"initial", "link/../main.lisp"},
		{"absolute", root + "/link/../main.lisp"},
		{"nested", "nested.lisp"},
		{"symlink-target", "target.lisp"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			lib, err := Open(root)
			require.NoError(t, err)
			t.Cleanup(func() { require.NoError(t, lib.Close()) })
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			env.Runtime.Library = lib
			require.True(t, lisp.InitializeUserEnv(env).IsNil())
			result := env.LoadFile(tc.path)
			assert.Equal(t, "42", result.String(), "nested load must follow the evaluated source's directory")
		})
	}
}

func TestLoadSourceRefusalDoesNotRevealTarget(t *testing.T) {
	root := t.TempDir()
	outside := filepath.Join(t.TempDir(), "private", "secret.lisp")
	require.NoError(t, os.Symlink(outside, filepath.Join(root, "escape.lisp")))
	lib, err := Open(root)
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, lib.Close()) })
	_, _, _, err = lib.LoadSource(lisp.NewSourceContext("", ""), "escape.lisp")
	require.Error(t, err)
	assert.NotContains(t, err.Error(), outside)
	assert.EqualError(t, err, fmt.Sprintf("cannot load %q within root directory %q", "escape.lisp", root))
	// A host-provided source context must not leak its outside directory
	// through the expanded request in the refusal either.
	_, _, _, err = lib.LoadSource(lisp.NewSourceContext("host", outside), "escape.lisp")
	require.Error(t, err)
	assert.NotContains(t, err.Error(), "private")
	assert.EqualError(t, err, fmt.Sprintf("cannot load %q within root directory %q", "escape.lisp", root))
}
