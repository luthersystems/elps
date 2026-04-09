package analysis

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNormalizePath_SymlinkedMissingNestedPath(t *testing.T) {
	root := t.TempDir()
	realDir := filepath.Join(root, "real")
	linkDir := filepath.Join(root, "alias")
	require.NoError(t, os.Mkdir(realDir, 0o750))
	if err := os.Symlink(realDir, linkDir); err != nil {
		t.Skipf("symlinks unavailable: %v", err)
	}

	got := NormalizePath(filepath.Join(linkDir, "sub", "dir", "file.lisp"))
	want := NormalizePath(filepath.Join(realDir, "sub", "dir", "file.lisp"))
	assert.Equal(t, want, got)
}
