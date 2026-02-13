// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

// testSourceContext implements lisp.SourceContext for unit tests.
type testSourceContext struct {
	name string
	loc  string
}

func (c *testSourceContext) Name() string     { return c.name }
func (c *testSourceContext) Location() string { return c.loc }

func TestRootDirConfinement_AllowsWithinRoot(t *testing.T) {
	root, err := filepath.Abs("testfixtures")
	if err != nil {
		t.Fatal(err)
	}
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{RootDir: root}

	lok := env.LoadFile(filepath.Join(root, "test1.lisp"))
	assert.NotEqual(t, lisp.LError, lok.Type, "loading file within root should succeed: %v", lok)
}

func TestRootDirConfinement_BlocksOutsideRoot(t *testing.T) {
	root, err := filepath.Abs("testfixtures")
	if err != nil {
		t.Fatal(err)
	}
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{RootDir: root}

	// Attempting to load a file outside the root must fail.
	lok := env.LoadFile("/etc/hosts")
	assert.Equal(t, lisp.LError, lok.Type, "loading file outside root should fail")

	// Path traversal with .. must also fail.
	lok = env.LoadFile(filepath.Join(root, "..", "library.go"))
	assert.Equal(t, lisp.LError, lok.Type, "path traversal outside root should fail")
}

func TestRootDirConfinement_EmptyRootAllowsAll(t *testing.T) {
	// When RootDir is empty (zero value), no confinement is applied.
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}

	lok := env.LoadFile("testfixtures/test1.lisp")
	assert.NotEqual(t, lisp.LError, lok.Type, "empty RootDir should allow loading: %v", lok)
}

func TestRootDirConfinement_BlocksSymlinksOutsideRoot(t *testing.T) {
	// Create a temp directory structure with a symlink pointing outside the root.
	rootDir := t.TempDir()
	outsideDir := t.TempDir()

	// Create a valid lisp file outside the root.
	outsideFile := filepath.Join(outsideDir, "secret.lisp")
	if err := os.WriteFile(outsideFile, []byte("(+ 1 1)"), 0600); err != nil {
		t.Fatal(err)
	}

	// Create a symlink inside rootDir that points to the outside file.
	symlinkPath := filepath.Join(rootDir, "escape.lisp")
	if err := os.Symlink(outsideFile, symlinkPath); err != nil {
		t.Skip("platform does not support symlinks")
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	// Loading via the symlink should be blocked because the real path
	// is outside the root directory.
	_, _, _, err := lib.LoadSource(ctx, symlinkPath)
	assert.Error(t, err, "symlink pointing outside root should be blocked")
	assert.Contains(t, err.Error(), "access denied")

	// Loading a real file inside the root should still work.
	realFile := filepath.Join(rootDir, "ok.lisp")
	if werr := os.WriteFile(realFile, []byte("(+ 2 3)"), 0600); werr != nil {
		t.Fatal(werr)
	}
	_, _, data, err := lib.LoadSource(ctx, realFile)
	assert.NoError(t, err, "real file inside root should load")
	assert.Equal(t, "(+ 2 3)", string(data))
}

func TestRootDirConfinement_ReadsResolvedPath(t *testing.T) {
	// Verify that after confinement check, the file is read from the
	// resolved (symlink-followed) path, not the original. This prevents
	// TOCTOU races where a symlink target changes between resolution and read.
	rootDir := t.TempDir()
	subDir := filepath.Join(rootDir, "sub")
	if err := os.Mkdir(subDir, 0700); err != nil {
		t.Fatal(err)
	}

	// Create real file inside the root.
	realFile := filepath.Join(subDir, "real.lisp")
	if err := os.WriteFile(realFile, []byte("(+ 10 20)"), 0600); err != nil {
		t.Fatal(err)
	}

	// Create symlink inside root pointing to the real file (also inside root).
	symlinkPath := filepath.Join(rootDir, "link.lisp")
	if err := os.Symlink(realFile, symlinkPath); err != nil {
		t.Skip("platform does not support symlinks")
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	// The returned trueloc should be the resolved path, not the symlink.
	_, trueloc, data, err := lib.LoadSource(ctx, symlinkPath)
	assert.NoError(t, err, "symlink within root should be allowed")
	assert.Equal(t, "(+ 10 20)", string(data))
	// Resolve realFile too — on macOS, /var is a symlink to /private/var.
	resolvedReal, rerr := filepath.EvalSymlinks(realFile)
	if rerr != nil {
		t.Fatal(rerr)
	}
	assert.Equal(t, resolvedReal, trueloc,
		"trueloc should be the resolved (real) path, not the symlink")
}

func TestRootDirConfinement_EvalSymlinksErrorOnLoc(t *testing.T) {
	// When RootDir is set and EvalSymlinks fails on loc (e.g., broken
	// symlink), LoadSource must return an error instead of silently using
	// the unresolved path.
	rootDir := t.TempDir()

	// Create a broken symlink (points to non-existent target).
	brokenLink := filepath.Join(rootDir, "broken.lisp")
	if err := os.Symlink(filepath.Join(rootDir, "nonexistent.lisp"), brokenLink); err != nil {
		t.Skip("platform does not support symlinks")
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	_, _, _, err := lib.LoadSource(ctx, brokenLink)
	assert.Error(t, err, "broken symlink should produce an error when RootDir is set")
	assert.Contains(t, err.Error(), "cannot resolve path")
}

func TestRootDirConfinement_EvalSymlinksErrorOnRoot(t *testing.T) {
	// When RootDir points to a non-existent directory, LoadSource must
	// return an error instead of silently using the unresolved path.
	lib := &lisp.RelativeFileSystemLibrary{RootDir: "/nonexistent/root/dir"}
	ctx := &testSourceContext{name: "test", loc: ""}

	_, _, _, err := lib.LoadSource(ctx, "/some/file.lisp")
	assert.Error(t, err, "non-existent RootDir should produce an error")
	assert.Contains(t, err.Error(), "cannot resolve root directory")
}

func TestRootDirConfinement_BlocksSymlinkChain(t *testing.T) {
	// Verify that symlink chains (link1 → link2 → outside) are blocked.
	rootDir := t.TempDir()
	outsideDir := t.TempDir()

	outsideFile := filepath.Join(outsideDir, "secret.lisp")
	if err := os.WriteFile(outsideFile, []byte("secret"), 0600); err != nil {
		t.Fatal(err)
	}

	// link2 → outside file
	link2 := filepath.Join(rootDir, "link2.lisp")
	if err := os.Symlink(outsideFile, link2); err != nil {
		t.Skip("platform does not support symlinks")
	}
	// link1 → link2 (chain: link1 → link2 → outside)
	link1 := filepath.Join(rootDir, "link1.lisp")
	if err := os.Symlink(link2, link1); err != nil {
		t.Fatal(err)
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	_, _, _, err := lib.LoadSource(ctx, link1)
	assert.Error(t, err, "symlink chain escaping root should be blocked")
	assert.Contains(t, err.Error(), "access denied")
}

func TestRootDirConfinement_BlocksSymlinkInParentDir(t *testing.T) {
	// Verify that symlinked parent directories pointing outside root
	// are detected and blocked.
	rootDir := t.TempDir()
	outsideDir := t.TempDir()

	outsideFile := filepath.Join(outsideDir, "escape.lisp")
	if err := os.WriteFile(outsideFile, []byte("escaped!"), 0600); err != nil {
		t.Fatal(err)
	}

	// Create symlink where a path component points outside root.
	linkDir := filepath.Join(rootDir, "link")
	if err := os.Symlink(outsideDir, linkDir); err != nil {
		t.Skip("platform does not support symlinks")
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	_, _, _, err := lib.LoadSource(ctx, filepath.Join(linkDir, "escape.lisp"))
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "access denied")
}

func TestRootDirConfinement_PrefixBoundary(t *testing.T) {
	// Verify that the confinement check is not fooled by paths that
	// share a common prefix but are not within the root directory.
	// E.g., root=/tmp/foo must not allow /tmp/foobar/file.lisp.
	parentDir := t.TempDir()
	rootDir := filepath.Join(parentDir, "root")
	siblingDir := filepath.Join(parentDir, "rootextra")
	if err := os.MkdirAll(rootDir, 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(siblingDir, 0700); err != nil {
		t.Fatal(err)
	}
	siblingFile := filepath.Join(siblingDir, "file.lisp")
	if err := os.WriteFile(siblingFile, []byte("(+ 1 1)"), 0600); err != nil {
		t.Fatal(err)
	}

	lib := &lisp.RelativeFileSystemLibrary{RootDir: rootDir}
	ctx := &testSourceContext{name: "test", loc: ""}

	_, _, _, err := lib.LoadSource(ctx, siblingFile)
	assert.Error(t, err, "file in sibling directory with shared prefix should be blocked")
	assert.Contains(t, err.Error(), "access denied")
}

func TestFSLibrary_LoadsFile(t *testing.T) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS("testfixtures")}

	lok := env.LoadFile("test1.lisp")
	assert.NotEqual(t, lisp.LError, lok.Type, "FSLibrary should load file: %v", lok)
}

func TestFSLibrary_BlocksPathTraversal(t *testing.T) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS("testfixtures")}

	// Path traversal with .. must fail (fs.FS contract rejects ..).
	lok := env.LoadFile("../library.go")
	assert.Equal(t, lisp.LError, lok.Type, "FSLibrary should block path traversal")

	// Absolute paths must fail (fs.FS paths must be unrooted).
	lok = env.LoadFile("/etc/hosts")
	assert.Equal(t, lisp.LError, lok.Type, "FSLibrary should block absolute paths")
}
