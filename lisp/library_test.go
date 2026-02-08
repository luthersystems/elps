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
