// Copyright © 2021 The ELPS authors

package libhelp_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDocstring(t *testing.T) {
	env := lisp.NewEnv(nil)
	reader := parser.NewReader()
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil())
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil())

	// check docs for builtin function, operator, and macro
	builtinSymbols := []string{"map", "lambda", "defun"}
	for _, name := range builtinSymbols {
		fun := env.Get(lisp.Symbol(name))
		if assert.Equal(t, lisp.LFun, fun.Type) {
			assert.NotEqual(t, "", fun.Docstring())
		}
	}

	rc = env.LoadString("test.lisp", `
	(defun const-string1 () "abc")
	(defun const-string2 () "abc" "")
	`)
	require.True(t, rc.IsNil())

	lisp1 := env.Get(lisp.Symbol("const-string1"))
	if assert.Equal(t, lisp.LFun, lisp1.Type) {
		assert.Equal(t, "", lisp1.Docstring())
	}
	lisp2 := env.Get(lisp.Symbol("const-string2"))
	if assert.Equal(t, lisp.LFun, lisp2.Type) {
		assert.Equal(t, "abc", lisp2.Docstring())
	}
}

func newTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil())
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil())
	return env
}

func TestPackageDoc(t *testing.T) {
	env := newTestEnv(t)

	// Core packages should have doc strings
	packages := []string{"lisp", "user", "time", "math", "string", "base64",
		"json", "regexp", "golang", "testing", "help", "s"}
	for _, name := range packages {
		pkg := env.Runtime.Registry.Packages[name]
		if assert.NotNilf(t, pkg, "package %q should exist", name) {
			assert.NotEqualf(t, "", pkg.Doc, "package %q should have a doc string", name)
		}
	}
}

func TestRenderPkgExported_IncludesDoc(t *testing.T) {
	env := newTestEnv(t)

	var buf bytes.Buffer
	err := libhelp.RenderPkgExported(&buf, env, "math")
	require.NoError(t, err)
	output := buf.String()

	// Should start with package header
	assert.True(t, strings.HasPrefix(output, "package math\n"),
		"output should start with package header, got: %s", output)
	// Should contain the doc string content
	assert.Contains(t, output, "Mathematical functions")
}

func TestPackageDocBuiltin(t *testing.T) {
	env := newTestEnv(t)

	// Use package-doc from lisp source
	rc := env.LoadString("test.lisp", `
	(in-package 'test-pkg)
	(package-doc "A test package for unit testing.")
	(export 'my-fn)
	(defun my-fn () 42)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	pkg := env.Runtime.Registry.Packages["test-pkg"]
	require.NotNil(t, pkg)
	assert.Equal(t, "A test package for unit testing.", pkg.Doc)

	// Verify it renders
	var buf bytes.Buffer
	err := libhelp.RenderPkgExported(&buf, env, "test-pkg")
	require.NoError(t, err)
	output := buf.String()
	assert.Contains(t, output, "package test-pkg")
	assert.Contains(t, output, "A test package for unit testing.")
	assert.Contains(t, output, "my-fn")
}
