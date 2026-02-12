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
	// (defun f () "abc" "") — body is all strings, so no docstring
	// (it's a constant function returning "")
	lisp2 := env.Get(lisp.Symbol("const-string2"))
	if assert.Equal(t, lisp.LFun, lisp2.Type) {
		assert.Equal(t, "", lisp2.Docstring())
	}

	// Multi-string docstrings are concatenated with spaces
	rc = env.LoadString("test2.lisp", `
	(defun multi-doc ()
		"First line of docs."
		"Second line of docs."
		42)
	`)
	require.True(t, rc.IsNil())
	multi := env.Get(lisp.Symbol("multi-doc"))
	if assert.Equal(t, lisp.LFun, multi.Type) {
		assert.Equal(t, "First line of docs. Second line of docs.", multi.Docstring())
	}

	// Empty strings produce paragraph breaks
	rc = env.LoadString("test3.lisp", `
	(defun para-doc ()
		"First paragraph."
		""
		"Second paragraph."
		42)
	`)
	require.True(t, rc.IsNil())
	para := env.Get(lisp.Symbol("para-doc"))
	if assert.Equal(t, lisp.LFun, para.Type) {
		assert.Equal(t, "First paragraph.\n\nSecond paragraph.", para.Docstring())
	}

	// All strings with no body = no docstring (it's a constant function)
	rc = env.LoadString("test4.lisp", `
	(defun all-strings () "a" "b" "c")
	`)
	require.True(t, rc.IsNil())
	allStr := env.Get(lisp.Symbol("all-strings"))
	if assert.Equal(t, lisp.LFun, allStr.Type) {
		assert.Equal(t, "", allStr.Docstring())
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

	// Doc string as trailing argument to in-package
	rc := env.LoadString("test.lisp", `
	(in-package 'test-pkg
		"A test package" "for unit testing.")
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

	// Empty strings produce paragraph breaks in package docs
	rc = env.LoadString("test2.lisp", `
	(in-package 'para-pkg
		"First paragraph."
		""
		"Second paragraph.")
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)
	paraPkg := env.Runtime.Registry.Packages["para-pkg"]
	require.NotNil(t, paraPkg)
	assert.Equal(t, "First paragraph.\n\nSecond paragraph.", paraPkg.Doc)
}

func TestSymbolDoc(t *testing.T) {
	env := newTestEnv(t)

	// set with trailing doc strings
	rc := env.LoadString("test.lisp", `
	(in-package 'sym-doc-pkg)
	(set 'my-const 42 "The answer" "to everything.")
	(export 'my-const)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	pkg := env.Runtime.Registry.Packages["sym-doc-pkg"]
	require.NotNil(t, pkg)
	assert.Equal(t, "The answer to everything.", pkg.SymbolDocs["my-const"])

	// set with paragraph breaks in docs
	rc = env.LoadString("test2.lisp", `
	(in-package 'sym-doc-pkg)
	(set 'documented 100 "First para." "" "Second para.")
	`)
	require.NotEqualf(t, lisp.LError, rc.Type, "LoadString failed: %v", rc)
	assert.Equal(t, "First para.\n\nSecond para.", pkg.SymbolDocs["documented"])

	// set without docs — no doc entry
	rc = env.LoadString("test3.lisp", `
	(in-package 'sym-doc-pkg)
	(set 'undoc 0)
	`)
	require.NotEqualf(t, lisp.LError, rc.Type, "LoadString failed: %v", rc)
	assert.Equal(t, "", pkg.SymbolDocs["undoc"])
}

func TestDefconstMacro(t *testing.T) {
	env := newTestEnv(t)

	rc := env.LoadString("test.lisp", `
	(in-package 'const-pkg)
	(defconst max-retries 3 "Maximum number of retries.")
	(defconst pi-approx 3.14
		"Approximate value of pi."
		"Good enough for most uses.")
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	pkg := env.Runtime.Registry.Packages["const-pkg"]
	require.NotNil(t, pkg)

	// Value is bound
	val := pkg.Get(lisp.Symbol("max-retries"))
	assert.Equal(t, lisp.LInt, val.Type)
	assert.Equal(t, 3, val.Int)

	// Symbol is exported
	assert.Contains(t, pkg.Externals, "max-retries")
	assert.Contains(t, pkg.Externals, "pi-approx")

	// Doc is set
	assert.Equal(t, "Maximum number of retries.", pkg.SymbolDocs["max-retries"])
	assert.Equal(t, "Approximate value of pi. Good enough for most uses.", pkg.SymbolDocs["pi-approx"])
}

func TestRenderVarWithDoc(t *testing.T) {
	env := newTestEnv(t)

	// math:pi should have a doc string and render it
	var buf bytes.Buffer
	err := libhelp.RenderVar(&buf, env, "math:pi")
	require.NoError(t, err)
	output := buf.String()
	assert.Contains(t, output, "pi")
	assert.Contains(t, output, "circumference")
}

func TestRenderPkgExported_ConstantDocs(t *testing.T) {
	env := newTestEnv(t)

	var buf bytes.Buffer
	err := libhelp.RenderPkgExported(&buf, env, "math")
	require.NoError(t, err)
	output := buf.String()

	// Constants should appear with their docs
	assert.Contains(t, output, "pi")
	assert.Contains(t, output, "circumference")
	assert.Contains(t, output, "inf")
}

func TestLookupSymbolDoc_Qualified(t *testing.T) {
	env := newTestEnv(t)

	// json:null should have a doc via qualified name lookup
	var buf bytes.Buffer
	err := libhelp.RenderVar(&buf, env, "json:null")
	require.NoError(t, err)
	output := buf.String()
	assert.Contains(t, output, "null")
	assert.Contains(t, output, "JSON")
}

func TestCheckMissing_StdlibAllDocumented(t *testing.T) {
	env := newTestEnv(t)

	// The stdlib should have complete documentation (CI enforces this).
	missing := libhelp.CheckMissing(env)
	assert.Empty(t, missing, "stdlib should have no missing docs, got: %v", missing)
}

func TestCheckMissing_DetectsUndocumented(t *testing.T) {
	env := newTestEnv(t)

	// Add a package with an undocumented export.
	rc := env.LoadString("test.lisp", `
	(in-package 'undoc-pkg "A documented package.")
	(export 'undoc-fn)
	(defun undoc-fn () 42)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	missing := libhelp.CheckMissing(env)

	// Should find the undocumented function.
	var found bool
	for _, m := range missing {
		if m.Name == "undoc-pkg:undoc-fn" {
			found = true
			assert.Equal(t, "function", m.Kind)
			break
		}
	}
	assert.True(t, found, "should detect undocumented function undoc-pkg:undoc-fn")
}

func TestCheckMissing_DetectsUndocumentedPackage(t *testing.T) {
	env := newTestEnv(t)

	// Add a package with no doc string.
	rc := env.LoadString("test.lisp", `
	(in-package 'nodoc-pkg)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	missing := libhelp.CheckMissing(env)

	var found bool
	for _, m := range missing {
		if m.Name == "nodoc-pkg" && m.Kind == "package" {
			found = true
			break
		}
	}
	assert.True(t, found, "should detect undocumented package nodoc-pkg")
}

func TestCheckMissing_DetectsUndocumentedConstant(t *testing.T) {
	env := newTestEnv(t)

	rc := env.LoadString("test.lisp", `
	(in-package 'const-check-pkg "A package with a constant.")
	(set 'my-val 42)
	(export 'my-val)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	missing := libhelp.CheckMissing(env)

	var found bool
	for _, m := range missing {
		if m.Name == "const-check-pkg:my-val" {
			found = true
			assert.Equal(t, "int", m.Kind, "undocumented int constant should have Kind 'int'")
			break
		}
	}
	assert.True(t, found, "should detect undocumented constant const-check-pkg:my-val")
}

func TestCheckMissing_DocumentedExportNotReported(t *testing.T) {
	env := newTestEnv(t)

	rc := env.LoadString("test.lisp", `
	(in-package 'doc-check-pkg "A documented package.")
	(export 'documented-fn)
	(defun documented-fn ()
		"This function is documented."
		42)
	`)
	require.Truef(t, rc.IsNil(), "LoadString failed: %v", rc)

	missing := libhelp.CheckMissing(env)

	for _, m := range missing {
		assert.NotEqual(t, "doc-check-pkg:documented-fn", m.Name,
			"documented function should not be reported as missing")
		if m.Kind == "package" {
			assert.NotEqual(t, "doc-check-pkg", m.Name,
				"documented package should not be reported as missing")
		}
	}
}

func TestCheckMissing_ExcludesUserPackage(t *testing.T) {
	env := newTestEnv(t)

	missing := libhelp.CheckMissing(env)

	for _, m := range missing {
		if m.Kind == "package" {
			assert.NotEqual(t, "user", m.Name,
				"user package should be excluded from missing docs check")
		}
	}
}

func TestLoadPackage_RestoresPreviousPackage(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())

	// Before LoadPackage, we should be in "user".
	require.Equal(t, "user", env.Runtime.Package.Name)

	rc = libhelp.LoadPackage(env)
	require.True(t, rc.IsNil())

	// After LoadPackage, we should still be in "user".
	assert.Equal(t, "user", env.Runtime.Package.Name,
		"LoadPackage should restore the previous package")
}

func TestCheckMissing_NoDuplicates(t *testing.T) {
	env := newTestEnv(t)

	missing := libhelp.CheckMissing(env)

	seen := make(map[string]bool, len(missing))
	for _, m := range missing {
		key := m.Kind + ":" + m.Name
		assert.False(t, seen[key], "duplicate entry in CheckMissing results: %s %s", m.Kind, m.Name)
		seen[key] = true
	}
}
