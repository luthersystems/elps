// Copyright © 2024 The ELPS authors

package analysis

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
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
`), 0600)
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
`), 0600)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`
(defun fn-b () 2)
(export 'fn-b)
`), 0600)
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
`), 0600)
	require.NoError(t, err)

	// Malformed file
	err = os.WriteFile(filepath.Join(dir, "bad.lisp"), []byte(`(unclosed`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range syms {
		names[s.Name] = true
	}
	assert.True(t, names["good-fn"], "good file should still be scanned")
}

func TestScanWorkspaceDefinitions_KeepsSameNameAcrossPackages(t *testing.T) {
	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "multi.lisp"), []byte(`
(in-package 'foo)
(defun helper () 1)
(in-package 'bar)
(defun helper () 2)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspaceDefinitions(dir)
	require.NoError(t, err)
	require.Len(t, syms, 2)

	packages := map[string]bool{}
	for _, sym := range syms {
		if sym.Name == "helper" {
			packages[sym.Package] = true
		}
	}
	assert.True(t, packages["foo"])
	assert.True(t, packages["bar"])
}

func TestScanWorkspace_DoesNotLeakNonExportedDefsAcrossPackages(t *testing.T) {
	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "multi.lisp"), []byte(`
(in-package 'foo)
(defun helper () 1)
(export 'helper)
(in-package 'bar)
(defun helper () 2)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)
	require.Len(t, syms, 1)
	assert.Equal(t, "helper", syms[0].Name)
	assert.Equal(t, "foo", syms[0].Package)
}

func TestScanWorkspacePackages_UsesPackageQualifiedDefinitions(t *testing.T) {
	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "multi.lisp"), []byte(`
(in-package 'foo)
(defun helper () 1)
(export 'helper)
(in-package 'bar)
(defun helper () 2)
`), 0600)
	require.NoError(t, err)

	pkgs, err := ScanWorkspacePackages(dir)
	require.NoError(t, err)
	require.Contains(t, pkgs, "foo")
	require.Len(t, pkgs["foo"], 1)
	assert.Equal(t, "helper", pkgs["foo"][0].Name)
	assert.Equal(t, "foo", pkgs["foo"][0].Package)
	assert.NotContains(t, pkgs, "bar")
}

func TestScanWorkspace_SkipsNonLisp(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "readme.txt"), []byte("not lisp"), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)
	assert.Empty(t, syms)
}

func TestScanWorkspace_Subdirectories(t *testing.T) {
	dir := t.TempDir()
	subdir := filepath.Join(dir, "sub")
	err := os.MkdirAll(subdir, 0750)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(subdir, "deep.lisp"), []byte(`
(defun deep-fn () 42)
(export 'deep-fn)
`), 0600)
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
`), 0600)
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
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "when", syms[0].Name)
	assert.Equal(t, SymMacro, syms[0].Kind)
}

func TestScanWorkspace_DocStringExtracted(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun greet (name)
  "Say hello to someone."
  (concat "Hello, " name))
(export 'greet)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "greet", syms[0].Name)
	assert.Equal(t, "Say hello to someone.", syms[0].DocString)
}

func TestScanWorkspace_NoDocStringWhenNoBody(t *testing.T) {
	dir := t.TempDir()

	// A function with only a string (constant function) has no docstring.
	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun version () "1.0.0")
(export 'version)
`), 0600)
	require.NoError(t, err)

	syms, err := ScanWorkspace(dir)
	require.NoError(t, err)

	require.Len(t, syms, 1)
	assert.Equal(t, "version", syms[0].Name)
	assert.Empty(t, syms[0].DocString, "string-only body should not be treated as docstring")
}

func TestScanWorkspaceFull_SkipsHiddenDirs(t *testing.T) {
	dir := t.TempDir()

	// Create a hidden directory with a lisp file.
	hiddenDir := filepath.Join(dir, ".git")
	err := os.MkdirAll(hiddenDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(hiddenDir, "config.lisp"), []byte(`
(defun hidden-fn () 42)
(export 'hidden-fn)
`), 0600)
	require.NoError(t, err)

	// Create a node_modules directory.
	nmDir := filepath.Join(dir, "node_modules")
	err = os.MkdirAll(nmDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(nmDir, "dep.lisp"), []byte(`
(defun dep-fn () 42)
(export 'dep-fn)
`), 0600)
	require.NoError(t, err)

	// Create a build directory.
	buildDir := filepath.Join(dir, "build")
	err = os.MkdirAll(buildDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(buildDir, "generated.lisp"), []byte(`
(defun build-fn () 42)
(export 'build-fn)
`), 0600)
	require.NoError(t, err)

	// Create an underscore-prefixed directory.
	archiveDir := filepath.Join(dir, "_archive")
	err = os.MkdirAll(archiveDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(archiveDir, "old.lisp"), []byte(`
(defun archive-fn () 42)
(export 'archive-fn)
`), 0600)
	require.NoError(t, err)

	// Create a visible file.
	err = os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun visible-fn () 42)
(export 'visible-fn)
`), 0600)
	require.NoError(t, err)

	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	names := make(map[string]bool)
	for _, s := range globals {
		names[s.Name] = true
	}
	for _, syms := range pkgs {
		for _, s := range syms {
			names[s.Name] = true
		}
	}

	assert.True(t, names["visible-fn"], "visible file should be scanned")
	assert.False(t, names["hidden-fn"], "hidden directory files should be skipped")
	assert.False(t, names["dep-fn"], "node_modules files should be skipped")
	assert.False(t, names["build-fn"], "build directory files should be skipped")
	assert.False(t, names["archive-fn"], "underscore-prefixed directory files should be skipped")
}

func TestCollectLispFiles_IncludeDirs(t *testing.T) {
	dir := t.TempDir()

	// Create an underscore-prefixed directory that would normally be skipped.
	exDir := filepath.Join(dir, "_examples")
	err := os.MkdirAll(exDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(exDir, "demo.lisp"), []byte(`(defun demo () 1)`), 0600)
	require.NoError(t, err)

	// Create a build directory that would normally be skipped.
	buildDir := filepath.Join(dir, "build")
	err = os.MkdirAll(buildDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(buildDir, "gen.lisp"), []byte(`(defun gen () 2)`), 0600)
	require.NoError(t, err)

	// Create a normal file.
	err = os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`(defun lib-fn () 3)`), 0600)
	require.NoError(t, err)

	// Without includes: only lib.lisp should be found.
	paths, _, err := collectLispFilesWithConfig(dir, nil)
	require.NoError(t, err)
	require.Len(t, paths, 1, "without includes, skipped dirs should be excluded")
	assert.Equal(t, "lib.lisp", filepath.Base(paths[0]), "only the normal file should be found")

	// With includes: _examples should be included, build still skipped.
	cfg := &ScanConfig{IncludeDirs: []string{"_examples"}}
	paths, _, err = collectLispFilesWithConfig(dir, cfg)
	require.NoError(t, err)
	assert.Len(t, paths, 2, "_examples should be included via IncludeDirs")

	found := make(map[string]bool)
	for _, p := range paths {
		found[filepath.Base(p)] = true
	}
	assert.True(t, found["lib.lisp"], "normal file should be found")
	assert.True(t, found["demo.lisp"], "included dir file should be found")
	assert.False(t, found["gen.lisp"], "non-included skipped dir should still be excluded")
}

func TestCollectLispFiles_IncludeDirs_Nested(t *testing.T) {
	dir := t.TempDir()

	// _examples is included, but _examples/_internal should still be skipped
	// because IncludeDirs matches directory names individually, not recursively.
	exDir := filepath.Join(dir, "_examples")
	require.NoError(t, os.MkdirAll(exDir, 0750))
	require.NoError(t, os.WriteFile(filepath.Join(exDir, "top.lisp"), []byte(`(defun top () 1)`), 0600))

	nestedDir := filepath.Join(exDir, "_internal")
	require.NoError(t, os.MkdirAll(nestedDir, 0750))
	require.NoError(t, os.WriteFile(filepath.Join(nestedDir, "deep.lisp"), []byte(`(defun deep () 2)`), 0600))

	cfg := &ScanConfig{IncludeDirs: []string{"_examples"}}
	paths, _, err := collectLispFilesWithConfig(dir, cfg)
	require.NoError(t, err)

	found := make(map[string]bool)
	for _, p := range paths {
		found[filepath.Base(p)] = true
	}
	assert.True(t, found["top.lisp"], "direct child of included dir should be found")
	assert.False(t, found["deep.lisp"], "nested underscore dir inside included dir should still be skipped")
}

func TestMatchesInclude(t *testing.T) {
	// nil list never matches.
	assert.False(t, matchesInclude("_examples", nil))

	// Empty list never matches.
	assert.False(t, matchesInclude("_examples", []string{}))

	// Exact match.
	assert.True(t, matchesInclude("_examples", []string{"_examples"}))

	// No match.
	assert.False(t, matchesInclude("_archive", []string{"_examples"}))

	// Multiple entries.
	assert.True(t, matchesInclude("vendor", []string{"_examples", "vendor"}))

	// Can override hidden dirs.
	assert.True(t, matchesInclude(".hidden", []string{".hidden"}))

	// Case sensitive — no match on different case.
	assert.False(t, matchesInclude("_Examples", []string{"_examples"}))
}

func TestShouldSkipDir(t *testing.T) {
	// Special cases: . and .. are never skipped.
	assert.False(t, ShouldSkipDir("."), "current directory should not be skipped")
	assert.False(t, ShouldSkipDir(".."), "parent directory should not be skipped")

	// Hidden directories (dot-prefixed).
	assert.True(t, ShouldSkipDir(".git"), "hidden directory should be skipped")
	assert.True(t, ShouldSkipDir(".vscode"), "hidden directory should be skipped")
	assert.True(t, ShouldSkipDir(".a"), "single-char hidden directory should be skipped")

	// Underscore-prefixed directories.
	assert.True(t, ShouldSkipDir("_archive"), "underscore-prefixed directory should be skipped")
	assert.True(t, ShouldSkipDir("_"), "bare underscore directory should be skipped")

	// Named directories.
	assert.True(t, ShouldSkipDir("node_modules"), "node_modules should be skipped")
	assert.True(t, ShouldSkipDir("vendor"), "vendor should be skipped")
	assert.True(t, ShouldSkipDir("build"), "build should be skipped")

	// Boundary: similar names that should NOT be skipped.
	assert.False(t, ShouldSkipDir("builder"), "builder should not be skipped (not exact match)")
	assert.False(t, ShouldSkipDir("src"), "normal directory should not be skipped")
	assert.False(t, ShouldSkipDir("lib"), "normal directory should not be skipped")

	// Empty string (defensive).
	assert.False(t, ShouldSkipDir(""), "empty string should not be skipped")
}

func TestScanWorkspaceFull_CombinedResults(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(in-package 'mylib)
(defun helper (x) (+ x 1))
(export 'helper)
`), 0600)
	require.NoError(t, err)

	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	// Globals should contain the exported symbol.
	require.Len(t, globals, 1)
	assert.Equal(t, "helper", globals[0].Name)
	assert.Equal(t, SymFunction, globals[0].Kind)

	// Package exports should group by package with Package field set.
	require.Contains(t, pkgs, "mylib")
	require.Len(t, pkgs["mylib"], 1)
	assert.Equal(t, "helper", pkgs["mylib"][0].Name)
	assert.Equal(t, "mylib", pkgs["mylib"][0].Package,
		"package export should have Package field set")
	assert.Equal(t, SymFunction, pkgs["mylib"][0].Kind)
}

func TestScanWorkspaceRefs_CrossFile(t *testing.T) {
	dir := t.TempDir()

	// File A defines and exports "helper".
	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`(defun helper (x) (+ x 1))
(export 'helper)
`), 0600)
	require.NoError(t, err)

	// File B calls "helper" inside a function.
	err = os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(defun caller () (helper 42))
`), 0600)
	require.NoError(t, err)

	// Phase 1: Scan definitions to build config.
	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   globals,
		PackageExports: pkgs,
	}

	// Phase 2: Scan references.
	refs := ScanWorkspaceRefs(dir, cfg, nil)

	// There should be a reference to "helper" from file B.
	helperKey := SymbolKey{Package: "user", Name: "helper", Kind: SymFunction}.String()
	helperRefs := refs[helperKey]
	require.NotEmpty(t, helperRefs, "should have cross-file references to helper")

	// Find the reference from file B and verify its position.
	bPath := filepath.Join(dir, "b.lisp")
	var found bool
	for _, ref := range helperRefs {
		if ref.File == bPath {
			found = true
			assert.Equal(t, "caller", ref.Enclosing, "reference should be inside 'caller'")
			require.NotNil(t, ref.Source, "reference should have source location")
			// "helper" is called at column 19 in: (defun caller () (helper 42))
			assert.Equal(t, 1, ref.Source.Line, "reference should be on line 1")
			assert.Equal(t, 19, ref.Source.Col, "reference should be at column 19")
			break
		}
	}
	assert.True(t, found, "should find helper reference from b.lisp")
}

func TestScanWorkspaceRefs_QuasiquoteTemplateCrossFile(t *testing.T) {
	dir := t.TempDir()

	// helpers.lisp defines and exports "get-valid-me" in the helpers package.
	err := os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`(in-package 'helpers)
(export 'get-valid-me)
(defun get-valid-me () 42)
`), 0600)
	require.NoError(t, err)

	// macro.lisp defines a macro whose quasiquote template references
	// helpers:get-valid-me. This is the pattern from def-acre-route.
	err = os.WriteFile(filepath.Join(dir, "macro.lisp"), []byte(`(in-package 'myapp)
(export 'my-macro)
(defmacro my-macro (name)
  (quasiquote
    (begin
      (helpers:get-valid-me)
      (unquote name))))
`), 0600)
	require.NoError(t, err)

	// Phase 1: Scan definitions.
	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   globals,
		PackageExports: pkgs,
	}

	// Phase 2: Scan references.
	refs := ScanWorkspaceRefs(dir, cfg, nil)

	// There should be a cross-file reference to "get-valid-me" from macro.lisp.
	key := SymbolKey{Package: "helpers", Name: "get-valid-me", Kind: SymFunction}.String()
	fnRefs := refs[key]
	require.NotEmpty(t, fnRefs,
		"should have cross-file reference to get-valid-me via quasiquote template")

	macroPath := filepath.Join(dir, "macro.lisp")
	var found bool
	for _, ref := range fnRefs {
		if ref.File == macroPath {
			found = true
			break
		}
	}
	assert.True(t, found,
		"should find get-valid-me reference from macro.lisp's quasiquote template")
}

func TestScanWorkspaceRefs_NonExportedQualifiedCrossFile(t *testing.T) {
	// A non-exported function referenced via qualified symbol in another
	// file's quasiquote template must produce a cross-file reference.
	// This exercises the PackageSymbols fallback (not PackageExports).
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`(in-package 'helpers)
(defun do-work () 42)
`), 0600)
	require.NoError(t, err)

	err = os.WriteFile(filepath.Join(dir, "macro.lisp"), []byte(`(in-package 'myapp)
(defmacro my-macro (name)
  (quasiquote
    (begin
      (helpers:do-work)
      (unquote name))))
`), 0600)
	require.NoError(t, err)

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		PackageSymbols: prescan.PkgAllSymbols,
	}

	refs := ScanWorkspaceRefs(dir, cfg, nil)

	key := SymbolKey{Package: "helpers", Name: "do-work", Kind: SymFunction}.String()
	fnRefs := refs[key]
	require.NotEmpty(t, fnRefs,
		"non-exported function should be found via PackageSymbols fallback")

	macroPath := filepath.Join(dir, "macro.lisp")
	var found bool
	for _, ref := range fnRefs {
		if ref.File == macroPath {
			found = true
			break
		}
	}
	assert.True(t, found,
		"should find do-work reference from macro.lisp")
}

func TestExtractFileRefs_SkipsBuiltinsAndLocals(t *testing.T) {
	// "my-fn" calls "other-fn" (a global), plus uses builtin "+", param "x", and local "local-var".
	src := []byte(`(defun other-fn () 1)
(defun my-fn (x)
  (let ((local-var 1))
    (+ x local-var (other-fn))))
`)
	filename := "test.lisp"
	result := AnalyzeFile(src, filename, nil)
	require.NotNil(t, result)

	refs := ExtractFileRefs(result, filename)

	// Must not be empty — "other-fn" is a global user-defined function and should be included.
	require.NotEmpty(t, refs, "should extract at least one reference")

	refNames := make(map[string]bool)
	for _, ref := range refs {
		refNames[ref.SymbolKey.Name] = true
	}

	// "other-fn" is a global user-defined function — should be included.
	assert.True(t, refNames["other-fn"], "global user function should be included")

	// "+" is a builtin, "x" is a parameter, "local-var" is a let binding — all excluded.
	assert.False(t, refNames["+"], "builtins should be excluded")
	assert.False(t, refNames["x"], "parameters should be excluded")
	assert.False(t, refNames["local-var"], "locals should be excluded")
}

func TestFindEnclosingFunction(t *testing.T) {
	src := []byte(`(defun outer ()
  (defun inner (x) (+ x 1))
  (inner 42))
`)
	result := AnalyzeFile(src, "test.lisp", nil)
	require.NotNil(t, result)
	require.NotNil(t, result.RootScope)

	t.Run("inside inner function body", func(t *testing.T) {
		// Column 22 is the position of "+" on line 2: "  (defun inner (x) (+ x 1))"
		enc := FindEnclosingFunction(result.RootScope, 2, 22)
		require.NotNil(t, enc)
		assert.Equal(t, "inner", enc.Name)
	})

	t.Run("inside outer function body", func(t *testing.T) {
		// Line 3, col 4 is inside "(inner 42)" which is in outer's body.
		enc := FindEnclosingFunction(result.RootScope, 3, 4)
		require.NotNil(t, enc)
		assert.Equal(t, "outer", enc.Name)
	})

	t.Run("before any function scope", func(t *testing.T) {
		// Line 100, col 1 is beyond the source — should be global scope, returns nil.
		enc := FindEnclosingFunction(result.RootScope, 100, 1)
		assert.Nil(t, enc, "position outside any function should return nil")
	})
}

func TestFindEnclosingFunction_MultiPackageSameName(t *testing.T) {
	src := []byte(`(in-package 'foo)
(defun caller () (+ 1 1))
(defun use-foo () (caller))
(in-package 'bar)
(defun caller () (+ 2 2))
(defun use-bar () (caller))
`)
	result := AnalyzeFile(src, "test.lisp", nil)
	require.NotNil(t, result)
	require.NotNil(t, result.RootScope)

	t.Run("inside foo caller body", func(t *testing.T) {
		enc := FindEnclosingFunction(result.RootScope, 2, 18)
		require.NotNil(t, enc)
		assert.Equal(t, "caller", enc.Name)
		assert.Equal(t, "foo", enc.Package)
	})

	t.Run("inside bar caller body", func(t *testing.T) {
		enc := FindEnclosingFunction(result.RootScope, 5, 18)
		require.NotNil(t, enc)
		assert.Equal(t, "caller", enc.Name)
		assert.Equal(t, "bar", enc.Package)
	})
}

func TestSymbolKey_String(t *testing.T) {
	key := SymbolKey{Name: "helper", Kind: SymFunction}
	assert.Equal(t, "helper/function", key.String())

	key2 := SymbolKey{Name: "my-var", Kind: SymVariable}
	assert.Equal(t, "my-var/variable", key2.String())

	key3 := SymbolKey{Package: "helpers", Name: "add-one", Kind: SymFunction}
	assert.Equal(t, "helpers:add-one/function", key3.String())
}

func TestScanWorkspaceRefs_QualifiedSymbol(t *testing.T) {
	dir := t.TempDir()

	// helpers.lisp defines and exports "add-one" in the helpers package.
	err := os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`(in-package 'helpers)
(export 'add-one)
(defun add-one (x) (+ x 1))
`), 0600)
	require.NoError(t, err)

	// consumer.lisp calls helpers:add-one WITHOUT use-package — exercises
	// the "create new symbol" path in resolveQualifiedSymbol.
	err = os.WriteFile(filepath.Join(dir, "consumer.lisp"), []byte(`(in-package 'consumer)
(defun do-work () (helpers:add-one 42))
`), 0600)
	require.NoError(t, err)

	// Phase 1: Scan definitions.
	globals, pkgs, err := ScanWorkspaceFull(dir)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   globals,
		PackageExports: pkgs,
	}

	// Phase 2: Scan references.
	refs := ScanWorkspaceRefs(dir, cfg, nil)

	// There should be a reference to "add-one" (unqualified key) from consumer.lisp.
	addOneKey := SymbolKey{Package: "helpers", Name: "add-one", Kind: SymFunction}.String()
	addOneRefs := refs[addOneKey]
	require.NotEmpty(t, addOneRefs, "should have cross-file reference to add-one via qualified symbol")

	consumerPath := filepath.Join(dir, "consumer.lisp")
	var found bool
	for _, ref := range addOneRefs {
		if ref.File == consumerPath {
			found = true
			assert.Equal(t, "do-work", ref.Enclosing, "reference should be inside 'do-work'")
			require.NotNil(t, ref.Source, "reference should have source location")
			// "helpers:add-one" starts at col 20 on line 2:
			// (defun do-work () (helpers:add-one 42))
			//                    ^ col 20
			assert.Equal(t, 2, ref.Source.Line, "reference should be on line 2")
			assert.Equal(t, 20, ref.Source.Col, "reference should be at column 20")
			break
		}
	}
	assert.True(t, found, "should find add-one reference from consumer.lisp")
}

func TestExtractFileRefs_QualifiedSymbol(t *testing.T) {
	cfg := &Config{
		PackageExports: map[string][]ExternalSymbol{
			"helpers": {
				{
					Name:   "add-one",
					Kind:   SymFunction,
					Source: &token.Location{File: "helpers.lisp", Line: 3, Col: 1, Pos: 40},
				},
			},
		},
	}

	t.Run("without use-package", func(t *testing.T) {
		// No use-package — forces resolveQualifiedSymbol to create the symbol
		// from PackageExports (the scope.Lookup(symName) returns nil path).
		src := []byte(`(in-package 'consumer)
(defun do-work () (helpers:add-one 42))
`)
		result := AnalyzeFile(src, "consumer.lisp", cfg)
		require.NotNil(t, result)

		// The analysis should resolve the qualified symbol.
		var foundRef *Reference
		for _, ref := range result.References {
			if ref.Symbol.Name == "add-one" && ref.Symbol.Kind == SymFunction {
				foundRef = ref
				break
			}
		}
		require.NotNil(t, foundRef, "analysis should resolve helpers:add-one as a reference")
		assert.True(t, foundRef.Symbol.External, "symbol should be marked External")
		assert.True(t, foundRef.Symbol.Exported, "symbol should be marked Exported")

		// ExtractFileRefs should produce a FileReference with full properties.
		fileRefs := ExtractFileRefs(result, "consumer.lisp")
		require.Len(t, fileRefs, 1, "should have exactly one cross-file reference")
		assert.Equal(t, "add-one", fileRefs[0].SymbolKey.Name)
		assert.Equal(t, SymFunction, fileRefs[0].SymbolKey.Kind)
		assert.Equal(t, "do-work", fileRefs[0].Enclosing)
		require.NotNil(t, fileRefs[0].Source)
		assert.Equal(t, 2, fileRefs[0].Source.Line)
	})

	t.Run("with use-package", func(t *testing.T) {
		// With use-package — the unqualified name is already in scope from
		// prescanUsePackage; resolveQualifiedSymbol should reuse that symbol.
		src := []byte(`(in-package 'consumer)
(use-package 'helpers)
(defun do-work () (helpers:add-one 42))
`)
		result := AnalyzeFile(src, "consumer.lisp", cfg)
		require.NotNil(t, result)

		fileRefs := ExtractFileRefs(result, "consumer.lisp")
		require.Len(t, fileRefs, 1, "should have exactly one cross-file reference")
		assert.Equal(t, "add-one", fileRefs[0].SymbolKey.Name)
		assert.Equal(t, SymFunction, fileRefs[0].SymbolKey.Kind)
		assert.Equal(t, "do-work", fileRefs[0].Enclosing)
	})

	t.Run("nonexistent package", func(t *testing.T) {
		src := []byte(`(defun f () (nosuchpkg:foo 1))`)
		result := AnalyzeFile(src, "test.lisp", cfg)
		require.NotNil(t, result)
		// Unknown package — should not produce any reference.
		fileRefs := ExtractFileRefs(result, "test.lisp")
		assert.Empty(t, fileRefs, "nonexistent package should produce no references")
	})

	t.Run("nonexistent symbol in valid package", func(t *testing.T) {
		src := []byte(`(defun f () (helpers:no-such-fn 1))`)
		result := AnalyzeFile(src, "test.lisp", cfg)
		require.NotNil(t, result)
		fileRefs := ExtractFileRefs(result, "test.lisp")
		assert.Empty(t, fileRefs, "nonexistent symbol in valid package should produce no references")
	})

	// Note: trailing colon (e.g. "helpers:") is a parse-level concern —
	// the parser either rejects it or splits it, so we don't test it here.

	t.Run("qualified symbol in quasiquote template", func(t *testing.T) {
		// A qualified symbol inside a defmacro's quasiquote template should
		// produce a cross-file reference, just like a direct call would.
		// This is the pattern used by def-acre-route: the macro template
		// references acre:get-valid-me which is defined in another file.
		cfgWithPkg := &Config{
			PackageExports: map[string][]ExternalSymbol{
				"helpers": {
					{
						Name:    "add-one",
						Kind:    SymFunction,
						Package: "helpers",
						Source:  &token.Location{File: "helpers.lisp", Line: 3, Col: 1, Pos: 40},
					},
				},
			},
		}
		src := []byte(`(in-package 'myapp)
(export 'my-macro)
(defmacro my-macro (name)
  (quasiquote
    (begin
      (helpers:add-one 1)
      (unquote name))))
`)
		result := AnalyzeFile(src, "macro.lisp", cfgWithPkg)
		require.NotNil(t, result)

		var foundRef *Reference
		for _, ref := range result.References {
			if ref.Symbol.Name == "add-one" && ref.Symbol.Kind == SymFunction {
				foundRef = ref
				break
			}
		}
		require.NotNil(t, foundRef,
			"quasiquote template should resolve helpers:add-one as a reference")

		fileRefs := ExtractFileRefs(result, "macro.lisp")
		var found bool
		for _, fref := range fileRefs {
			if fref.SymbolKey.Name == "add-one" && fref.SymbolKey.Package == "helpers" {
				found = true
				break
			}
		}
		assert.True(t, found,
			"ExtractFileRefs should include the quasiquote template reference")
	})

	t.Run("non-exported qualified symbol via PackageSymbols", func(t *testing.T) {
		// Symbol is ONLY in PackageSymbols (not PackageExports).
		// This exercises the resolveQualifiedSymbol fallback.
		cfgNonExported := &Config{
			PackageSymbols: map[string][]ExternalSymbol{
				"helpers": {
					{
						Name:    "do-work",
						Kind:    SymFunction,
						Package: "helpers",
						Source:  &token.Location{File: "helpers.lisp", Line: 2, Col: 1, Pos: 22},
					},
				},
			},
		}
		src := []byte(`(in-package 'myapp)
(defun caller () (helpers:do-work))`)
		result := AnalyzeFile(src, "test.lisp", cfgNonExported)
		require.NotNil(t, result)

		var foundRef *Reference
		for _, ref := range result.References {
			if ref.Symbol.Name == "do-work" {
				foundRef = ref
				break
			}
		}
		require.NotNil(t, foundRef,
			"should resolve non-exported helpers:do-work via PackageSymbols")
		assert.False(t, foundRef.Symbol.Exported,
			"symbol resolved via PackageSymbols should NOT be marked Exported")
		assert.True(t, foundRef.Symbol.External,
			"symbol should be marked External")
	})
}

func TestExtractFileDefinitions(t *testing.T) {
	source := []byte(`(in-package 'mylib)
(defun helper (x) (+ x 1))
(defmacro with-thing (body) body)
(set 'max-retries 3)
(export 'helper)`)

	defs := ExtractFileDefinitions(source, "mylib.lisp")
	require.NotEmpty(t, defs)

	names := make(map[string]SymbolKind)
	for _, d := range defs {
		names[d.Name] = d.Kind
		assert.Equal(t, "mylib", d.Package, "all defs should be in mylib package")
	}

	assert.Equal(t, SymFunction, names["helper"])
	assert.Equal(t, SymMacro, names["with-thing"])
	assert.Equal(t, SymVariable, names["max-retries"])
}

func TestExtractFileDefinitions_InvalidSource(t *testing.T) {
	// Unparseable source should return nil, not panic.
	defs := ExtractFileDefinitions([]byte("(defun broken"), "bad.lisp")
	assert.Nil(t, defs)
}

func TestPrescanWorkspace_DefForms(t *testing.T) {
	dir := t.TempDir()

	// File A defines a def-prefixed macro with 2+ params.
	err := os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(`
(defmacro def-handler (name routes)
  "Define a handler with routes."
  (list 'defun name (list 'req) routes))
(export 'def-handler)
`), 0600)
	require.NoError(t, err)

	// File B uses the macro — (def-handler name (formals) body...).
	err = os.WriteFile(filepath.Join(dir, "handlers.lisp"), []byte(`
(def-handler my-api (req)
  (respond req "ok"))
`), 0600)
	require.NoError(t, err)

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	require.NotNil(t, prescan)

	// Should produce a DefFormSpec for def-handler.
	var found *DefFormSpec
	for i := range prescan.DefForms {
		if prescan.DefForms[i].Head == "def-handler" {
			found = &prescan.DefForms[i]
			break
		}
	}
	require.NotNil(t, found, "should produce DefFormSpec for def-handler")
	assert.True(t, found.BindsName, "def-handler should bind a name")
	assert.Equal(t, 1, found.NameIndex, "name should be at index 1")
	assert.Equal(t, 2, found.FormalsIndex, "formals should be at index 2")

	// Verify that analysis with the DefForms recognizes my-api as a definition.
	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		DefForms:       prescan.DefForms,
		Filename:       filepath.Join(dir, "handlers.lisp"),
	}
	handlerSrc, _ := os.ReadFile(filepath.Join(dir, "handlers.lisp")) //nolint:gosec // test file
	result := AnalyzeFile(handlerSrc, filepath.Join(dir, "handlers.lisp"), cfg)
	require.NotNil(t, result)

	// my-api should be in the symbols list as a definition.
	var myAPI *Symbol
	for _, sym := range result.Symbols {
		if sym.Name == "my-api" {
			myAPI = sym
			break
		}
	}
	assert.NotNil(t, myAPI, "my-api should be recognized as a definition via def-handler DefFormSpec")
}

func TestPrescanWorkspace_NoDefPrefixMacroSkipped(t *testing.T) {
	dir := t.TempDir()

	// Macro without "def" prefix should NOT produce a DefFormSpec.
	err := os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(`
(defmacro with-logging (body)
  (list 'progn (list 'log "start") body (list 'log "end")))
(export 'with-logging)
`), 0600)
	require.NoError(t, err)

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	assert.Empty(t, prescan.DefForms, "non-def-prefixed macros should not produce DefFormSpecs")
}

func TestExtractDefFormSpecs_MinArity(t *testing.T) {
	// A macro with only 1 required param (arity < 2) can't have both
	// name and formals, so no DefFormSpec should be produced.
	defs := []ExternalSymbol{
		{
			Name: "def-simple",
			Kind: SymMacro,
			Signature: &Signature{
				Params: []lisp.ParamInfo{{Name: "body", Kind: lisp.ParamRequired}},
			},
		},
	}
	specs := extractDefFormSpecs(defs)
	assert.Empty(t, specs, "macro with < 2 params should not produce DefFormSpec")
}

func TestExtractDefFormSpecs_ValidMacro(t *testing.T) {
	// A def-prefixed macro with arity >= 2 should produce a DefFormSpec.
	defs := []ExternalSymbol{
		{
			Name: "def-handler",
			Kind: SymMacro,
			Signature: &Signature{
				Params: []lisp.ParamInfo{
					{Name: "name", Kind: lisp.ParamRequired},
					{Name: "routes", Kind: lisp.ParamRequired},
				},
			},
		},
	}
	specs := extractDefFormSpecs(defs)
	require.Len(t, specs, 1)
	assert.Equal(t, "def-handler", specs[0].Head)
	assert.True(t, specs[0].BindsName)
	assert.Equal(t, 1, specs[0].NameIndex)
	assert.Equal(t, 2, specs[0].FormalsIndex)
	assert.Equal(t, SymFunction, specs[0].NameKind)
}

func TestExtractDefFormSpecs_NonMacroSkipped(t *testing.T) {
	// A function (not macro) with "def" prefix should NOT produce a DefFormSpec.
	defs := []ExternalSymbol{
		{
			Name: "def-helper",
			Kind: SymFunction,
			Signature: &Signature{
				Params: []lisp.ParamInfo{
					{Name: "name", Kind: lisp.ParamRequired},
					{Name: "value", Kind: lisp.ParamRequired},
				},
			},
		},
	}
	specs := extractDefFormSpecs(defs)
	assert.Empty(t, specs, "non-macro symbols should not produce DefFormSpecs")
}

func TestExtractDefFormSpecs_NilSignatureSkipped(t *testing.T) {
	defs := []ExternalSymbol{
		{Name: "def-broken", Kind: SymMacro, Signature: nil},
	}
	specs := extractDefFormSpecs(defs)
	assert.Empty(t, specs, "macro with nil signature should not produce DefFormSpec")
}

func TestCollectLispFilesWithConfig_MaxFiles(t *testing.T) {
	dir := t.TempDir()

	// Create 5 .lisp files.
	for i := range 5 {
		err := os.WriteFile(filepath.Join(dir, fmt.Sprintf("f%d.lisp", i)), []byte("()"), 0600)
		require.NoError(t, err)
	}

	// Limit to 3 files.
	paths, truncated, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFiles: 3})
	require.NoError(t, err)
	assert.Len(t, paths, 3)
	assert.True(t, truncated, "should be truncated when hitting MaxFiles limit")
}

func TestCollectLispFilesWithConfig_NoTruncation(t *testing.T) {
	dir := t.TempDir()

	// Create 2 .lisp files.
	for i := range 2 {
		err := os.WriteFile(filepath.Join(dir, fmt.Sprintf("f%d.lisp", i)), []byte("()"), 0600)
		require.NoError(t, err)
	}

	paths, truncated, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFiles: 10})
	require.NoError(t, err)
	assert.Len(t, paths, 2)
	assert.False(t, truncated, "should not be truncated when under limit")
}

func TestCollectLispFilesWithConfig_MaxFileBytes(t *testing.T) {
	dir := t.TempDir()

	// Small file (under limit).
	err := os.WriteFile(filepath.Join(dir, "small.lisp"), []byte("(+ 1 1)"), 0600)
	require.NoError(t, err)

	// Large file (over limit).
	err = os.WriteFile(filepath.Join(dir, "large.lisp"), make([]byte, 200), 0600)
	require.NoError(t, err)

	paths, _, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFileBytes: 100})
	require.NoError(t, err)
	assert.Len(t, paths, 1, "only the small file should be collected")
	assert.Contains(t, paths[0], "small.lisp")
}

func TestCollectLispFilesWithConfig_MaxFileBytesExactBoundary(t *testing.T) {
	dir := t.TempDir()

	// File exactly at the limit (100 bytes). The implementation uses
	// strict > so exactly-at-limit files should be included.
	err := os.WriteFile(filepath.Join(dir, "exact.lisp"), make([]byte, 100), 0600)
	require.NoError(t, err)

	// File 1 byte over the limit.
	err = os.WriteFile(filepath.Join(dir, "over.lisp"), make([]byte, 101), 0600)
	require.NoError(t, err)

	paths, _, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFileBytes: 100})
	require.NoError(t, err)
	assert.Len(t, paths, 1, "file exactly at limit should be included, over should be excluded")
	assert.Contains(t, paths[0], "exact.lisp")
}

func TestCollectLispFilesWithConfig_Defaults(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte("()"), 0600)
	require.NoError(t, err)

	// nil config uses defaults.
	paths, truncated, err := collectLispFilesWithConfig(dir, nil)
	require.NoError(t, err)
	assert.Len(t, paths, 1)
	assert.False(t, truncated)
}

func TestCollectLispFilesWithConfig_ZeroMaxFilesUsesDefault(t *testing.T) {
	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte("()"), 0600)
	require.NoError(t, err)

	// MaxFiles: 0 should use DefaultMaxWorkspaceFiles, not collect zero files.
	paths, truncated, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFiles: 0})
	require.NoError(t, err)
	assert.Len(t, paths, 1, "MaxFiles: 0 should use default, not collect zero files")
	assert.False(t, truncated)
}

func TestCollectLispFilesWithConfig_ZeroMaxFileBytesUsesDefault(t *testing.T) {
	dir := t.TempDir()

	// A small file that would be excluded if MaxFileBytes: 0 meant "skip all".
	err := os.WriteFile(filepath.Join(dir, "a.lisp"), []byte("()"), 0600)
	require.NoError(t, err)

	paths, _, err := collectLispFilesWithConfig(dir, &ScanConfig{MaxFileBytes: 0})
	require.NoError(t, err)
	assert.Len(t, paths, 1, "MaxFileBytes: 0 should use default, not skip all files")
}

func TestCollectLispFilesWithConfig_Excludes(t *testing.T) {
	dir := t.TempDir()

	// Create 3 .lisp files.
	for _, name := range []string{"main.lisp", "generated.lisp", "utils.lisp"} {
		err := os.WriteFile(filepath.Join(dir, name), []byte("()"), 0600)
		require.NoError(t, err)
	}

	// Exclude "generated.lisp" by base name.
	paths, truncated, err := collectLispFilesWithConfig(dir, &ScanConfig{Excludes: []string{"generated.lisp"}})
	require.NoError(t, err)
	assert.False(t, truncated)
	assert.Len(t, paths, 2, "generated.lisp should be excluded")
	for _, p := range paths {
		assert.NotContains(t, p, "generated.lisp")
	}
}

func TestCollectLispFilesWithConfig_ExcludesDirectory(t *testing.T) {
	dir := t.TempDir()

	// Create a "build" subdirectory with a file.
	buildDir := filepath.Join(dir, "build")
	err := os.MkdirAll(buildDir, 0750)
	require.NoError(t, err)
	err = os.WriteFile(filepath.Join(buildDir, "output.lisp"), []byte("()"), 0600)
	require.NoError(t, err)

	// And a regular file.
	err = os.WriteFile(filepath.Join(dir, "main.lisp"), []byte("()"), 0600)
	require.NoError(t, err)

	paths, _, err := collectLispFilesWithConfig(dir, &ScanConfig{Excludes: []string{"build"}})
	require.NoError(t, err)
	assert.Len(t, paths, 1, "build directory files should be excluded")
	assert.Contains(t, paths[0], "main.lisp")
}

func TestScanUsePackages(t *testing.T) {
	parse := func(src string) []*lisp.LVal {
		t.Helper()
		s := token.NewScanner("test.lisp", strings.NewReader(src))
		p := rdparser.New(s)
		exprs, err := p.ParseProgram()
		require.NoError(t, err)
		return exprs
	}

	t.Run("basic", func(t *testing.T) {
		exprs := parse(`(in-package 'svc) (use-package 'utils)`)
		result := scanUsePackages(exprs)
		assert.Equal(t, []string{"utils"}, result["svc"])
	})

	t.Run("multiple packages", func(t *testing.T) {
		exprs := parse(`(in-package 'svc) (use-package 'utils) (use-package 'router)`)
		result := scanUsePackages(exprs)
		assert.Equal(t, []string{"utils", "router"}, result["svc"])
	})

	t.Run("default user package", func(t *testing.T) {
		exprs := parse(`(use-package 'helpers)`)
		result := scanUsePackages(exprs)
		assert.Equal(t, []string{"helpers"}, result["user"])
	})

	t.Run("no use-package", func(t *testing.T) {
		exprs := parse(`(in-package 'svc) (defun foo () 1)`)
		result := scanUsePackages(exprs)
		assert.Empty(t, result)
	})
}

func TestPrescanWorkspace_PackageImports(t *testing.T) {
	dir := t.TempDir()

	// main.lisp imports utils in the svc package.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(use-package 'utils)
(defun start () 1)
(export 'start)
`), 0600))

	// helpers.lisp is in svc but does NOT have use-package 'utils.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(in-package 'svc)
(defun helper () 2)
(export 'helper)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	// Exact match: svc imports exactly [utils], nothing spurious.
	assert.Equal(t, []string{"utils"}, prescan.PackageImports["svc"])
}

func TestPrescanWorkspace_PackageImports_Dedup(t *testing.T) {
	dir := t.TempDir()

	// Both files import utils in the svc package — should be deduplicated.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(use-package 'utils)
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(in-package 'svc)
(use-package 'utils)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	// utils should appear exactly once despite being imported in two files.
	assert.Equal(t, []string{"utils"}, prescan.PackageImports["svc"])
}

func TestBuildLoadTree_Basic(t *testing.T) {
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "a.lisp")
(load-file "b.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`(defun a-fn () 1)`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(defun b-fn () 2)`), 0600))

	tree := buildLoadTree(filepath.Join(dir, "main.lisp"))

	absA, _ := filepath.Abs(filepath.Join(dir, "a.lisp"))
	absB, _ := filepath.Abs(filepath.Join(dir, "b.lisp"))
	assert.Equal(t, "svc", tree[absA])
	assert.Equal(t, "svc", tree[absB])
}

func TestBuildLoadTree_PackageSwitch(t *testing.T) {
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "a.lisp")
(in-package 'other)
(load-file "b.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`(defun a-fn () 1)`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(defun b-fn () 2)`), 0600))

	tree := buildLoadTree(filepath.Join(dir, "main.lisp"))

	absA, _ := filepath.Abs(filepath.Join(dir, "a.lisp"))
	absB, _ := filepath.Abs(filepath.Join(dir, "b.lisp"))
	assert.Equal(t, "svc", tree[absA])
	assert.Equal(t, "other", tree[absB])
}

func TestBuildLoadTree_Nested(t *testing.T) {
	dir := t.TempDir()

	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "a.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`
(load-file "b.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(defun b-fn () 2)`), 0600))

	tree := buildLoadTree(filepath.Join(dir, "main.lisp"))

	absA, _ := filepath.Abs(filepath.Join(dir, "a.lisp"))
	absB, _ := filepath.Abs(filepath.Join(dir, "b.lisp"))
	assert.Equal(t, "svc", tree[absA], "a.lisp loaded from main in svc context")
	assert.Equal(t, "svc", tree[absB], "b.lisp loaded from a.lisp which inherited svc context")
}

func TestPrescanWorkspace_DefaultPackage(t *testing.T) {
	dir := t.TempDir()

	// main.lisp sets the package and loads a bare file.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(use-package 'helpers)
(load-file "consumer.lisp")
`), 0600))
	// helpers.lisp exports helper-fn.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "helpers.lisp"), []byte(`
(in-package 'helpers)
(defun helper-fn (x) (+ x 1))
(export 'helper-fn)
`), 0600))
	// consumer.lisp is bare — no in-package.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "consumer.lisp"), []byte(`
(defun do-work () (helper-fn 42))
(export 'do-work)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	assert.Equal(t, "svc", prescan.DefaultPackage)

	// do-work from bare consumer.lisp should be in svc package (not user).
	found := false
	for _, d := range prescan.AllDefs {
		if d.Name == "do-work" {
			assert.Equal(t, "svc", d.Package, "bare file def should be remapped to main.lisp's package")
			found = true
		}
	}
	assert.True(t, found, "do-work should be in AllDefs")
}

// TestPrescanWorkspace_PhylumPattern replicates a typical phylum workspace:
// - main.lisp sets in-package and imports packages via use-package
// - main.lisp loads a utils file that switches packages then switches back
// - main.lisp loads many bare files that inherit the package context
// - test files have explicit in-package + use-package 'testing
// - cross-file symbol resolution must work for all patterns
func TestPrescanWorkspace_PhylumPattern(t *testing.T) {
	dir := t.TempDir()

	// main.lisp: entry point — sets package, imports, loads files.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'myapp)
(use-package 'router)
(use-package 'myutils)

(load-file "myutils.lisp")
(load-file "auth.lisp")
(load-file "client.lisp")
(load-file "init.lisp")
`), 0600))

	// myutils.lisp: defines symbols in myutils package, then returns to myapp.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "myutils.lisp"), []byte(`
(in-package 'myutils)
(defun helper-fn (x) (+ x 1))
(export 'helper-fn)
(defun string-check (s) (not (nil? s)))
(export 'string-check)
(in-package 'myapp)
`), 0600))

	// router package: provides route-ok (simulates Go-registered package).
	require.NoError(t, os.WriteFile(filepath.Join(dir, "router.lisp"), []byte(`
(in-package 'router)
(defun route-ok (data) data)
(export 'route-ok)
`), 0600))

	// auth.lisp: bare file — no in-package. Uses symbols from imported packages.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "auth.lisp"), []byte(`
(defun authenticate (token)
  (string-check token))

(defun auth-route (req)
  (route-ok (authenticate (get req "token"))))

(export 'authenticate)
(export 'auth-route)
`), 0600))

	// client.lisp: bare file — references symbols from auth.lisp (load order).
	require.NoError(t, os.WriteFile(filepath.Join(dir, "client.lisp"), []byte(`
(defun get-client (id)
  (helper-fn id))

(export 'get-client)
`), 0600))

	// init.lisp: bare file — defines bootstrap constants.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "init.lisp"), []byte(`
(set 'bootstrap-id "boot-001")
`), 0600))

	// auth_test.lisp: test file — explicit in-package + use-package 'testing.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "auth_test.lisp"), []byte(`
(in-package 'myapp)
(use-package 'testing)

(test "auth works"
  (assert-equal "ok" (authenticate "ok")))
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	// Verify DefaultPackage is myapp (from main.lisp).
	assert.Equal(t, "myapp", prescan.DefaultPackage)

	// Verify bare file definitions are in myapp package.
	defPkgs := make(map[string]string)
	for _, d := range prescan.AllDefs {
		defPkgs[d.Name] = d.Package
	}
	assert.Equal(t, "myapp", defPkgs["authenticate"], "bare file def should be in myapp")
	assert.Equal(t, "myapp", defPkgs["auth-route"], "bare file def should be in myapp")
	assert.Equal(t, "myapp", defPkgs["get-client"], "bare file def should be in myapp")
	assert.Equal(t, "myapp", defPkgs["bootstrap-id"], "bare file constant should be in myapp")
	assert.Equal(t, "myutils", defPkgs["helper-fn"], "myutils def stays in myutils")
	assert.Equal(t, "myutils", defPkgs["string-check"], "myutils def stays in myutils")

	// Verify PackageImports includes all use-package declarations.
	assert.Contains(t, prescan.PackageImports["myapp"], "router")
	assert.Contains(t, prescan.PackageImports["myapp"], "myutils")
	assert.Contains(t, prescan.PackageImports["myapp"], "testing")

	// Verify cross-file resolution works: analyze auth.lisp (bare file)
	// with the prescan config — helper-fn and route-ok should resolve.
	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		PackageImports: prescan.PackageImports,
		DefaultPackage: prescan.DefaultPackage,
	}
	authSrc := []byte("(defun authenticate (token)\n  (string-check token))\n\n(defun auth-route (req)\n  (route-ok (authenticate (get req \"token\"))))\n\n(export 'authenticate)\n(export 'auth-route)\n")
	result := AnalyzeFile(authSrc, filepath.Join(dir, "auth.lisp"), cfg)
	require.NotNil(t, result)

	unresolvedNames := make(map[string]bool)
	for _, u := range result.Unresolved {
		unresolvedNames[u.Name] = true
	}
	assert.False(t, unresolvedNames["string-check"],
		"string-check from myutils should resolve via cross-file use-package")
	assert.False(t, unresolvedNames["route-ok"],
		"route-ok from router should resolve via cross-file use-package")

	// Verify test file resolution: auth_test.lisp has explicit in-package.
	testSrc := []byte("(in-package 'myapp)\n(use-package 'testing)\n\n(test \"auth works\"\n  (assert-equal \"ok\" (authenticate \"ok\")))\n")
	testResult := AnalyzeFile(testSrc, filepath.Join(dir, "auth_test.lisp"), cfg)
	require.NotNil(t, testResult)

	testUnresolved := make(map[string]bool)
	for _, u := range testResult.Unresolved {
		testUnresolved[u.Name] = true
	}
	assert.False(t, testUnresolved["authenticate"],
		"authenticate from auth.lisp should resolve in test file (same myapp package)")
}

func TestPrescanWorkspace_ExplicitUserPackageNotRemapped(t *testing.T) {
	dir := t.TempDir()

	// main.lisp sets svc and loads a file that explicitly declares user package.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "user-prefs.lisp")
`), 0600))
	// user-prefs.lisp EXPLICITLY declares user package — must NOT be remapped.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "user-prefs.lisp"), []byte(`
(in-package 'user)
(defun pref-fn () 1)
(export 'pref-fn)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	assert.Equal(t, "svc", prescan.DefaultPackage)

	for _, d := range prescan.AllDefs {
		if d.Name == "pref-fn" {
			assert.Equal(t, "user", d.Package,
				"explicit (in-package 'user) should NOT be remapped to svc")
			return
		}
	}
	t.Fatal("pref-fn should be in AllDefs")
}

func TestPrescanWorkspace_NoMainLisp(t *testing.T) {
	dir := t.TempDir()

	// Workspace without main.lisp — no DefaultPackage, defs stay in user.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte(`
(defun lib-fn () 1)
(export 'lib-fn)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	assert.Equal(t, "", prescan.DefaultPackage)

	for _, d := range prescan.AllDefs {
		if d.Name == "lib-fn" {
			assert.Equal(t, "user", d.Package,
				"without main.lisp, defs should stay in user package")
			return
		}
	}
	t.Fatal("lib-fn should be in AllDefs")
}

func TestBuildLoadTree_CycleDetection(t *testing.T) {
	dir := t.TempDir()

	// a.lisp loads b.lisp, b.lisp loads a.lisp — must terminate.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`
(in-package 'svc)
(load-file "b.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`
(load-file "a.lisp")
`), 0600))

	tree := buildLoadTree(filepath.Join(dir, "a.lisp"))
	assert.NotNil(t, tree, "should terminate without infinite loop")

	absB, _ := filepath.Abs(filepath.Join(dir, "b.lisp"))
	assert.Equal(t, "svc", tree[absB])
}

func TestPrescanWorkspace_DefsBeforeInPackage(t *testing.T) {
	dir := t.TempDir()

	// main.lisp loads mixed.lisp which has defs before its first in-package.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "mixed.lisp")
(load-file "consumer.lisp")
`), 0600))

	// mixed.lisp: defs before in-package should inherit svc from load context.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "mixed.lisp"), []byte(`
(defun helper-a () 1)
(defun helper-b () 2)

(in-package 'internal)
(defun internal-fn () 3)
(export 'internal-fn)
(in-package 'svc)
`), 0600))

	// consumer.lisp: bare file that uses helper-a.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "consumer.lisp"), []byte(`
(defun do-work () (helper-a))
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	defPkgs := make(map[string]string)
	for _, d := range prescan.AllDefs {
		defPkgs[d.Name] = d.Package
	}

	// Defs before in-package should be remapped to svc (inherited from load context).
	assert.Equal(t, "svc", defPkgs["helper-a"],
		"def before in-package should be remapped to load-tree package")
	assert.Equal(t, "svc", defPkgs["helper-b"],
		"def before in-package should be remapped to load-tree package")
	// Def after in-package 'internal should stay in internal.
	assert.Equal(t, "internal", defPkgs["internal-fn"],
		"def after in-package should keep its explicit package")
	// Bare file def should be remapped to svc.
	assert.Equal(t, "svc", defPkgs["do-work"],
		"bare file def should be remapped to load-tree package")
}

func TestAnalyze_DefsBeforeInPackage_Resolve(t *testing.T) {
	dir := t.TempDir()

	// Same workspace as above — verify consumer.lisp resolves helper-a.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "main.lisp"), []byte(`
(in-package 'svc)
(load-file "mixed.lisp")
(load-file "consumer.lisp")
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "mixed.lisp"), []byte(`
(defun helper-a () 1)
(in-package 'internal)
(defun internal-fn () 3)
(export 'internal-fn)
(in-package 'svc)
`), 0600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "consumer.lisp"), []byte(`
(defun do-work () (helper-a))
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		PackageImports: prescan.PackageImports,
		DefaultPackage: prescan.DefaultPackage,
	}

	consumerSrc := []byte("(defun do-work () (helper-a))\n")
	result := AnalyzeFile(consumerSrc, filepath.Join(dir, "consumer.lisp"), cfg)
	require.NotNil(t, result)

	for _, u := range result.Unresolved {
		assert.NotEqual(t, "helper-a", u.Name,
			"helper-a should resolve — its pre-in-package def was remapped to svc")
	}
}

func TestScanConfig_EffectiveDefaults(t *testing.T) {
	// Verify the effective* methods return defaults for zero/nil.
	var nilCfg *ScanConfig
	assert.Equal(t, DefaultMaxWorkspaceFiles, nilCfg.effectiveMaxFiles())
	assert.Equal(t, int64(DefaultMaxFileBytes), nilCfg.effectiveMaxFileBytes())

	zeroCfg := &ScanConfig{MaxFiles: 0, MaxFileBytes: 0}
	assert.Equal(t, DefaultMaxWorkspaceFiles, zeroCfg.effectiveMaxFiles())
	assert.Equal(t, int64(DefaultMaxFileBytes), zeroCfg.effectiveMaxFileBytes())

	// Negative values also use defaults.
	negCfg := &ScanConfig{MaxFiles: -1, MaxFileBytes: -1}
	assert.Equal(t, DefaultMaxWorkspaceFiles, negCfg.effectiveMaxFiles())
	assert.Equal(t, int64(DefaultMaxFileBytes), negCfg.effectiveMaxFileBytes())
}

func TestPrescanWorkspace_Preamble(t *testing.T) {
	dir := t.TempDir()

	// File with in-package, use-package, export, defmacro, and defun.
	// Only preamble forms should be collected.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(`
(in-package 'mypkg)
(use-package 'utils)
(export 'my-macro)
(defmacro my-macro (x) (quasiquote (+ (unquote x) 1)))
(defun not-a-preamble () 42)
`), 0600))

	// Bare file with defmacro only.
	require.NoError(t, os.WriteFile(filepath.Join(dir, "user.lisp"), []byte(`
(defmacro user-macro () '1)
`), 0600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)

	// Count preamble forms by type.
	heads := make(map[string]int)
	for _, form := range prescan.Preamble {
		heads[astutil.HeadSymbol(form)]++
	}
	assert.Equal(t, 1, heads["in-package"], "should collect in-package")
	assert.Equal(t, 1, heads["use-package"], "should collect use-package")
	assert.Equal(t, 1, heads["export"], "should collect export")
	assert.Equal(t, 2, heads["defmacro"], "should collect both defmacro forms")
	assert.Equal(t, 1, heads["defun"], "should collect defun forms")
}
