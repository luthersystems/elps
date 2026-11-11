// Copyright © 2026 The ELPS authors

package analysis

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// reloadFixture is an expander whose env holds the macros of a scan, as
// after the index build of the LSP server.
type reloadFixture struct {
	exp  *EnvMacroExpander
	scan *WorkspaceScan
}

func newReloadFixture(t *testing.T, files map[string]string) *reloadFixture {
	t.Helper()
	scan := NewWorkspaceScan("")
	for _, path := range []string{"/ws/a.lisp", "/ws/b.lisp"} {
		if src, ok := files[path]; ok {
			scan, _ = scan.WithFile(path, []byte(src))
		}
	}
	exp := &EnvMacroExpander{Env: newTestEnv(t)}
	require.Empty(t, exp.LoadWorkspaceMacros(scan.Prescan().Preamble))
	return &reloadFixture{exp: exp, scan: scan}
}

// save replaces the source of path and reloads its macros.
func (f *reloadFixture) save(t *testing.T, path, src string) bool {
	t.Helper()
	reloaded, errs := f.saveCtx(context.Background(), path, src)
	require.Empty(t, errs)
	return reloaded
}

// saveCtx is save with a context, and it returns the errors.
func (f *reloadFixture) saveCtx(ctx context.Context, path, src string) (bool, []error) {
	next, _ := f.scan.WithFile(path, []byte(src))
	reloaded, errs := f.exp.ReloadFileMacros(ctx, f.scan, next, path)
	f.scan = next
	return reloaded, errs
}

// expand returns the expansion of (name) in package demo, as text, or ""
// when the expander does not expand it.
func (f *reloadFixture) expand(name string) string {
	return f.expandIn("demo", name)
}

// expandIn is expand in package pkg.
func (f *reloadFixture) expandIn(pkg, name string) string {
	got := f.exp.ExpandMacro(lisp.SExpr([]*lisp.LVal{lisp.Symbol(name)}), pkg)
	if got == nil {
		return ""
	}
	return got.String()
}

func TestReloadFileMacros(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'demo)\n(defmacro m () 'one)\n(defmacro gone () 'x)\n",
	})
	require.Equal(t, "one", f.expand("m"))
	require.Equal(t, "x", f.expand("gone"))

	assert.False(t, f.save(t, "/ws/a.lisp", "; comment\n(in-package 'demo)\n(defmacro m () 'one)\n(defmacro gone () 'x)\n"),
		"same forms: nothing to evaluate")

	assert.True(t, f.save(t, "/ws/a.lisp", "(in-package 'demo)\n(defmacro m () 'two)\n"))
	assert.Equal(t, "two", f.expand("m"), "the new definition wins")
	assert.Empty(t, f.expand("gone"), "a removed macro is retired")

	assert.True(t, f.save(t, "/ws/a.lisp", "(in-package 'demo)\n(defmacro m () 'two)\n(defmacro gone () 'y)\n"))
	assert.Equal(t, "y", f.expand("gone"), "a new definition of a retired name expands")
}

// TestReloadFileMacrosKeepsMacroOfOtherFile checks that a macro another
// file still defines is not retired when one file drops it.
func TestReloadFileMacrosKeepsMacroOfOtherFile(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'demo)\n(defmacro shared () 'a)\n",
		"/ws/b.lisp": "(in-package 'demo)\n(defmacro shared () 'b)\n",
	})
	require.Equal(t, "b", f.expand("shared"), "b loads last")

	assert.True(t, f.save(t, "/ws/b.lisp", "(in-package 'demo)\n"))
	assert.NotEmpty(t, f.expand("shared"), "a still defines shared")
}

// TestReloadFileMacrosRemovedFile checks a file the scan no longer holds.
func TestReloadFileMacrosRemovedFile(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'demo)\n(defmacro m () 'one)\n",
	})
	next, held := f.scan.WithoutFile("/ws/a.lisp")
	require.True(t, held)
	reloaded, errs := f.exp.ReloadFileMacros(context.Background(), f.scan, next, "/ws/a.lisp")
	require.Empty(t, errs)
	assert.True(t, reloaded)
	assert.Empty(t, f.expand("m"))
}

func TestReloadFileMacrosNilEnv(t *testing.T) {
	reloaded, errs := (&EnvMacroExpander{}).ReloadFileMacros(context.Background(), nil, NewWorkspaceScan(""), "/ws/a.lisp")
	assert.False(t, reloaded)
	assert.Empty(t, errs)
}

// TestSamePreamble checks the comparison that decides whether a save
// evaluates the file again: positions do not count, code does.
func TestSamePreamble(t *testing.T) {
	forms := func(src string) []*lisp.LVal {
		return scanWorkspaceFile([]byte(src), "/ws/a.lisp").preamble
	}
	base := "(in-package 'demo)\n(set 'x '(a 1 \"s\"))\n(defmacro m (y) (list 'f y 2.5))\n"
	assert.True(t, samePreamble(forms(base), forms("\n; moved\n"+base)))
	for _, changed := range []string{
		"(in-package 'demo)\n(set 'x (a 1 \"s\"))\n(defmacro m (y) (list 'f y 2.5))\n",
		"(in-package 'demo)\n(set 'x '(a 2 \"s\"))\n(defmacro m (y) (list 'f y 2.5))\n",
		"(in-package 'demo)\n(set 'x '(a 1 \"t\"))\n(defmacro m (y) (list 'f y 2.5))\n",
		"(in-package 'demo)\n(set 'x '(a 1 \"s\"))\n(defmacro m (y) (list 'f y 3.5))\n",
		"(in-package 'demo)\n(set 'x '(a 1 \"s\"))\n",
	} {
		assert.False(t, samePreamble(forms(base), forms(changed)), changed)
	}
}

// TestReloadFileMacrosBareFileKeepsBuildPackage checks that a file with
// no in-package form reloads in the package the index build gave it. The
// build evaluates the preambles of all files in a row, so a bare file
// runs in the package the file before it left active (here foo). A
// reload in the start package of the env would define user:m and leave
// foo:m stale.
func TestReloadFileMacrosBareFileKeepsBuildPackage(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'foo)\n",
		"/ws/b.lisp": "(defmacro m () 'one)\n",
	})
	require.Equal(t, "one", f.expandIn("foo", "m"), "the build defines m in foo")

	assert.True(t, f.save(t, "/ws/b.lisp", "(defmacro m () 'two)\n"))
	assert.Equal(t, "two", f.expandIn("foo", "m"), "the reload defines m in foo again")
	assert.Empty(t, f.expandIn(lisp.DefaultUserPackage, "m"), "the reload does not define user:m")

	assert.True(t, f.save(t, "/ws/b.lisp", "(defun g () 1)\n"))
	assert.Empty(t, f.expandIn("foo", "m"), "the removed macro is retired in foo")
}

// loopingSet is a preamble form that never ends: an empty dotimes of
// about 2^62 turns.
const loopingSet = "(set 'x (dotimes (i 4611686018427387904) ()))\n"

// TestReloadFileMacrosBoundsEvaluation checks that a preamble form that
// does not end stops at the step limit of a preamble form, and that the
// macros of the file keep their old definitions. A save evaluates the
// preamble while it holds the lock of every expansion, so an unbounded
// form would stop all analysis.
func TestReloadFileMacrosBoundsEvaluation(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'demo)\n(defmacro m () 'one)\n(defmacro gone () 'x)\n",
	})
	done := make(chan struct{})
	var reloaded bool
	var errs []error
	go func() {
		defer close(done)
		reloaded, errs = f.saveCtx(context.Background(), "/ws/a.lisp",
			"(in-package 'demo)\n(defmacro m () 'two)\n(defmacro new () 'n)\n"+loopingSet)
	}()
	select {
	case <-done:
	case <-time.After(30 * time.Second):
		t.Fatal("the reload did not stop")
	}
	assert.False(t, reloaded, "a reload that overran changes no macro")
	require.NotEmpty(t, errs)
	require.ErrorIs(t, errs[len(errs)-1], ErrPreambleLimit)
	assert.Equal(t, "one", f.expand("m"), "m keeps its old definition")
	assert.Equal(t, "x", f.expand("gone"), "a macro the overrun file dropped is not retired")
	assert.Empty(t, f.expand("new"), "a macro the overrun file added does not expand")
	limit, _ := f.exp.Env.Runtime.StepBudget()
	assert.Zero(t, limit, "the step budget of the env is cleared after the reload")
}

// TestReloadFileMacrosHonorsContext checks that a cancelled context stops
// the reload with the same result as the step limit.
func TestReloadFileMacrosHonorsContext(t *testing.T) {
	f := newReloadFixture(t, map[string]string{
		"/ws/a.lisp": "(in-package 'demo)\n(defmacro m () 'one)\n",
	})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	reloaded, errs := f.saveCtx(ctx, "/ws/a.lisp", "(in-package 'demo)\n(defmacro m () 'two)\n")
	assert.False(t, reloaded)
	require.NotEmpty(t, errs)
	require.ErrorIs(t, errs[0], ErrPreambleLimit)
	assert.Equal(t, "one", f.expand("m"))
}

// TestLoadWorkspaceMacrosContextBoundsEvaluation checks the index build:
// a form that does not end stops at its step limit, and the forms after
// it still load.
func TestLoadWorkspaceMacrosContextBoundsEvaluation(t *testing.T) {
	env := newTestEnv(t)
	preamble := parsePreamble(t, "(in-package 'demo)\n"+loopingSet+"(defmacro after () 'a)\n")
	load := LoadWorkspaceMacrosContext(context.Background(), env, preamble)
	require.Len(t, load.Errors, 1)
	require.ErrorIs(t, load.Errors[0], ErrPreambleLimit)
	assert.True(t, load.Stopped)
	assert.Equal(t, len(preamble), load.Unfinished, "the context did not end")
	exp := &EnvMacroExpander{Env: env}
	assert.NotNil(t, exp.ExpandMacro(lisp.SExpr([]*lisp.LVal{lisp.Symbol("after")}), "demo"))
}

// TestLoadWorkspaceMacrosContextReportsUnfinished checks that a load whose
// context ends says which form it stopped at, so that the caller can load
// the rest later.
func TestLoadWorkspaceMacrosContextReportsUnfinished(t *testing.T) {
	env := newTestEnv(t)
	preamble := parsePreamble(t, "(in-package 'demo)\n(defmacro after () 'a)\n")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	load := LoadWorkspaceMacrosContext(ctx, env, preamble)
	assert.True(t, load.Stopped)
	assert.Equal(t, 0, load.Unfinished)
	require.Len(t, load.Errors, 1)
	require.ErrorIs(t, load.Errors[0], ErrPreambleLimit)
}

// TestLoadPackagesIsMemoized checks that a scan walks its load order once
// for the packages of its files, and that a scan made from it walks again.
func TestLoadPackagesIsMemoized(t *testing.T) {
	scan := NewWorkspaceScan("")
	scan, _ = scan.WithFile("/ws/a.lisp", []byte("(in-package 'demo)\n(defmacro m () 'one)\n"))
	scan, _ = scan.WithFile("/ws/b.lisp", []byte("(defmacro n () 'two)\n"))
	first := scan.loadPackages("user")
	again := scan.loadPackages("user")
	require.Len(t, first, 2)
	assert.Same(t, &first[0], &again[0], "the second call reuses the walk")

	next, _ := scan.WithFile("/ws/a.lisp", []byte("(in-package 'other)\n"))
	assert.Equal(t, "other", next.loadPackageOf("/ws/b.lisp", "user"), "the new scan walks again")
	assert.Equal(t, "demo", scan.loadPackageOf("/ws/b.lisp", "user"), "the old scan keeps its packages")
}

// TestPreambleFilesFrom checks the map from a form of the preamble to the
// files of that form and of the forms after it.
func TestPreambleFilesFrom(t *testing.T) {
	scan := NewWorkspaceScan("")
	scan, _ = scan.WithFile("/ws/a.lisp", []byte("(in-package 'demo)\n(defmacro m () 'one)\n"))
	scan, _ = scan.WithFile("/ws/b.lisp", []byte("(in-package 'demo)\n(defmacro n () 'two)\n"))
	preamble := scan.Prescan().Preamble
	require.Len(t, preamble, 4)
	assert.Equal(t, []string{"/ws/a.lisp", "/ws/b.lisp"}, scan.PreambleFilesFrom(0))
	assert.Equal(t, []string{"/ws/a.lisp", "/ws/b.lisp"}, scan.PreambleFilesFrom(1))
	assert.Equal(t, []string{"/ws/b.lisp"}, scan.PreambleFilesFrom(2))
	assert.Empty(t, scan.PreambleFilesFrom(len(preamble)))
}
