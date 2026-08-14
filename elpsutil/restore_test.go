// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// The two behaviours pinned here belong to the package-restoration work
// (issues #99 and #352): the caller's package is restored whether or not the
// load succeeds, and an environment with no current package is reported as an
// error rather than dereferenced.
//
// Both live in the same three functions this change rewrites -- Load, LoadAll
// and PackageLoader -- so they are the two things a merge is most likely to
// drop on the floor. Each test below fails if its behaviour is removed.

// switchTo returns a Loader that defines pkg, switches into it, and then
// returns result. It is the shape of a real loader that fails after having
// already moved the environment somewhere else.
func switchTo(pkg string, result func() *lisp.LVal) elpsutil.Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		name := lisp.Symbol(pkg)
		if e := env.DefinePackage(name); !e.IsNil() {
			return e
		}
		if e := env.InPackage(name); !e.IsNil() {
			return e
		}
		return result()
	}
}

func okResult() *lisp.LVal  { return lisp.Nil() }
func badResult() *lisp.LVal { return lisp.Errorf("loader failed after switching packages") }

// TestLoad_ErrorRestoresPrevious pins the error-path half of the restore. A
// Load that returns the loader's error *before* restoring leaves the embedder
// in whichever package the failed loader happened to stop in, which is the
// same defect #352 fixes for the success path.
func TestLoad_ErrorRestoresPrevious(t *testing.T) {
	env := testEnv(t)
	caller := lisp.Symbol("err-restore-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	rc := elpsutil.Load(env, switchTo("err-restore-callee", badResult))
	require.Equal(t, lisp.LError, rc.Type, "Load must return the loader's error")
	require.Contains(t, rc.String(), "loader failed after switching packages",
		"Load must prefer the loader's error over any restore error")
	require.Equal(t, "err-restore-caller", env.Runtime.Package.Name,
		"Load must restore the caller's package on the error path too")
}

// TestLoadAll_ErrorRestoresPrevious is the same assertion for LoadAll, which
// runs each loader from the caller's package and must not strand the embedder
// when one of them fails.
func TestLoadAll_ErrorRestoresPrevious(t *testing.T) {
	env := testEnv(t)
	caller := lisp.Symbol("all-restore-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	rc := elpsutil.LoadAll(
		switchTo("all-restore-ok", okResult),
		switchTo("all-restore-bad", badResult),
	)(env)
	require.Equal(t, lisp.LError, rc.Type, "LoadAll must return the failing loader's error")
	require.Contains(t, rc.String(), "loader failed after switching packages")
	require.Equal(t, "all-restore-caller", env.Runtime.Package.Name,
		"LoadAll must restore the caller's package on the error path too")
}

// TestLoadAll_RunsEachLoaderFromCallersPackage pins the success-path half for
// LoadAll: every loader starts from the package the caller was in, not from
// wherever the previous loader stopped, and not from the user package.
func TestLoadAll_RunsEachLoaderFromCallersPackage(t *testing.T) {
	env := testEnv(t)
	caller := lisp.Symbol("all-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	var seen []string
	record := func(env *lisp.LEnv) *lisp.LVal {
		seen = append(seen, env.Runtime.Package.Name)
		return lisp.Nil()
	}
	rc := elpsutil.LoadAll(
		switchTo("all-first", okResult),
		record,
		switchTo("all-second", okResult),
		record,
	)(env)
	require.True(t, rc.IsNil(), "LoadAll: %v", rc)
	require.Equal(t, []string{"all-caller", "all-caller"}, seen,
		"each loader must run from the caller's package")
	require.Equal(t, "all-caller", env.Runtime.Package.Name,
		"LoadAll must end in the caller's package")
}

// TestLoadersRejectUninitialisedEnv pins the guard that keeps the restore from
// nil-dereferencing. An LEnv that has not been through lisp.InitializeUserEnv
// has a nil Runtime.Package, and reading its Name to save the caller's package
// panics in the embedder's process -- the class of defect #351 is about. Every
// entry point that saves the package is covered.
func TestLoadersRejectUninitialisedEnv(t *testing.T) {
	entries := []struct {
		name string
		run  func(env *lisp.LEnv) *lisp.LVal
	}{
		{"Load", func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.Load(env, func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() })
		}},
		{"LoadAll", func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.LoadAll(func(*lisp.LEnv) *lisp.LVal { return lisp.Nil() })(env)
		}},
		{"PackageLoader", func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.PackageLoader(&validationPackage{name: "uninit"})(env)
		}},
		{"LibraryLoader", func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.LibraryLoader(&validationPackage{name: "uninit"})(env)
		}},
	}
	for _, e := range entries {
		t.Run(e.name, func(t *testing.T) {
			// lisp.NewEnv(nil) alone: no InitializeUserEnv, so
			// Runtime.Package is nil.
			env := lisp.NewEnv(nil)
			require.Nil(t, env.Runtime.Package,
				"precondition: an uninitialised env has no current package")

			var rc *lisp.LVal
			require.NotPanics(t, func() { rc = e.run(env) },
				"%s panicked on an environment that never ran InitializeUserEnv", e.name)
			require.NotNil(t, rc)
			require.Equal(t, lisp.LError, rc.Type,
				"%s accepted an uninitialised environment: %v", e.name, rc)
			require.True(t,
				strings.Contains(rc.String(), "no current package"),
				"%s should say what is wrong with the environment, got: %v", e.name, rc)
		})
	}
}
