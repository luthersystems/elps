// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"bytes"
	"runtime/debug"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libmath"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

// This file covers issue #425: EnvTestSuite indexed
// env.Runtime.Registry.Package(DefaultPackageName) without a nil check and
// called Get on the result. A runtime that never loaded the testing package
// has no such key, the zero value of the map is a nil *Package, and
// Package.get dereferences pkg.symbols on it -- a nil pointer dereference
// inside a function whose documented contract is to return nil when there is
// no suite.
//
// The reachable shape is an embedder, not lisp source: EnvTestSuite is
// exported, documented as returning nil "if there is none", and takes an
// arbitrary *lisp.LEnv. Asking an arbitrary runtime whether it has a suite is
// the question the nil return advertises, and on main asking it panicked the
// host. That is issue #367's rule ("an elps program must never panic the
// host") reached through the embedder API.
//
// callEnvTestSuite recovers so the red run stays readable. A nil dereference
// is an ordinary panic, unlike the runtime throw issue #420 needed a child
// process for, so recovering is enough to keep the rest of the package
// reporting -- but an unrecovered panic here would still take the whole test
// binary down, which is exactly what an embedder gets.
func callEnvTestSuite(tb testing.TB, env *lisp.LEnv) (suite *libtesting.TestSuite) {
	tb.Helper()
	defer func() {
		if r := recover(); r != nil {
			tb.Fatalf("EnvTestSuite panicked: %v\n\n%s", r, debug.Stack())
		}
	}()
	return libtesting.EnvTestSuite(env)
}

// TestEnvTestSuiteWithoutTestingPackage is the catch for #425: on main every
// subtest panics with a nil pointer dereference where the documented answer is
// a nil *TestSuite.
func TestEnvTestSuiteWithoutTestingPackage(t *testing.T) {
	tests := []struct {
		name  string
		build func(testing.TB) *lisp.LEnv
	}{{
		// The minimum an embedder can hold: a runtime with an empty
		// registry. StandardRuntime starts with no packages at all.
		name: "bare-runtime",
		build: func(tb testing.TB) *lisp.LEnv {
			return lisp.NewEnv(nil)
		},
	}, {
		// The ordinary embedder shape: a usable user environment that
		// simply did not opt into the testing package.
		name: "user-env-without-testing",
		build: func(tb testing.TB) *lisp.LEnv {
			tb.Helper()
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
				tb.Fatalf("initialize-user-env: %v", rc)
			}
			return env
		},
	}, {
		// A populated registry that still lacks this one key, so the
		// failure is the missing key and not an empty map.
		name: "other-lisplib-packages-loaded",
		build: func(tb testing.TB) *lisp.LEnv {
			tb.Helper()
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
				tb.Fatalf("initialize-user-env: %v", rc)
			}
			if rc := libmath.LoadPackage(env); !rc.IsNil() {
				tb.Fatalf("load math: %v", rc)
			}
			return env
		},
	}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			env := test.build(t)
			if env.Runtime.Registry.Package(libtesting.DefaultPackageName) != nil {
				t.Fatalf("premise broken: %q is already in the registry, so this subtest does not exercise the missing-package path", libtesting.DefaultPackageName)
			}
			if suite := callEnvTestSuite(t, env); suite != nil {
				t.Errorf("EnvTestSuite = %p, want nil on a runtime with no testing package", suite)
			}
		})
	}
}

// TestEnvTestSuitePackagePresentWithoutSuite is a GUARD, not a catch: it
// passes on main. It pins the neighbouring case the function always handled --
// the package exists but the suite symbol is unbound or is not a *TestSuite --
// so a future rewrite of the lookup cannot regress it while fixing the missing
// -package case.
func TestEnvTestSuitePackagePresentWithoutSuite(t *testing.T) {
	newEnv := func(tb testing.TB) *lisp.LEnv {
		tb.Helper()
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
			tb.Fatalf("initialize-user-env: %v", rc)
		}
		name := lisp.Symbol(libtesting.DefaultPackageName)
		if rc := env.DefinePackage(name); !rc.IsNil() {
			tb.Fatalf("define-package: %v", rc)
		}
		if rc := env.InPackage(name); !rc.IsNil() {
			tb.Fatalf("in-package: %v", rc)
		}
		return env
	}

	t.Run("symbol-unbound", func(t *testing.T) {
		env := newEnv(t)
		if suite := callEnvTestSuite(t, env); suite != nil {
			t.Errorf("EnvTestSuite = %p, want nil when the suite symbol is unbound", suite)
		}
	})

	t.Run("symbol-bound-to-non-native", func(t *testing.T) {
		env := newEnv(t)
		if rc := env.PutGlobal(lisp.Symbol(libtesting.DefaultSuiteSymbol), lisp.String("not a suite")); rc.Type == lisp.LError {
			t.Fatalf("put-global: %v", rc)
		}
		if suite := callEnvTestSuite(t, env); suite != nil {
			t.Errorf("EnvTestSuite = %p, want nil when the suite symbol is not a native value", suite)
		}
	})

	t.Run("symbol-bound-to-other-native", func(t *testing.T) {
		env := newEnv(t)
		// A real native value of some other type, so this exercises the
		// type assertion rather than stopping at the LNative check.
		other := lisp.Native(new(bytes.Buffer))
		if other.Type != lisp.LNative {
			t.Fatalf("premise broken: lisp.Native produced %v, not LNative", other.Type)
		}
		if rc := env.PutGlobal(lisp.Symbol(libtesting.DefaultSuiteSymbol), other); rc.Type == lisp.LError {
			t.Fatalf("put-global: %v", rc)
		}
		if suite := callEnvTestSuite(t, env); suite != nil {
			t.Errorf("EnvTestSuite = %p, want nil when the native value is not a *TestSuite", suite)
		}
	})
}

// TestEnvTestSuiteAfterLoadPackage is a GUARD: it passes on main and pins the
// positive case, so a fix that returns nil too eagerly is caught here.
func TestEnvTestSuiteAfterLoadPackage(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libtesting.LoadPackage(env); !rc.IsNil() {
		t.Fatalf("load-package: %v", rc)
	}
	if suite := callEnvTestSuite(t, env); suite == nil {
		t.Error("EnvTestSuite = nil after LoadPackage, want the installed suite")
	}
}
