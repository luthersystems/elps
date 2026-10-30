package lisp_test

import (
	"fmt"
	"sync"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// sealedHostEnv defines a host package "host" with a value, a function and
// one export, then seals it with policy.
func sealedHostEnv(t *testing.T, policy lisp.SealPolicy) *lisp.LEnv {
	t.Helper()
	env := templateTestEnv(t)
	// in-package creates each package using lisp, as a host's would.
	require.NoError(t, lisp.GoError(env.LoadString("pkgs.lisp", `(in-package 'other) (in-package 'host)`)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol("host"))))
	env.AddBuiltins(true, elpsutil.Function("answer", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(42) }))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("config"), lisp.Int(1))))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol("other"))))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("extra"), lisp.Int(7))))
	env.Runtime.Registry.Package("other").Export("extra")
	require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))))
	require.NoError(t, env.Runtime.Registry.Package("host").Seal(policy))
	return env
}

// Each Lisp write path that reaches a package: qualified and unqualified
// set, set!, defun and defmacro, a new export, and use-package.
var sealWrites = []struct {
	source string
	name   string
	write  lisp.SealWrite
}{
	{`(set 'host:config 2)`, "config", lisp.SealBind},
	{`(set! 'host:config 2)`, "config", lisp.SealBind},
	{`(defun host:answer () 0)`, "answer", lisp.SealBind},
	{`(defmacro host:answer () 0)`, "answer", lisp.SealBind},
	{`(set 'target 'host:config) (set target 2)`, "config", lisp.SealBind},
	{`(set 'host:added 2)`, "added", lisp.SealBind},
	{`(in-package 'host) (set 'config 2)`, "config", lisp.SealBind},
	{`(in-package 'host) (set! config 2)`, "config", lisp.SealBind},
	{`(in-package 'host) (defun answer () 0)`, "answer", lisp.SealBind},
	{`(in-package 'host) (set 'added 2)`, "added", lisp.SealBind},
	{`(in-package 'host) (export 'added)`, "added", lisp.SealExport},
	{`(in-package 'host) (use-package 'other)`, "extra", lisp.SealUse},
}

func TestPackageSealRefuses(t *testing.T) {
	for _, tc := range sealWrites {
		t.Run(tc.source, func(t *testing.T) {
			env := sealedHostEnv(t, nil)
			got := env.LoadString("seal.lisp", tc.source)
			require.Equal(t, lisp.LError, got.Type, "got %v", got)
			assert.Contains(t, got.String(), "cannot rebind sealed package binding: host:"+tc.name)
			assert.Equal(t, "1", env.GetGlobal(lisp.Symbol("host:config")).String())
			assert.Equal(t, "42", env.LoadString("call.lisp", `(host:answer)`).String())
			_, added := env.Runtime.Registry.Package("host").Symbol("added")
			assert.False(t, added, "a refused write must not bind")
		})
	}
}

func TestPackageSealPolicyReports(t *testing.T) {
	for _, tc := range sealWrites {
		t.Run(tc.source, func(t *testing.T) {
			var got []lisp.SealViolation
			var boundBefore []bool
			env := sealedHostEnv(t, func(env *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
				_, ok := env.Runtime.Registry.Package(v.Package).Symbol(v.Symbol)
				boundBefore = append(boundBefore, ok)
				got = append(got, v)
				return nil
			})
			rc := env.LoadString("seal.lisp", tc.source)
			require.NotEqual(t, lisp.LError, rc.Type, "a reporting policy must let the write go ahead: %v", rc)
			require.Len(t, got, 1)
			assert.Equal(t, lisp.SealViolation{Package: "host", Symbol: tc.name, Write: tc.write}, got[0])
			// The policy runs before the write: only config and answer
			// are bound in host yet.
			assert.Equal(t, tc.name == "config" || tc.name == "answer", boundBefore[0])
		})
	}
}

func TestPackageSealPolicyError(t *testing.T) {
	env := sealedHostEnv(t, func(env *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
		return env.ErrorConditionf("host-sealed", "no %s of %s:%s", v.Write, v.Package, v.Symbol)
	})
	got := env.LoadString("seal.lisp", `(set 'host:config 2)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Equal(t, "host-sealed", got.Str)
	assert.Contains(t, got.String(), "no bind of host:config")
	assert.Equal(t, "1", env.GetGlobal(lisp.Symbol("host:config")).String())
	// The condition can be handled like any other.
	assert.Equal(t, "'handled", env.LoadString("handle.lisp",
		`(handler-bind ((host-sealed (lambda (c &rest _) 'handled))) (set 'host:config 3))`).String())
}

func TestPackageSealLeavesOtherWritesAlone(t *testing.T) {
	env := sealedHostEnv(t, nil)
	for _, source := range []string{
		// The user package's own copy of an import is not the host's binding.
		`(use-package 'host) (set 'config 5) config`,
		// A lexical binding that shares the name belongs to its let.
		`(in-package 'host) (let ((config 3)) (set! config 5) config)`,
		// Re-exporting an existing export changes nothing.
		`(in-package 'other) (export 'extra) 5`,
	} {
		assert.Equal(t, "5", env.LoadString("ok.lisp", source).String(), source)
	}
	// Go writes are trusted, and unlike the lisp package's seal, a host's
	// seal lets Go shadow a binding too: the host owns the package.
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("host:config"), lisp.Int(9))))
	require.NoError(t, lisp.GoError(env.Update(lisp.Symbol("host:config"), lisp.Int(10))))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol("host"))))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, elpsutil.Function("answer", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(43) }))))
	assert.Equal(t, "43", env.LoadString("call.lisp", `(answer)`).String())
	assert.Equal(t, "10", env.GetGlobal(lisp.Symbol("host:config")).String())
}

func TestPackageSealOnce(t *testing.T) {
	env := sealedHostEnv(t, nil)
	assert.True(t, env.Runtime.Registry.Package("host").Sealed())
	assert.False(t, env.Runtime.Registry.Package("other").Sealed())
	require.ErrorIs(t, env.Runtime.Registry.Package("host").Seal(nil), lisp.ErrPackageSealed)
	lispPkg := env.Runtime.Registry.Package(env.Runtime.Registry.Lang)
	assert.True(t, lispPkg.Sealed())
	require.ErrorIs(t, lispPkg.Seal(func(*lisp.LEnv, lisp.SealViolation) *lisp.LVal { return nil }), lisp.ErrPackageSealed)
	// The lisp package keeps its own error text.
	got := env.LoadString("seal.lisp", `(set 'lisp:if 1)`)
	assert.Contains(t, got.String(), "cannot rebind lisp package binding: if")
}

func TestPackageSealTemplateAndAdmission(t *testing.T) {
	for _, opts := range [][]lisp.TemplateOption{
		{templateCorePolicy()},
		{templateCorePolicy(), lisp.TemplateWithEagerInstantiation()},
		{templateCorePolicy(), lisp.TemplateWithFrozenPackages("host")},
	} {
		var mu sync.Mutex
		var reports []string
		env := sealedHostEnv(t, func(env *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
			mu.Lock()
			defer mu.Unlock()
			reports = append(reports, fmt.Sprintf("%s:%s", v.Package, v.Symbol))
			return env.Errorf("refused %s", v.Symbol)
		})
		tmpl, err := lisp.NewTemplate(env, opts...)
		require.NoError(t, err)
		vm, err := tmpl.NewVM()
		require.NoError(t, err)
		admitted := templateTestEnv(t)
		require.True(t, admitted.Runtime.Registry.AddPackage(env.Runtime.Registry.Package("host")))
		for _, candidate := range []*lisp.LEnv{vm, admitted} {
			got := candidate.LoadString("seal.lisp", `(set 'host:config 2)`)
			assert.Contains(t, got.String(), "refused config", "copied package lost its seal")
			assert.Equal(t, "1", candidate.GetGlobal(lisp.Symbol("host:config")).String())
		}
		assert.Equal(t, []string{"host:config", "host:config"}, reports)
	}
}

// A policy that only records must see only writes that happen.
func TestPackageSealPolicySeesOnlyRealWrites(t *testing.T) {
	for _, source := range []string{
		`(set! 'host:missing 1)`,
		`(in-package 'host) (set! missing 1)`,
		`(in-package 'host) (set 'true 1)`,
		`(set 'host:a:b 1)`,
		`(in-package 'host) (export 'new 'x:y)`,
		`(in-package 'host) (use-package 'other 'no-such-package 3)`,
	} {
		t.Run(source, func(t *testing.T) {
			var got []lisp.SealViolation
			env := sealedHostEnv(t, func(_ *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
				got = append(got, v)
				return nil
			})
			rc := env.LoadString("bad.lisp", source)
			require.Equal(t, lisp.LError, rc.Type, "got %v", rc)
			assert.Empty(t, got, "the policy saw a write that was refused anyway")
		})
	}
}

// A use-package that changes no binding is not a write: in-package already
// made host use lisp.
func TestPackageSealUseThatChangesNothing(t *testing.T) {
	env := sealedHostEnv(t, nil)
	assert.Equal(t, "5", env.LoadString("use.lisp", `(in-package 'host) (use-package 'lisp) 5`).String())
}

// A policy that refuses one name of a later package leaves the sealed
// package without the names of the earlier ones.
func TestPackageSealUseIsAllOrNothing(t *testing.T) {
	env := sealedHostEnv(t, func(env *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
		if v.Symbol == "extra" {
			return env.Errorf("refused %s", v.Symbol)
		}
		return nil
	})
	require.Equal(t, lisp.LError, env.LoadString("pkgs.lisp",
		`(in-package 'first) (set 'early 1) (export 'early) (in-package 'host) (use-package 'first 'other)`).Type)
	_, ok := env.Runtime.Registry.Package("host").Symbol("early")
	assert.False(t, ok, "host bound a name of a use-package that was refused")
}

// An error a policy builds without env gets the caller's stack.
func TestPackageSealPolicyErrorHasStack(t *testing.T) {
	env := sealedHostEnv(t, func(_ *lisp.LEnv, v lisp.SealViolation) *lisp.LVal {
		return lisp.Errorf("refused %s", v.Symbol)
	})
	got := env.LoadString("seal.lisp", "\n(set 'host:config 2)")
	require.Equal(t, lisp.LError, got.Type)
	assert.NotNil(t, got.CallStack())
	assert.Contains(t, got.String(), "seal.lisp:2")
}

// The seal covers the package's own Lisp functions, which run with it as
// the current package (documented on Package.Seal).
func TestPackageSealCoversOwnFunctions(t *testing.T) {
	env := templateTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("host.lisp",
		`(in-package 'host) (set 'counter 0) (defun bump () (set 'counter (+ counter 1))) (export 'bump)`)))
	require.NoError(t, env.Runtime.Registry.Package("host").Seal(nil))
	got := env.LoadString("call.lisp", `(host:bump)`)
	assert.Contains(t, got.String(), "cannot rebind sealed package binding: host:counter")
}
