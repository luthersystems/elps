// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// registryEnv returns a user environment, without the testing package so
// that it can be published as a template, with the builtin reg-fn
// registered in package regpkg, which user imports.
func registryEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadRuntimeLibrary(env)))
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "regpkg")))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Export: true}, constBuiltin("reg-fn", 7))))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	require.NoError(t, lisp.GoError(env.LoadString("test", `(use-package 'regpkg)`)))
	return env
}

func requireName(t *testing.T, reg *lisp.PackageRegistry, fn *lisp.LVal, pkg, name string) {
	t.Helper()
	gotPkg, gotName, ok := reg.RegisteredBuiltinName(fn)
	require.True(t, ok, "%v is not a registered builtin", fn)
	assert.Equal(t, pkg, gotPkg)
	assert.Equal(t, name, gotName)
}

func requireNotRegistered(t *testing.T, reg *lisp.PackageRegistry, fn *lisp.LVal) {
	t.Helper()
	pkg, name, ok := reg.RegisteredBuiltinName(fn)
	assert.False(t, ok, "%v answered %s:%s", fn, pkg, name)
	assert.Empty(t, pkg)
	assert.Empty(t, name)
}

// Rebinding the name, shadowing it in another package and aliasing it
// leave the registry entry and the answer for the original unchanged.
func TestRegisteredBuiltinSurvivesRebinding(t *testing.T) {
	env := registryEnv(t)
	reg := env.Runtime.Registry
	orig := reg.RegisteredBuiltin("regpkg", "reg-fn")
	require.NotNil(t, orig)
	bound, ok := reg.Package("regpkg").Symbol("reg-fn")
	require.True(t, ok)
	assert.Same(t, bound, orig, "the registry holds the bound value itself")
	requireName(t, reg, orig, "regpkg", "reg-fn")

	// A header copy through a symbol lookup is the same function.
	viaLookup := env.LoadString("test", `reg-fn`)
	require.NoError(t, lisp.GoError(viaLookup))
	requireName(t, reg, viaLookup, "regpkg", "reg-fn")

	// An alias, then a rebinding of the registered name and a shadowing
	// defun in user.
	evalOK(t, env, `(in-package 'regpkg)
(set 'reg-alias reg-fn)
(set 'kept reg-fn)
(set 'reg-fn (lambda (x) 0))
(in-package 'user)
(defun reg-fn (x) 1)`)
	assert.Same(t, orig, reg.RegisteredBuiltin("regpkg", "reg-fn"))
	requireName(t, reg, orig, "regpkg", "reg-fn")
	alias, _ := reg.Package("regpkg").Symbol("reg-alias")
	requireName(t, reg, alias, "regpkg", "reg-fn")
	rebound, _ := reg.Package("regpkg").Symbol("reg-fn")
	requireNotRegistered(t, reg, rebound)
	shadow, _ := reg.Package("user").Symbol("reg-fn")
	requireNotRegistered(t, reg, shadow)

	// The registered value still runs as the builtin.
	assert.Equal(t, "7", evalOK(t, env, `(funcall regpkg:kept 1)`))

	// Language builtins, macros and special operators are registered too.
	car := reg.RegisteredBuiltin(lisp.DefaultLangPackage, "car")
	require.NotNil(t, car)
	requireName(t, reg, car, lisp.DefaultLangPackage, "car")
	require.NotNil(t, reg.RegisteredBuiltin(lisp.DefaultLangPackage, "defun"))
	assert.Equal(t, lisp.LFunMacro, reg.RegisteredBuiltin(lisp.DefaultLangPackage, "defun").FunType)
	assert.Equal(t, lisp.LFunSpecialOp, reg.RegisteredBuiltin(lisp.DefaultLangPackage, "if").FunType)
	assert.Nil(t, reg.RegisteredBuiltin(lisp.DefaultLangPackage, "no-such-builtin"))
	assert.Nil(t, reg.RegisteredBuiltin(lisp.DefaultUserPackage, "car"), "user imports car; it registers nothing")
	assert.Nil(t, reg.RegisteredBuiltin("no-such-package", "car"))
}

// Only values registration created answer ok.  The FID text plays no part.
func TestRegisteredBuiltinNameRefusesLookalikes(t *testing.T) {
	env := registryEnv(t)
	reg := env.Runtime.Registry
	car := reg.RegisteredBuiltin(lisp.DefaultLangPackage, "car")
	require.NotNil(t, car)
	spoof := lisp.FunInPackage(lisp.DefaultLangPackage, car.FID(), lisp.Formals("x"), car.Builtin())
	require.Equal(t, car.FID(), spoof.FID())
	requireNotRegistered(t, reg, spoof)
	lambda := env.LoadString("test", `(lambda (x) x)`)
	require.NoError(t, lisp.GoError(lambda))
	for _, v := range []*lisp.LVal{
		spoof, lambda, lisp.Native(42), lisp.Int(1), lisp.Symbol("car"), lisp.Nil(), nil,
	} {
		requireNotRegistered(t, reg, v)
	}
	// A nil registry answers nothing.
	var none *lisp.PackageRegistry
	assert.Nil(t, none.RegisteredBuiltin(lisp.DefaultLangPackage, "car"))
	_, _, ok := none.RegisteredBuiltinName(car)
	assert.False(t, ok)
}

// A later registration of the same package and name replaces the entry; the
// replaced builtin no longer answers.
func TestRegisteredBuiltinReregistration(t *testing.T) {
	env := registryEnv(t)
	reg := env.Runtime.Registry
	old := reg.RegisteredBuiltin("regpkg", "reg-fn")
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, constBuiltin("reg-fn", 8))))
	cur := reg.RegisteredBuiltin("regpkg", "reg-fn")
	require.NotSame(t, old, cur)
	requireName(t, reg, cur, "regpkg", "reg-fn")
	requireNotRegistered(t, reg, old)
	// The registry of another runtime knows neither.
	other := registryEnv(t).Runtime.Registry
	requireNotRegistered(t, other, cur)
}

// The answers are equal in the source and in every kind of template VM:
// identity is the registration record the VMs share, not the *LVal.
func TestRegisteredBuiltinParity(t *testing.T) {
	source := registryEnv(t)
	evalOK(t, source, `(in-package 'regpkg)
(set 'reg-alias reg-fn)
(set 'reg-fn (lambda (x) 0))
(in-package 'user)`)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	vms := []struct {
		env  *lisp.LEnv
		name string
	}{{source, "cold"}}
	for _, c := range []struct {
		tmpl *lisp.Template
		name string
		opts []lisp.VMOption
	}{
		{eager, "eager", nil},
		{lazy, "lazy", nil},
		{lazy, "lazy prewarmed", []lisp.VMOption{lisp.VMWithPrewarm()}},
	} {
		vm, vmErr := c.tmpl.NewVM(c.opts...)
		require.NoError(t, vmErr)
		vms = append(vms, struct {
			env  *lisp.LEnv
			name string
		}{vm, c.name})
	}
	for _, vm := range vms {
		t.Run(vm.name, func(t *testing.T) {
			reg := vm.env.Runtime.Registry
			orig := reg.RegisteredBuiltin("regpkg", "reg-fn")
			require.NotNil(t, orig)
			requireName(t, reg, orig, "regpkg", "reg-fn")
			alias, _ := reg.Package("regpkg").Symbol("reg-alias")
			requireName(t, reg, alias, "regpkg", "reg-fn")
			// Within one VM the registry and the alias are one function.
			assert.Equal(t, alias.Native, orig.Native)
			rebound, _ := reg.Package("regpkg").Symbol("reg-fn")
			requireNotRegistered(t, reg, rebound)
			// An unchanged name binds the registry's own value.
			car, _ := reg.Package(lisp.DefaultLangPackage).Symbol("car")
			assert.Same(t, reg.RegisteredBuiltin(lisp.DefaultLangPackage, "car"), car)
			assert.Equal(t, "7", evalOK(t, vm.env, `(funcall regpkg:reg-alias 1)`))
			// The source's value is the same registration in every VM.
			requireName(t, reg, source.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn"), "regpkg", "reg-fn")
		})
	}
	// A registration made in a VM replaces the inherited entry there only.
	vm, err := lazy.NewVM()
	require.NoError(t, err)
	inherited := vm.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn")
	require.NoError(t, lisp.GoError(vm.InPackage(lisp.String("regpkg"))))
	require.NoError(t, lisp.GoError(vm.BindBuiltins(lisp.BindOpts{Shadow: true}, constBuiltin("reg-fn", 9))))
	requireNotRegistered(t, vm.Runtime.Registry, inherited)
	requireName(t, vm.Runtime.Registry, vm.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn"), "regpkg", "reg-fn")
	other, err := lazy.NewVM()
	require.NoError(t, err)
	requireName(t, other.Runtime.Registry, inherited, "regpkg", "reg-fn")
}

// A registered builtin no binding holds is still published, so a builtin
// policy is asked about it, and a refusal names it.
func TestRegisteredBuiltinPublication(t *testing.T) {
	source := registryEnv(t)
	orig := source.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn")
	evalOK(t, source, `(in-package 'regpkg) (set 'reg-fn (lambda (x) 0)) (in-package 'user) (defun reg-fn (x) 1)`)
	_, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Native != orig.Native }))
	require.Error(t, err)
	assert.Contains(t, err.Error(), "registered builtin regpkg:reg-fn")
}

func evalOK(t *testing.T, env *lisp.LEnv, src string) string {
	t.Helper()
	v := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(v), src)
	return v.String()
}
