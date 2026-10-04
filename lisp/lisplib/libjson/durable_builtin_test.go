// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// builtinEnv returns a user environment, publishable as a template, with
// the builtin regpkg:reg-fn (it returns 7) registered, then aliased as
// regpkg:kept and rebound to a lambda.  So only the registry and the alias
// hold the builtin.
func builtinEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadRuntimeLibrary(env)))
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "regpkg")))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Export: true},
		elpsutil.Function("reg-fn", lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(7) }))))
	evalString(t, env, `(set 'kept reg-fn) (set 'reg-fn (lambda (x) 0)) (export 'kept)`)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	evalString(t, env, `(defun my-fn () 1)`)
	return env
}

const builtinDoc = `["~#durable",[1,["~#list",[["~#builtin",["regpkg","reg-fn"]],["~#fn","lisp:car"],["~#fn","user:my-fn"]]]]]`

// A registered builtin its name no longer binds is written by its
// registration and restored through the registry, whatever the name holds.
func TestDurableBuiltinAfterRebinding(t *testing.T) {
	env := builtinEnv(t)
	v := env.LoadString("test", `(list regpkg:kept car my-fn)`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	assert.Equal(t, builtinDoc, string(b))

	// Rebind the name again before the load: the restore does not follow it.
	evalString(t, env, `(in-package 'regpkg) (set 'reg-fn (lambda (x) 1)) (in-package 'user)`)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	reg := env.Runtime.Registry
	pkg, name, ok := reg.RegisteredBuiltinName(back.Cells[0])
	require.True(t, ok)
	assert.Equal(t, "regpkg", pkg)
	assert.Equal(t, "reg-fn", name)
	assert.Equal(t, reg.RegisteredBuiltin("regpkg", "reg-fn").Native, back.Cells[0].Native)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	assert.Equal(t, "7", evalString(t, env, `(funcall (first r) 1)`))
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, builtinDoc, string(again))
}

// A builtin shadowed in another package is still bound in its own, so it
// is written as ~#fn, and a load of the name finds its own package's binding.
func TestDurableBuiltinShadowed(t *testing.T) {
	env := builtinEnv(t)
	car := env.LoadString("test", `car`)
	require.NoError(t, lisp.GoError(car))
	evalString(t, env, `(defun car (x) 'shadow)`)
	b, err := libjson.DumpDurable(env, car, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#fn","lisp:car"]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Equal(t, car.Native, back.Native)
}

// A builtin whose FID copies a registered builtin's is refused.  A builtin
// a later registration replaced is named by a binding that holds it, and
// refused when none does.
func TestDurableBuiltinRefusals(t *testing.T) {
	env := builtinEnv(t)
	car := env.Runtime.Registry.RegisteredBuiltin(lisp.DefaultLangPackage, "car")
	spoof := lisp.FunInPackage(lisp.DefaultLangPackage, car.FID(), lisp.Formals("x"), car.Builtin())
	_, err := libjson.DumpDurable(env, spoof, nil)
	require.Error(t, err)
	assert.Equal(t, "durable json: cannot encode an anonymous function", err.Error())

	old := env.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn")
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true},
		elpsutil.Function("reg-fn", lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(8) }))))
	// The replaced builtin is no registration's, so it goes by the name
	// that binds it, as any unregistered builtin does.
	b, err := libjson.DumpDurable(env, old, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#fn","regpkg:kept"]]]`, string(b))
	// Once no name binds it, the FID leads only to the new registration,
	// which is another function.
	evalString(t, env, `(set 'kept 0)`)
	_, err = libjson.DumpDurable(env, old, nil)
	require.Error(t, err)
	assert.Equal(t, "durable json: cannot encode an anonymous function", err.Error())
	cur := env.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn")
	b, err = libjson.DumpDurable(env, cur, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#fn","regpkg:reg-fn"]]]`, string(b))
}

func TestLoadDurableBuiltinRejects(t *testing.T) {
	env := builtinEnv(t)
	for _, c := range []struct{ name, doc, want string }{
		{"unknown name", `["~#durable",[1,["~#builtin",["lisp","no-such-builtin"]]]]`, "builtin lisp:no-such-builtin: not registered"},
		{"unknown package", `["~#durable",[1,["~#builtin",["no-such-pkg","car"]]]]`, "builtin no-such-pkg:car: not registered"},
		{"wrong package", `["~#durable",[1,["~#builtin",["user","car"]]]]`, "builtin user:car: not registered"},
		{"wrong package of a rebound name", `["~#durable",[1,["~#builtin",["user","reg-fn"]]]]`, "builtin user:reg-fn: not registered"},
		{"lisp function", `["~#durable",[1,["~#builtin",["user","my-fn"]]]]`, "builtin user:my-fn: not registered"},
		{"macro", `["~#durable",[1,["~#builtin",["lisp","defun"]]]]`, "builtin lisp:defun: registered as a macro or special operator"},
		{"special operator", `["~#durable",[1,["~#builtin",["lisp","if"]]]]`, "builtin lisp:if: registered as a macro or special operator"},
		{"bound builtin", `["~#durable",[1,["~#builtin",["lisp","car"]]]]`, "builtin lisp:car: its name binds it, so it is written as ~#fn"},
		{"alias as ~#fn", `["~#durable",[1,["~#fn","regpkg:kept"]]]`, "function regpkg:kept: the global holds the builtin registered as regpkg:reg-fn"},
		{"one string", `["~#durable",[1,["~#builtin","lisp:car"]]]`, "expected '['"},
		{"one element", `["~#durable",[1,["~#builtin",["lisp"]]]]`, "expected ','"},
		{"three elements", `["~#durable",[1,["~#builtin",["lisp","car","x"]]]]`, "expected ']'"},
		{"not a string", `["~#durable",[1,["~#builtin",["lisp",1]]]]`, `expected '"'`},
		{"as an object", `["~#durable",[1,["~#list",[["~#obj",[0,["~#builtin",["regpkg","reg-fn"]]]],["~#ref",0]]]]]`, "an object must be"},
	} {
		t.Run(c.name, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), nil)
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
}

// The bytes, the charges and the restored identity are equal in a cold
// environment and in eager, lazy and prewarmed template VMs.
func TestDurableBuiltinParity(t *testing.T) {
	source := builtinEnv(t)
	dump := func(env *lisp.LEnv) (string, []int) {
		t.Helper()
		var charges []int
		v := env.LoadString("test", `(list regpkg:kept car my-fn regpkg:kept)`)
		require.NoError(t, lisp.GoError(v))
		b, err := libjson.DumpDurable(env, v, nil, libjson.WithTypedCharge(func(n int) error { charges = append(charges, n); return nil }))
		require.NoError(t, err)
		return string(b), charges
	}
	want, wantCharges := dump(source)
	assert.Contains(t, want, `["~#builtin",["regpkg","reg-fn"]]`)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	for _, c := range []struct {
		name string
		tmpl *lisp.Template
		opts []lisp.VMOption
	}{
		{"eager", eager, nil},
		{"lazy", lazy, nil},
		{"lazy prewarmed", lazy, []lisp.VMOption{lisp.VMWithPrewarm()}},
	} {
		vm, err := c.tmpl.NewVM(c.opts...)
		require.NoError(t, err)
		got, charges := dump(vm)
		assert.Equal(t, want, got, c.name)
		assert.Equal(t, wantCharges, charges, c.name)
		back, err := libjson.LoadDurable(vm, []byte(want), nil)
		require.NoError(t, err, c.name)
		pkg, name, ok := vm.Runtime.Registry.RegisteredBuiltinName(back.Cells[0])
		require.True(t, ok, c.name)
		assert.Equal(t, "regpkg:reg-fn", pkg+":"+name, c.name)
		kept, _ := vm.Runtime.Registry.Package("regpkg").Symbol("kept")
		assert.Equal(t, kept.Native, back.Cells[0].Native, "%s: the restore is the VM's own builtin", c.name)
	}
}

// A value lisp:builtin returns after a rebinding saves as ~#builtin and
// restores to the original builtin after another rebinding.
func TestDurableLispBuiltinAcrossRebind(t *testing.T) {
	env := builtinEnv(t)
	evalString(t, env, `(in-package 'math) (lisp:set 'floor (lisp:lambda (x) 0)) (lisp:in-package 'user)`)
	v := env.LoadString("test", `(builtin 'math:floor)`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#builtin",["math","floor"]]]]`, string(b))
	evalString(t, env, `(in-package 'math) (lisp:set 'floor (lisp:lambda (x) 1)) (lisp:in-package 'user)`)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("restored"), back)))
	assert.Equal(t, "2", evalString(t, env, `(funcall restored 2.5)`))
	assert.Equal(t, `"math:floor"`, evalString(t, env, `(builtin-name restored)`))
}
