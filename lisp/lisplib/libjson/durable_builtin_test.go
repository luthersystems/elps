// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// constFn is a builtin definition that returns n.
func constFn(name string, n int) lisp.LBuiltinDef {
	return elpsutil.Function(name, lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(n) })
}

// freshBuiltinEnv returns a user environment, publishable as a template,
// with the builtin regpkg:reg-fn (it returns 7) registered and exported,
// and the Lisp function user:my-fn.
func freshBuiltinEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadRuntimeLibrary(env)))
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "regpkg")))
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Export: true}, constFn("reg-fn", 7))))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	evalString(t, env, `(defun my-fn () 1)`)
	return env
}

// builtinEnv is freshBuiltinEnv with regpkg:reg-fn aliased as regpkg:kept
// and then rebound to a lambda, so only the registry and the alias hold the
// builtin.
func builtinEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := freshBuiltinEnv(t)
	evalString(t, env, `(in-package 'regpkg) (set 'kept reg-fn) (set 'reg-fn (lambda (x) 0)) (export 'kept) (in-package 'user)`)
	return env
}

const builtinDoc = `["~#durable",[1,["~#list",[["~#builtin",["regpkg","reg-fn"]],["~#builtin",["lisp","car"]],["~#fn","user:my-fn"]]]]]`

// Every registered builtin is written by its registration and restored
// through the registry, whatever its names bind when it is saved or loaded.
func TestDurableBuiltinAfterRebinding(t *testing.T) {
	env := builtinEnv(t)
	v := env.LoadString("test", `(list regpkg:kept car my-fn)`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	assert.Equal(t, builtinDoc, string(b))

	check := func(env *lisp.LEnv) {
		t.Helper()
		back, err := libjson.LoadDurable(env, b, nil)
		require.NoError(t, err)
		reg := env.Runtime.Registry
		pkg, name, ok := reg.RegisteredBuiltinName(back.Cells[0])
		require.True(t, ok)
		assert.Equal(t, "regpkg:reg-fn", pkg+":"+name)
		assert.Equal(t, reg.RegisteredBuiltin("regpkg", "reg-fn").Native, back.Cells[0].Native)
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
		assert.Equal(t, "7", evalString(t, env, `(funcall (first r) 1)`))
		again, err := libjson.DumpDurable(env, back, nil)
		require.NoError(t, err)
		assert.Equal(t, builtinDoc, string(again))
	}
	// Rebinding the name again before the load changes nothing.
	evalString(t, env, `(in-package 'regpkg) (set 'reg-fn (lambda (x) 1)) (in-package 'user)`)
	check(env)
	// Nor does binding the name to the builtin again.
	evalString(t, env, `(in-package 'regpkg) (set 'reg-fn kept) (in-package 'user)`)
	check(env)
	// A fresh environment, where the name binds the builtin, loads it.
	check(freshBuiltinEnv(t))
}

// A builtin shadowed in another package is written by its registration.
func TestDurableBuiltinShadowed(t *testing.T) {
	env := builtinEnv(t)
	car := env.LoadString("test", `car`)
	require.NoError(t, lisp.GoError(car))
	evalString(t, env, `(defun car (x) 'shadow)`)
	b, err := libjson.DumpDurable(env, car, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#builtin",["lisp","car"]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Equal(t, car.Native, back.Native)
}

// Builtins a later registration replaced share their FID with every other
// generation of the name.  Each is named by a binding that holds that very
// function, so each restores as itself, whether its alias sorts before or
// after the registered name.
func TestDurableBuiltinGenerations(t *testing.T) {
	env := freshBuiltinEnv(t)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	evalString(t, env, `(set 'a-gen1 reg-fn) (set 'z-gen1 reg-fn) (export 'a-gen1 'z-gen1 'y-gen2)`)
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, constFn("reg-fn", 8))))
	evalString(t, env, `(set 'y-gen2 reg-fn)`)
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, constFn("reg-fn", 9))))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	v := env.LoadString("test", `(list regpkg:z-gen1 regpkg:y-gen2 regpkg:a-gen1 regpkg:reg-fn)`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#fn","regpkg:a-gen1"],["~#fn","regpkg:y-gen2"],["~#fn","regpkg:a-gen1"],["~#builtin",["regpkg","reg-fn"]]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	assert.Equal(t, "'(7 8 7 9)", evalString(t, env, `(map 'list (lambda (f) (funcall f 0)) r)`))
	for i, f := range back.Cells {
		assert.Equal(t, v.Cells[i].Native, f.Native, "generation %d restored as another function", i)
	}
}

// A builtin no registration names is named by a binding that holds it
// itself.  One whose FID copies a registered builtin's is not that builtin.
func TestDurableUnregisteredBuiltin(t *testing.T) {
	env := builtinEnv(t)
	reg := env.Runtime.Registry
	car := reg.RegisteredBuiltin(lisp.DefaultLangPackage, "car")
	unbound := lisp.FunInPackage(lisp.DefaultLangPackage, car.FID(), lisp.Formals("x"), car.Builtin())
	_, err := libjson.DumpDurable(env, unbound, nil)
	require.Error(t, err)
	assert.Equal(t, "durable json: cannot encode an anonymous function", err.Error())

	orig := reg.RegisteredBuiltin("regpkg", "reg-fn")
	spoof := lisp.FunInPackage("regpkg", orig.FID(), lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(3) })
	require.NoError(t, lisp.GoError(reg.Package("regpkg").Put(lisp.Symbol("spoof"), spoof)))
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{spoof, orig}), nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#fn","regpkg:spoof"],["~#builtin",["regpkg","reg-fn"]]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Equal(t, spoof.Native, back.Cells[0].Native)
	assert.Equal(t, orig.Native, back.Cells[1].Native)
}

// A ~#fn whose global holds a registered builtin (a Lisp function an
// upgrade moved to Go) loads that builtin, and re-encodes as ~#builtin.
func TestDurableFnNamingRegisteredBuiltin(t *testing.T) {
	env := builtinEnv(t)
	back, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#fn","regpkg:kept"]]]`), nil)
	require.NoError(t, err)
	assert.Equal(t, env.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn").Native, back.Native)
	b, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#builtin",["regpkg","reg-fn"]]]]`, string(b))
}

func TestLoadDurableBuiltinRejects(t *testing.T) {
	env := builtinEnv(t)
	for _, c := range []struct{ name, doc, want string }{
		{"unknown name", `["~#durable",[1,["~#builtin",["lisp","no-such-builtin"]]]]`, "builtin lisp:no-such-builtin: not registered"},
		{"unknown package", `["~#durable",[1,["~#builtin",["no-such-pkg","car"]]]]`, "builtin no-such-pkg:car: not registered"},
		{"wrong package", `["~#durable",[1,["~#builtin",["user","car"]]]]`, "builtin user:car: not registered"},
		{"wrong package of a rebound name", `["~#durable",[1,["~#builtin",["user","reg-fn"]]]]`, "builtin user:reg-fn: not registered"},
		{"lisp function", `["~#durable",[1,["~#builtin",["user","my-fn"]]]]`, "builtin user:my-fn: not registered"},
		{"alias", `["~#durable",[1,["~#builtin",["regpkg","kept"]]]]`, "builtin regpkg:kept: not registered"},
		{"macro", `["~#durable",[1,["~#builtin",["lisp","defun"]]]]`, "builtin lisp:defun: registered as a macro or special operator"},
		{"special operator", `["~#durable",[1,["~#builtin",["lisp","if"]]]]`, "builtin lisp:if: registered as a macro or special operator"},
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

// Saving and loading a builtin reads no binding, so in a lazy VM a large
// value a registered name was rebound to stays unbuilt.  Naming an
// unregistered builtin peeks at the bindings without building them.
func TestDurableBuiltinBuildsNoBinding(t *testing.T) {
	source := freshBuiltinEnv(t)
	evalString(t, source, `(in-package 'regpkg)
(set 'kept reg-fn)
(set 'reg-fn (map 'list (lambda (i) (list i (to-string i))) (make-sequence 0 50000)))
(set 'big2 (map 'list (lambda (i) (list i)) (make-sequence 0 50000)))
(in-package 'user)`)
	unreg := lisp.FunInPackage("regpkg", "unreg-fn", lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(1) })
	require.NoError(t, lisp.GoError(source.Runtime.Registry.Package("regpkg").Put(lisp.Symbol("unreg"), unreg)))
	tmpl, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	fn := vm.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn")
	require.NotNil(t, fn)
	var b []byte
	allocated := allocatedBy(func() { b, err = libjson.DumpDurable(vm, fn, nil) })
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#builtin",["regpkg","reg-fn"]]]]`, string(b))
	assert.Less(t, allocated, uint64(64<<10), "dump bytes allocated")
	allocated = allocatedBy(func() { _, err = libjson.LoadDurable(vm, b, nil) })
	require.NoError(t, err)
	assert.Less(t, allocated, uint64(64<<10), "load bytes allocated")

	u, ok := vm.Runtime.Registry.Package("regpkg").Symbol("unreg")
	require.True(t, ok)
	allocated = allocatedBy(func() { b, err = libjson.DumpDurable(vm, u, nil) })
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#fn","regpkg:unreg"]]]`, string(b))
	assert.Less(t, allocated, uint64(64<<10), "unregistered dump bytes allocated")
}

// parityProbe evaluates src in env and records what consensus depends on:
// the result, the evaluation steps, the dump bytes and charges, the load
// charges, the restored functions' answers, and the next gensym names.
type parityProbe struct {
	result, doc, after string
	gensyms            string
	steps              int64
	dumpCharges        []int
	loadCharges        []int
}

func probeParity(t *testing.T, env *lisp.LEnv) parityProbe {
	t.Helper()
	var p parityProbe
	before := env.Runtime.TotalSteps()
	v := env.LoadStringContext(context.Background(), "probe", `(list regpkg:kept car my-fn (builtin 'regpkg:reg-fn)
  (s:make-validator "n" s:int) (s:make-validator "f" s:float) (s:make-validator "x" s:number))`)
	require.NoError(t, lisp.GoError(v))
	p.steps = env.Runtime.TotalSteps() - before
	require.Positive(t, p.steps, "steps are counted")
	p.result = v.String()
	funcs := lisp.QExpr(v.Cells[:4])
	b, err := libjson.DumpDurable(env, funcs, nil, libjson.WithTypedCharge(func(n int) error { p.dumpCharges = append(p.dumpCharges, n); return nil }))
	require.NoError(t, err)
	p.doc = string(b)
	back, err := libjson.LoadDurable(env, b, nil, libjson.WithTypedCharge(func(n int) error { p.loadCharges = append(p.loadCharges, n); return nil }))
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("restored"), back)))
	p.after = evalString(t, env, `(list (funcall (first restored) 1) (builtin-name (first restored)) (builtin-name (second restored)))`)
	var names []string
	for range 3 {
		names = append(names, evalString(t, env, `(gensym)`))
	}
	p.gensyms = strings.Join(names, " ")
	return p
}

// Bytes, charges both ways, steps, restored identity and later gensym names
// are equal in independently built cold environments and in eager, lazy
// and prewarmed template VMs, with the numeric validator constructors in
// the probe.
func TestDurableBuiltinParity(t *testing.T) {
	want := probeParity(t, builtinEnv(t))
	assert.Contains(t, want.doc, `["~#builtin",["regpkg","reg-fn"]]`)
	assert.Equal(t, `'(7 "regpkg:reg-fn" "lisp:car")`, want.after)
	assert.Equal(t, want, probeParity(t, builtinEnv(t)), "a second cold environment")

	source := builtinEnv(t)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	coldPrewarm, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	vm := func(tmpl *lisp.Template, opts ...lisp.VMOption) *lisp.LEnv {
		v, err := tmpl.NewVM(opts...)
		require.NoError(t, err)
		return v
	}
	// coldPrewarm has an empty hot set; lazy's hot set is filled by the
	// lazy VM before the prewarmed one is built.
	cases := []struct {
		env  func() *lisp.LEnv
		name string
	}{
		{func() *lisp.LEnv { return vm(eager) }, "eager"},
		{func() *lisp.LEnv { return vm(coldPrewarm, lisp.VMWithPrewarm()) }, "prewarmed, empty hot set"},
		{func() *lisp.LEnv { return vm(lazy) }, "lazy"},
		{func() *lisp.LEnv { return vm(lazy, lisp.VMWithPrewarm()) }, "prewarmed, warm hot set"},
	}
	for _, c := range cases {
		assert.Equal(t, want, probeParity(t, c.env()), c.name)
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
