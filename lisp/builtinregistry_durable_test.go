// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// generationsEnv returns registryEnv with three generations of regpkg:reg-fn
// (returning 7, 8 and 9) and an unregistered builtin.  The replaced
// generations and the unregistered builtin are held only by FunRef aliases,
// distinct headers over one function, one sorting before and one after
// the registered name:
//
//	gen1: a-gen1, z-gen1    gen2: b-gen2, y-gen2    unregistered: u-a, u-b
func generationsEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := registryEnv(t)
	reg := env.Runtime.Registry
	pkg := reg.Package("regpkg")
	alias := func(fn *lisp.LVal, names ...string) {
		for _, name := range names {
			require.NoError(t, lisp.GoError(pkg.Put(lisp.Symbol(name), lisp.FunRef(lisp.Symbol(name), fn))))
		}
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("regpkg"))))
	alias(reg.RegisteredBuiltin("regpkg", "reg-fn"), "z-gen1", "a-gen1")
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, constBuiltin("reg-fn", 8))))
	alias(reg.RegisteredBuiltin("regpkg", "reg-fn"), "y-gen2", "b-gen2")
	require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Shadow: true}, constBuiltin("reg-fn", 9))))
	alias(lisp.FunInPackage("regpkg", "u-fid", lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Int(5) }), "u-b", "u-a")
	pkg.Exports("a-gen1", "z-gen1", "b-gen2", "y-gen2", "u-a", "u-b")
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

const generationsDoc = `["~#durable",[1,["~#list",[["~#fn","regpkg:a-gen1"],["~#fn","regpkg:b-gen2"],["~#fn","regpkg:u-a"],["~#builtin",["regpkg","reg-fn"]],["~#builtin",["lisp","car"]]]]]]`

// generationValues reads each function through the alias that sorts after
// its first name, so the first name's binding can stay unbuilt.
func generationValues(t *testing.T, env *lisp.LEnv) *lisp.LVal {
	t.Helper()
	pkg := env.Runtime.Registry.Package("regpkg")
	var out []*lisp.LVal
	for _, name := range []string{"z-gen1", "y-gen2", "u-b"} {
		v, ok := pkg.Symbol(name)
		require.True(t, ok, name)
		out = append(out, v)
	}
	out = append(out, env.Runtime.Registry.RegisteredBuiltin("regpkg", "reg-fn"), env.Runtime.Registry.RegisteredBuiltin("lisp", "car"))
	return lisp.QExpr(out)
}

// The first name of a replaced generation or an unregistered builtin is
// found by identity in every package state of a lazy VM, without building
// the binding: untouched (pending), after a rebinding, after a thaw, and in
// a VM of a template published from a VM.
func TestDurableGenerationsInLazyPackageStates(t *testing.T) {
	source := generationsEnv(t)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	tmpl, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	newVM := func(tm *lisp.Template) *lisp.LEnv {
		vm, err := tm.NewVM()
		require.NoError(t, err)
		return vm
	}
	republished := func() *lisp.LEnv {
		tm, err := lisp.NewTemplate(newVM(tmpl), policy)
		require.NoError(t, err)
		return newVM(tm)
	}
	for _, c := range []struct {
		vm    func() *lisp.LEnv
		name  string
		setup func(*lisp.Package)
	}{
		{func() *lisp.LEnv { return newVM(tmpl) }, "pending", nil},
		{func() *lisp.LEnv { return newVM(tmpl) }, "rebound", func(p *lisp.Package) { p.Put(lisp.Symbol("reg-fn"), lisp.Int(0)) }},
		{func() *lisp.LEnv { return newVM(tmpl) }, "thawed", func(p *lisp.Package) { p.Put(lisp.Symbol("brand-new"), lisp.Int(0)) }},
		{republished, "republished", nil},
	} {
		t.Run(c.name, func(t *testing.T) {
			vm := c.vm()
			if c.setup != nil {
				c.setup(vm.Runtime.Registry.Package("regpkg"))
			}
			v := generationValues(t, vm)
			before := lisp.LazyMaterialized(vm)
			b, err := libjson.DumpDurable(vm, v, nil)
			require.NoError(t, err)
			assert.Equal(t, generationsDoc, string(b))
			assert.Equal(t, before, lisp.LazyMaterialized(vm), "the dump built a binding")
			back, err := libjson.LoadDurable(vm, b, nil)
			require.NoError(t, err)
			for i := range v.Cells {
				assert.Equal(t, v.Cells[i].Native, back.Cells[i].Native, "function %d restored as another", i)
			}
		})
	}
}

// loadProbe is what consensus depends on when a document loads into a VM:
// the load charges, what the restored functions return and the steps they
// take, the next gensym names and how many plan values the load built.
type loadProbe struct {
	results, gensyms string
	loadCharges      []int
	steps            int64
	materialized     int
}

func probeLoad(t *testing.T, env *lisp.LEnv, doc []byte) loadProbe {
	t.Helper()
	var p loadProbe
	before := lisp.LazyMaterialized(env)
	back, err := libjson.LoadDurable(env, doc, nil, libjson.WithTypedCharge(func(n int) error { p.loadCharges = append(p.loadCharges, n); return nil }))
	require.NoError(t, err)
	if before >= 0 {
		p.materialized = lisp.LazyMaterialized(env) - before
	}
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("restored"), back)))
	steps := env.Runtime.TotalSteps()
	v := env.LoadStringContext(context.Background(), "probe", `(list (funcall (nth restored 0) 0) (funcall (nth restored 1) 0)
  (funcall (nth restored 2) 0) (funcall (nth restored 3) 0) (funcall (nth restored 4) '(1 2))
  (builtin-name (nth restored 3)) (builtin-name (nth restored 0))
  (s:make-validator "n" s:int) (s:make-validator "f" s:float) (s:make-validator "x" s:number))`)
	require.NoError(t, lisp.GoError(v))
	p.steps = env.Runtime.TotalSteps() - steps
	p.results = v.String()
	var names []string
	for range 3 {
		names = append(names, evalOK(t, env, `(gensym)`))
	}
	p.gensyms = strings.Join(names, " ")
	return p
}

// A document produced in one VM loads the same into separately built fresh
// cold, eager, lazy and prewarmed VMs: the same load charges, results,
// steps and later gensym names.  A lazy VM and a prewarmed VM with an empty
// hot set build the same number of values; a VM prewarmed from a warm hot
// set builds no more.
func TestDurableFreshLoadParity(t *testing.T) {
	producer := generationsEnv(t)
	doc, err := libjson.DumpDurable(producer, generationValues(t, producer), nil)
	require.NoError(t, err)
	require.Equal(t, generationsDoc, string(doc))

	want := probeLoad(t, generationsEnv(t), doc)
	assert.True(t, strings.HasPrefix(want.results, `'(7 8 5 9 1 "regpkg:reg-fn" ()`), want.results)
	assert.Positive(t, want.steps)

	source := generationsEnv(t)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	coldPrewarm, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	vm := func(tm *lisp.Template, opts ...lisp.VMOption) *lisp.LEnv {
		v, err := tm.NewVM(opts...)
		require.NoError(t, err)
		return v
	}
	same := func(name string, got loadProbe) {
		t.Helper()
		assert.Equal(t, want.results, got.results, name)
		assert.Equal(t, want.gensyms, got.gensyms, name)
		assert.Equal(t, want.loadCharges, got.loadCharges, name)
		assert.Equal(t, want.steps, got.steps, name)
	}
	same("eager", probeLoad(t, vm(eager), doc))
	empty := probeLoad(t, vm(coldPrewarm, lisp.VMWithPrewarm()), doc)
	same("prewarmed, empty hot set", empty)
	lazyProbe := probeLoad(t, vm(lazy), doc)
	same("lazy", lazyProbe)
	assert.Equal(t, lazyProbe.materialized, empty.materialized, "lazy and empty-prewarm VMs build different counts")
	warm := probeLoad(t, vm(lazy, lisp.VMWithPrewarm()), doc)
	same("prewarmed, warm hot set", warm)
	assert.LessOrEqual(t, warm.materialized, lazyProbe.materialized)
}
