// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// vmValueModes lists the ways a template mints VMs. VM values must behave
// the same in each.
var vmValueModes = []struct {
	name string
	tmpl []lisp.TemplateOption
	vm   []lisp.VMOption
	warm bool // read every VM value in a few VMs first, so prewarm builds them
}{
	{name: "lazy"},
	{name: "eager", tmpl: []lisp.TemplateOption{lisp.TemplateWithEagerInstantiation()}},
	{name: "prewarm", vm: []lisp.VMOption{lisp.VMWithPrewarm()}, warm: true},
	{name: "frozen", tmpl: []lisp.TemplateOption{lisp.TemplateWithFrozenPackages("lisp", "user")}},
}

func vmValueLoad(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("vmvalues.lisp", src)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	return v
}

func vmValueCall(t *testing.T, env *lisp.LEnv, fn *lisp.LVal, args ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	v := env.FunCallContext(context.Background(), fn, lisp.SExpr(args))
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	return v
}

func vmValueGet(t *testing.T, rt *lisp.Runtime, name string) *lisp.LVal {
	t.Helper()
	v, ok := rt.VMValue(name)
	require.True(t, ok, "VM value %q is not set", name)
	return v
}

// mapKeys renders a sorted map's keys, for comparisons.
func mapKeys(t *testing.T, m *lisp.LVal) string {
	t.Helper()
	require.Equal(t, lisp.LSortMap, m.Type)
	keys := m.MapKeys().Cells
	out := make([]string, len(keys))
	for i, k := range keys {
		out[i] = k.String()
	}
	return "(" + strings.Join(out, " ") + ")"
}

// vmValueSource builds a source with VM values of the shapes an embedder's
// router tables have: a sorted map of endpoints, a hook list of closures, and
// a map that a package global and a VM value share.
func vmValueSource(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := templateTestEnv(t)
	routes := vmValueLoad(t, env, `(sorted-map "a" 1 "b" 2)`)
	hooks := vmValueLoad(t, env, `(let ((n 10)) (list (lambda (x) (+ x n)) (lambda (x) (* x n))))`)
	shared := vmValueLoad(t, env, `(set 'shared (sorted-map "k" 0))`)
	adder := vmValueLoad(t, env, `(lambda (m k) (assoc! m k true))`)
	env.Runtime.SetVMValue("routes", routes)
	env.Runtime.SetVMValue("hooks", hooks)
	env.Runtime.SetVMValue("shared", shared)
	env.Runtime.SetVMValue("adder", adder)
	env.Runtime.SetVMValue("unset", nil)
	return env
}

// vmValueWarm mints VMs that read every VM value of tmpl, so the hot set
// that VMWithPrewarm reads names them.
func vmValueWarm(t *testing.T, tmpl *lisp.Template) {
	t.Helper()
	for range 4 {
		vm, err := tmpl.NewVM()
		require.NoError(t, err)
		for _, name := range []string{"routes", "hooks", "shared", "adder", "unset"} {
			vmValueGet(t, vm.Runtime, name)
		}
	}
}

func TestVMValueColdRuntime(t *testing.T) {
	env := templateTestEnv(t)
	rt := env.Runtime
	_, ok := rt.VMValue("x")
	assert.False(t, ok, "a cold runtime has no VM values")
	rt.DeleteVMValue("x") // removing an unset name does nothing

	v := lisp.Int(7)
	rt.SetVMValue("x", v)
	assert.Same(t, v, vmValueGet(t, rt, "x"), "SetVMValue stores the value itself")
	rt.SetVMValue("nil", nil)
	assert.True(t, vmValueGet(t, rt, "nil").IsNil())
	rt.DeleteVMValue("x")
	_, ok = rt.VMValue("x")
	assert.False(t, ok)

	// A VM value has no symbol, so Lisp code cannot reach it.
	assert.Equal(t, lisp.LError, env.LoadString("cold.lisp", `x`).Type)
}

func TestVMValuePublishedToEveryVM(t *testing.T) {
	for _, mode := range vmValueModes {
		t.Run(mode.name, func(t *testing.T) {
			source := vmValueSource(t)
			tmpl, err := lisp.NewTemplate(source, append([]lisp.TemplateOption{templateCorePolicy()}, mode.tmpl...)...)
			require.NoError(t, err)
			// A write to the source after publication does not reach the template.
			source.Runtime.SetVMValue("late", lisp.Int(1))
			vmValueCall(t, source, vmValueGet(t, source.Runtime, "adder"), vmValueGet(t, source.Runtime, "routes"), lisp.String("source"))

			if mode.warm {
				vmValueWarm(t, tmpl)
			}
			vm1, err := tmpl.NewVM(mode.vm...)
			require.NoError(t, err)
			vm2, err := tmpl.NewVM(mode.vm...)
			require.NoError(t, err)

			for _, vm := range []*lisp.LEnv{vm1, vm2} {
				// A VM value that shares a map with a package global still
				// shares it in the VM.
				assert.Same(t, vmValueLoad(t, vm, `shared`), vmValueGet(t, vm.Runtime, "shared"))
				assert.Equal(t, `("a" "b")`, mapKeys(t, vmValueGet(t, vm.Runtime, "routes")))
				assert.True(t, vmValueGet(t, vm.Runtime, "unset").IsNil())
				_, ok := vm.Runtime.VMValue("late")
				assert.False(t, ok, "a source write after publication reached a VM")
				// A closure in a VM value runs in the VM.
				hooks := vmValueGet(t, vm.Runtime, "hooks")
				assert.Equal(t, 11, vmValueCall(t, vm, hooks.Cells[0], lisp.Int(1)).Int)
				assert.Equal(t, 20, vmValueCall(t, vm, hooks.Cells[1], lisp.Int(2)).Int)
			}
			assert.NotSame(t, vmValueGet(t, vm1.Runtime, "routes"), vmValueGet(t, vm2.Runtime, "routes"))

			// assoc! in one VM is seen by no other VM and not by the template.
			vmValueCall(t, vm1, vmValueGet(t, vm1.Runtime, "adder"), vmValueGet(t, vm1.Runtime, "routes"), lisp.String("vm1"))
			assert.Equal(t, `("a" "b" "vm1")`, mapKeys(t, vmValueGet(t, vm1.Runtime, "routes")))
			assert.Equal(t, `("a" "b")`, mapKeys(t, vmValueGet(t, vm2.Runtime, "routes")))
			vm3, err := tmpl.NewVM(mode.vm...)
			require.NoError(t, err)
			assert.Equal(t, `("a" "b")`, mapKeys(t, vmValueGet(t, vm3.Runtime, "routes")))
			assert.Equal(t, `("a" "b" "source")`, mapKeys(t, vmValueGet(t, source.Runtime, "routes")))

			// A value a VM value shares with a package global stays shared
			// in each VM, as in the source.
			vmValueLoad(t, vm1, `(assoc! shared "global" true)`)
			assert.Equal(t, `("global" "k")`, mapKeys(t, vmValueGet(t, vm1.Runtime, "shared")))
			vmValueCall(t, vm2, vmValueGet(t, vm2.Runtime, "adder"), vmValueGet(t, vm2.Runtime, "shared"), lisp.String("slot"))
			assert.Equal(t, `("k" "slot")`, mapKeys(t, vmValueLoad(t, vm2, `shared`)))
			assert.Equal(t, `("k")`, mapKeys(t, vmValueLoad(t, vm3, `shared`)))

			// A set or delete in one VM is seen by no other VM.
			vm1.Runtime.SetVMValue("routes", lisp.Int(5))
			vm1.Runtime.DeleteVMValue("hooks")
			vm1.Runtime.SetVMValue("own", lisp.String("vm1"))
			_, ok := vm1.Runtime.VMValue("hooks")
			assert.False(t, ok)
			assert.Equal(t, 5, vmValueGet(t, vm1.Runtime, "routes").Int)
			assert.Equal(t, lisp.LSortMap, vmValueGet(t, vm2.Runtime, "routes").Type)
			assert.Len(t, vmValueGet(t, vm2.Runtime, "hooks").Cells, 2)
			_, ok = vm2.Runtime.VMValue("own")
			assert.False(t, ok)

			// A template published from a VM carries that VM's values.
			again, err := lisp.NewTemplate(vm1, append([]lisp.TemplateOption{templateCorePolicy()}, mode.tmpl...)...)
			require.NoError(t, err)
			vm4, err := again.NewVM(mode.vm...)
			require.NoError(t, err)
			assert.Equal(t, 5, vmValueGet(t, vm4.Runtime, "routes").Int)
			assert.Equal(t, "vm1", vmValueGet(t, vm4.Runtime, "own").Str)
			_, ok = vm4.Runtime.VMValue("hooks")
			assert.False(t, ok, "a deleted VM value was published")
			assert.Equal(t, `("global" "k")`, mapKeys(t, vmValueGet(t, vm4.Runtime, "shared")))
			vmValueLoad(t, vm4, `(assoc! shared "vm4" true)`)
			assert.Equal(t, `("global" "k" "vm4")`, mapKeys(t, vmValueGet(t, vm4.Runtime, "shared")))
		})
	}
}

func TestVMValueTemplateRefusesUnsafeNative(t *testing.T) {
	newSource := func(t *testing.T) *lisp.LEnv {
		env := templateTestEnv(t)
		m := lisp.SortedMap()
		require.False(t, m.Map().Set(lisp.String("handle"), lisp.Native(new(int))).IsError())
		env.Runtime.SetVMValue("bad", m)
		return env
	}
	env := newSource(t)
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	require.Nil(t, tmpl)
	require.Error(t, err)
	assert.Contains(t, err.Error(), `VM value "bad"`)
	assert.Contains(t, err.Error(), "has no template immutability declaration")

	// The same payload as a package global is refused for the same reason.
	global := templateTestEnv(t)
	require.False(t, global.PutGlobal(lisp.Symbol("bad"), lisp.Native(new(int))).IsError())
	_, err = lisp.NewTemplate(global, templateCorePolicy())
	require.Error(t, err)
	assert.Contains(t, err.Error(), "has no template immutability declaration")

	// A native policy that approves the payload admits it, as for a global.
	env = newSource(t)
	tmpl, err = lisp.NewTemplate(env, templateCorePolicy(), lisp.TemplateWithNativePolicy(func(any) bool { return true }))
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	assert.Equal(t, lisp.LSortMap, vmValueGet(t, vm.Runtime, "bad").Type)

	// A builtin in a VM value needs a builtin policy, as for a global.
	env = templateTestEnv(t)
	env.Runtime.SetVMValue("fn", vmValueLoad(t, env, `lisp:car`))
	_, err = lisp.NewTemplate(env)
	require.Error(t, err)
	assert.Contains(t, err.Error(), "has no template sharing declaration")
}

// TestVMValueConcurrentVMs mints VMs from one template on many goroutines.
// Each VM changes its own copy of the published map in place. Run it with
// -race.
func TestVMValueConcurrentVMs(t *testing.T) {
	for _, mode := range vmValueModes {
		t.Run(mode.name, func(t *testing.T) {
			tmpl, err := lisp.NewTemplate(vmValueSource(t), append([]lisp.TemplateOption{templateCorePolicy()}, mode.tmpl...)...)
			require.NoError(t, err)
			const n = 16
			var wg sync.WaitGroup
			errs := make(chan error, n)
			if mode.warm {
				vmValueWarm(t, tmpl)
			}
			for i := range n {
				wg.Add(1)
				go func() {
					defer wg.Done()
					errs <- vmValueRace(tmpl, mode.vm, i)
				}()
			}
			wg.Wait()
			close(errs)
			for err := range errs {
				assert.NoError(t, err)
			}
		})
	}
}

func vmValueRace(tmpl *lisp.Template, opts []lisp.VMOption, i int) error {
	vm, err := tmpl.NewVM(opts...)
	if err != nil {
		return err
	}
	for round := range 4 {
		routes, _ := vm.Runtime.VMValue("routes")
		adder, _ := vm.Runtime.VMValue("adder")
		key := fmt.Sprintf("vm%d-%d", i, round)
		if v := vm.FunCallContext(context.Background(), adder, lisp.SExpr([]*lisp.LVal{routes, lisp.String(key)})); v.IsError() {
			return fmt.Errorf("vm %d: %v", i, v)
		}
		if got, want := routes.Map().Len(), 3+round; got != want {
			return fmt.Errorf("vm %d: routes has %d entries, want %d: another VM's write is visible", i, got, want)
		}
		hooks, _ := vm.Runtime.VMValue("hooks")
		if v := vm.FunCallContext(context.Background(), hooks.Cells[0], lisp.SExpr([]*lisp.LVal{lisp.Int(round)})); v.Int != round+10 {
			return fmt.Errorf("vm %d: hook returned %v", i, v)
		}
		vm.Runtime.SetVMValue(key, lisp.Int(i))
	}
	return nil
}
