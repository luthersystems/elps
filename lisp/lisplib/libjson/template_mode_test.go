package libjson_test

import (
	"fmt"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// modeTemplate loads program into a fresh environment and publishes it.
func modeTemplate(t *testing.T, program string) *lisp.Template {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadRuntimeLibrary(env).IsNil())
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
	if program != "" {
		res := env.LoadString("mode.lisp", program)
		require.NotEqual(t, lisp.LError, res.Type, "%v", res)
	}
	tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	return tmpl
}

func modeVM(t *testing.T, tmpl *lisp.Template) *lisp.LEnv {
	t.Helper()
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	return vm
}

func evalMode(t *testing.T, vm *lisp.LEnv, src string) string {
	t.Helper()
	res := vm.LoadString("probe.lisp", src)
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)
	return res.String()
}

const modeProbe = `(list (json:string-numbers?) (json:load-string "12345678901234567"))`

// Issue #678: a mode set inside one forked VM must not leak into a sibling.
func TestJSONModeDoesNotLeakAcrossTemplateVMs(t *testing.T) {
	tmpl := modeTemplate(t, "")
	a, b := modeVM(t, tmpl), modeVM(t, tmpl)
	baseline := evalMode(t, b, modeProbe)
	evalMode(t, a, `(json:use-string-numbers true) (json:use-exact-integers true)`)
	require.Equal(t, `'(true "12345678901234567")`, evalMode(t, a, modeProbe))
	require.Equal(t, baseline, evalMode(t, b, modeProbe), "mode set in VM a leaked into VM b")
	require.Equal(t, baseline, evalMode(t, modeVM(t, tmpl), modeProbe), "mode leaked into a new VM")
	// exact-integers alone
	evalMode(t, a, `(json:use-string-numbers false)`)
	require.Equal(t, "12345678901234567", evalMode(t, a, `(json:load-string "12345678901234567")`))
	require.Equal(t, baseline, evalMode(t, b, modeProbe))
}

// A mode set while the program loads is captured by the template.
func TestJSONLoadTimeModeInheritedByForks(t *testing.T) {
	tmpl := modeTemplate(t, `(json:use-string-numbers true)`)
	a, b := modeVM(t, tmpl), modeVM(t, tmpl)
	require.Equal(t, `'(true "12345678901234567")`, evalMode(t, a, modeProbe))
	evalMode(t, a, `(json:use-string-numbers false)`)
	require.Equal(t, "false", evalMode(t, a, `(json:string-numbers?)`))
	require.Equal(t, `'(true "12345678901234567")`, evalMode(t, b, modeProbe))

	tmpl = modeTemplate(t, `(json:use-exact-integers true)`)
	require.Equal(t, "12345678901234567", evalMode(t, modeVM(t, tmpl), `(json:load-string "12345678901234567")`))
}

// Concurrent VMs toggling their own mode: must be race-free and isolated.
func TestJSONModeConcurrentTemplateVMs(t *testing.T) {
	tmpl := modeTemplate(t, "")
	var wg sync.WaitGroup
	errs := make(chan error, 8)
	for i := range 8 {
		vm := modeVM(t, tmpl)
		on := i%2 == 0
		wg.Add(1)
		go func() {
			defer wg.Done()
			want := "false"
			if on {
				want = "true"
			}
			for range 200 {
				res := vm.LoadString("c.lisp", fmt.Sprintf(`(json:use-string-numbers %s) (json:use-exact-integers %s) (json:string-numbers?)`, want, want))
				if res.String() != want {
					errs <- fmt.Errorf("vm wanted %s, got %v", want, res)
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
}
