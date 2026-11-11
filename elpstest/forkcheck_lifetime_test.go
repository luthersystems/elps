// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"runtime"
	"slices"
	"testing"
	"weak"

	"github.com/luthersystems/elps/lisp"
)

// Return no strong fixture reference except the census under test. In #625's
// CI failure, constructing the second VM could reuse the first VM's addresses
// because an address-only extraction helper discarded its census retainers.
func scalarLifetimeCensus(t *testing.T) (oracleCensus, weak.Pointer[lisp.LVal]) {
	t.Helper()
	value := lisp.Int(42)
	return newOracleCensus(visibilityEnv(t, value)), weak.Make(value)
}

func TestForkOracleCensusRetainsTemporaryEnvironment(t *testing.T) {
	first, value := scalarLifetimeCensus(t)
	runtime.GC()
	if value.Value() == nil {
		t.Fatal("address census outlived its scalar owner; a new VM can reuse the recorded address")
	}
	second := newOracleCensus(visibilityEnv(t, lisp.Int(42)))
	runtime.GC()
	if shared := sharedOracleCensuses(first, second, nil); len(shared) != 0 {
		t.Fatalf("independent VMs reported shared after collection: %v", shared)
	}
	leaked := newOracleCensus(visibilityEnv(t, value.Value()))
	runtime.GC()
	if shared := sharedOracleCensuses(first, leaked, nil); !slices.Equal(shared, []string{"user:a"}) {
		t.Fatalf("collection concealed an actual shared scalar: %v", shared)
	}
}

// Larger than Go's tiny-allocation block, so unrelated small objects cannot
// keep this witness alive. The oracle, not the weak pointer, must retain it.
type oracleLifetimeNative [64]byte

func TestForkCheckRetainsPreSetupNativeApprovals(t *testing.T) {
	var originals []weak.Pointer[oracleLifetimeNative]
	setups := 0
	RunForkCheck(t, ForkCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			env, err := NewForkCheckEnv()
			if err != nil {
				return nil, err
			}
			value := &oracleLifetimeNative{17}
			originals = append(originals, weak.Make(value))
			return env, lisp.GoError(env.PutGlobal(lisp.Symbol("constant"), lisp.Native(value)))
		},
		TemplateOptions: []lisp.TemplateOption{
			lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }),
			lisp.TemplateWithNativePolicy(func(value any) bool {
				_, ok := value.(*oracleLifetimeNative)
				return ok
			}),
		},
		Setup: func(env *lisp.LEnv) error {
			setups++
			// Remove the only VM reference to the cold arm's original native.
			// Its approved identity remains in use for later comparisons.
			if rc := env.PutGlobal(lisp.Symbol("constant"), lisp.Nil()); rc.Type == lisp.LError {
				return lisp.GoError(rc)
			}
			runtime.GC()
			for i, original := range originals {
				value := original.Value()
				if value == nil {
					return fmt.Errorf("pre-Setup native %d was collected while its identity was still approved", i)
				}
				if value[0] != 17 {
					return fmt.Errorf("pre-Setup native %d changed: got %d, want 17", i, value[0])
				}
			}
			return nil
		},
		Tx: []string{"constant", "constant"},
	})
	if len(originals) != 3 || setups != 6 {
		t.Fatalf("want one template, two cold arms and six setups; originals=%d setups=%d", len(originals), setups)
	}
}
