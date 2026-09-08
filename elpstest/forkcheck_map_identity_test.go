// Copyright © 2026 The ELPS authors

package elpstest

import (
	"context"
	"os"
	"os/exec"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

func oracleJSONValue(data map[string]any) *lisp.LVal {
	return jsonraw.Wrap(data)
}

// #625: identities name physical storage, not the representation exposing it.
func TestForkOracleSeesSwappedNativeAndLispMapBacking(t *testing.T) {
	a, b := make(map[string]any), make(map[string]any)
	first := visibilityEnv(t, oracleJSONValue(a), lisp.Native(b))
	second := visibilityEnv(t, oracleJSONValue(b), lisp.Native(a))
	if envState(first) != envState(second) || aliasSignature(first) != aliasSignature(second) {
		t.Fatal("premise: equal contents and within-VM aliases must not expose the cross-VM swap")
	}
	if shared := sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil); len(shared) != 2 {
		t.Fatalf("both swapped physical maps must be reported shared: %v", shared)
	}
	independent := visibilityEnv(t, oracleJSONValue(make(map[string]any)), lisp.Native(make(map[string]any)))
	if shared := sharedOracleCensuses(newOracleCensus(first), newOracleCensus(independent), nil); len(shared) != 0 {
		t.Fatalf("independent maps incorrectly reported shared: %v", shared)
	}
	if rc := first.Get(lisp.Symbol("a")).MapSet("witness", lisp.Int(17)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if got := second.Get(lisp.Symbol("b")).Native.(map[string]any)["witness"]; got.(*lisp.LVal).Int != 17 {
		t.Fatalf("premise: the native exposure did not observe the Lisp map write: %v", got)
	}
}

func TestForkOracleSeesSwappedDirectNativeStorage(t *testing.T) {
	for _, tc := range []struct {
		name string
		make func() (*lisp.LVal, any)
	}{
		{"byte-slots", func() (*lisp.LVal, any) {
			value := lisp.Bytes([]byte("ab"))
			return value, value.Bytes()
		}},
		{"scalar-header", func() (*lisp.LVal, any) {
			value := lisp.Int(17)
			return value, value
		}},
		{"cell-slots", func() (*lisp.LVal, any) {
			value := lisp.QExpr([]*lisp.LVal{lisp.Int(17)})
			return value, value.Cells
		}},
		{"byte-header", func() (*lisp.LVal, any) {
			value := lisp.Bytes([]byte("ab"))
			return value, value.Native
		}},
		{"map-wrapper", func() (*lisp.LVal, any) {
			value := lisp.SortedMap()
			return value, value.Map()
		}},
		{"closure-environment", func() (*lisp.LVal, any) {
			env := mustEnv(t, "")
			value := env.Lambda(lisp.Formals(), []*lisp.LVal{lisp.Int(17)})
			return value, env
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			a, nativeA := tc.make()
			b, nativeB := tc.make()
			first := visibilityEnv(t, a, lisp.Native(nativeB))
			second := visibilityEnv(t, b, lisp.Native(nativeA))
			if envState(first) != envState(second) || aliasSignature(first) != aliasSignature(second) {
				t.Fatal("premise: swapped storage must have equal contents and within-VM alias shapes")
			}
			if shared := sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil); len(shared) == 0 {
				t.Fatal("direct native exposure concealed shared writable storage")
			}
			c, _ := tc.make()
			_, nativeD := tc.make()
			independent := visibilityEnv(t, c, lisp.Native(nativeD))
			if shared := sharedOracleCensuses(newOracleCensus(first), newOracleCensus(independent), nil); len(shared) != 0 {
				t.Fatalf("independent direct storage reported shared: %v", shared)
			}
			for _, nativeFirst := range []bool{false, true} {
				values := []*lisp.LVal{a, lisp.Native(nativeA)}
				if nativeFirst {
					values[0], values[1] = values[1], values[0]
				}
				census := newOracleCensus(visibilityEnv(t, values...))
				for _, id := range nativeReferenceIDs(nativeA) {
					if _, exemptible := census.natives[id]; exemptible {
						t.Fatalf("native-first=%t: VM-owned address retained native exemption metadata", nativeFirst)
					}
				}
			}
			nativeOnly, ownedOnly := visibilityEnv(t, lisp.Native(nativeA)), visibilityEnv(t, a)
			approve := func(nativePayloadIdentity, any) bool { return true }
			if len(oracleSharedPayloads(nativeOnly, ownedOnly, approve)) == 0 || len(oracleSharedPayloads(ownedOnly, nativeOnly, approve)) == 0 {
				t.Fatal("one native-only occurrence concealed another VM's owned storage")
			}
		})
	}
}

type oracleImmutablePayload struct {
	templatepolicy.Marker
	n int
}

func TestForkOracleMutableMapStorageOverridesNativeMetadata(t *testing.T) {
	for _, nativeFirst := range []bool{false, true} {
		t.Run(map[bool]string{false: "map-first", true: "native-first"}[nativeFirst], func(t *testing.T) {
			data := make(map[string]any)
			values := []*lisp.LVal{oracleJSONValue(data), lisp.Native(data)}
			if nativeFirst {
				values[0], values[1] = values[1], values[0]
			}
			census := newOracleCensus(visibilityEnv(t, values...))
			id := nativePayloadIdentity{reflect.Map, reflect.ValueOf(data).Pointer()}
			if _, found := census.ids[id]; !found {
				t.Fatal("physical map is missing from the ownership census")
			}
			if _, found := census.natives[id]; found {
				t.Fatal("mutable Lisp storage must not retain native-sharing exemption metadata")
			}
			nativeOnly := newOracleCensus(visibilityEnv(t, lisp.Native(data)))
			payload, found := nativeOnly.natives[id].(map[string]any)
			if !found || reflect.ValueOf(payload).Pointer() != reflect.ValueOf(data).Pointer() {
				t.Fatal("a native-only exposure lost the exact payload offered to the sharing policy")
			}
		})
	}
}

func TestForkCheckMutableMapBackingOverridesNativeApproval(t *testing.T) {
	const childFlag = "ELPS_ORACLE_MUTABLE_MAP_CHILD"
	if mode := os.Getenv(childFlag); mode != "" {
		data := make(map[string]any)
		nativeName, mapName := "a", "z"
		if mode == "map-first" {
			nativeName, mapName = mapName, nativeName
		}
		setups := 0
		RunForkCheck(t, ForkCheck{
			NewEnv: func() (*lisp.LEnv, error) {
				env, err := NewForkCheckEnv()
				if err != nil {
					return nil, err
				}
				return env, lisp.GoError(env.PutGlobal(lisp.Symbol(nativeName), lisp.Native(data)))
			},
			TemplateOptions: []lisp.TemplateOption{
				lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }),
				// A raw map cannot embed the closed internal marker. Approve
				// this exact backing explicitly, retaining the same adversarial
				// native/Lisp alias and the same required oracle failure.
				lisp.TemplateWithNativePolicy(func(value any) bool {
					payload, ok := value.(map[string]any)
					return ok && reflect.ValueOf(payload).Pointer() == reflect.ValueOf(data).Pointer()
				}),
			},
			SharedSetupNative: func(any) bool { return true },
			Setup: func(env *lisp.LEnv) error {
				setups++
				backing := data
				if mode == "asymmetric" && setups == 1 {
					backing = make(map[string]any)
				}
				return lisp.GoError(env.PutGlobal(lisp.Symbol(mapName), oracleJSONValue(backing)))
			},
			Tx: []string{"()"},
		})
		return
	}
	for _, mode := range []string{"map-first", "native-first", "asymmetric"} {
		t.Run(mode, func(t *testing.T) {
			executable, err := os.Executable()
			if err != nil {
				t.Fatal(err)
			}
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
			defer cancel()
			// #nosec G204 -- this test binary and fixed selector contain no user input.
			command := exec.CommandContext(ctx, executable, "-test.run=^TestForkCheckMutableMapBackingOverridesNativeApproval$")
			command.Env = append(os.Environ(), childFlag+"="+mode)
			output, err := command.CombinedOutput()
			needle := "mutable payload(s) shared with tx[0] cold"
			if mode == "asymmetric" {
				needle = "mutable payload(s) shared with the template"
			}
			if err == nil || !strings.Contains(string(output), needle) {
				t.Fatalf("native approval concealed mutable Lisp storage: error=%v\n%s", err, output)
			}
		})
	}
}

func TestForkCheckPreservesNativeOnlyApproval(t *testing.T) {
	for _, mode := range []string{"policy", "marker-value", "marker-pointer-policy"} {
		t.Run(mode, func(t *testing.T) {
			var data any = map[string]any{"n": 17}
			switch mode {
			case "marker-value":
				data = oracleImmutablePayload{n: 17}
			case "marker-pointer-policy":
				data = &oracleImmutablePayload{n: 17}
			}
			RunForkCheck(t, ForkCheck{
				SharedSetupNative: func(any) bool { return mode != "marker-value" },
				Setup: func(env *lisp.LEnv) error {
					return lisp.GoError(env.PutGlobal(lisp.Symbol("constant"), lisp.Native(data)))
				},
				Tx: []string{"constant"},
			})
		})
	}
}

func TestForkOracleCensusPreservesEarlierOwnedStorage(t *testing.T) {
	value := lisp.Int(17)
	data := lisp.Bytes([]byte("ab"))
	list := lisp.QExpr([]*lisp.LVal{value})
	mapValue := oracleJSONValue(make(map[string]any))
	oldMapID := oracleMapBackingIDs(mapValue.Map())[0]
	env := visibilityEnv(t, value, data, list, mapValue)
	snapshot := newOracleCensus(env)
	oldBytes, oldCells := data.Bytes(), list.Cells
	for _, name := range []string{"a", "b", "c", "d"} {
		if rc := env.PutGlobal(lisp.Symbol(name), lisp.Nil()); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
	}
	*data.Native.(*[]byte) = []byte("different") //elps:mutates deliberate detachment after the oracle snapshot
	list.Cells = nil                             //elps:mutates deliberate detachment after the oracle snapshot
	*mapValue.Map() = *jsonraw.Wrap(make(map[string]any)).Map()
	// Keeping only the mutable headers would retain their NEW backing, not
	// the snapshot addresses. Pin the copied storage headers in the retainer.
	bytesRetained, cellsRetained, mapRetained := false, false, false
	for _, retained := range snapshot.retained {
		switch retained := retained.(type) {
		case []byte:
			bytesRetained = bytesRetained || (len(retained) == len(oldBytes) && &retained[0] == &oldBytes[0])
		case []*lisp.LVal:
			cellsRetained = cellsRetained || (len(retained) == len(oldCells) && &retained[0] == &oldCells[0])
		case lisp.MapData:
			mapRetained = mapRetained || slices.Contains(oracleMapBackingIDs(&retained), oldMapID)
		}
	}
	if !bytesRetained || !cellsRetained || !mapRetained {
		t.Fatalf("census failed to retain detached backing: bytes=%t cells=%t map=%t", bytesRetained, cellsRetained, mapRetained)
	}
	leaking := visibilityEnv(t, lisp.Native(value))
	before := sharedOracleCensuses(snapshot, newOracleCensus(leaking), nil)
	if !slices.Contains(before, "user:a") {
		t.Fatalf("snapshot forgot an earlier owned header after unbinding: %v", before)
	}
	if now := oracleSharedPayloads(env, leaking, nil); len(now) != 0 {
		t.Fatalf("premise: the current environment still exposes old storage: %v", now)
	}
}
