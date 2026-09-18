// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// The three walks below used to read the MaxValueDepth constant rather than
// the runtime's configured limit, so WithMaxValueDepth -- documented as
// configuring template admission "among other walks" -- did not reach them.
// 2048 is above the 1024 floor WithMaxValueDepth accepts, so a 3000-deep value
// is over it and a 1500-deep value is under it, and the default is far above
// both: before the fix every case below accepted 3000.
const (
	runtimeDepthLimit = 2048
	overRuntimeLimit  = 3000
	underRuntimeLimit = 1500
)

func TestGoValueHonoursRuntimeDepthLimit(t *testing.T) {
	rt := &Runtime{MaxValueDepth: runtimeDepthLimit}

	deep := depthTestValue(overRuntimeLimit)
	err, ok := GoValueWithRuntime(rt, deep).(error)
	if !ok {
		t.Fatalf("GoValueWithRuntime accepted a %d-deep value under a %d limit", overRuntimeLimit, runtimeDepthLimit)
	}
	if !strings.Contains(err.Error(), "maximum: 2048") {
		t.Errorf("depth error names the wrong limit: %v", err)
	}
	if _, ok := GoSliceWithRuntime(rt, deep); ok {
		t.Error("GoSliceWithRuntime accepted a value over the runtime limit")
	}
	m := SortedMap()
	m.Map().Set(String("deep"), deep)
	if _, ok := GoMapWithRuntime(rt, m); ok {
		t.Error("GoMapWithRuntime accepted a value over the runtime limit")
	}

	shallow := depthTestValue(underRuntimeLimit)
	if _, ok := GoValueWithRuntime(rt, shallow).(error); ok {
		t.Errorf("GoValueWithRuntime rejected a %d-deep value under a %d limit", underRuntimeLimit, runtimeDepthLimit)
	}
	if _, ok := GoSliceWithRuntime(rt, shallow); !ok {
		t.Error("GoSliceWithRuntime rejected a value under the runtime limit")
	}

	// The runtime-less entry points keep the default, which is the
	// compatibility half of the contract.
	if _, ok := GoValue(deep).(error); ok {
		t.Error("GoValue must keep the MaxValueDepth default")
	}
}

func TestPackageAdmissionHonoursRuntimeDepthLimit(t *testing.T) {
	admit := func(depth int) *LVal {
		env := NewEnv(nil)
		if rc := WithMaxValueDepth(runtimeDepthLimit)(env); rc.Type == LError {
			t.Fatalf("configuring the limit failed: %s", rc)
		}
		pkg := NewPackage("admit-depth")
		pkg.Put(Symbol("deep"), depthTestValue(depth))
		if !env.Runtime.Registry.AddPackage(pkg) {
			t.Fatal("package was not admitted")
		}
		v, ok := env.Runtime.Registry.Package("admit-depth").Symbol("deep")
		if !ok {
			t.Fatal("admitted package lost the binding")
		}
		return v
	}

	got := admit(overRuntimeLimit)
	if got.Type != LError || !strings.Contains(got.String(), "maximum: 2048") {
		t.Fatalf("admission accepted a %d-deep value under a %d limit: %s", overRuntimeLimit, runtimeDepthLimit, got)
	}
	if got := admit(underRuntimeLimit); got.Type == LError {
		t.Fatalf("admission rejected a %d-deep value under a %d limit: %s", underRuntimeLimit, runtimeDepthLimit, got)
	}
}
