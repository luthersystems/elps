// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"runtime"
	"strings"
	"testing"
	"time"
	"weak"
)

// TestOwnershipCheck_LiveValueOutlivesOwner pins that collecting a Runtime
// does not launder the values it adopted: an embedder that retains a value
// bound into runtime A, drops A, and binds the value into runtime B must
// still be caught after A's cleanup has run.
func TestOwnershipCheck_LiveValueOutlivesOwner(t *testing.T) {
	m := SortedMap()
	v := SExpr([]*LVal{Int(1), Int(2)})
	v.quoted = true
	wa := func() weak.Pointer[Runtime] {
		envA := newOwnershipTestEnv()
		if res := envA.Put(Symbol("m"), m); res.Type == LError {
			t.Fatal(res)
		}
		if res := envA.Put(Symbol("v"), v); res.Type == LError {
			t.Fatal(res)
		}
		return weak.Make(envA.Runtime)
	}()
	for i := 0; ; i++ {
		runtime.GC()
		if _, ok := ownershipRuntimes.Load(wa); !ok && wa.Value() == nil {
			break
		}
		if i > 200 {
			t.Fatal("runtime A's cleanup never ran")
		}
		time.Sleep(5 * time.Millisecond)
	}
	envB := newOwnershipTestEnv()
	for _, val := range []*LVal{m, v} {
		func() {
			defer func() {
				r := recover()
				ov, ok := r.(ownershipViolation)
				if !ok {
					t.Fatalf("expected ownership violation after owner was collected, got %v", r)
				}
				if !strings.Contains(ov.msg, "owner runtime:  <collected>") {
					t.Fatalf("message should name a collected owner: %s", ov.msg)
				}
			}()
			envB.Put(Symbol("x"), val)
		}()
	}
}

// TestOwnershipCheck_OrphanSweepDropsDeadKeys pins the memory bound for the
// fix above: entries kept because their value outlived its owner are
// removed once the value itself dies.
func TestOwnershipCheck_OrphanSweepDropsDeadKeys(t *testing.T) {
	func() {
		envA := newOwnershipTestEnv()
		for range 20 {
			envA.Put(Symbol("x"), SExpr([]*LVal{Int(1)}))
		}
	}()
	for i := 0; ; i++ {
		runtime.GC()
		sweepOwnershipOrphans()
		if ownershipOrphanCount() == 0 {
			return
		}
		if i > 200 {
			t.Fatalf("orphan entries never swept: %d remain", ownershipOrphanCount())
		}
		time.Sleep(5 * time.Millisecond)
	}
}
