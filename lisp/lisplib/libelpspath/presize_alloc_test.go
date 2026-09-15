// Copyright © 2026 The ELPS authors

//go:build !race

package libelpspath

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Copying scalar map entries must not allocate a destination per entry.
func TestCopyMapWideAllocations(t *testing.T) {
	for _, n := range presizeSizes {
		t.Run(strconv.Itoa(n), func(t *testing.T) {
			env := lisp.NewEnv(nil)
			call := lisp.QExpr([]*lisp.LVal{benchWideMap(n), lisp.String("key0000"), lisp.String("v")})
			allocs := testing.AllocsPerRun(100, func() {
				if v := BuiltinQuerySet(env, call); v.Type == lisp.LError {
					t.Fatal(v)
				}
			})
			base := 28.0
			if n == 1000 {
				base = 30
			}
			if allocs > base*1.05 {
				t.Fatalf("got %g allocations, want at most 5%% above %g", allocs, base)
			}
		})
	}
}

// Nested maps must not add a closure and a child destination per entry either.
func TestCopyGuardAllocations(t *testing.T) {
	for name, doc := range guardCostDocs() {
		t.Run(name, func(t *testing.T) {
			measure := func(copyValue func(*lisp.LVal) (*lisp.LVal, error)) float64 {
				return testing.AllocsPerRun(100, func() {
					if _, err := copyValue(doc); err != nil {
						t.Fatal(err)
					}
				})
			}
			guarded, unguarded := measure(copyLVal), measure(copyLValUnguarded)
			if guarded > unguarded {
				t.Fatalf("guarded copy allocated %g times, unguarded %g", guarded, unguarded)
			}
		})
	}
}

// A whole-range replacement never copies an off-path value. Its guard state
// must stay on the stack even though the general copier can grow heap frames.
func TestRangePathNilAllocations(t *testing.T) {
	for _, n := range presizeSizes {
		t.Run(strconv.Itoa(n), func(t *testing.T) {
			p := Root(Chain(Range(0, n, false)))
			in := benchIntList(n)
			allocs := testing.AllocsPerRun(100, func() {
				out, err := p.Nil(in)
				if err != nil {
					t.Fatal(err)
				}
				if len(out.Cells) != n || !out.Cells[0].IsNil() || in.Cells[0].IsNil() {
					t.Fatal("range replacement changed the source or lost nils")
				}
			})
			if allocs != 4 {
				t.Fatalf("got %g allocations, want 4", allocs)
			}
		})
	}
}
