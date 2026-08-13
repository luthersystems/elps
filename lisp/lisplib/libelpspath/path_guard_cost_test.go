// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// The cost of the cycle guard, measured with the guard as the ONLY difference
// between the two arms.
//
// BenchmarkTypeCheck and BenchmarkCopy measure the shipped walkers, which is
// what a caller pays; they cannot say how much of that is the guard.  The
// replicas below are the same walkers with the three guard calls (Descend,
// Tracking, Ascend) deleted and nothing else changed, so a benchstat between
// the arms is the guard's price and nothing else.
//
// They are deliberately dumb copies rather than a shared body with a flag: a
// flag would put a branch in the shipped path that only the benchmark needs.
// TestUnguardedReplicasMatchTheShippedWalkers is the drift guard — it requires
// the two to agree on every acyclic document the suite builds, so a change to
// the real walker that is not mirrored here fails rather than quietly
// benchmarking a stale copy.

func okSimpleTypeUnguarded(in *lisp.LVal) error {
	if in.IsNil() {
		return nil
	}
	switch in.Type {
	case lisp.LString, lisp.LInt, lisp.LFloat:
		return nil
	case lisp.LSymbol:
		if in.Str == lisp.TrueSymbol || in.Str == lisp.FalseSymbol {
			return nil
		}
		return okSimpleContainerTypeUnguarded(in)
	default:
		return okSimpleContainerTypeUnguarded(in)
	}
}

func okSimpleContainerTypeUnguarded(in *lisp.LVal) error {
	if in.IsNil() {
		return errors.New("nil container type invalid")
	}
	switch in.Type {
	case lisp.LSortMap:
		entries := sortedMapEntries(in.Map())
		if err := lisp.GoError(entries); err != nil {
			return err
		}
		for _, ent := range entries.Cells {
			if err := okSimpleTypeUnguarded(ent.Cells[1]); err != nil {
				return err
			}
		}
		return nil
	case lisp.LArray:
		if in.Cells[0].Len() > 1 {
			return errors.New("cannot index multi-dimensional array")
		}
		for _, v := range in.Cells[1].Cells {
			if err := okSimpleTypeUnguarded(v); err != nil {
				return err
			}
		}
		return nil
	case lisp.LSExpr:
		for _, v := range in.Cells {
			if err := okSimpleTypeUnguarded(v); err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
}

func copyLValUnguarded(v *lisp.LVal) (*lisp.LVal, error) {
	switch v.Type {
	case lisp.LSortMap:
		m0 := v.Map()
		if m0 == nil {
			return nil, errors.New("first argument is not a map")
		}
		entries := sortedMapEntries(m0)
		if err := lisp.GoError(entries); err != nil {
			return nil, err
		}
		sm := lisp.SortedMap()
		m := sm.Map()
		for _, pair := range entries.Cells {
			val, err := copyLValUnguarded(pair.Cells[1])
			if err != nil {
				return nil, err
			}
			if lerr := m.Set(pair.Cells[0], val); lerr.Type == lisp.LError {
				return nil, lisp.GoError(lerr)
			}
		}
		return sameQuoting(v, sm), nil
	case lisp.LArray:
		if v.Cells[0].Len() > 1 {
			return lisp.Nil(), nil
		}
		cells := v.Cells[1].Cells
		out := make([]*lisp.LVal, len(cells))
		for i := range cells {
			c, err := copyLValUnguarded(cells[i])
			if err != nil {
				return nil, err
			}
			out[i] = c
		}
		return sameQuoting(v, toVector(out)), nil
	case lisp.LSExpr:
		cells := v.Cells
		out := make([]*lisp.LVal, len(cells))
		for i := range cells {
			c, err := copyLValUnguarded(cells[i])
			if err != nil {
				return nil, err
			}
			out[i] = c
		}
		return sameQuoting(v, toList(out)), nil
	default:
		return v, nil
	}
}

// guardCostDocs are the shapes the guard cost is reported over: the
// transaction-path record, and a spine deep enough to be worth checking that
// the cost stays flat.  Neither is cyclic — the guard's price is what it
// charges values that will never trip it.
func guardCostDocs() map[string]*lisp.LVal {
	return map[string]*lisp.LVal{
		"record": benchDoc(),
		"spine8": benchSpineDoc(8),
	}
}

// BenchmarkCycleGuardCost is the guard-only comparison.  Run it as
//
//	go test -run=XXX -bench=BenchmarkCycleGuardCost -count=10 ./lisp/lisplib/libelpspath/
//	benchstat -col /arm bench.txt
//
// The two arms alternate within a single process, so a machine that drifts
// during the run moves both.
func BenchmarkCycleGuardCost(b *testing.B) {
	for name, doc := range guardCostDocs() {
		b.Run("doc="+name, func(b *testing.B) {
			b.Run("walk=typecheck/arm=guarded", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if err := okSimpleType(doc); err != nil {
						b.Fatal(err)
					}
				}
			})
			b.Run("walk=typecheck/arm=unguarded", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if err := okSimpleTypeUnguarded(doc); err != nil {
						b.Fatal(err)
					}
				}
			})
			b.Run("walk=copy/arm=guarded", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if _, err := copyLVal(doc); err != nil {
						b.Fatal(err)
					}
				}
			})
			b.Run("walk=copy/arm=unguarded", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if _, err := copyLValUnguarded(doc); err != nil {
						b.Fatal(err)
					}
				}
			})
		})
	}
}

// TestUnguardedReplicasMatchTheShippedWalkers keeps the benchmark honest: the
// replicas above must answer identically to the shipped walkers on every
// acyclic document in the suite, or the arms are not measuring one difference.
func TestUnguardedReplicasMatchTheShippedWalkers(t *testing.T) {
	t.Parallel()
	docs := guardCostDocs()
	for name, mk := range aliasSources {
		docs["alias-"+name] = mk()
	}
	for name, doc := range docs {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			assert.Equal(t, okSimpleTypeUnguarded(doc), okSimpleType(doc),
				"the replica gate disagrees with the shipped one")
			want, wantErr := copyLValUnguarded(doc)
			got, gotErr := copyLVal(doc)
			require.Equal(t, wantErr, gotErr)
			assert.Equal(t, want.String(), got.String(),
				"the replica copy disagrees with the shipped one")
		})
	}
}

// TestCycleGuardAllocationCost pins the guard's allocation price, which is the
// half of the cost a benchmark's noise can hide.
//
// The claim in lisp/cycleexport.go is that a walk allocates ONE cycleState for
// the whole walk however wide or deep the value is, and reaches the path set
// only once it nests past the guard depth.  So the difference between the
// guarded and unguarded arms must be exactly one allocation on a value that
// stays below that depth — not one per node, and not one per level, which is
// the regression #391 had to back out of the kernel's walks.
func TestCycleGuardAllocationCost(t *testing.T) {
	const runs = 200
	for name, doc := range guardCostDocs() {
		t.Run(name, func(t *testing.T) {
			guarded := testing.AllocsPerRun(runs, func() {
				if err := okSimpleType(doc); err != nil {
					t.Fatal(err)
				}
			})
			unguarded := testing.AllocsPerRun(runs, func() {
				if err := okSimpleTypeUnguarded(doc); err != nil {
					t.Fatal(err)
				}
			})
			assert.Equal(t, 1.0, guarded-unguarded,
				"the gate's guard must cost exactly one allocation per walk "+
					"(the shared cycleState), got %v vs %v", guarded, unguarded)
		})
	}
}

// TestCycleGuardAllocationDoesNotScale is the property the per-walk number
// above is only interesting because of: doubling the width of the value must
// not double the guard's allocation.  A guard that built its path set in the
// by-value struct would fail this at every width.
func TestCycleGuardAllocationDoesNotScale(t *testing.T) {
	const runs = 100
	overhead := func(doc *lisp.LVal) float64 {
		guarded := testing.AllocsPerRun(runs, func() {
			if err := okSimpleType(doc); err != nil {
				t.Fatal(err)
			}
		})
		unguarded := testing.AllocsPerRun(runs, func() {
			if err := okSimpleTypeUnguarded(doc); err != nil {
				t.Fatal(err)
			}
		})
		return guarded - unguarded
	}
	wide := func(n int) *lisp.LVal {
		m := lisp.SortedMap()
		for i := range n {
			inner := lisp.SortedMap()
			inner.MapSet("v", lisp.Int(i))
			m.MapSet("k"+fmt.Sprint(i), inner)
		}
		return m
	}
	assert.Equal(t, overhead(wide(4)), overhead(wide(64)),
		"the guard's allocation must not grow with the width of the value")
}
