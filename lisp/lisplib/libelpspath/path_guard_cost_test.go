// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"strconv"
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
					if err := OKSimpleType(doc); err != nil {
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
			assert.Equal(t, okSimpleTypeUnguarded(doc), OKSimpleType(doc),
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
// The claim in cycle.go is that a walk allocates ONE cycleState for the whole
// walk however wide or deep the value is, that the state is the caller's local
// so it never reaches the heap, and that the path set is touched only once the
// walk nests past the guard depth.  So the difference between the guarded and
// unguarded arms must be exactly ZERO allocations on a value that stays below
// that depth — not one per node, and not one per level, which is the
// regression #391 had to back out of the kernel's walks.
//
// The number used to be one.  While the guard was lisp.CycleGuard the shared
// state was allocated by the exported Descend, on the far side of a package
// boundary, where nothing could prove it did not outlive the walk; an exported
// guard also has to work from its zero value, so there was nowhere else to put
// it.  Unexporting the guard into this package let the walk's entry point own
// the state as a local (see newCycleGuard), and escape analysis keeps it on
// the stack.  wantGuardAllocs is spelled out below rather than folded into the
// assertion so that a future change that reintroduces a per-walk allocation
// has to say so here, in a diff a reviewer reads, rather than by relaxing a
// delta.
const wantGuardAllocs = 0.0

func TestCycleGuardAllocationCost(t *testing.T) {
	const runs = 200
	for name, doc := range guardCostDocs() {
		t.Run(name, func(t *testing.T) {
			guarded := testing.AllocsPerRun(runs, func() {
				if err := OKSimpleType(doc); err != nil {
					t.Fatal(err)
				}
			})
			unguarded := testing.AllocsPerRun(runs, func() {
				if err := okSimpleTypeUnguarded(doc); err != nil {
					t.Fatal(err)
				}
			})
			assert.InDelta(t, wantGuardAllocs, guarded-unguarded, 0.001,
				"the gate's guard must cost exactly %v allocations per walk, "+
					"got %v vs %v", wantGuardAllocs, guarded, unguarded)
		})
	}
}

// TestCycleGuardAllocationDoesNotScale is the property the per-walk number
// above is only interesting because of: doubling the width of the value must
// not double the guard's allocation.  A guard that built its path set in the
// by-value struct would fail this at every width.
//
// It is checked twice, because the two widths matter for different reasons.
// Shallow, the walk never reaches the path set at all and the property is that
// the per-walk cost of the guard itself does not multiply.  DEEP — past
// cycleGuardDepth, where the path set exists and every node really is entered
// on it — is where a per-node path set would actually be built, and is the
// arm that still has teeth now that the shallow overhead is zero and its
// equality is 0 == 0.
func TestCycleGuardAllocationDoesNotScale(t *testing.T) {
	const runs = 100
	overhead := func(doc *lisp.LVal) float64 {
		guarded := testing.AllocsPerRun(runs, func() {
			if err := OKSimpleType(doc); err != nil {
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
	wideMap := func(n int) *lisp.LVal {
		m := lisp.SortedMap()
		for i := range n {
			inner := lisp.SortedMap()
			inner.MapSet("v", lisp.Int(i))
			m.MapSet("k"+strconv.Itoa(i), inner)
		}
		return m
	}
	assert.InDelta(t, overhead(wideMap(4)), overhead(wideMap(64)), 0.001,
		"the guard's allocation must not grow with the width of the value")

	// deepWide puts n siblings at EXACTLY cycleGuardDepth, which is the one
	// place a per-node path set is built: a guard copy inherits a nil path
	// from its parent one level above the threshold and makes its own, while
	// every node below inherits a non-nil one and shares it.  So the spine
	// carries the wide map to depth cycleGuardDepth-1 and its children are
	// the nodes at the threshold.  Hanging the wide map deeper than that
	// measures nothing — one node allocates and the rest inherit — which is
	// how the first draft of this arm passed against a deliberately per-node
	// guard.
	deepWide := func(n int) *lisp.LVal {
		v := wideMap(n)
		for i := range cycleGuardDepth - 2 {
			m := lisp.SortedMap()
			m.MapSet("child", v)
			m.MapSet("tag", lisp.String("level"+strconv.Itoa(i)))
			v = m
		}
		return v
	}
	assert.InDelta(t, overhead(deepWide(4)), overhead(deepWide(64)), 0.001,
		"past the guard depth the path set must still be allocated once for "+
			"the whole walk, not once per node entered on it")
}
