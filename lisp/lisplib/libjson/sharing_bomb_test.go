// Copyright © 2026 The ELPS authors

package libjson

import (
	"fmt"
	"math/bits"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
)

// The deprecated Serializer conversions (GoValue, GoSlice, GoMap) follow
// lisp/sharing.go's rule, as lisp.GoValue does (#720): past
// sharedWalkBudget a shared container converts once, and the Go value
// shares the conversion where the LVal shared the container.

// sharingBomb returns (set! x (list x x)) depth times over leaf: depth
// containers, 2^depth paths.
func sharingBomb(depth int, leaf *lisp.LVal) *lisp.LVal {
	for range depth {
		leaf = lisp.SExpr([]*lisp.LVal{leaf, leaf})
	}
	return leaf
}

// mapBomb is sharingBomb built from sorted maps: {"a": x, "b": x}.
func mapBomb(depth int) *lisp.LVal {
	v := lisp.Int(1)
	for range depth {
		m := lisp.SortedMap()
		m.Map().Set(lisp.String("a"), v)
		m.Map().Set(lisp.String("b"), v)
		v = m
	}
	return v
}

func TestSerializerGoValueKeepsSharing(t *testing.T) {
	s := DefaultSerializer()
	// The bottom levels -- about log2(sharedWalkBudget) of them -- are
	// converted before the memo switches on and may be unshared.
	levels := 40 - bits.Len(sharedWalkBudget)
	t.Run("list", func(t *testing.T) {
		v := sharingBomb(40, lisp.Int(1))
		var got any
		var slice []any
		var ok bool
		testdeadline.Watch("Serializer.GoValue over a 40-level sharing bomb", 20*time.Second, 1<<30, func() {
			got = s.GoValue(v, false)
			slice, ok = s.GoSlice(v, true)
		})
		if !ok || len(slice) != 2 {
			t.Fatalf("GoSlice: %v, %d", ok, len(slice))
		}
		for i := range levels {
			s, ok := got.([]any)
			if !ok || len(s) != 2 {
				t.Fatalf("level %d: got %T", i, got)
			}
			a, b := s[0].([]any), s[1].([]any)
			if &a[0] != &b[0] {
				t.Fatalf("level %d: the conversion unshared the value", i)
			}
			got = s[0]
		}
	})
	t.Run("map", func(t *testing.T) {
		v := mapBomb(40)
		var got map[string]any
		var ok bool
		testdeadline.Watch("Serializer.GoMap over a 40-level sharing bomb", 20*time.Second, 1<<30, func() {
			got, ok = s.GoMap(v, false)
		})
		if !ok {
			t.Fatal("GoMap failed")
		}
		for i := range levels {
			a, aok := got["a"].(map[string]any)
			b, bok := got["b"].(map[string]any)
			if !aok || !bok || len(a) != 2 {
				t.Fatalf("level %d: got %T, %T", i, got["a"], got["b"])
			}
			// Maps are reference values: one conversion reached twice
			// is the same map.
			a["probe"] = true
			_, shared := b["probe"]
			delete(a, "probe")
			if !shared {
				t.Fatalf("level %d: the conversion unshared the value", i)
			}
			got = a
		}
	})
}

// A small value that shares a container stays under the budget, so it
// converts exactly as it always did: one Go container per path.
func TestSerializerGoValueSmallSharingUnchanged(t *testing.T) {
	x := lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	got, ok := DefaultSerializer().GoValue(lisp.SExpr([]*lisp.LVal{x, x}), false).([]any)
	if !ok || len(got) != 2 {
		t.Fatalf("got %T", got)
	}
	a, b := got[0].([]any), got[1].([]any)
	if &a[0] == &b[0] {
		t.Fatal("a value under the budget converted with sharing")
	}
}

// A tree larger than the budget converts exactly as before: every Go
// container distinct.
func TestSerializerGoValueLargeTreeUnshared(t *testing.T) {
	cells := make([]*lisp.LVal, 3*sharedWalkBudget)
	for i := range cells {
		cells[i] = lisp.SExpr([]*lisp.LVal{lisp.Int(i)})
	}
	got, ok := DefaultSerializer().GoValue(lisp.SExpr(cells), false).([]any)
	if !ok || len(got) != len(cells) {
		t.Fatalf("got %T", got)
	}
	seen := map[*any]bool{}
	for i, c := range got {
		s := c.([]any)
		if seen[&s[0]] || s[0] != i {
			t.Fatalf("element %d shared or wrong: %v", i, s)
		}
		seen[&s[0]] = true
	}
}

// depthChain returns n nested one-cell lists around leaf, allocated in two
// slabs; with quoted, every third level is an LQuote wrapper instead.
func depthChain(n int, leaf *lisp.LVal, quoted bool) *lisp.LVal {
	nodes := make([]lisp.LVal, n)
	cells := make([]*lisp.LVal, n)
	for i := n - 1; i >= 0; i-- {
		cells[i] = leaf
		nodes[i].Type = lisp.LSExpr
		if quoted && i%3 == 1 {
			nodes[i].Type = lisp.LQuote
		}
		nodes[i].Cells = cells[i : i+1 : i+1]
		leaf = &nodes[i]
	}
	return leaf
}

// A memo hit fails the value depth limit exactly where re-converting would.
// The value is (filler x (chain k x)): the filler pushes the walk past the
// budget, x -- nearly lisp.MaxValueDepth deep -- is converted at depth 1,
// and reached again at depth k+1.  The oracle is the same value with a
// distinct second x, a tree, which fails exactly when k+1+height(x)
// reaches the limit.  Each conversion walks a million levels, so only the
// two k either side of the limit are run.
func TestSerializerGoValueMemoHitHonoursValueDepthLimit(t *testing.T) {
	if testing.Short() {
		t.Skip("converts million-level values")
	}
	const margin = 600
	height := lisp.MaxValueDepth - margin
	boundary := lisp.MaxValueDepth - 1 - height // first failing k
	filler := func() *lisp.LVal {
		cells := make([]*lisp.LVal, sharedWalkBudget+10)
		for i := range cells {
			cells[i] = lisp.SExpr([]*lisp.LVal{lisp.Int(i)})
		}
		return lisp.SExpr(cells)
	}()
	s := DefaultSerializer()
	for _, quoted := range []bool{false, true} {
		t.Run(fmt.Sprintf("quoted=%v", quoted), func(t *testing.T) {
			x := depthChain(height, lisp.Int(7), quoted)
			other := depthChain(height, lisp.Int(7), quoted)
			for k := boundary - 1; k <= boundary; k++ {
				fails := func(second *lisp.LVal) (bool, any) {
					got := s.GoValue(lisp.SExpr([]*lisp.LVal{filler, x, depthChain(k, second, false)}), false)
					_, isErr := got.(error)
					return isErr, got
				}
				treeErr, _ := fails(other)
				dagErr, dag := fails(x)
				if treeErr != (k >= boundary) {
					t.Fatalf("k=%d: tree failed=%v, want failure from k=%d", k, treeErr, boundary)
				}
				if dagErr != treeErr {
					t.Fatalf("k=%d: tree failed=%v, shared failed=%v", k, treeErr, dagErr)
				}
				if dagErr {
					continue
				}
				// The shared x converted once: its conversion appears in
				// both places.
				top := dag.([]any)
				second := top[2]
				for range k {
					second = second.([]any)[0]
				}
				first, again := top[1].([]any), second.([]any)
				if &first[0] != &again[0] {
					t.Fatalf("k=%d: the shared chain was converted per path", k)
				}
			}
		})
	}
}
