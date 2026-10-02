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

// anySlice returns v as a []any, failing the test (with the formatted
// context) when it is anything else.
func anySlice(t testing.TB, v any, format string, args ...any) []any {
	t.Helper()
	s, ok := v.([]any)
	if !ok {
		t.Fatalf("%s: got %T, want []any", fmt.Sprintf(format, args...), v)
	}
	return s
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
		var viaSlice any = slice
		for i := range levels {
			s := anySlice(t, viaSlice, "GoSlice level %d", i)
			a, b := anySlice(t, s[0], "GoSlice level %d a", i), anySlice(t, s[1], "GoSlice level %d b", i)
			if &a[0] != &b[0] {
				t.Fatalf("GoSlice level %d: the conversion unshared the value", i)
			}
			viaSlice = s[0]
		}
		for i := range levels {
			s, ok := got.([]any)
			if !ok || len(s) != 2 {
				t.Fatalf("level %d: got %T", i, got)
			}
			a, b := anySlice(t, s[0], "level %d a", i), anySlice(t, s[1], "level %d b", i)
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
	a, b := anySlice(t, got[0], "a"), anySlice(t, got[1], "b")
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
		s := anySlice(t, c, "element %d", i)
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

// A memo hit fails the value depth limit exactly where re-converting would,
// so the height it records must be exact.  Each value is
// (filler first... (chain k again)): the filler pushes the walk past the
// budget, the containers after it are converted and memoised, and again is
// reached a second time at depth k+1.  A conversion walks a million levels,
// so each case runs only the two k either side of its boundary, and the
// oracle is the tree's: a value fails exactly when some path reaches
// lisp.MaxValueDepth.  The first case checks that oracle against the same
// value with a distinct copy in place of the second reference.
func TestSerializerGoValueMemoHitHonoursValueDepthLimit(t *testing.T) {
	if testing.Short() {
		t.Skip("converts million-level values")
	}
	const margin = 600
	height := lisp.MaxValueDepth - margin
	filler := func() *lisp.LVal {
		cells := make([]*lisp.LVal, sharedWalkBudget+10)
		for i := range cells {
			cells[i] = lisp.SExpr([]*lisp.LVal{lisp.Int(i)})
		}
		return lisp.SExpr(cells)
	}()
	s := DefaultSerializer()
	// convert reports whether (filler first... (chain k again)) fails,
	// and when it succeeds, requires again's conversion to be shared.
	convert := func(t *testing.T, k int, again *lisp.LVal, first ...*lisp.LVal) bool {
		t.Helper()
		cells := append([]*lisp.LVal{filler}, first...)
		cells = append(cells, depthChain(k, again, false))
		got := s.GoValue(lisp.SExpr(cells), false)
		if _, isErr := got.(error); isErr {
			return true
		}
		top := anySlice(t, got, "k=%d: top", k)
		second := top[len(top)-1]
		for j := range k {
			second = anySlice(t, second, "k=%d: chain level %d", k, j)[0]
		}
		for i, c := range first {
			if c == again {
				a, b := anySlice(t, top[1+i], "k=%d: first %d", k, i), anySlice(t, second, "k=%d: again", k)
				if &a[0] != &b[0] {
					t.Fatalf("k=%d: the shared container was converted per path", k)
				}
				return false
			}
		}
		return false
	}
	// check requires the value to fail exactly from k = boundary.
	check := func(t *testing.T, boundary int, again *lisp.LVal, first ...*lisp.LVal) {
		t.Helper()
		for k := boundary - 1; k <= boundary; k++ {
			if failed := convert(t, k, again, first...); failed != (k >= boundary) {
				t.Fatalf("k=%d: failed=%v, want failure from k=%d", k, failed, boundary)
			}
		}
	}
	// x's leaf is height levels below x: reached at depth k+1, the leaf is
	// at k+1+height, which fails once it reaches the limit.
	xBoundary := lisp.MaxValueDepth - 1 - height
	t.Run("tree oracle", func(t *testing.T) {
		x, other := depthChain(height, lisp.Int(7), false), depthChain(height, lisp.Int(7), false)
		check(t, xBoundary, other, x)
		check(t, xBoundary, x, x)
	})
	t.Run("quote wrappers", func(t *testing.T) {
		x := depthChain(height, lisp.Int(7), true)
		check(t, xBoundary, x, x)
	})
	t.Run("height through a hit", func(t *testing.T) {
		// p's only deep child, x, is a memo hit when p is first converted,
		// so p's recorded height comes from the hit: one more than x's.
		x := depthChain(height, lisp.Int(7), false)
		cells := []*lisp.LVal{x}
		for i := range sharedMemoGrain {
			cells = append(cells, lisp.Int(i))
		}
		p := lisp.SExpr(cells)
		check(t, xBoundary-1, p, x, p)
	})
	t.Run("height after a deeper sibling", func(t *testing.T) {
		// small is converted after the far deeper d: its recorded height
		// is its own, not d's.  Its boundary is far beyond any k run here,
		// so every conversion must succeed.
		d := depthChain(height, lisp.Int(7), false)
		small := depthChain(2*sharedMemoGrain, lisp.Int(7), false)
		for _, k := range []int{xBoundary} {
			if convert(t, k, small, d, small) {
				t.Fatalf("k=%d: a shallow shared container failed the depth limit", k)
			}
		}
	})
}
