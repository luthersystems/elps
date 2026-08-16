// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// benchSpineDoc builds a document with a spine of depth nested maps, each
// level carrying a couple of scalar siblings and a small record array off the
// spine.  It is the shape a copy that rebuilds the spine one level at a time
// gets progressively worse on: every level used to deep-copy its whole
// subtree, including the next level down, so the deepest record was copied
// once per level above it and discarded all but once.
func benchSpineDoc(depth int) *lisp.LVal {
	rec := func(id int) *lisp.LVal {
		m := lisp.SortedMap()
		m.MapSet("id", lisp.Int(id))
		m.MapSet("name", lisp.String("name"))
		return m
	}
	v := lisp.String("leaf")
	for i := depth - 1; i >= 0; i-- {
		m := lisp.SortedMap()
		m.MapSet("child", v)
		m.MapSet("tag", lisp.String("level"+strconv.Itoa(i)))
		m.MapSet("count", lisp.Int(i))
		m.MapSet("items", lisp.Vector([]*lisp.LVal{rec(1), rec(2)}))
		v = m
	}
	return v
}

func spineSteps(depth int) []*lisp.LVal {
	steps := make([]*lisp.LVal, depth)
	for i := range steps {
		steps[i] = lisp.String("child")
	}
	return steps
}

// spineDepths sweeps the path depth, which is the variable the spine rebuild
// used to be quadratic in: at depth 1 there is one on-path child to skip and
// nothing to skip it inside, and the two curves separate from there.
var spineDepths = []int{1, 2, 4, 8}

// BenchmarkBuiltinSetCopyDeep is BenchmarkBuiltinSetCopy over a path deep
// enough for the spine rebuild to show.
func BenchmarkBuiltinSetCopyDeep(b *testing.B) {
	forEachSpineDepth(b, func(b *testing.B, d int) {
		benchSpine(b, BuiltinQuerySet, d, lisp.String("Paris"))
	})
}

// BenchmarkBuiltinNilCopyDeep is the same for the copying nil.
func BenchmarkBuiltinNilCopyDeep(b *testing.B) {
	forEachSpineDepth(b, func(b *testing.B, d int) { benchSpine(b, BuiltinQueryNil, d) })
}

// BenchmarkBuiltinDelCopyDeep is the same for the copying delete.
func BenchmarkBuiltinDelCopyDeep(b *testing.B) {
	forEachSpineDepth(b, func(b *testing.B, d int) { benchSpine(b, BuiltinQueryDelete, d) })
}

func forEachSpineDepth(b *testing.B, fn func(*testing.B, int)) {
	b.Helper()
	for _, d := range spineDepths {
		b.Run("depth"+strconv.Itoa(d), func(b *testing.B) { fn(b, d) })
	}
}

func benchSpine(b *testing.B, fn func(*lisp.LEnv, *lisp.LVal) *lisp.LVal, depth int, tail ...*lisp.LVal) {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	args := make([]*lisp.LVal, 0, depth+2)
	args = append(args, benchSpineDoc(depth))
	args = append(args, spineSteps(depth)...)
	args = append(args, tail...)
	call := lisp.QExpr(args)
	b.ReportAllocs()
	for b.Loop() {
		if v := fn(env, call); v.Type == lisp.LError {
			b.Fatal(v)
		}
	}
}
