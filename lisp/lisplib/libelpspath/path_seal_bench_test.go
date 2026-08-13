// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Benchmarks for the propagate-vs-copy decision on rangePath.Get (issue
// #392).
//
// Fixing the seal leak has two correct answers. Propagating the source's
// flag onto the fresh header keeps ? an O(1) view and is what the kernel
// does at the same kind of site (builtinSlice, builtinCdr, builtinRest).
// Returning a copy is unconditionally safe but turns every sealed read into
// an allocation proportional to the window -- on a path substrate runs per
// transaction.
//
// These benchmarks are the evidence for that choice. They are measured by
// compiling this package's test binary from BOTH implementations and
// interleaving the two binaries' runs on one machine, because a fixed-order
// A-then-B run on a shared container measures the container, not the code.
//
// The three cases separate the effects that matter:
//
//	SealedLiteral -- a window out of a sealed program literal. The only
//	  case where the two implementations differ at all.
//	RuntimeList   -- the same query over unsealed runtime data. Neither
//	  implementation may touch this: it is the no-regression control, and
//	  it is the majority of real traffic.
//	SealedLiteralWide -- a wider window, to show the copy arm's cost is
//	  proportional to the range rather than a constant.
func benchSealedLiteral(b *testing.B, n int) *lisp.LVal {
	b.Helper()
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	v := lisp.QExpr(cells)
	// Seal it exactly as the parser seals a quoted literal. SealAST is the
	// same call rdparser.ParseExpression makes at nesting depth zero.
	v.SealAST()
	if !v.IsSealed() {
		b.Fatal("anti-vacuity: the benchmark source is not sealed")
	}
	return v
}

func benchRuntimeList(b *testing.B, n int) *lisp.LVal {
	b.Helper()
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	v := lisp.QExpr(cells)
	if v.IsSealed() {
		b.Fatal("anti-vacuity: the runtime control is sealed")
	}
	return v
}

func benchGet(b *testing.B, src *lisp.LVal, path Path) {
	b.Helper()
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		out, err := path.Get(src)
		if err != nil {
			b.Fatal(err)
		}
		if out == nil {
			b.Fatal("nil result")
		}
	}
}

func BenchmarkRangeGetSealedLiteral(b *testing.B) {
	benchGet(b, benchSealedLiteral(b, 16), Root(Chain(Range(0, 4, false))))
}

func BenchmarkRangeGetSealedLiteralWide(b *testing.B) {
	benchGet(b, benchSealedLiteral(b, 256), Root(Chain(Range(0, 192, false))))
}

func BenchmarkRangeGetRuntimeList(b *testing.B) {
	benchGet(b, benchRuntimeList(b, 16), Root(Chain(Range(0, 4, false))))
}
