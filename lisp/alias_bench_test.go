// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// Benchmarks for the sequence producers and consumers touched by the issue
// #373 capacity-clamp fix.  They exist to price the fix honestly rather than
// to defend a number: clamping a capacity is free, but it removes an append's
// ability to grow into a source's spare capacity, so appends that used to be
// amortised now reallocate.  The chain benchmarks below are where that shows
// up, and they are expected to regress.
//
// Run both arms from the same file so the comparison is like-for-like:
//
//	go test -run '^$' -bench 'Alias' -benchmem -count=10 ./lisp/

// --- producers -------------------------------------------------------------

func BenchmarkAliasSliceVector(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1000) (append! v n))
	  (dotimes (n 1000) (slice 'vector v 10 900))
	`)
}

func BenchmarkAliasSliceList(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'l (make-sequence 0 1000))
	  (dotimes (n 1000) (slice 'list l 10 900))
	`)
}

// NOTE:  elpstest.RunBenchmark builds its env with InitializeUserEnv only --
// lisplib is NOT loaded -- so the stdlib `string:` package is unavailable
// here.  Byte sources are built with core builtins instead.  (An earlier
// draft of this file used string:repeat; the benchmarks failed at runtime and
// benchstat silently dropped them from the comparison.)
func BenchmarkAliasSliceBytes(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'bs (to-bytes ""))
	  (dotimes (n 1000) (append-bytes! bs "x"))
	  (dotimes (n 1000) (slice 'bytes bs 10 900))
	`)
}

func BenchmarkAliasSliceString(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 's (to-string (to-bytes "")))
	  (dotimes (n 200) (set 's (concat 'string s "xxxxx")))
	  (dotimes (n 1000) (slice 'string s 10 900))
	`)
}

func BenchmarkAliasCDR(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'l (make-sequence 0 1000))
	  (dotimes (n 1000) (cdr l))
	`)
}

func BenchmarkAliasRest(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1000) (append! v n))
	  (dotimes (n 1000) (rest v))
	`)
}

// --- consumers: single append off a fixed source ---------------------------

func BenchmarkAliasAppendVectorOnce(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 100) (append! v n))
	  (dotimes (n 1000) (append 'vector v 1))
	`)
}

func BenchmarkAliasAppendListOnce(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'l (make-sequence 0 100))
	  (dotimes (n 1000) (append 'list l 1))
	`)
}

func BenchmarkAliasAppendBytesOnce(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'bs (to-bytes ""))
	  (dotimes (n 100) (append-bytes! bs "x"))
	  (dotimes (n 1000) (append-bytes bs "y"))
	`)
}

// --- consumers: the accumulate-by-rebinding idiom --------------------------
//
// This is the pattern the fix makes more expensive.  Before the clamp these
// appends could grow into spare capacity and were amortised O(1); now each one
// copies.  It is also the pattern that was silently unsound whenever anyone
// held on to an earlier result.  append! is the supported accumulator and is
// benchmarked alongside for comparison.

func BenchmarkAliasAppendVectorChain(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 500) (set 'v (append 'vector v n)))
	`)
}

func BenchmarkAliasAppendBytesChain(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'bs (to-bytes ""))
	  (dotimes (n 500) (set 'bs (append-bytes bs "y")))
	`)
}

func BenchmarkAliasAppendListChain(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'l ())
	  (dotimes (n 500) (set 'l (append 'list l n)))
	`)
}

// --- the accumulation cost, measured at several sizes ----------------------
//
// The point of these is to show the SHAPE of the cost, not just its size at
// one n.  `append` must now copy its source, so accumulating with
// (set 'v (append 'vector v x)) does total work proportional to n^2, where
// before the clamp it could grow into spare capacity and was amortised O(n).
// Doubling n should therefore roughly double the time on main and roughly
// quadruple it on this branch.
//
// `append!` is the supported accumulator and is measured at the same sizes as
// the control: it is O(n) total in both arms.

func BenchmarkAliasAccumAppend200(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 200) (set 'v (append 'vector v n)))
	`)
}

func BenchmarkAliasAccumAppend400(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 400) (set 'v (append 'vector v n)))
	`)
}

func BenchmarkAliasAccumAppend800(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 800) (set 'v (append 'vector v n)))
	`)
}

func BenchmarkAliasAccumAppend1600(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1600) (set 'v (append 'vector v n)))
	`)
}

func BenchmarkAliasAccumMutate200(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 200) (append! v n))
	`)
}

func BenchmarkAliasAccumMutate400(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 400) (append! v n))
	`)
}

func BenchmarkAliasAccumMutate800(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 800) (append! v n))
	`)
}

func BenchmarkAliasAccumMutate1600(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1600) (append! v n))
	`)
}

// --- mutating accumulators (unchanged by the fix) --------------------------

func BenchmarkAliasAppendMutate(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1000) (append! v n))
	`)
}

func BenchmarkAliasAppendBytesMutate(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'bs (to-bytes ""))
	  (dotimes (n 1000) (append-bytes! bs "y"))
	`)
}

// --- slice-then-append, the shape from the bug report ----------------------

func BenchmarkAliasSliceThenAppendMutate(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 200) (append! v n))
	  (dotimes (n 500) (append! (slice 'vector v 0 100) 1))
	`)
}

// --- stable-sort (touched only in docs; benchmarked to prove that) ---------

func BenchmarkAliasStableSort(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'src (make-sequence 0 500))
	  (dotimes (n 20) (stable-sort < (concat 'list src)))
	`)
}

func BenchmarkAliasStableSortSliceView(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 500) (append! v n))
	  (dotimes (n 20) (stable-sort < (slice 'vector v 0 400)))
	`)
}

// --- concat, the documented copy idiom ------------------------------------

func BenchmarkAliasConcatCopy(b *testing.B) {
	elpstest.RunBenchmark(b, `
	  (set 'v (vector))
	  (dotimes (n 1000) (append! v n))
	  (dotimes (n 500) (concat 'vector (slice 'vector v 0 900)))
	`)
}
