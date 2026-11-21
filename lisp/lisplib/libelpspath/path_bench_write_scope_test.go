// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkSetMutateScaling measures one (elpspath:?set! doc i "a" 1) on a
// document of n records, at n and 10n (luthersystems/elps#1005).
//
// A mutating write changes one container on its path, so its cost should
// not depend on n. Before #1005 every mutating write first validated the
// whole document, so the 10n arm cost ten times the n arm, and a program
// that filled a document with n writes paid for n whole-document walks:
// quadratic in n. Compare the two arms: they should be about the same.
func BenchmarkSetMutateScaling(b *testing.B) {
	for _, n := range []int{1000, 10000} {
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			env := lisp.NewEnv(nil)
			doc := validateBenchRecords(n)
			call := lisp.QExpr([]*lisp.LVal{doc, lisp.Int(n / 2), lisp.String("a"), lisp.Int(1)})
			b.ReportAllocs()
			for b.Loop() {
				if v := BuiltinQuerySetMutate(env, call); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}

// BenchmarkFillByMutatingWrites is the shape of the substrate report behind
// #1005: n mutating writes that build an n-entry map one key at a time.
// The allocations of the 10n arm are ten times the n arm's; before the fix
// its time was more than a hundred times (4.4 s for 5000 writes).  The sizes
// stay small because the benchmark gate also runs the base branch, and on a
// base with the quadratic gate a larger arm costs seconds per round.
func BenchmarkFillByMutatingWrites(b *testing.B) {
	for _, n := range []int{200, 2000} {
		keys := make([]*lisp.LVal, n)
		for i := range keys {
			keys[i] = lisp.String("k" + strconv.Itoa(i))
		}
		b.Run(strconv.Itoa(n), func(b *testing.B) {
			env := lisp.NewEnv(nil)
			b.ReportAllocs()
			for b.Loop() {
				doc := lisp.SortedMap()
				for _, k := range keys {
					rec := lisp.SortedMap()
					rec.MapSetString("v", lisp.Int(1))
					call := lisp.QExpr([]*lisp.LVal{doc, k, rec})
					if v := BuiltinQuerySetMutate(env, call); v.Type == lisp.LError {
						b.Fatal(v)
					}
				}
			}
		})
	}
}
