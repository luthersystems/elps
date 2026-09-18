// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
)

// TestSelectRejectDoNotPresizeToInput pins that a filtering builtin no longer
// pays for the whole input before its predicate has kept anything.
//
// select/reject building a 'vector used to call MakeVector(list.Len()), so
// filtering a 100k-element vector down to nothing allocated the 100k-pointer
// backing anyway.  The measurement is differential: the same call building a
// 'list runs the identical predicate over the identical input and presizes
// nothing, so the difference between the two is the result backing alone and
// is not swamped by the evaluator's per-element call machinery (which is two
// orders of magnitude larger and is not what this test is about).
func TestSelectRejectDoNotPresizeToInput(t *testing.T) {
	const n = 100_000
	// One pointer per input element: what presizing to the input costs.
	const inputBacking = n * 8
	for _, tc := range []struct {
		name string
		call func(*LEnv, *LVal) *LVal
		keep func(int) bool
	}{
		{"select", builtinSelect, never},
		{"reject", builtinReject, always},
	} {
		t.Run(tc.name, func(t *testing.T) {
			measure := func(typespec string) int64 {
				t.Helper()
				env := NewEnv(nil)
				if rc := InitializeUserEnv(env); rc.Type == LError {
					t.Fatalf("initialize-user-env: %v", rc)
				}
				args := selectBenchInput(n, tc.keep)
				args.Cells[0] = Symbol(typespec)
				// Anti-vacuity: an early error would allocate nothing.
				got := tc.call(env, args)
				if got.Type == LError || got.Len() != 0 {
					t.Fatalf("%s %s did not filter to an empty result: %v", tc.name, typespec, got)
				}
				result := testing.Benchmark(func(b *testing.B) {
					for b.Loop() {
						tc.call(env, args)
					}
				})
				return result.AllocedBytesPerOp()
			}
			extra := measure("vector") - measure("list")
			if extra >= inputBacking/8 {
				t.Errorf("building the vector result cost %d bytes more than the list result; presizing to the %d-element input allocates about %d",
					extra, n, inputBacking)
			}
		})
	}
}

// TestSelectRejectResultsUnchanged pins the results themselves across the
// capacity change: both output types, and keeping all, none and some.
func TestSelectRejectResultsUnchanged(t *testing.T) {
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	for _, tc := range []struct {
		name          string
		call          func(*LEnv, *LVal) *LVal
		keep          func(int) bool
		list, vectorS string
	}{
		{"select/all", builtinSelect, always, "'(0 1 2 3 4)", "(vector 0 1 2 3 4)"},
		{"select/none", builtinSelect, never, "'()", "(vector)"},
		{"select/some", builtinSelect, even, "'(0 2 4)", "(vector 0 2 4)"},
		{"reject/all", builtinReject, never, "'(0 1 2 3 4)", "(vector 0 1 2 3 4)"},
		{"reject/none", builtinReject, always, "'()", "(vector)"},
		{"reject/some", builtinReject, even, "'(1 3)", "(vector 1 3)"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for _, typespec := range []string{"list", "vector"} {
				args := selectBenchInput(5, tc.keep)
				args.Cells[0] = Symbol(typespec)
				if typespec == "list" {
					// A list input exercises the same predicate over the
					// other input representation.
					args.Cells[2] = QExpr([]*LVal{Int(0), Int(1), Int(2), Int(3), Int(4)})
				}
				got := tc.call(env, args)
				if got.Type == LError {
					t.Fatalf("%s %s: %v", tc.name, typespec, got)
				}
				want := tc.list
				if typespec == "vector" {
					want = tc.vectorS
				}
				if got.String() != want {
					t.Errorf("%s %s = %s, want %s", tc.name, typespec, got, want)
				}
				// The result's dims are its own and describe its own length.
				if typespec == "vector" && got.Cells[0].Cells[0].Int != len(got.Cells[1].Cells) {
					t.Errorf("%s vector dims %v disagree with %d cells", tc.name, got.Cells[0], len(got.Cells[1].Cells))
				}
			}
		})
	}
}
