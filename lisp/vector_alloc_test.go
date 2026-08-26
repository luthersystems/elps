// Copyright © 2018 The ELPS authors

// The allocation assertions for the vector-constructing builtins live behind
// !race because the race detector allocates on its own account, and behind
// !elpscheck because the checked build adds ownership bookkeeping to every
// eval (lisp/ownership_check_elpscheck.go), which moves the insert-sorted
// count.  Pinning either configuration would pin the instrumentation rather
// than the builtins.  Every other build runs them, including the plain
// `go test ./...` the CI gate uses.

//go:build !race && !elpscheck

package lisp

import "testing"

// TestVectorBuiltinAllocations pins what each vector-constructing builtin
// allocates.
//
// These builtins used to build their own one-element dims list and pass it to
// Array as CALLER-supplied dims, which costs the validation loop and, more
// expensively, the deferred dims.Copy() Array must perform on a list it does
// not own (issue #544 and TestArrayDoesNotAliasCallerDims explain why that
// copy is not optional for caller dims).  They now let Array derive the
// identical dims for itself through MakeVector/Vector, which is copy-free.
//
// The counts are equalities, not bounds, and they are the point of the change:
// each vector constructed is exactly 2 allocations cheaper than it was.  The
// "was" column was measured on origin/main at 63fc6b2:
//
//	map 'vector            41 -> 39
//	select 'vector         41 -> 39
//	reject 'vector         41 -> 39
//	reverse 'vector         9 -> 7
//	concat 'vector          9 -> 7
//	concat 'vector (empty)  8 -> 6
//	zip 'vector            81 -> 63
//	slice 'vector           9 -> 7
//	insert-index 'vector    9 -> 7
//	insert-sorted 'vector  33 -> 31
//
// zip moves by 18 rather than 2 because it builds nine vectors for this input
// -- one per element plus the outer one -- which is what makes it the row that
// shows the saving is per-vector and not per-call.
//
// The fixtures are built once, outside the measured closure, so what is
// counted is the builtin's own work.  Every builtin here is deterministic, so
// AllocsPerRun's mean over its runs is the count itself.
func TestVectorBuiltinAllocations(t *testing.T) {
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	// Go-implemented predicates: a lisp lambda would put the evaluator's own
	// allocations, which this change does not touch, into every count.
	even := FunInPackage(DefaultUserPackage, "even?", Formals("x"), func(env *LEnv, args *LVal) *LVal {
		return Bool(args.Cells[0].Int%2 == 0)
	})
	less := FunInPackage(DefaultUserPackage, "less?", Formals("a", "b"), func(env *LEnv, args *LVal) *LVal {
		return Bool(args.Cells[0].Int < args.Cells[1].Int)
	})
	vec := MakeVector(8)
	for i := range vec.Cells[1].Cells {
		vec.Cells[1].Cells[i] = Int(i)
	}
	vector := Symbol("vector")

	mapArgs := QExpr([]*LVal{vector, even, vec})
	predArgs := QExpr([]*LVal{vector, even, vec})
	seqArgs := QExpr([]*LVal{vector, vec})
	concatArgs := QExpr([]*LVal{vector, vec, vec})
	concatEmptyArgs := QExpr([]*LVal{vector})
	zipArgs := QExpr([]*LVal{vector, vec, vec})
	sliceArgs := QExpr([]*LVal{vector, vec, Int(1), Int(4)})
	insertArgs := QExpr([]*LVal{vector, vec, Int(2), Int(99)})
	insertSortedArgs := QExpr([]*LVal{vector, vec, less, Int(3)})

	tests := []struct {
		name string
		call func() *LVal
		want int
	}{
		{"map", func() *LVal { return builtinMap(env, mapArgs) }, 39},
		{"select", func() *LVal { return builtinSelect(env, predArgs) }, 39},
		{"reject", func() *LVal { return builtinReject(env, predArgs) }, 39},
		{"reverse", func() *LVal { return builtinReverse(env, seqArgs) }, 7},
		{"concat", func() *LVal { return builtinConcatSeq(env, concatArgs) }, 7},
		{"concat-empty", func() *LVal { return builtinConcatSeq(env, concatEmptyArgs) }, 6},
		{"zip", func() *LVal { return builtinZip(env, zipArgs) }, 63},
		{"slice", func() *LVal { return builtinSlice(env, sliceArgs) }, 7},
		{"insert-index", func() *LVal { return builtinInsertIndex(env, insertArgs) }, 7},
		{"insert-sorted", func() *LVal { return builtinInsertSorted(env, insertSortedArgs) }, 31},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Anti-vacuity: a builtin that errored out early would allocate
			// less and pass a bound, so assert it produced a vector first.
			got := test.call()
			if got.Type != LArray {
				t.Fatalf("%s did not build a vector: %v", test.name, got)
			}
			n := testing.AllocsPerRun(200, func() { test.call() })
			if int(n) != test.want {
				t.Errorf("%s allocated %v times per call, want %d", test.name, n, test.want)
			}
		})
	}
}
