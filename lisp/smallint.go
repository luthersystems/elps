// Copyright © 2026 The ELPS authors

package lisp

import "fmt"

// Shared small integers.
//
// About one allocation in ten was an integer, and about nine in ten of those
// held a value in [-16, 1024): loop counters, indices, lengths, small sums
// (luthersystems/elps#944).  SharedInt returns those from a table built
// once, the way Nil and Bool return their singletons, so they cost no
// allocation.  Int still allocates, as Float does: a caller that did not ask
// for a shared value never gets one.
//
// A table integer is shared by every Runtime in the process, so it is
// IMMUTABLE, like the three singletons (lisp/singleton.go):
//
//   - It is constructed sealed.  SetSource and sealAST are no-ops on a
//     sealed value, so the two writes a parser or a loader would make do
//     not land.  Code that needs an integer of its own, to give it a
//     source location or to write its Int field, calls Int.
//   - cmd/elpsvet's freshness rule does not treat SharedInt as a constructor of
//     a value the caller owns, so a field write on its result is reported
//     in this module and in any module that runs elpsvet.
//   - In elpscheck builds SharedInt verifies the entry it hands out, and
//     SingletonSnapshot.Verify checks the whole table.
//
// The table holds pointers to separate small allocations, not one array of
// LVal structs.  One array is a single large heap object, and the garbage
// collector's write barrier finds the object for each pointer into a large
// object on a slower path than for a pointer into a span of small objects:
// a list of table integers copied while the collector ran took about 12%
// longer to write (BenchmarkDeleteCompaction, n=64).  SharedInt is an index and
// one load.
//
// WHAT SHARING DOES NOT CHANGE.  A sealed integer was already an ordinary
// runtime value: a literal evaluates to its own sealed parser node.  So the
// kernel already treats one as immutable wherever a value can go, and a
// template already shares one with every VM.  Nothing in the language
// compares numbers by identity, and no walker keys a leaf by its pointer in
// a way a program can see: the durable JSON encoder gives an integer no
// identity (objectKey), so two cells holding one table entry are written as
// two numbers, and Copy and detach build a fresh integer for each.  The
// macro stamp is the one place that must tell a table integer from a sealed
// parser node: see sealedSyntax (lisp/macro.go).
const (
	smallIntMin = -16
	smallIntMax = 1024 // exclusive
)

//elpsvet:allow guarded shared table, the integer counterpart of the Nil and Bool singletons: sealed at birth, immutable by decree, verified in elpscheck builds
var smallInts = newSmallInts()

func newSmallInts() *[smallIntMax - smallIntMin]*LVal {
	var t [smallIntMax - smallIntMin]*LVal
	for i := range t {
		t[i] = &LVal{Type: LInt, Int: i + smallIntMin, sealed: true}
	}
	return &t
}

// isSmallInt reports whether v is an entry of the shared table.  It is
// identity-based: a fresh integer with a small value is not one.
func isSmallInt(v *LVal) bool {
	if v == nil || v.Type != LInt {
		return false
	}
	i := v.Int - smallIntMin
	return uint(i) < uint(len(smallInts)) && v == smallInts[i]
}

// smallIntDrift reports whether v, entry i of a table newSmallInts built, no
// longer holds the value it was built with.
func smallIntDrift(v *LVal, i int) bool {
	want := LVal{Type: LInt, Int: i + smallIntMin, sealed: true}
	return !singletonLValEqual(v, &want)
}

// verifySmallInts returns a description of the first entry of the table t
// that no longer holds the value it was built with, or the empty string.
func verifySmallInts(t *[smallIntMax - smallIntMin]*LVal) string {
	for i := range t {
		if smallIntDrift(t[i], i) {
			return fmt.Sprintf("SharedInt(%d)", i+smallIntMin)
		}
	}
	return ""
}

// Int returns a new LVal representing the number x.  It always allocates,
// as Float does, and the caller owns the result.  SharedInt is the
// variant that does not allocate for a small x.
func Int(x int) *LVal {
	return &LVal{
		Type: LInt,
		Int:  x,
	}
}
