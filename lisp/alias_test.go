// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestSliceCapacityAliasing pins the fix for issue #373 and the corruption it
// caused (issue #369).
//
// Before the fix every sequence producer that carved a sub-slice out of a
// source handed back a two-index reslice -- `cells[i:j]` -- which keeps the
// SOURCE's capacity.  A later `append` then had spare room to grow into and
// wrote through the view into memory the caller never named:
//
//	(set 'v (vector 10 20 30 40))
//	(set 'view (slice 'vector v 0 2))
//	(append! view 999)
//	v  ; => (vector 10 20 999 40)   <-- v[2] silently changed
//
// The write lands OUTSIDE the view the caller was handed, so no reading of
// `slice`'s contract justifies it.  Worse, the source is frequently a quoted
// literal from the program text itself, in which case the corruption is to the
// program and persists for the lifetime of the process.
//
// The fix is capacity discipline: every escaping view is produced with a
// three-index reslice (`cells[i:j:j]`) so its capacity stops at its length,
// and every non-mutating append clamps its INPUT the same way so it can never
// write into its source's spare capacity.  Any append that needs more room
// must reallocate.
//
// These tests are deliberately self-contained -- they assert on observable
// lisp values only, with no Go-level capacity probing -- so that they keep
// their meaning if the internals are reorganised.
func TestSliceCapacityAliasing(t *testing.T) {
	tests := elpstest.TestSuite{
		// Variant 1: the report's original reproduction.  A vector view
		// grown with the MUTATING append wrote past its own end into the
		// source.
		{"slice view + append! does not write into the source vector", elpstest.TestSequence{
			{`(set 'v (vector 10 20 30 40))`, `(vector 10 20 30 40)`, ``},
			{`(set 'view (slice 'vector v 0 2))`, `(vector 10 20)`, ``},
			{`(append! view 999)`, `(vector 10 20 999)`, ``},
			// v[2] must still be 30.  Before the fix this read
			// (vector 10 20 999 40).
			{`v`, `(vector 10 20 30 40)`, ``},
		}},

		// Variant 2: the same defect reached through the NON-mutating
		// append, with a quoted literal as the victim.  `append` promises
		// not to mutate the original; before the fix it rewrote an element
		// of the program's own source text.
		{"append 'vector on a slice view does not corrupt a quoted literal", elpstest.TestSequence{
			{`(set 'lit '(1 2 3))`, `'(1 2 3)`, ``},
			// slice 'vector would wrap the literal's backing in a mutable
			// window; the guard refuses it outright with the catchable
			// modify-literal-error condition (issue #378; the site used to
			// copy-on-write silently).
			{`(slice 'vector lit 0 1)`,
				`test:1:1: modify-literal-error: cannot modify a program literal; take a (copy ...) first`, ``},
			// The sanctioned route: copy first, then slice and append freely.
			{`(set 'view (slice 'vector (copy lit) 0 1))`, `(vector 1)`, ``},
			{`(append 'vector view 99)`, `(vector 1 99)`, ``},
			// Before the guards lit read '(1 99 3) -- and stayed that way
			// for the rest of the process, because `lit` is the literal node
			// in the function body, not a copy of it.
			{`lit`, `'(1 2 3)`, ``},
		}},

		// The literal corruption in variant 2 is persistent, not per-call.
		// Calling the same function twice must see a pristine literal both
		// times, with the corrupting write refused on each call.
		{"a corrupted literal does not persist across calls", elpstest.TestSequence{
			{`(defun probe ()
			    (let ([lit '(1 2 3)])
			      (ignore-errors (append 'vector (slice 'list lit 0 1) 99))
			      lit))`, `()`, ``},
			{`(probe)`, `'(1 2 3)`, ``},
			// Before the guards the second call returned '(1 99 3).
			{`(probe)`, `'(1 2 3)`, ``},
		}},

		// Variant 3: the bytes flavour.  slice 'bytes handed out a
		// capacity-retaining []byte view and append-bytes grew into it,
		// overwriting index 2 of the source.
		{"slice 'bytes view + append-bytes does not write into the source", elpstest.TestSequence{
			{`(set 'src (to-bytes "ABCD"))`, `#<bytes 65 66 67 68>`, ``},
			{`(set 'view (slice 'bytes src 0 2))`, `#<bytes 65 66>`, ``},
			{`(append-bytes view (to-bytes "Z"))`, `#<bytes 65 66 90>`, ``},
			// src[2] must still be 'C' (67).  Before the fix it was 'Z' (90).
			{`src`, `#<bytes 65 66 67 68>`, ``},
			{`(to-string src)`, `"ABCD"`, ``},
		}},

		{"slice 'bytes view + append 'bytes does not write into the source", elpstest.TestSequence{
			{`(set 'src (to-bytes "ABCD"))`, `#<bytes 65 66 67 68>`, ``},
			{`(set 'view (slice 'bytes src 0 2))`, `#<bytes 65 66>`, ``},
			{`(append 'bytes view 90)`, `#<bytes 65 66 90>`, ``},
			{`(to-string src)`, `"ABCD"`, ``},
		}},

		{"slice 'bytes view + append! does not write into the source", elpstest.TestSequence{
			{`(set 'src (to-bytes "ABCD"))`, `#<bytes 65 66 67 68>`, ``},
			{`(set 'view (slice 'bytes src 0 2))`, `#<bytes 65 66>`, ``},
			{`(append! view 90)`, `#<bytes 65 66 90>`, ``},
			{`(to-string src)`, `"ABCD"`, ``},
		}},

		{"slice 'bytes view + append-bytes! does not write into the source", elpstest.TestSequence{
			{`(set 'src (to-bytes "ABCD"))`, `#<bytes 65 66 67 68>`, ``},
			{`(set 'view (slice 'bytes src 0 2))`, `#<bytes 65 66>`, ``},
			{`(append-bytes! view "Z")`, `#<bytes 65 66 90>`, ``},
			{`(to-string src)`, `"ABCD"`, ``},
		}},

		// Two independent non-mutating appends off the same source must not
		// see each other.  This row happens to pass before the fix too --
		// a freshly built (vector 1 2 3) has no spare capacity to fight
		// over -- so it is a guard rather than a reproduction.  The two
		// rows below it are the reproductions: they arrange for the source
		// to carry spare capacity first.
		{"two appends off one vector are independent", elpstest.TestSequence{
			{`(set 'a (vector 1 2 3))`, `(vector 1 2 3)`, ``},
			{`(set 'x (append 'vector a 100))`, `(vector 1 2 3 100)`, ``},
			{`(set 'y (append 'vector a 200))`, `(vector 1 2 3 200)`, ``},
			{`x`, `(vector 1 2 3 100)`, ``},
			{`y`, `(vector 1 2 3 200)`, ``},
			{`a`, `(vector 1 2 3)`, ``},
		}},

		// The same, but where the spare capacity was created by append!
		// rather than by a previous append.  append! is documented to
		// mutate in place and keeps its amortised growth, so its target
		// legitimately carries spare capacity -- the non-mutating append
		// must refuse to use it.
		{"append after append! does not reuse the mutated vector's spare capacity", elpstest.TestSequence{
			{`(set 'a (vector 1 2 3))`, `(vector 1 2 3)`, ``},
			{`(append! a 4)`, `(vector 1 2 3 4)`, ``},
			{`(set 'x (append 'vector a 100))`, `(vector 1 2 3 4 100)`, ``},
			{`(set 'y (append 'vector a 200))`, `(vector 1 2 3 4 200)`, ``},
			{`x`, `(vector 1 2 3 4 100)`, ``},
			{`y`, `(vector 1 2 3 4 200)`, ``},
			{`a`, `(vector 1 2 3 4)`, ``},
		}},

		{"append-bytes after append-bytes! does not reuse spare capacity", elpstest.TestSequence{
			{`(set 'b (to-bytes "ab"))`, `#<bytes 97 98>`, ``},
			{`(append-bytes! b "c")`, `#<bytes 97 98 99>`, ``},
			{`(set 'x (append-bytes b "d"))`, `#<bytes 97 98 99 100>`, ``},
			{`(set 'y (append-bytes b "e"))`, `#<bytes 97 98 99 101>`, ``},
			{`(to-string x)`, `"abcd"`, ``},
			{`(to-string y)`, `"abce"`, ``},
			{`(to-string b)`, `"abc"`, ``},
		}},

		// cdr and rest are the other two producers that carved a view with
		// `cells[1:]`.  Their views must not be able to grow into whatever
		// spare capacity the source happened to carry.
		{"cdr view cannot grow into the source's spare capacity", elpstest.TestSequence{
			{`(set 'base (vector 1 2 3))`, `(vector 1 2 3)`, ``},
			{`(append! base 4)`, `(vector 1 2 3 4)`, ``},
			{`(set 'keep (append 'vector base 9))`, `(vector 1 2 3 4 9)`, ``},
			{`(set 'tl (rest base))`, `'(2 3 4)`, ``},
			{`(append 'vector tl 77)`, `(vector 2 3 4 77)`, ``},
			// keep must be untouched.
			{`keep`, `(vector 1 2 3 4 9)`, ``},
			{`base`, `(vector 1 2 3 4)`, ``},
		}},

		{"cdr view of a list cannot grow into the source's spare capacity", elpstest.TestSequence{
			{`(set 'base (vector 1 2 3))`, `(vector 1 2 3)`, ``},
			{`(append! base 4)`, `(vector 1 2 3 4)`, ``},
			{`(set 'keep (append 'vector base 9))`, `(vector 1 2 3 4 9)`, ``},
			{`(set 'tl (cdr (slice 'list base 0 4)))`, `'(2 3 4)`, ``},
			{`(append 'vector tl 77)`, `(vector 2 3 4 77)`, ``},
			{`keep`, `(vector 1 2 3 4 9)`, ``},
		}},

		// slice 'list off a vector shares the vector's cells; growing the
		// list view must not reach back into the vector.
		{"slice 'list view cannot grow into the source vector", elpstest.TestSequence{
			{`(set 'v (vector 10 20 30 40))`, `(vector 10 20 30 40)`, ``},
			{`(set 'l (slice 'list v 0 2))`, `'(10 20)`, ``},
			{`(append 'vector l 999)`, `(vector 10 20 999)`, ``},
			{`v`, `(vector 10 20 30 40)`, ``},
		}},

		// The concat-as-copy idiom documented in docs/func.md: concat
		// always allocates, so it is the way to take a snapshot that no
		// later append can disturb.
		{"concat is a copy that later appends cannot disturb", elpstest.TestSequence{
			{`(set 'v (vector 10 20 30 40))`, `(vector 10 20 30 40)`, ``},
			{`(set 'copy (concat 'vector (slice 'vector v 0 2)))`, `(vector 10 20)`, ``},
			{`(append! copy 999)`, `(vector 10 20 999)`, ``},
			{`v`, `(vector 10 20 30 40)`, ``},
			{`copy`, `(vector 10 20 999)`, ``},
		}},

		// Ordinary behaviour must be unchanged: append! still accumulates
		// in place and slice still reads the right elements.
		{"append! still accumulates in place", elpstest.TestSequence{
			{`(set 'v (vector))`, `(vector)`, ``},
			{`(dotimes (n 5) (append! v n))`, `()`, ``},
			{`v`, `(vector 0 1 2 3 4)`, ``},
			{`(length v)`, `5`, ``},
		}},

		{"slice still reads the right elements", elpstest.TestSequence{
			{`(slice 'vector (vector 1 2 3 4 5) 1 4)`, `(vector 2 3 4)`, ``},
			{`(slice 'list (vector 1 2 3 4 5) 1 4)`, `'(2 3 4)`, ``},
			{`(slice 'list '(1 2 3 4 5) 0 5)`, `'(1 2 3 4 5)`, ``},
			{`(slice 'string "abcdef" 1 3)`, `"bc"`, ``},
			{`(to-string (slice 'bytes (to-bytes "abcdef") 1 3))`, `"bc"`, ``},
			{`(slice 'vector (vector 1 2 3) 2 2)`, `(vector)`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestSliceViewSharesElements documents the aliasing that this change does
// NOT remove, so that the boundary of the fix is explicit rather than
// discovered later by a user.
//
// Issue #373 is about writes that land OUTSIDE the view the caller was handed
// -- spare capacity.  A view still SHARES the elements inside its own bounds
// with its source, exactly as a Go slice does, so an in-place mutation of the
// view is still visible through the source.  `stable-sort` is documented to
// sort in place and returns the mutated sequence, so sorting a view sorts that
// region of the source.
//
// Closing that would require `slice` to copy (or quoted literals to be copied
// on evaluation), which is #373 called "a separate semantic decision with its
// own cost, and deliberately not made here", with the note that "these rows
// pin the current behaviour so the decision is a visible test change whenever
// someone does make it."
//
// HALF OF IT HAS NOW BEEN MADE, and this is that visible test change.
// Sealing (lisp/seal.go) draws the line at PROVENANCE rather than at the
// operation: a quoted program literal is sealed at parse time and every
// in-place mutation site copies before writing, because a literal is shared by
// every environment evaluating the same parse and rewriting it corrupts the
// program process-wide (elps#369, luthersystems/substrate#378).  A view over
// RUNTIME storage is not sealed, is owned by its caller, and keeps the
// documented Go-slice sharing unchanged -- the first case below.
//
// So the two cases have diverged on purpose, and the pair is worth keeping for
// exactly that reason: it is where the difference between "a view shares its
// source" (still true) and "a mutation rewrites the program text" (no longer
// true) is stated side by side.
func TestSliceViewSharesElements(t *testing.T) {
	tests := elpstest.TestSuite{
		{"stable-sort through a view still reorders the source", elpstest.TestSequence{
			{`(set 'src (vector 5 4 3 2 1))`, `(vector 5 4 3 2 1)`, ``},
			{`(set 'view (slice 'vector src 0 3))`, `(vector 5 4 3)`, ``},
			{`(stable-sort < view)`, `(vector 3 4 5)`, ``},
			// Unchanged by sealing: src is runtime storage the caller owns,
			// and the first three elements are the same cells the view
			// sorted.
			{`src`, `(vector 3 4 5 2 1)`, ``},
		}},
		{"stable-sort does not mutate a quoted literal", elpstest.TestSequence{
			// Sorting a literal is refused with the catchable
			// modify-literal-error condition (issue #378; the site used to
			// copy-on-write silently), so the literal in the function body
			// reads as written...
			{`(defun probe () (let ([lit '(3 1 2)]) (ignore-errors (stable-sort < lit)) lit))`, `()`, ``},
			{`(probe)`, `'(3 1 2)`, ``},
			// ...and still does on the second call.  Before sealing the
			// literal came back '(1 2 3) here, and the second call was the
			// tell: the program's own text had been rewritten for the life of
			// the process (elps#369).
			{`(probe)`, `'(3 1 2)`, ``},
			// The refusal is an ordinary error with the pinned message.
			{`(let ([lit '(3 1 2)]) (stable-sort < lit))`,
				`test:1:23: modify-literal-error: cannot modify a program literal; take a (copy ...) first`, ``},
			// And a copy of the literal sorts in place as any runtime list
			// does, which is the sanctioned way to sort literal-derived data.
			{`(let ([lit (copy '(3 1 2))]) (stable-sort < lit) lit)`, `'(1 2 3)`, ``},
			{`(let ([lit (concat 'list '(3 1 2))]) (stable-sort < lit) lit)`, `'(1 2 3)`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
