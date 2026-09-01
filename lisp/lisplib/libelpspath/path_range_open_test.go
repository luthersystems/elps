// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// TestOpenRangeTracksInputLengthForEveryOp is the property a closed range
// CANNOT express, and the reason the open form deserves tests of its own
// rather than being treated as shorthand for '(range from n).
//
// A closed range carries its end. An open one resolves its end against the
// document at evaluation time, so ONE path value applied to two documents of
// different lengths must splice to two different places. Every other test in
// this package builds a fresh path per document, which makes the two
// spellings indistinguishable; here the path is built once, outside the
// loop, and reused.
//
// It goes through the Go Path API rather than the builtins deliberately.
// The builtins re-parse their steps on every call, so a path value cannot be
// reused across calls through them at all -- reuse is exactly what an
// embedder does, and exactly what parse-path now makes worthwhile for lisp
// callers too.
//
// There was one test of this shape before (the lisp-side "? range with an
// implicit end tracks the input length") and it covered only Get. A
// regression in the splicing operations' handling of implicitTo would have
// left it green.
func TestOpenRangeTracksInputLengthForEveryOp(t *testing.T) {
	t.Parallel()

	// Built ONCE. Reusing this value is the whole point.
	const from = 2
	open := Range(from, 0, true)

	// seq builds (1 .. n) as a vector; vectors throughout because the
	// mutating operations refuse lists (errMutateList) and an emptied list
	// is nil.
	seq := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = lisp.Int(i + 1)
		}
		return lisp.Vector(cells)
	}
	repl := func() *lisp.LVal {
		return lisp.Vector([]*lisp.LVal{lisp.Int(91), lisp.Int(92)})
	}

	// The expectations are written as functions of n, computed here rather
	// than tabulated, because a table would have to encode the very
	// resolution rule under test.
	type expect struct {
		name string
		// run applies the operation and returns the sequence to compare.
		run func(doc *lisp.LVal) (*lisp.LVal, error)
		// want is the result for a document of length n.
		want func(n int) []int
		// wantDoc is the document AFTER the call, nil when unchanged.
		wantDoc func(n int) []int
	}
	head := func(n int) []int {
		out := make([]int, 0, from)
		for i := 0; i < from && i < n; i++ {
			out = append(out, i+1)
		}
		return out
	}
	tail := func(n int) []int { // the window the open range names: [from, n)
		out := []int{}
		for i := from; i < n; i++ {
			out = append(out, i+1)
		}
		return out
	}
	spliced := func(n int) []int { return append(append([]int{}, head(n)...), 91, 92) }

	ops := []expect{
		{
			name: "Get",
			run:  func(d *lisp.LVal) (*lisp.LVal, error) { return open.Get(d) },
			want: tail,
		},
		{
			name:    "Set",
			run:     func(d *lisp.LVal) (*lisp.LVal, error) { return open.Set(d, repl()) },
			want:    spliced,
			wantDoc: func(n int) []int { return seqInts(n) }, // copying: untouched
		},
		{
			name:    "SetMutate",
			run:     func(d *lisp.LVal) (*lisp.LVal, error) { return open.SetMutate(d, repl()) },
			want:    spliced,
			wantDoc: spliced,
		},
		{
			name:    "Delete",
			run:     func(d *lisp.LVal) (*lisp.LVal, error) { return open.Delete(d) },
			want:    head,
			wantDoc: func(n int) []int { return seqInts(n) },
		},
		{
			name:    "DeleteMutate",
			run:     func(d *lisp.LVal) (*lisp.LVal, error) { return open.DeleteMutate(d) },
			want:    head,
			wantDoc: head,
		},
	}

	// from=2, so n < 2 is the boundary where the window is empty and n >= 2
	// is where it is not. Both sides are here on purpose: an implementation
	// that ignored implicitTo and spliced to a fixed 0 would still agree
	// with these on n <= 2.
	lengths := []int{0, 1, 2, 3, 5, 9}

	for _, op := range ops {
		for _, n := range lengths {
			t.Run(fmt.Sprintf("%s/n=%d", op.name, n), func(t *testing.T) {
				doc := seq(n)
				got, err := op.run(doc)
				if n < from {
					// The window starts past the end: index out of range.
					require.Errorf(t, err, "n=%d: want an error, got %v", n, got)
					return
				}
				require.NoErrorf(t, err, "n=%d", n)
				require.Equalf(t, op.want(n), intCells(t, got),
					"%s over (1..%d) with '(range %d)", op.name, n, from)
				if op.wantDoc != nil {
					require.Equalf(t, op.wantDoc(n), intCells(t, doc),
						"%s left the document wrong for n=%d", op.name, n)
				}
			})
		}
	}

	// Nil and NilMutate are separated out because their result holds nils
	// rather than ints, so they cannot share intCells with the loop above.
	//
	// BOTH are here, over the SAME lengths, and that is the point of the
	// block rather than an accident of tidiness. NilMutate resolves the open
	// end against the document it is about to rewrite IN PLACE, which is a
	// different arithmetic from the copying Nil's and was covered by nothing
	// -- exactly the gap DeleteMutate has above and Nil alone did not. The
	// n < from lengths are included too: "the window starts past the end" has
	// to be an ERROR for the nilling pair as it is for the rest, because a
	// silent no-op there is indistinguishable from a correct empty window and
	// would let a regression that ignored implicitTo entirely stay green.
	nilOps := []struct {
		name    string
		run     func(*lisp.LVal) (*lisp.LVal, error)
		mutates bool
	}{
		{
			name: "Nil",
			run:  func(d *lisp.LVal) (*lisp.LVal, error) { return open.Nil(d) },
		},
		{
			name:    "NilMutate",
			run:     func(d *lisp.LVal) (*lisp.LVal, error) { return open.NilMutate(d) },
			mutates: true,
		},
	}
	// nilled asserts the shape both operations must produce: the length is
	// unchanged, everything below from is untouched, everything from there on
	// is nil.
	nilled := func(t *testing.T, v *lisp.LVal, n int, what string) {
		t.Helper()
		cells, err := toCells(v)
		require.NoError(t, err)
		require.Lenf(t, cells, n, "%s: the length changed", what)
		for i, c := range cells {
			if i < from {
				require.Equalf(t, lisp.LInt, c.Type, "%s: position %d was nilled", what, i)
				continue
			}
			require.Truef(t, c.IsNil(), "%s: position %d (>= %d) was not nilled", what, i, from)
		}
	}
	for _, op := range nilOps {
		for _, n := range lengths {
			t.Run(fmt.Sprintf("%s/n=%d", op.name, n), func(t *testing.T) {
				doc := seq(n)
				got, err := op.run(doc)
				if n < from {
					// The window starts past the end: index out of range.
					require.Errorf(t, err, "n=%d: want an error, got %v", n, got)
					return
				}
				require.NoErrorf(t, err, "n=%d", n)
				nilled(t, got, n, op.name+" result")
				if op.mutates {
					nilled(t, doc, n, op.name+" document")
					return
				}
				// The copying variant must leave the input alone, which is
				// the other half of what "copying" means and what the
				// mutating arm above would otherwise be indistinguishable
				// from.
				require.Equalf(t, seqInts(n), intCells(t, doc),
					"Nil rewrote the document it was given")
			})
		}
	}
}

// seqInts is (1..n) as plain ints, the untouched-document expectation.
func seqInts(n int) []int {
	out := make([]int, n)
	for i := range out {
		out[i] = i + 1
	}
	return out
}
