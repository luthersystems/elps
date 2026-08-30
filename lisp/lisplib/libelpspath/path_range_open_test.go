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

	// Nil is separated out because its result contains nils rather than
	// ints, so it cannot share intCells with the rest.
	for _, n := range []int{2, 3, 5, 9} {
		t.Run(fmt.Sprintf("Nil/n=%d", n), func(t *testing.T) {
			doc := seq(n)
			got, err := open.Nil(doc)
			require.NoErrorf(t, err, "n=%d", n)
			cells, err := toCells(got)
			require.NoError(t, err)
			require.Lenf(t, cells, n, "Nil changed the length")
			for i, c := range cells {
				if i < from {
					require.Equalf(t, lisp.LInt, c.Type, "position %d was nilled", i)
					continue
				}
				require.Truef(t, c.IsNil(), "position %d (>= %d) was not nilled", i, from)
			}
		})
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
