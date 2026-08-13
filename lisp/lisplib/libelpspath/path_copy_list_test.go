// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// TestCopyOpsOnList is the regression test for the list write-back defect
// FuzzPathEngine found (issue #379).
//
// TestMutateListRejected covers the MUTATING ops: they refuse lists outright
// (errMutateList), which is what closed the substrate#378 corruption vector.
// The non-mutating ops (?set, ?del, ?nil) are the other half — they accept a
// list, and they are SUPPOSED to: they copy first and rework the copy. But
// the rework wrote the ARRAY layout (`in.Cells[1].Cells = vals`, then array
// dims bookkeeping) into whatever it was handed, and on a list copy that
// meant:
//
//   - a one- or zero-element list panicked with "index out of range [1]",
//     an uncatchable crash where lisp code expected a value, and
//   - a longer list silently had the new cell slice written into its SECOND
//     ELEMENT and an Int field of its FIRST corrupted as "dims" — a wrong
//     answer returned with no error at all, which is worse.
//
// Both are fixed by storeCells dispatching on the container layout. This
// test pins the outcome for every non-mutating op across the list lengths
// that separate the two failure modes, and asserts the ORIGINAL list is
// untouched (the copy contract) in each case.
func TestCopyOpsOnList(t *testing.T) {
	t.Parallel()

	mkList := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = lisp.Int((i + 1) * 10)
		}
		return lisp.QExpr(cells)
	}

	for _, tc := range []struct {
		name string
		step Path
	}{
		{"index-0", Index(0)},
		{"index-last", Index(-1)},
		{"range-0-1", Range(0, 1, false)},
		{"range-all", Range(0, 0, true)},
	} {
		for _, n := range []int{0, 1, 2, 3, 5} {
			t.Run(fmt.Sprintf("%s/len-%d", tc.name, n), func(t *testing.T) {
				t.Parallel()
				for _, op := range []struct {
					name string
					run  func(p Path, in *lisp.LVal) (*lisp.LVal, error)
				}{
					{"?set", func(p Path, in *lisp.LVal) (*lisp.LVal, error) {
						return p.Set(in, lisp.String("NEW"))
					}},
					{"?del", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Delete(in) }},
					{"?nil", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Nil(in) }},
				} {
					in := mkList(n)
					before := fpAST([]*lisp.LVal{in})

					// A panic here is the defect: these ops must answer with
					// a value or a catchable error, never a crash.
					var res *lisp.LVal
					var err error
					func() {
						defer func() {
							if r := recover(); r != nil {
								t.Fatalf("%s %s on a %d-element list panicked: %v",
									op.name, tc.name, n, r)
							}
						}()
						res, err = op.run(Root(Chain(tc.step)), in)
					}()

					if err == nil && res == nil {
						t.Fatalf("%s %s on a %d-element list returned (nil, nil)", op.name, tc.name, n)
					}
					// The copy contract: whatever the answer, the input list
					// is unchanged.
					if after := fpAST([]*lisp.LVal{in}); after != before {
						t.Fatalf("%s %s changed its input, a %d-element list (now %v)",
							op.name, tc.name, n, in)
					}
					// A successful copy op on a list must return a list, not
					// a half-array with dims bookkeeping spliced into it.
					if err == nil && res.Type != lisp.LSExpr && !res.IsNil() {
						t.Fatalf("%s %s on a %d-element list returned a %v, want a list: %v",
							op.name, tc.name, n, res.Type, res)
					}
				}
			})
		}
	}
}

// TestCopyOpOnListDoesNotWriteThroughASharedLeaf is the reach the write-back
// defect has that TestCopyOpsOnList cannot see, and the one a parallel audit
// of substrate's copy of this engine surfaced first.
//
// A correct deep copy shares its LEAVES with the source.  It has to: an int
// or a string is an immutable LVal, so rebuilding one buys nothing and costs
// an allocation per scalar, and every copy helper in this package returns a
// leaf unchanged.  The array-layout write-back then aimed its two stores at
// `in.Cells[1]` and `in.Cells[0].Cells[0]`.  On a LIST copy those index the
// list's own ELEMENTS — which are the source's leaves — so the write landed
// on an integer the source was still holding, and `(?del cp "l" 0)` rewrote
// `'(1 2)` to `'(1 1)` in a document the operation was never given.
//
// TestCopyOpsOnList checks a list in isolation, where the leaves belong to
// nobody else and the corruption is invisible.  This test puts the list
// inside a document, copies the document, and asserts on the SOURCE.
func TestCopyOpOnListDoesNotWriteThroughASharedLeaf(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		step Path
	}{
		{"index-0", Index(0)},
		{"index-1", Index(1)},
		{"range-0-1", Range(0, 1, false)},
	} {
		for _, op := range []struct {
			name string
			run  func(p Path, in *lisp.LVal) (*lisp.LVal, error)
		}{
			{"?set", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Set(in, lisp.String("NEW")) }},
			{"?del", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Delete(in) }},
			{"?nil", func(p Path, in *lisp.LVal) (*lisp.LVal, error) { return p.Nil(in) }},
		} {
			t.Run(tc.name+"/"+op.name, func(t *testing.T) {
				t.Parallel()

				src := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
				doc := lisp.SortedMap()
				doc.MapSet("l", src)
				before := fpAST([]*lisp.LVal{doc})

				// The copy an ordinary ?set on an unrelated key returns.
				cp, err := copyLVal(doc)
				require.NoError(t, err)
				cpList, ok := cp.Map().Get(lisp.String("l"))
				require.True(t, ok)

				// The copy owns its containers...
				require.NotSame(t, src, cpList,
					"the deep copy must not share the list container")
				// ...and shares its leaves, which is why the write-back has
				// to know what layout it is writing into.
				require.Same(t, src.Cells[1], cpList.Cells[1],
					"a deep copy shares immutable leaves; if it stops, this "+
						"test no longer covers the defect it was written for")

				_, err = op.run(Root(Chain(tc.step)), cpList)
				if err != nil {
					t.Logf("%s %s: %v", op.name, tc.name, err)
				}

				assert.Equal(t, before, fpAST([]*lisp.LVal{doc}),
					"%s %s through the copy reached the source document: %v",
					op.name, tc.name, doc)
			})
		}
	}
}
