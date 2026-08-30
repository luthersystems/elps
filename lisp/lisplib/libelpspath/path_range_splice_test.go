// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// The range-splice battery.
//
// rangePath.setMutate built the spliced result with
//
//	vals := append(cells[:from], setCells...)
//	if to < n {
//	    vals = append(vals, cells[to:]...)
//	}
//
// The first append writes setCells THROUGH cells' own backing array starting
// at from; the second then reads cells[to:] out of that same, already
// overwritten array.  When the replacement is longer than the range it
// replaces, the source elements in [to, from+len(setCells)) are destroyed
// before they are read and the result repeats the replacement's own tail:
//
//	(?set (vector 1 2 3 4 5) '(range 0 1) (vector 97 98 99))
//	  gave (vector 97 98 99 98 99 4 5)
//	  want (vector 97 98 99 2 3 4 5)
//
// No error, no panic — a wrong answer.  It is the same defect class the rest
// of this branch is about (#373/#392): appending into a backing array the
// function does not own.
//
// WHY IT SURVIVED THE SUITE.  Three corners hid it, and this battery covers
// all three deliberately rather than by luck:
//
//   - Equal-length and shorter replacements never grow past `to`, so nothing
//     is overwritten before it is read.  TestMutateArrayMapInPlace's only
//     range-set case used a shorter replacement.
//   - `to == n` skips the tail append entirely, so there is nothing left to
//     read corrupted.
//   - A replacement long enough to push from+len(setCells) past cap(cells)
//     makes the first append REALLOCATE, which leaves the source untouched
//     and the answer accidentally right.  The private copy handed in by the
//     copying Set always has cap == len (copySeqOffPath), so this is the only
//     escape and it needs a big replacement — hence the reallocating control
//     case below, which is green on the unfixed tree.
//
// So the axes are crossed on purpose: replacement longer than / equal to /
// shorter than the range, at from == 0 and from > 0, with to == n and
// to < n, over both list and vector sources, through the copying ?set and
// the in-place ?set!.  Each case asserts the RESULT exactly, and the copying
// cases additionally assert the source is untouched — the two halves of the
// contract, one of which no existing test in the package checks at all.
type rangeSpliceCase struct {
	name string
	n    int // source is (1 .. n)
	from int
	to   int  // ignored when open; see resolvedTo
	repl int  // replacement is (91 .. 90+repl)
	open bool // '(range from), whose end resolves to n at evaluation time
}

// resolvedTo is the end the engine will actually splice to. The open form
// carries no end at all, so it is n by definition -- which is why every open
// case below leaves `to` unset rather than writing n twice.
func (c rangeSpliceCase) resolvedTo() int {
	if c.open {
		return c.n
	}
	return c.to
}

// resolvedFrom applies the negative-index rule the engine documents: a
// negative from counts back from the end. Resolved here independently rather
// than read off the implementation, so the oracle cannot inherit its
// arithmetic.
func (c rangeSpliceCase) resolvedFrom() int {
	if c.from < 0 {
		return c.n + c.from
	}
	return c.from
}

// step renders the case's range as the positional step the ? family parses,
// in the spelling the case asks for. The two spellings reach DIFFERENT arms
// of validateRange -- the open one resolves its end against the document
// instead of carrying one -- so a splice bound can be right in one and wrong
// in the other. Issue #563 was exactly that asymmetry in String().
func (c rangeSpliceCase) step() *lisp.LVal {
	if c.open {
		return lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(c.from)})
	}
	return rangeStep(c.from, c.to)
}

// label renders the range the way a caller would write it, for failure
// messages that say which spelling broke.
func (c rangeSpliceCase) label() string {
	if c.open {
		return fmt.Sprintf("'(range %d)", c.from)
	}
	return fmt.Sprintf("'(range %d %d)", c.from, c.to)
}

var rangeSpliceCases = []rangeSpliceCase{
	// --- replacement LONGER than the range: the defect's territory ---
	{name: "longer/from=0/to<n", n: 5, from: 0, to: 1, repl: 3},
	{name: "longer/from>0/to<n", n: 5, from: 1, to: 2, repl: 3},
	{name: "longer/from>0/to<n/wide", n: 8, from: 2, to: 3, repl: 4},
	{name: "longer/from=0/to=n", n: 5, from: 0, to: 5, repl: 7},
	{name: "longer/from>0/to=n", n: 5, from: 3, to: 5, repl: 3},
	{name: "longer/empty-range/insert", n: 5, from: 2, to: 2, repl: 3},
	{name: "longer/insert-at-head", n: 4, from: 0, to: 0, repl: 2},
	// A replacement big enough that from+len(setCells) exceeds cap(cells):
	// the unfixed append reallocates and is accidentally correct.  Kept as
	// the control that pins WHY the defect is conditional.
	{name: "longer/reallocating-control", n: 5, from: 4, to: 5, repl: 4},

	// --- replacement EQUAL in length to the range ---
	{name: "equal/from=0/to<n", n: 5, from: 0, to: 2, repl: 2},
	{name: "equal/from>0/to<n", n: 5, from: 2, to: 4, repl: 2},
	{name: "equal/from>0/to=n", n: 5, from: 3, to: 5, repl: 2},
	{name: "equal/whole", n: 4, from: 0, to: 4, repl: 4},

	// --- replacement SHORTER than the range ---
	{name: "shorter/from=0/to<n", n: 5, from: 0, to: 3, repl: 1},
	{name: "shorter/from>0/to<n", n: 5, from: 1, to: 4, repl: 2},
	{name: "shorter/from>0/to=n", n: 5, from: 2, to: 5, repl: 1},
	{name: "shorter/empty-replacement", n: 5, from: 1, to: 3, repl: 0},
	{name: "shorter/empty-replacement/to=n", n: 5, from: 2, to: 5, repl: 0},

	// --- the OPEN form, '(range from) ---
	//
	// Every case above writes its end explicitly, so before these the whole
	// exact-result battery ran on one of validateRange's two arms. The open
	// arm resolves its end against the document rather than carrying one,
	// and it is the arm issue #563 broke on its own: String() ignored
	// implicitTo and printed ".[1:]" as ".[1:0]", an empty slice, which is a
	// different path. A defect confined to this arm would have been
	// invisible here.
	//
	// The bounds are necessarily to == n, so these mirror the "to=n" cases
	// above rather than adding new geometry -- the point is the spelling.
	{name: "open/longer/from=0", n: 5, from: 0, repl: 7, open: true},
	{name: "open/longer/from>0", n: 5, from: 3, repl: 3, open: true},
	{name: "open/longer/from=n/append", n: 3, from: 3, repl: 2, open: true},
	{name: "open/equal", n: 5, from: 3, repl: 2, open: true},
	{name: "open/equal/whole", n: 4, from: 0, repl: 4, open: true},
	{name: "open/shorter", n: 5, from: 1, repl: 2, open: true},
	{name: "open/shorter/truncate-to-one", n: 5, from: 1, repl: 1, open: true},
	{name: "open/empty-replacement/truncate", n: 5, from: 2, repl: 0, open: true},
	{name: "open/empty-replacement/clear-all", n: 5, from: 0, repl: 0, open: true},
	{name: "open/from=n/empty-replacement/no-op", n: 4, from: 4, repl: 0, open: true},
	// A negative from counts from the end and is resolved BEFORE the
	// implicit end is filled in, so the two rewrites in validateRange have
	// to compose. Nothing else in this file crosses them.
	{name: "open/negative-from", n: 5, from: -2, repl: 3, open: true},
	{name: "open/negative-from/whole", n: 5, from: -5, repl: 1, open: true},
	{name: "open/empty-source", n: 0, from: 0, repl: 2, open: true},
}

// spliceSource builds the source sequence 1..n in the requested shape.
func spliceSource(shape string, n int) *lisp.LVal {
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i + 1)
	}
	if shape == "vector" {
		return lisp.Vector(cells)
	}
	return lisp.QExpr(cells)
}

// spliceReplacement builds the replacement 91..90+k, values disjoint from
// every source element so a repeated or dropped cell is unambiguous.
func spliceReplacement(shape string, k int) *lisp.LVal {
	cells := make([]*lisp.LVal, k)
	for i := range cells {
		cells[i] = lisp.Int(91 + i)
	}
	if shape == "vector" {
		return lisp.Vector(cells)
	}
	return lisp.QExpr(cells)
}

// wantSplice is the answer, computed the only way that cannot share the
// implementation's mistake: three appends onto a slice built here.
func wantSplice(c rangeSpliceCase) []int {
	from, to := c.resolvedFrom(), c.resolvedTo()
	out := make([]int, 0, from+c.repl+(c.n-to))
	for i := range from {
		out = append(out, i+1)
	}
	for i := range c.repl {
		out = append(out, 91+i)
	}
	for i := to; i < c.n; i++ {
		out = append(out, i+1)
	}
	return out
}

func rangeStep(from, to int) *lisp.LVal {
	return lisp.QExpr([]*lisp.LVal{
		lisp.Symbol("range"), lisp.Int(from), lisp.Int(to),
	})
}

// intCells reads a result sequence back as plain ints, failing on any shape
// that is not a flat sequence of integers.
func intCells(t *testing.T, v *lisp.LVal) []int {
	t.Helper()
	cells, err := toCells(v)
	require.NoError(t, err, "result is not a sequence: %v", v)
	out := make([]int, len(cells))
	for i, c := range cells {
		require.Equalf(t, lisp.LInt, c.Type,
			"result element %d is %v, not an int (full result: %v)", i, c.Type, v)
		out[i] = c.Int
	}
	return out
}

func renderInts(xs []int) string {
	parts := make([]string, len(xs))
	for i, x := range xs {
		parts[i] = strconv.Itoa(x)
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// TestRangeSpliceResult is the regression test: the spliced result is
// prefix ++ replacement ++ suffix, exactly, for every crossing of the axes,
// and the copying ?set leaves its source and its value argument alone.
func TestRangeSpliceResult(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	for _, c := range rangeSpliceCases {
		// Source shape crossed with replacement shape: the splice reads the
		// replacement through toCells and writes the result through
		// storeCells, and those two dispatch on different values.
		for _, shape := range []string{"vector", "list"} {
			for _, replShape := range []string{"vector", "list"} {
				// An empty LIST is '() which is nil in elps, and toCells
				// rejects nil before the splice is reached.  That is
				// pre-existing engine behaviour, not part of this defect;
				// the empty-replacement axis is covered by the vector
				// shape, which has a genuine zero-length form.
				if c.repl == 0 && replShape == "list" {
					continue
				}
				// Same reason on the SOURCE side: an empty list is nil, and
				// toCells rejects nil before any range is resolved. The
				// empty-source axis is covered by the vector shape, which
				// has a genuine zero-length form.
				if c.n == 0 && shape == "list" {
					continue
				}
				// And on the RESULT side: a splice that empties a list
				// yields '(), which is nil, so intCells has nothing to
				// read. That outcome is pinned on its own by
				// TestSpliceEmptyingAListYieldsNil rather than skipped
				// silently.
				if len(wantSplice(c)) == 0 && shape == "list" {
					continue
				}
				t.Run(c.name+"/"+shape+"/repl="+replShape+"/?set", func(t *testing.T) {
					src := spliceSource(shape, c.n)
					repl := spliceReplacement(replShape, c.repl)
					srcBefore, replBefore := src.String(), repl.String()

					got := callBuiltin(env, BuiltinQuerySet,
						src, c.step(), repl)
					require.NotEqualf(t, lisp.LError, got.Type, "?set errored: %v", got)

					want := wantSplice(c)
					require.Equalf(t, want, intCells(t, got),
						"(?set %s %s %s)\n  want %s\n  got  %s",
						srcBefore, c.label(), replBefore, renderInts(want), got)

					// ?set is documented to return a copy and leave the
					// original alone.  The splice used to write the
					// replacement through the copy's backing array; a
					// future one writing through the SOURCE's would be
					// caught here.
					require.Equalf(t, srcBefore, src.String(),
						"?set changed its source document")
					require.Equalf(t, replBefore, repl.String(),
						"?set changed its value argument")
				})
			}
		}

		// The in-place variant, on a vector only: errMutateList rejects a
		// list, which TestMutateListRejected pins.
		t.Run(c.name+"/vector/?set!", func(t *testing.T) {
			src := spliceSource("vector", c.n)
			repl := spliceReplacement("vector", c.repl)
			replBefore := repl.String()

			got := callBuiltin(env, BuiltinQuerySetMutate,
				src, c.step(), repl)
			require.NotEqualf(t, lisp.LError, got.Type, "?set! errored: %v", got)

			want := wantSplice(c)
			require.Equalf(t, want, intCells(t, got),
				"(?set! (1..%d) %s %s)\n  want %s\n  got  %s",
				c.n, c.label(), replBefore, renderInts(want), got)

			// ?set! reworks the caller's vector in place, so the document
			// itself must show the splice too.
			require.Equalf(t, want, intCells(t, src),
				"?set! returned the right answer but left the document wrong")
			require.Equalf(t, replBefore, repl.String(),
				"?set! changed its value argument")
		})
	}
}

// TestRangeSpliceReportedCase pins the exact reproducer from the review of
// #402, verbatim, so the report and the suite name the same thing.
func TestRangeSpliceReportedCase(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	src := lisp.Vector([]*lisp.LVal{
		lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5),
	})
	repl := lisp.Vector([]*lisp.LVal{lisp.Int(97), lisp.Int(98), lisp.Int(99)})

	got := callBuiltin(env, BuiltinQuerySet, src, rangeStep(0, 1), repl)
	require.NotEqualf(t, lisp.LError, got.Type, "?set errored: %v", got)
	require.Equal(t, []int{97, 98, 99, 2, 3, 4, 5}, intCells(t, got),
		"the unfixed splice gave (vector 97 98 99 98 99 4 5): the source's"+
			" 2 and 3 replaced by the replacement's own 98 and 99")
}

// TestRangeNilPreservesSurroundings covers the sibling route into
// setMutate.  rangePath.nilMutate builds an all-nil replacement of exactly
// the range's length and splices it in, so it can never be longer than what
// it replaces — which is why ?nil was safe throughout.  Pinned so a future
// change to nilMutate's replacement length cannot quietly re-enter the
// defect through the back door.
func TestRangeNilPreservesSurroundings(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	for _, shape := range []string{"vector", "list"} {
		t.Run(shape, func(t *testing.T) {
			src := spliceSource(shape, 5)
			srcBefore := src.String()

			got := callBuiltin(env, BuiltinQueryNil, src, rangeStep(1, 3))
			require.NotEqualf(t, lisp.LError, got.Type, "?nil errored: %v", got)

			cells, err := toCells(got)
			require.NoError(t, err)
			require.Len(t, cells, 5)
			for i, c := range cells {
				switch i {
				case 1, 2:
					require.Truef(t, c.IsNil(), "element %d should be nil, got %v", i, c)
				default:
					require.Equalf(t, lisp.LInt, c.Type, "element %d: %v", i, c)
					require.Equalf(t, i+1, c.Int,
						"element %d outside the range was not preserved: %v", i, got)
				}
			}
			require.Equal(t, srcBefore, src.String(), "?nil changed its source")
		})
	}
}

// TestRangeGetClampsCapacity pins the three-index slice in rangePath.Get.
//
// A two-index `cells[from:to]` hands back a view carrying the source's spare
// capacity, and an append into that capacity writes through to the source.
// That is the aliasing class of issues #369 and #373, which the kernel has
// since settled by clamping every sequence view where it is produced
// (lisp.clampCap).  With that settled, an unclamped view from this Get would
// be the only remaining producer of it in the tree.
//
// The assertion is on cap rather than on an observed corruption because cap
// is the property the settlement is stated in.  The end-to-end arm --
// (append! (? v '(range 0 3)) 99) leaving v alone -- is in
// libelpspath_test.lisp next to the kernel control it has to match.
//
// Red-proof: revert to `cells[from:to]` and every sub-test whose range is a
// strict prefix reports cap 5 against len 3.
func TestRangeGetClampsCapacity(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	for _, shape := range []string{"vector", "list"} {
		for _, r := range [][2]int{{0, 3}, {1, 4}, {2, 5}, {0, 5}} {
			from, to := r[0], r[1]
			name := shape + "/" + strconv.Itoa(from) + ":" + strconv.Itoa(to)
			t.Run(name, func(t *testing.T) {
				src := spliceSource(shape, 5)

				got := callBuiltin(env, BuiltinQueryGet, src, rangeStep(from, to))
				require.NotEqualf(t, lisp.LError, got.Type, "? errored: %v", got)

				cells, err := toCells(got)
				require.NoError(t, err)
				require.Len(t, cells, to-from)
				require.Equalf(t, len(cells), cap(cells),
					"range view kept the source's spare capacity: len %d cap %d",
					len(cells), cap(cells))
			})
		}
	}
}

// TestSpliceEmptyingAListYieldsNil pins the one outcome TestRangeSpliceResult
// skips for list sources.
//
// A splice that removes every element of a list produces '(), and in elps
// '() IS nil -- so the result is not an empty sequence, it is the nil value,
// and a caller that goes on to index it gets "first argument is nil" rather
// than "index out of range". That is engine-wide behaviour rather than
// anything this package chose, but it is the shape a range splice can
// produce most easily, and a reader of the skip above deserves to see it
// asserted somewhere instead of merely stepped around.
//
// The vector arm is the contrast that makes the point: the same splice over
// a vector yields an empty VECTOR, which is a perfectly ordinary value.
func TestSpliceEmptyingAListYieldsNil(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	// '(range 0) over a five-element source, replaced by nothing.
	openAll := lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0)})

	got := callBuiltin(env, BuiltinQuerySet,
		spliceSource("list", 5), openAll, lisp.Vector(nil))
	require.NotEqualf(t, lisp.LError, got.Type, "?set over a list errored: %v", got)
	require.Truef(t, got.IsNil(), "emptying a list gave %v, want nil", got)

	gotVec := callBuiltin(env, BuiltinQuerySet,
		spliceSource("vector", 5), openAll, lisp.Vector(nil))
	require.NotEqualf(t, lisp.LError, gotVec.Type, "?set over a vector errored: %v", gotVec)
	require.Falsef(t, gotVec.IsNil(), "emptying a vector gave nil, want an empty vector")
	require.Equalf(t, []int{}, intCells(t, gotVec), "emptying a vector gave %v", gotVec)
}
