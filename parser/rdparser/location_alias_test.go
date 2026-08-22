// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"bytes"
	"sync"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// WHAT ISSUE #362 BECAME, and why these tests changed shape.
//
// These tests were written against a process-wide "<native code>" Location --
// package lisp's unexported defaultSourceLocation, which nativeSource handed
// to every natively-constructed LVal.  The hazard was a parsed AST holding a
// pointer to it: an AST walker that stamps positions while descending would
// then corrupt the reported position of every constructed value in the
// process.  The tests asserted that no node the reader emits aliases that one
// object, and used a constructed value (lisp.Symbol("probe").Source) as the
// handle on it, since that was the only handle available from out here.
//
// #362's fix deleted the object.  Constructors leave the location nil and
// lisp synthesizes "<native code>" BY VALUE when someone asks, so there is no
// singleton to alias and no handle to take.  The property survives in a
// stronger form -- every node owns its own Location, with no exception -- and
// that is what the tests below now state.
//
// Locations are read by REFERENCE here, through internal/astraw.SourceRef.
// lisp.LVal.Source() returns a value copy on purpose (issue #382): a caller
// must not be able to hold, and so write through, a Location a value stores.
// That also makes "do two nodes share one?" unaskable through the public API,
// which is exactly the question these tests exist to ask, so they go through
// the module-internal accessor.  It is read-only by contract everywhere except
// here, where the writes below are the demonstration.

// walkLVal visits v and every node beneath it.
func walkLVal(v *lisp.LVal, fn func(*lisp.LVal)) {
	if v == nil {
		return
	}
	fn(v)
	for _, c := range v.Cells {
		walkLVal(c, fn)
	}
}

func parseAll(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	exprs, err := rdparser.New(token.NewScanner("test", bytes.NewReader([]byte(src)))).ParseProgram()
	require.NoError(t, err, "parsing %q", src)
	return exprs
}

// funRefSources are the sources exercised by the tests below.  #' is the one
// that mattered -- ParseFunRef synthesized its head symbol with lisp.Symbol
// and never re-stamped it -- but the invariant is stated over the whole
// grammar so a future synthesized node cannot reintroduce the leak quietly.
var funRefSources = []string{
	"#'car",
	"#'+",
	"(map 'list #'car '((1 2) (3 4)))",
	"'a",
	"'()",
	"'(a b c)",
	"(quote x)",
	"#^a",
	"(defun f (x) (#'g x))",
	"1",
	"\"s\"",
	"()",
}

// TestConstructedValuesHaveNoLocationToAlias is the premise the rest of the
// file rests on, stated rather than assumed.
//
// It is what replaced "sharedNativeLocation()".  There is no shared native
// Location to hand back any more, and the way to keep that true is to check
// it: if a constructor ever starts stamping one again, this fails here rather
// than the aliasing tests below quietly going vacuous.
func TestConstructedValuesHaveNoLocationToAlias(t *testing.T) {
	t.Parallel()

	for _, v := range []*lisp.LVal{
		lisp.Symbol("probe"), lisp.Int(1), lisp.String("s"), lisp.Nil(),
	} {
		require.Nilf(t, astraw.SourceRef(v),
			"a natively-constructed %v carries a *token.Location; the #362 singleton is back,"+
				" and every parsed AST can alias it again", v.Type)
	}
}

// TestParserGivesEveryNodeItsOwnLocation is what
// TestParserDoesNotAliasSharedNativeLocation became.
//
// It was a catch when written: parsing "#'car" produced an AST whose head
// symbol (lisp:function) held the process-wide pointer, reachable from any AST
// walk.  PR #419 (issue #370) then fixed the same reader defect from the other
// end, and better -- locateSynthesized gives the synthesized head the PREFIX
// TOKEN's own real Location, so the head reports where the user wrote "#'"
// instead of "<native code>".  #362 then removed the singleton entirely.
//
// So the narrow question ("does any node alias THAT object?") no longer has an
// object to name, and the general one it was standing in for is asserted
// instead: within a parse, no two nodes may hold one Location.  That is
// strictly stronger -- it catches a synthesized node handed a SIBLING's
// Location, which the old check could not see -- and it is the invariant
// LVal.Copy's per-node separation (#446) and the prefix-form fixups (#426)
// both depend on.
func TestParserGivesEveryNodeItsOwnLocation(t *testing.T) {
	t.Parallel()

	for _, src := range funRefSources {
		owner := map[*token.Location]*lisp.LVal{}
		for _, expr := range parseAll(t, src) {
			walkLVal(expr, func(v *lisp.LVal) {
				loc := astraw.SourceRef(v)
				if loc == nil {
					return
				}
				if prev, dup := owner[loc]; dup {
					t.Errorf("parsing %q produced nodes %v %q and %v %q sharing one *token.Location %v;"+
						" every parsed node must own its position (#362/#426)",
						src, prev.Type, prev.Str, v.Type, v.Str, loc)
					return
				}
				owner[loc] = v
			})
		}
	}
}

// TestFunRefHeadLocationIsPrivate demonstrates the consequence directly, and
// without the race detector: editing one parse's location must not be visible
// to an unrelated parse.
//
// The third arm of this test is gone with the singleton.  It used to check
// that the edit did not move an unrelated CONSTRUCTED value's position, which
// was the whole point when every constructed value shared one Location.  A
// constructed value now has no position to move, and
// TestConstructedValuesHaveNoLocationToAlias is where that is pinned.
func TestFunRefHeadLocationIsPrivate(t *testing.T) {
	t.Parallel()

	first := parseAll(t, "#'car")[0]
	second := parseAll(t, "#'cdr")[0]

	firstHead := first.Cells[0]
	secondHead := second.Cells[0]
	require.Equal(t, "lisp:function", firstHead.Str)
	require.Equal(t, "lisp:function", secondHead.Str)

	firstLoc := astraw.SourceRef(firstHead)
	secondLoc := astraw.SourceRef(secondHead)
	require.NotNil(t, firstLoc, "the synthesized #' head lost its location")
	require.NotNil(t, secondLoc)
	require.NotSame(t, firstLoc, secondLoc,
		"two independent parses' #' heads hold one *token.Location (#362)")

	secondPos := secondLoc.Pos

	// The kind of write a position-stamping AST walker makes.
	firstLoc.Pos = 7

	require.Equal(t, secondPos, secondLoc.Pos,
		"editing one parsed #' form's location moved another parse's location (#362)")
}

// TestFunRefHeadLocationRace is the -race half.  Two goroutines parse their own
// program and stamp their own AST -- no shared state by construction -- so
// under `go test -race` this is silent unless the two ASTs are handed the same
// Location, which is what sharing the process-wide singleton did.  Before #419
// it reported "WARNING: DATA RACE" on a data-segment address.
func TestFunRefHeadLocationRace(t *testing.T) {
	t.Parallel()

	// Parse on the test goroutine -- parsing is not what is under test here,
	// and require.* may only be called from the goroutine running the test.
	const n = 8
	exprs := make([]*lisp.LVal, n)
	for i := range exprs {
		exprs[i] = parseAll(t, "#'car")[0]
	}

	var start sync.WaitGroup
	var done sync.WaitGroup
	start.Add(1)
	for i := range n {
		done.Add(1)
		go func(i int) {
			defer done.Done()
			start.Wait()
			walkLVal(exprs[i], func(v *lisp.LVal) {
				if loc := astraw.SourceRef(v); loc != nil {
					loc.Pos = i
					loc.Line = i
				}
			})
		}(i)
	}
	start.Done()
	done.Wait()
}
