// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"bytes"
	"sync"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// sharedNativeLocation returns the exact *token.Location that lisp's
// nativeSource hands to every natively-constructed LVal.  It is package
// lisp's unexported defaultSourceLocation; a constructed value is the only
// handle on it from outside the package, and it is the object issue #362 is
// about.
func sharedNativeLocation() *token.Location {
	return lisp.Symbol("probe").Source
}

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
//
// GUARDS, NOT CATCHES.  The two tests below were written against the tree
// before PR #419 (issue #370) landed and failed there; they pass unmodified on
// current main, because #419 fixed the same reader defect from the other end
// -- see the note on each.
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

// TestParserDoesNotAliasSharedNativeLocation is a GUARD on current main, not a
// catch.  It states issue #362's property over the reader's whole output: no
// node the parser produces may hold the process-wide "<native code>" Location.
//
// It was a catch when written.  Parsing "#'car" produced an AST whose head
// symbol (lisp:function) held that pointer, reachable from any AST walk, so a
// walker stamping positions while descending corrupted the reported position
// of every natively-constructed value in the process.
//
// PR #419 (issue #370) then fixed the same reader defect from the other end,
// and better: locateSynthesized gives the synthesized head the PREFIX TOKEN's
// own real Location, so the head reports where the user wrote "#'" instead of
// "<native code>", and the shared pointer is gone as a side effect rather than
// replaced by a private copy of itself.  #419 was chasing the stamp walk
// writing into a caller's parse tree; this test was chasing the same node
// holding process-global state.  Two routes to one line of the reader.
//
// It stays because the two issues bound different things and #419's fix is not
// obliged to keep satisfying this one.  #419 pins that the reader emits no
// SYNTHETIC location (TestParserEmitsNoSyntheticSourceLocations); this pins
// that it emits no pointer to the SHARED one.  A future synthesized node given
// a private nativeSource copy would satisfy neither, but a node given a
// distinct Location whose Pos happens to be -1 would satisfy #419's and not
// this one.
func TestParserDoesNotAliasSharedNativeLocation(t *testing.T) {
	t.Parallel()
	shared := sharedNativeLocation()
	require.NotNil(t, shared, "expected constructed values to carry a native source location")

	for _, src := range funRefSources {
		for _, expr := range parseAll(t, src) {
			walkLVal(expr, func(v *lisp.LVal) {
				if v.Source == shared {
					t.Errorf("parsing %q produced node %v %q holding the shared process-wide native Location; a parsed AST must not alias it (#362)",
						src, v.Type, v.Str)
				}
			})
		}
	}
}

// TestFunRefHeadLocationIsPrivate demonstrates the consequence directly, and
// without the race detector: editing one parse's location must not be visible
// to an unrelated parse or to an unrelated constructed value.
//
// Also a GUARD on current main rather than a catch -- see the note above. It
// failed before #419 with the second parse's Pos reading 7 instead of -1.
func TestFunRefHeadLocationIsPrivate(t *testing.T) {
	t.Parallel()

	first := parseAll(t, "#'car")[0]
	second := parseAll(t, "#'cdr")[0]

	firstHead := first.Cells[0]
	secondHead := second.Cells[0]
	require.Equal(t, "lisp:function", firstHead.Str)
	require.Equal(t, "lisp:function", secondHead.Str)

	witness := lisp.Int(1)
	witnessPos := witness.Source.Pos
	secondPos := secondHead.Source.Pos

	// The kind of write a position-stamping AST walker makes.
	firstHead.Source.Pos = 7

	require.Equal(t, secondPos, secondHead.Source.Pos,
		"editing one parsed #' form's location moved another parse's location (#362)")
	require.Equal(t, witnessPos, witness.Source.Pos,
		"editing a parsed #' form's location moved the location of an unrelated constructed value (#362)")
}

// TestFunRefHeadLocationRace is the -race half, and likewise a GUARD on
// current main.  Two goroutines parse their own program and stamp their own
// AST -- no shared state by construction -- so under `go test -race` this is
// silent unless the two ASTs are handed the same Location, which is what
// sharing the process-wide singleton did.  Before #419 it reported
// "WARNING: DATA RACE" on a data-segment address (defaultSourceLocation).
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
				if v.Source != nil {
					v.Source.Pos = i
					v.Source.Line = i
				}
			})
		}(i)
	}
	start.Done()
	done.Wait()
}
