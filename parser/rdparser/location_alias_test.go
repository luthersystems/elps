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

// TestParserDoesNotAliasSharedNativeLocation is the regression test for issue
// #362's concrete instance.
//
// Parsing "#'car" produced an AST whose head symbol (lisp:function) held a
// pointer to the process-wide "<native code>" Location.  Nothing writes
// through it in-tree today, which is why the issue calls it a latent trap
// rather than a live defect -- but the pointer is reachable from any AST
// walk, and a walker that stamps positions while descending corrupts the
// reported position of every natively-constructed value in the process, with
// the damage surfacing nowhere near the write.
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

// TestFunRefHeadLocationRace is the -race half.  Two goroutines parse their
// own program and stamp their own AST -- no shared state by construction --
// so under `go test -race` this is silent unless the two ASTs are handed the
// same Location, which is what sharing the process-wide singleton did.
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
