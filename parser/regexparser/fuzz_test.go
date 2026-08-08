// Copyright © 2026 The ELPS authors

package regexparser_test

import (
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/regexparser"
)

// skipReason explains why FuzzParseLVal is not enabled.  See
// TestParseLValCatastrophicBacktracking below for the measurements.
const skipReason = `regexparser.ParseLVal backtracks catastrophically on unmatched
opening brackets: parse time roughly quadruples for every two additional
consecutive "(" (measured: 12 -> 68ms, 14 -> 292ms, 16 -> 1.1s, 18 -> 4.8s,
20 -> no result in 10s).  A ~24-byte input therefore never returns.

That makes this target unusable as written.  Native go fuzzing has no
per-input deadline, so the first mutation that stacks two dozen open parens --
which a mutator reaches almost immediately -- wedges a worker silently for the
rest of the run and takes the CI job with it.

The blow-up is not something this change can responsibly fix.  It is inherent
to the grammar in newParsecParser: sexpr, sexprOUnmatched, vector and
qexprOUnmatched are OrdChoice alternatives that each re-run Kleene(&expr) over
the same suffix with no memoisation, so every nesting level doubles the work.
Making it linear means either packrat memoisation in goparsec or replacing the
grammar -- a redesign, not a fix.

Scope note: nothing in this repository selects this reader.  parser.NewReader
returns rdparser, and no non-test file references package regexparser, so the
exposure is limited to an external embedder that constructs it directly.

TODO(fuzz): delete this skip once ParseLVal is linear in nesting depth, or
once the package is retired.  The target below is complete and correct; it
needs no changes to be re-enabled.`

// FuzzParseLVal would fuzz the alternative, goparsec-backed parser.  Its AST
// construction is markedly less defensive than rdparser's -- newAST indexes
// fixed node positions, type-asserts without the comma-ok form, and ends in an
// explicit panic on an unrecognised node type -- so it is worth fuzzing.  It
// is disabled only because of the backtracking blow-up described above.
func FuzzParseLVal(f *testing.F) {
	f.Skip(skipReason)

	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		vals, n, err := regexparser.ParseLVal(src)
		if n < 0 || n > len(src) {
			t.Fatalf("ParseLVal reported %d bytes consumed for a %d byte input", n, len(src))
		}
		for i, v := range vals {
			if v == nil {
				t.Fatalf("ParseLVal returned a nil LVal at index %d (err=%v)", i, err)
			}
			// String() walks the value; an AST that cannot be printed is as
			// unusable as one that cannot be built.
			_ = v.String()
			_ = lisp.GetType(v)
		}
	})
}

// TestParseLValHandlesSmallInput is the coverage that survives the skip: the
// shapes that DO terminate quickly still have to come back with a sane result
// and without panicking.  Nesting is kept at or below eight unmatched opens,
// which measured at ~6ms; see skipReason for the curve.
func TestParseLValHandlesSmallInput(t *testing.T) {
	for _, src := range []string{
		"",
		"()",
		"(a b c)",
		"[1 2 3]",
		"'(1 2 3)",
		"(defun f (x) (+ x 1))",
		`"abc"`,
		`"""raw"""`,
		"; comment",
		"#^(+ 1 2)",
		"#^",
		"(((((((( ",
		"))))",
		"(]",
		"1e",
		"9223372036854775808",
		"a:b:c",
		"\xff",
		"\x00",
	} {
		t.Run(src, func(t *testing.T) {
			vals, n, _ := regexparser.ParseLVal([]byte(src))
			if n < 0 || n > len(src) {
				t.Fatalf("reported %d bytes consumed for a %d byte input", n, len(src))
			}
			for i, v := range vals {
				if v == nil {
					t.Fatalf("nil LVal at index %d", i)
				}
				_ = v.String()
			}
		})
	}
}

// TestParseLValCatastrophicBacktracking records the defect that keeps
// FuzzParseLVal disabled.  It deliberately asserts nothing -- a wall-clock
// threshold is flaky on a shared runner, and a green board must not depend on
// a bug staying slow.  It logs which side of the deadline the parse landed on,
// so a future run that reports "returned" is the prompt to re-measure the
// curve and delete the skip above.
func TestParseLValCatastrophicBacktracking(t *testing.T) {
	if testing.Short() {
		t.Skip("timing-sensitive")
	}
	const depth = 16
	src := []byte(strings.Repeat("(", depth))
	done := make(chan struct{})
	go func() {
		defer close(done)
		_, _, _ = regexparser.ParseLVal(src)
	}()
	select {
	case <-done:
		t.Logf("ParseLVal returned for %d unmatched parens in under 500ms; "+
			"if this is reproducible, the backtracking blow-up may be fixed -- "+
			"re-check the curve in skipReason and consider re-enabling FuzzParseLVal", depth)
	case <-time.After(500 * time.Millisecond):
		// Expected: the parse is still running.  Leave it; the goroutine is
		// bounded by the test binary's lifetime and burns one core for a few
		// seconds at most at this depth.
		t.Logf("ParseLVal has not returned for %d unmatched parens after 500ms, as expected", depth)
	}
}
