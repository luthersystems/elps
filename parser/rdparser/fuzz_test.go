// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"bytes"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// The recursive-descent parser is the only thing standing between untrusted
// source text and the interpreter.  In substrate a phylum is customer-supplied
// source executed as Fabric chaincode, so a parser panic, a stack overflow or
// a non-terminating token loop is a chaincode-process crash.  These targets
// assert the three properties that hold for ALL input, valid or not:
//
//  1. ParseProgram terminates without panicking.
//  2. A successful parse never yields a nil or LError expression -- Parse
//     converts LError into a Go error before returning (parser.go: Parse).
//  3. ParseProgramFaultTolerant agrees with ParseProgram whenever ParseProgram
//     succeeds.  Both drive the identical p.Parse() loop and the recovery
//     branch is only reachable from an error, so a clean parse must produce
//     the same expressions and no errors.
//
// Nothing here asserts that a given input parses: most fuzzer-generated input
// is garbage and a parse error is the correct answer.

func newParser(src []byte) *rdparser.Parser {
	return rdparser.New(token.NewScanner("fuzz", bytes.NewReader(src)))
}

func addSeeds(f *testing.F) {
	f.Helper()
	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
}

// FuzzParseProgram fuzzes the standard (non-recovering) entry point used by
// lisp.Runtime to read source.
func FuzzParseProgram(f *testing.F) {
	addSeeds(f)
	f.Fuzz(func(t *testing.T, src []byte) {
		exprs, err := newParser(src).ParseProgram()
		if err != nil {
			if len(exprs) != 0 {
				t.Fatalf("ParseProgram returned %d expressions alongside error %v", len(exprs), err)
			}
			return
		}
		for i, expr := range exprs {
			if expr == nil {
				t.Fatalf("ParseProgram returned a nil expression at index %d", i)
			}
			if expr.Type == lisp.LError {
				t.Fatalf("ParseProgram returned an LError expression at index %d without an error: %v", i, expr)
			}
			// String() walks the whole tree; it must not panic either.  A
			// parsed value is rendered by every error path in the
			// interpreter, so an unprintable AST is as fatal as an unparsable
			// one.
			_ = expr.String()
		}
	})
}

// FuzzParseProgramFaultTolerant fuzzes the error-recovering entry point used
// by the LSP and the linter, and cross-checks it against ParseProgram.
func FuzzParseProgramFaultTolerant(f *testing.F) {
	addSeeds(f)
	f.Fuzz(func(t *testing.T, src []byte) {
		result := newParser(src).ParseProgramFaultTolerant()
		for i, expr := range result.Exprs {
			if expr == nil {
				t.Fatalf("fault-tolerant parse returned a nil expression at index %d", i)
			}
			if expr.Type == lisp.LError {
				t.Fatalf("fault-tolerant parse returned an LError in Exprs at index %d: %v", i, expr)
			}
			_ = expr.String()
		}

		// Agreement with the strict parser.  Only checked in the direction
		// that is actually guaranteed: a clean strict parse implies a clean
		// recovering parse over the same tokens.  The converse does not hold
		// -- recovery exists precisely to salvage expressions the strict
		// parser abandons.
		strict, err := newParser(src).ParseProgram()
		if err != nil {
			return
		}
		if len(result.Errors) != 0 {
			t.Fatalf("ParseProgram succeeded but fault-tolerant parse reported %d errors: %v",
				len(result.Errors), result.Errors[0])
		}
		if len(result.Exprs) != len(strict) {
			t.Fatalf("expression count mismatch: strict=%d fault-tolerant=%d", len(strict), len(result.Exprs))
		}
		for i := range strict {
			if got, want := result.Exprs[i].String(), strict[i].String(); got != want {
				t.Fatalf("expression %d mismatch:\n strict = %s\n tolerant = %s", i, want, got)
			}
		}
	})
}

// FuzzParseFormatting fuzzes the format-preserving parser mode used by
// `elps fmt`, the LSP and the minifier.  It builds far more metadata than the
// standard mode (comment attachment, bracket kinds, blank-line runs), all of
// which is driven by token bookkeeping that untrusted input can skew.
func FuzzParseFormatting(f *testing.F) {
	addSeeds(f)
	f.Fuzz(func(t *testing.T, src []byte) {
		p := rdparser.NewFormatting(token.NewScanner("fuzz", bytes.NewReader(src)))
		exprs, err := p.ParseProgram()
		if err != nil {
			return
		}
		for i, expr := range exprs {
			if expr == nil {
				t.Fatalf("formatting parse returned a nil expression at index %d", i)
			}
			_ = expr.String()
		}
		// PendingComments is read by the formatter after every parse; it must
		// not hand back nil entries.
		for i, tok := range p.PendingComments() {
			if tok == nil {
				t.Fatalf("PendingComments returned a nil token at index %d", i)
			}
		}

		// The format-preserving parse must recover the same program as the
		// standard parse.  Formatting mode only ATTACHES metadata; if it ever
		// changed the tree, `elps fmt` would silently rewrite semantics.
		// (formatter.roundTripEqual asserts the same equality for repository
		// files -- this generalises it to arbitrary input.)
		strict, strictErr := newParser(src).ParseProgram()
		if strictErr != nil {
			t.Fatalf("formatting parse succeeded but standard parse failed: %v", strictErr)
		}
		if len(strict) != len(exprs) {
			t.Fatalf("expression count mismatch: standard=%d formatting=%d", len(strict), len(exprs))
		}
		for i := range strict {
			if got, want := exprs[i].String(), strict[i].String(); got != want {
				t.Fatalf("expression %d mismatch:\n standard = %s\n formatting = %s", i, want, got)
			}
		}
	})
}

// TestParserDepthGuardHoldsForNestedInput pins the behaviour the fuzz targets
// depend on to stay bounded: deeply nested input must produce a parse error
// rather than a fatal stack overflow, which recover() cannot catch.
func TestParserDepthGuardHoldsForNestedInput(t *testing.T) {
	src := []byte(strings.Repeat("(", rdparser.DefaultMaxParseDepth+10))
	_, err := newParser(src).ParseProgram()
	if err == nil {
		t.Fatal("expected a depth error for input nested beyond DefaultMaxParseDepth")
	}
	if !strings.Contains(err.Error(), "maximum depth") {
		t.Fatalf("expected a maximum-depth error, got: %v", err)
	}
}

// TestParserSurvivesPathologicalInput runs the oversized regression inputs
// that are deliberately kept OUT of the fuzz seed corpus (they would throttle
// the mutator) through every parser entry point.  The property asserted is
// termination: each parse must finish, with or without an error, and must not
// panic.  Both of the defects these targets found -- the lexer spinning on an
// undecodable byte and the scanner indexing a discarded buffer position --
// presented as a hang or a panic here, not as a wrong parse.
func TestParserSurvivesPathologicalInput(t *testing.T) {
	modes := map[string]func([]byte){
		"ParseProgram": func(src []byte) {
			_, _ = newParser(src).ParseProgram()
		},
		"ParseProgramFaultTolerant": func(src []byte) {
			_ = newParser(src).ParseProgramFaultTolerant()
		},
		"ParseProgramFormatting": func(src []byte) {
			p := rdparser.NewFormatting(token.NewScanner("fuzz", bytes.NewReader(src)))
			_, _ = p.ParseProgram()
		},
	}
	for name, src := range fuzzseed.Pathological() {
		for mode, parse := range modes {
			t.Run(name+"/"+mode, func(t *testing.T) {
				done := make(chan struct{})
				go func() {
					defer close(done)
					parse(src)
				}()
				select {
				case <-done:
				case <-time.After(30 * time.Second):
					// A leaked goroutine is acceptable here: the run has
					// already failed and the alternative is a hung CI job.
					t.Fatalf("%s did not terminate on %q", mode, name)
				}
			})
		}
	}
}
