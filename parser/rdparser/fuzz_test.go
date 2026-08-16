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
		// Property 4: nothing the reader hands back carries a SYNTHETIC
		// source location.  lisp.stampMacroExpansion rewrites Source on every
		// expanded node whose location is nil or has Pos < 0, and macro
		// arguments reach the expansion as the caller's own parse-tree nodes
		// -- so a synthetic location here is a node the interpreter writes
		// into, concurrently, across environments that share the tree
		// (elps#370).  Stated over the whole tree rather than over the two
		// heads that broke it, so the next desugaring is covered too.
		assertRealSourceLocations(t, string(src), exprs)
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
		// The elps#370 invariant again, this time for the mode the LSP and
		// `elps fmt` parse in.  It builds the metadata-carrying tree that is
		// held across requests, so a stampable node here is the longest-lived
		// one there is.
		assertRealSourceLocations(t, string(src), exprs)
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

// FuzzParsedLocationInvariants states, over arbitrary input, the position
// properties the reader has to satisfy.  Three are the ones elps#426 broke; the
// fourth is elps#463.  For every node the reader produces:
//
//  1. It owns its *token.Location.  No two distinct nodes in one parse tree
//     may hold the same object.  Sharing is what let applyPrefixLocation move
//     a prefix form and drag its operand along, and it is a live hazard
//     independently of that: lsp/, lint/ and lisp.stampMacroExpansion all walk
//     the tree writing positions, and a shared Location turns any one of those
//     writes into an edit of an unrelated node's reported position.
//
//  2. Its span lies inside the source.  0 <= Pos <= EndPos <= len(src), so
//     slicing the source by a reported span cannot panic -- which the LSP,
//     the diagnostic renderer and `elps fmt` all effectively do.
//
//  3. Its span lies inside its parent's.  A child that escapes its parent
//     produces an inverted or negative LSP range.  This is the property that
//     detected the second-order damage: once applyPrefixLocation had moved a
//     token's Location, token.TokenEnd computed the NEXT node's end position
//     by walking the token text forward from the MOVED column, so "'#'car"
//     reported its outer form as ending four columns before its own operand
//     did.
//
//  4. Its column span agrees with its byte span.  On a node that begins and
//     ends on ONE line, EndCol-Col == EndPos-Pos.  This is elps#463: TokenEnd
//     derived EndCol by counting RUNES onto the byte-valued Col that
//     Scanner.LocStart computes, so on a token holding any multi-byte rune the
//     two ends of a single Location were counted in different units and EndCol
//     was short by len(text)-runeCount(text).
//
//     It belongs HERE, at the reader, rather than only in the LSP tests where
//     the damage showed up.  EndCol has consumers in lsp/, lint/, analysis/ and
//     mcpserver/ that each add it to or compare it against a byte column, and
//     one wrong producer is what made all of them wrong at once; a property
//     stated at the producer covers the consumers that exist and the ones that
//     have not been written.  The rename corruption it caused is pinned
//     separately and end-to-end in
//     lsp.TestRenameNonASCIIIdentifierRewritesWholeName -- an integer here, the
//     user's file there.
//
//     Note what this does NOT say: which unit.  It says the two ends agree,
//     which they must whatever elps#464 concludes about the UTF-16 code units
//     LSP asks for on the wire.  A multi-line node is exempt because its column
//     restarts at its last newline, so the two spans measure different things
//     by construction.
//
// Locations are checked only for nodes that carry a real one (Pos >= 0);
// synthetic locations are a separate invariant, pinned by
// assertRealSourceLocations and TestParserEmitsNoSyntheticSourceLocations.
func FuzzParsedLocationInvariants(f *testing.F) {
	addSeeds(f)
	for _, s := range []string{
		"'a", "''a", "'''''test", "#'car", "#^a", "'#'car", "'#^a", "''#^a",
		"#^#'a", "#^(+ %1 1)", "(quote x)", "(map 'list #'car '((1 2)))",
		"(quasiquote ''(unquote-splicing '(+ 2 3)))",
		// NON-ASCII, for invariant 4 (elps#463).  Seeded deliberately and not
		// relied on from fuzzseed.All(), for the reason PR #462 had to seed the
		// escaped-string case by hand: an assertion no seed reaches is not an
		// assertion, and mutation alone is a poor way to arrive at well-formed
		// multi-byte UTF-8 inside an identifier.  Two, three and four byte
		// runes, in a symbol, a keyword, a string, a comment, a package
		// qualifier, quoted and prefixed forms, and spanning a line break.
		"(defun éx (a) a)\n(éx 1)",
		"(set λ 1)\nλ",
		"(defun 加算 (a b) (+ a b))\n(加算 1 2)",
		"(defun 𝛼𝛽 (a) a)\n(𝛼𝛽 1)",
		"'éx", "#'éx", "#^éx", "''é",
		":é", "é:ê", "(é:ê 1)",
		`"é"`, `"a\té"`, `"""é\nê"""`,
		"; é\n(f é)",
		"(f é) ; ê",
		"(défun f (é ê) (+ é ê))",
		"(f\n  é\n  ê)",
		"é\r\nê",
		"\té\t(f é)",
	} {
		f.Add([]byte(s))
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		for _, formatting := range []bool{false, true} {
			sc := token.NewScanner("fuzz", bytes.NewReader(src))
			p := rdparser.New(sc)
			if formatting {
				p = rdparser.NewFormatting(sc)
			}
			exprs, err := p.ParseProgram()
			if err != nil {
				continue
			}
			owner := make(map[*token.Location]*lisp.LVal)
			var check func(v, parent *lisp.LVal)
			check = func(v, parent *lisp.LVal) {
				loc := v.Source
				if loc == nil {
					return
				}
				if prev, dup := owner[loc]; dup {
					t.Fatalf("formatting=%v: nodes %v %q and %v %q share one *token.Location %v; every node must own its position (#426)",
						formatting, prev.Type, prev.Str, v.Type, v.Str, loc)
				}
				owner[loc] = v
				if loc.Pos >= 0 && loc.EndPos > 0 {
					if loc.EndPos < loc.Pos || loc.EndPos > len(src) {
						t.Fatalf("formatting=%v: node %v %q reports span [%d,%d) outside a %d-byte source (#426)",
							formatting, v.Type, v.Str, loc.Pos, loc.EndPos, len(src))
					}
					if parent != nil && parent.Source != nil &&
						parent.Source.Pos >= 0 && parent.Source.EndPos > 0 {
						if loc.Pos < parent.Source.Pos || loc.EndPos > parent.Source.EndPos {
							t.Fatalf("formatting=%v: child %v %q spans [%d,%d), escaping parent %v %q span [%d,%d) (#426)",
								formatting, v.Type, v.Str, loc.Pos, loc.EndPos,
								parent.Type, parent.Str, parent.Source.Pos, parent.Source.EndPos)
						}
					}
				}
				// Invariant 4 (elps#463): on a single-line node the column
				// span and the byte span are the same span, so they have the
				// same width.  Guarded on all four fields being tracked --
				// the fault-tolerant parser can leave end positions unset.
				if loc.Line > 0 && loc.Col > 0 && loc.EndLine == loc.Line && loc.EndCol > 0 && loc.EndPos > 0 {
					if colSpan, byteSpan := loc.EndCol-loc.Col, loc.EndPos-loc.Pos; colSpan != byteSpan {
						t.Fatalf("formatting=%v: node %v %q at %v spans %d columns (Col %d..EndCol %d) but %d bytes (Pos %d..EndPos %d); Location's ends are in different units (#463)",
							formatting, v.Type, v.Str, loc, colSpan, loc.Col, loc.EndCol, byteSpan, loc.Pos, loc.EndPos)
					}
				}
				for _, c := range v.Cells {
					check(c, v)
				}
			}
			for _, e := range exprs {
				check(e, nil)
			}
		}
	})
}
