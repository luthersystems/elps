// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// This file holds the COVERAGE property for semantic tokens, which is strictly
// stronger than the bounds property elps#428 / PR #448 added next door in
// semantic_tokens_bounds_test.go.
//
// Bounds asks only that a token name SOME text in the document.  Coverage asks
// that it name the RIGHT text: that the characters a token claims are the
// source atom the server is classifying, and not its neighbours or half of
// itself.  Both defects in elps#449 are in bounds and so invisible to the
// weaker property -- they are wrong-but-in-range highlighting, which is why the
// assertion PR #448 added to FuzzLSPSession does not go red on either.
//
// UNITS.  Byte offsets, because that is the unit the server emits: the scanner
// sets token.Location.Col to a byte offset within the line (Scanner.LocStart),
// elpsToLSPPosition passes it through unchanged, and position.go slices lines
// by byte throughout.  LSP 3.16 actually specifies UTF-16 code units by default
// and this server neither negotiates positionEncoding nor converts, so its
// columns are wrong for any non-ASCII line whatever this file asserts.  That is
// a server-wide gap, filed separately and not fixable by a length; what these
// tests can and do insist on is that a length be measured in the same unit as
// the column it is added to.

// tokenCoverage is one decoded token together with the source text it claims.
type tokenCoverage struct {
	tok     rawToken
	covered string
}

// semanticTokenDefects decodes a semanticTokens/full response back to absolute
// (line, startChar, length) triples and returns a description of every token
// that does not cover the source atom it is classifying.
//
// It checks:
//
//  1. BOUNDS -- the token lies inside its line (the property PR #448 added).
//  2. NO READER PUNCTUATION -- no token starts on a ' , which is the prefix
//     rdparser.applyPrefixLocation folds into a quoted atom's location.  PR
//     #448 decided a reader prefix gets no semantic token; this is that
//     decision expressed as a property (elps#449 part a).
//  3. ONE ATOM -- a non-string token contains no delimiter, so it names one
//     atom and not an atom plus whatever follows it.
//  4. WHOLE LITERALS -- a string token starts at a quote delimiter and ends at
//     the matching one, or, being the first line of a multi-line literal, runs
//     to the end of its line (elps#449 part b, and the escaped-newline
//     overshoot found while fixing it).
func semanticTokenDefects(content string, data []protocol.UInteger) []string {
	lines := strings.Split(content, "\n")
	var bad []string
	for _, tok := range decodeTokens(data) {
		if tok.line < 0 || tok.line >= len(lines) {
			bad = append(bad, fmt.Sprintf("token [%d:%d,+%d) is on line %d, but the document has %d lines",
				tok.line, tok.startChar, tok.length, tok.line, len(lines)))
			continue
		}
		// A CRLF document keeps its "\r"; the editor treats it as part of the
		// line and so does this.
		line := lines[tok.line]
		end := tok.startChar + tok.length
		if tok.startChar < 0 || tok.length < 0 || end > len(line) {
			bad = append(bad, fmt.Sprintf("token [%d:%d,+%d) overruns line %d (%d bytes: %q)",
				tok.line, tok.startChar, tok.length, tok.line, len(line), line))
			continue
		}
		c := tokenCoverage{tok: tok, covered: line[tok.startChar:end]}
		if msg := coverageDefect(c, line); msg != "" {
			bad = append(bad, fmt.Sprintf("token [%d:%d,+%d) covers %q: %s",
				tok.line, tok.startChar, tok.length, c.covered, msg))
		}
	}
	return bad
}

// coverageDefect returns "" if c covers a source atom, and otherwise says what
// is wrong with it.
//
// WHAT IS DELIBERATELY NOT ASSERTED: that a token's neighbours are delimiters.
// The obvious stronger rule -- "a token must not stop where a word character
// continues" -- is WRONG against this parser, because it is fault-tolerant and
// splits malformed input into genuinely adjacent atoms with nothing between
// them.  fuzzseed.All() has several: "1.2.3" reads as the float 1.2 followed by
// the symbol .3, and "0x" as the int 0 followed by the symbol x.  Both tokens
// name exactly the text they were computed from, so flagging them would be a
// false positive on input committed to this repository.  What is left below is
// checkable from the response alone and true of every correct token.
func coverageDefect(c tokenCoverage, line string) string {
	if c.tok.length == 0 {
		return "empty token"
	}
	if c.covered[0] == '\'' {
		// The reader's ' prefix.  It is punctuation the server has nothing to
		// say about; PR #448 suppressed the #' and #^ heads for exactly that
		// reason and cited ' as the precedent it was matching.
		return "starts on a ' , which is reader punctuation and not part of the atom"
	}
	if c.tok.tokenType == semTokenString {
		return stringLiteralDefect(c, line)
	}
	// Every other token names one atom, and an atom stops at a delimiter, so a
	// token containing one covers more than the thing it classifies.
	if i := strings.IndexAny(c.covered, " \t()[]\";'"); i >= 0 {
		return fmt.Sprintf("contains the delimiter %q at offset %d, so it spans more than one atom",
			c.covered[i:i+1], i)
	}
	return ""
}

// stringLiteralDefect checks a string token against the literal it claims.
//
// The server highlights only the FIRST LINE of a multi-line literal, so a token
// with no closing delimiter is admissible exactly when it runs to the end of
// its line.  Anything else means the length did not come from the source.
func stringLiteralDefect(c tokenCoverage, line string) string {
	var delim string
	switch {
	case strings.HasPrefix(c.covered, `"""`):
		delim = `"""`
	case strings.HasPrefix(c.covered, `"`):
		delim = `"`
	default:
		return "string token does not start at a quote delimiter"
	}
	raw := delim == `"""`
	body := c.covered[len(delim):]
	closeAt := -1
	for i := 0; i < len(body); {
		if !raw && body[i] == '\\' {
			i += 2
			continue
		}
		if strings.HasPrefix(body[i:], delim) {
			closeAt = i
			break
		}
		i++
	}
	if closeAt < 0 {
		if c.tok.startChar+c.tok.length == len(line) {
			return "" // first line of a multi-line literal
		}
		return "the literal is not closed and the token does not run to the end of the line"
	}
	if want := 2*len(delim) + closeAt; want != len(c.covered) {
		return fmt.Sprintf("the literal closes after %d characters but the token claims %d", want, len(c.covered))
	}
	return ""
}

// TestSemanticTokensCoverQuotedAtoms is the RED test for elps#449 part (a).
//
// rdparser.applyPrefixLocation moves a quoted atom's Col back onto the ' so
// that 'a reports the position a reader would point at, but the LSymbol case
// still took its length from len(v.Str), which does not include the quote, so
// the token started one character early and ended one character early.
//
// Run on ed2538c, the commit this branch is based on.  RED -- these failed:
//
//	'foo                    [0:0,+3) covers "'fo"
//	'a                      [0:0,+1) covers "'"
//	(list 'a 'b)            [0:6,+1) and [0:9,+1) each cover "'"
//	(error 'type-error 1)   [0:7,+10) covers "'type-erro"
//	'"x"                    [0:0,+3) covers "'\"x"
//	'42                     [0:0,+3) covers "'42"
//	' a  ' \n a  ':kw  'foo:bar  (f '(a b) 'c)
//
// GUARD -- '#^0 passed on ed2538c, where PR #448 put it, and still passes.
func TestSemanticTokensCoverQuotedAtoms(t *testing.T) {
	s := testServer()
	for i, content := range []string{
		"'foo",
		"'a",
		"(list 'a 'b)",
		"(error 'type-error 1)",
		`'"x"`,
		"'42",
		"' a",
		"'\na",
		"':kw",
		"'foo:bar",
		"(f '(a b) 'c)",
		"'#^0", // GUARD (PR #448)
	} {
		t.Run(fmt.Sprintf("%d/%q", i, content), func(t *testing.T) {
			data := tokensFor(t, s, fmt.Sprintf("file:///test/quoted%d.lisp", i), content)
			for _, msg := range semanticTokenDefects(content, data) {
				t.Errorf("%s", msg)
			}
		})
	}
}

// TestSemanticTokensQuotedAtomExactSpans pins the DECISION taken for elps#449
// part (a): the token covers the ATOM, and the ' gets no token at all, rather
// than the token being widened to cover "'a".
//
// This is the call PR #448 made for #^ and #' -- reader punctuation the server
// cannot say anything true about is left to the client's syntax grammar -- and
// #448 cited ' as the precedent it was being made consistent with.  Widening
// instead would have made that citation false, because the quote would then be
// painted with the SYMBOL's classification: variable-coloured in 'a,
// function-coloured in 'car, keyword-coloured in 'if.  None of those says
// anything about a quote.
//
// All RED on ed2538c.
func TestSemanticTokensQuotedAtomExactSpans(t *testing.T) {
	s := testServer()
	for i, tc := range []struct {
		content string
		want    []rawToken
	}{
		{"'a", []rawToken{{line: 0, startChar: 1, length: 1, tokenType: semTokenVariable}}},
		{"'foo", []rawToken{{line: 0, startChar: 1, length: 3, tokenType: semTokenVariable}}},
		{"'42", []rawToken{{line: 0, startChar: 1, length: 2, tokenType: semTokenNumber}}},
		{`'"x"`, []rawToken{{line: 0, startChar: 1, length: 3, tokenType: semTokenString}}},
		// Whitespace between the prefix and the atom is legal, and the atom
		// still gets its own token -- on its own line if it moved to one.
		{"' a", []rawToken{{line: 0, startChar: 2, length: 1, tokenType: semTokenVariable}}},
		{"'\na", []rawToken{{line: 1, startChar: 0, length: 1, tokenType: semTokenVariable}}},
		{"(list 'a 'b)", []rawToken{
			{line: 0, startChar: 1, length: 4, tokenType: semTokenFunction},
			{line: 0, startChar: 7, length: 1, tokenType: semTokenVariable},
			{line: 0, startChar: 10, length: 1, tokenType: semTokenVariable},
		}},
		// GUARD: a quoted LIST is not an atom.  The prefix was never inside a
		// child's span, and the children are unaffected either way.
		{"'(a b)", []rawToken{
			{line: 0, startChar: 2, length: 1, tokenType: semTokenVariable},
			{line: 0, startChar: 4, length: 1, tokenType: semTokenVariable},
		}},
	} {
		t.Run(fmt.Sprintf("%d/%q", i, tc.content), func(t *testing.T) {
			assertTokens(t, tokensFor(t, s, fmt.Sprintf("file:///test/qspan%d.lisp", i), tc.content), tc.want)
		})
	}
}

// TestSemanticTokensCoverStringLiterals is the RED test for elps#449 part (b).
//
// The LString case computed len(v.Str)+2, and v.Str is the string AFTER escape
// processing, so every escape sequence was counted as the bytes it decodes to.
// Run on ed2538c.  RED:
//
//	(f "x\ty")     [0:3,+5) covers "\"x\\ty"      -- 1 short
//	"a\tb\tc"      [0:0,+7) covers "\"a\\tb\\t"   -- 2 short
//	"\U0001F600"   [0:0,+6) covers "\"\\U000"     -- 6 short
//	"""raw"""      [0:0,+5) covers "\"\"\"ra"     -- 4 short, the """ delimiters
//	(f """r""" c)  [0:3,+3) covers "\"\"\""
//	(f "a\nb" c)   [0:3,+9) covers "\"a\\nb\" c)" -- 3 LONG, see below
//
// The last row is a third symptom, not named in elps#449 and found by running
// this property while fixing it: the multi-line branch tested the DECODED value
// for a newline, so a single-line literal containing the ESCAPE \n took that
// branch and was handed "the rest of the line", running past its own closing
// quote and over everything after it.  It is an OVERSHOOT, which elps#449 says
// cannot happen here ("Always an undershoot").
//
// GUARD -- these passed on ed2538c: "x", "é", "😀", a genuine multi-line raw
// literal, and an unterminated "abc (which produces no token at all).
func TestSemanticTokensCoverStringLiterals(t *testing.T) {
	s := testServer()
	for i, content := range []string{
		`(f "x\ty")`,
		`"a\tb\tc"`,
		`"\U0001F600"`,
		`(f "a\nb" c)`,
		`"""raw"""`,
		`(f """r""" c)`,
		`"x"`,                  // GUARD
		`"é"`,                  // GUARD
		`"😀"`,                  // GUARD
		"\"\"\"a\nb\"\"\"",     // GUARD
		"\"\"\"é\nb\"\"\"",     // GUARD
		"(f \"\"\"a\nb\"\"\")", // GUARD
		`"abc`,                 // GUARD: no token at all
	} {
		t.Run(fmt.Sprintf("%d/%q", i, content), func(t *testing.T) {
			data := tokensFor(t, s, fmt.Sprintf("file:///test/str%d.lisp", i), content)
			for _, msg := range semanticTokenDefects(content, data) {
				t.Errorf("%s", msg)
			}
		})
	}
}

// TestSemanticTokensStringExactSpans pins the lengths part (b)'s fix produces,
// including the ones that were already right so that a later change cannot
// trade one class of literal for another.
func TestSemanticTokensStringExactSpans(t *testing.T) {
	s := testServer()
	for i, tc := range []struct {
		content string
		want    []rawToken
	}{
		{`"x"`, []rawToken{{line: 0, startChar: 0, length: 3, tokenType: semTokenString}}},           // GUARD
		{`"x\ty"`, []rawToken{{line: 0, startChar: 0, length: 6, tokenType: semTokenString}}},        // RED: was 5
		{`"a\tb\tc"`, []rawToken{{line: 0, startChar: 0, length: 9, tokenType: semTokenString}}},     // RED: was 7
		{`"\U0001F600"`, []rawToken{{line: 0, startChar: 0, length: 12, tokenType: semTokenString}}}, // RED: was 6
		{`"""raw"""`, []rawToken{{line: 0, startChar: 0, length: 9, tokenType: semTokenString}}},     // RED: was 5
		// RED: the escaped \n took the multi-line branch and swallowed the rest
		// of the line.
		{`(f "a\nb" c)`, []rawToken{
			{line: 0, startChar: 1, length: 1, tokenType: semTokenVariable},
			{line: 0, startChar: 3, length: 6, tokenType: semTokenString},
			{line: 0, startChar: 10, length: 1, tokenType: semTokenVariable},
		}},
		// GUARD: measured in BYTES, like the columns they are added to.  "é" is
		// four bytes of source and "😀" six, and len(v.Str)+2 already got both
		// right, so a fix in some other unit would have broken them.
		{`"é"`, []rawToken{{line: 0, startChar: 0, length: 4, tokenType: semTokenString}}},
		{`"😀"`, []rawToken{{line: 0, startChar: 0, length: 6, tokenType: semTokenString}}},
		// GUARD: a genuinely multi-line literal still gets its first line only.
		{"\"\"\"a\nb\"\"\"", []rawToken{{line: 0, startChar: 0, length: 4, tokenType: semTokenString}}},
		{"\"\"\"é\nb\"\"\"", []rawToken{{line: 0, startChar: 0, length: 5, tokenType: semTokenString}}},
	} {
		t.Run(fmt.Sprintf("%d/%q", i, tc.content), func(t *testing.T) {
			assertTokens(t, tokensFor(t, s, fmt.Sprintf("file:///test/sspan%d.lisp", i), tc.content), tc.want)
		})
	}
}

// TestSemanticTokensNonASCIISymbolSpans is a GUARD.  All of it passed on
// ed2538c and all of it must keep passing.
//
// It is here because the obvious way to write elps#449's fix -- take the length
// from Location.EndCol-Col, the way the number case did -- is WRONG, and this
// is the test that says so.  TokenEnd derives EndCol by counting RUNES onto a
// Col that counts BYTES, so EndCol-Col is a rune width while every column
// beside it is a byte offset.  For "déf" that is 3 against a 4-byte symbol.
// The fix uses EndPos-Pos, which is byte-to-byte, instead.
//
// (EndCol being in neither unit is a defect in its own right, filed separately;
// elpsToLSPRange uses it directly, so it is not only this file's problem.)
func TestSemanticTokensNonASCIISymbolSpans(t *testing.T) {
	s := testServer()
	for i, tc := range []struct {
		content string
		want    []rawToken
	}{
		{"déf", []rawToken{{line: 0, startChar: 0, length: 4, tokenType: semTokenVariable}}},
		{"(déf 1)", []rawToken{
			{line: 0, startChar: 1, length: 4, tokenType: semTokenVariable},
			{line: 0, startChar: 6, length: 1, tokenType: semTokenNumber},
		}},
		{"(f é x)", []rawToken{
			{line: 0, startChar: 1, length: 1, tokenType: semTokenVariable},
			{line: 0, startChar: 3, length: 2, tokenType: semTokenVariable},
			{line: 0, startChar: 6, length: 1, tokenType: semTokenVariable},
		}},
		// RED on ed2538c: the quote, not the guard.
		{"'déf", []rawToken{{line: 0, startChar: 1, length: 4, tokenType: semTokenVariable}}},
	} {
		t.Run(fmt.Sprintf("%d/%q", i, tc.content), func(t *testing.T) {
			assertTokens(t, tokensFor(t, s, fmt.Sprintf("file:///test/uni%d.lisp", i), tc.content), tc.want)
		})
	}
}

// TestSemanticTokensCoverSeedCorpus runs the coverage property over every seed
// committed to this repository, which is how elps#428 was found.
//
// RED on ed2538c on 22 of them (433 offending tokens), including ELPS sources
// ships: (in-package 'foo), (error 'type-error ...) and (set 'method-table ...)
// all put the token on the quote.
func TestSemanticTokensCoverSeedCorpus(t *testing.T) {
	s := testServer()
	for i, seed := range fuzzseed.All() {
		content := string(seed)
		data := tokensFor(t, s, fmt.Sprintf("file:///test/cseed%d.lisp", i), content)
		for _, msg := range semanticTokenDefects(content, data) {
			t.Errorf("seed %d %q: %s", i, content, msg)
		}
	}
}

// TestSemanticTokensPrefixesEmitNoToken states, for all three reader prefixes
// at once, what PR #448's TestSemanticTokensQuotePrefixEmitsNoToken meant to
// say.  That test asserted only that no token was exactly [0:0,+1) -- which
// 'foo passed while covering "'fo" -- so it could not have caught part (a).
//
// GUARD for #^ and #' (PR #448 landed those); RED on ed2538c for ' .
func TestSemanticTokensPrefixesEmitNoToken(t *testing.T) {
	s := testServer()
	for i, content := range []string{
		"'foo", "'a", "'42", `'"x"`,
		"#'foo", "#^a", // GUARD
		"(list 'a #'car #^b)",
	} {
		t.Run(fmt.Sprintf("%d/%q", i, content), func(t *testing.T) {
			data := tokensFor(t, s, fmt.Sprintf("file:///test/prefix%d.lisp", i), content)
			for _, tok := range decodeTokens(data) {
				if tok.line != 0 {
					continue
				}
				for j := tok.startChar; j < tok.startChar+tok.length && j < len(content); j++ {
					if content[j] == '\'' ||
						(content[j] == '#' && j+1 < len(content) && (content[j+1] == '\'' || content[j+1] == '^')) {
						t.Errorf("token [%d:%d,+%d) covers reader punctuation at byte %d of %q",
							tok.line, tok.startChar, tok.length, j, content)
					}
				}
			}
		})
	}
}

// assertTokens compares a decoded token stream against an exact expectation.
func assertTokens(t *testing.T, data []protocol.UInteger, want []rawToken) {
	t.Helper()
	got := decodeTokens(data)
	if len(got) != len(want) {
		t.Fatalf("got %d tokens %+v, want %d %+v", len(got), got, len(want), want)
	}
	for i := range got {
		if got[i].line != want[i].line || got[i].startChar != want[i].startChar ||
			got[i].length != want[i].length || got[i].tokenType != want[i].tokenType {
			t.Errorf("token %d: got %+v, want %+v", i, got[i], want[i])
		}
	}
}
