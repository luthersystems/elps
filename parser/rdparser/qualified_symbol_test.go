// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// TestQualifiedSymbolHalvesMustBeNames pins the accept/reject table for
// package-qualified symbols (issue #319).
//
// "pkg:name" is the qualified spelling of the symbol "name" inside package
// "pkg", so both halves have to be names the reader gives back as symbols on
// their own. They were not checked, and "a:1" got through -- the symbol named
// "1", which a bare 1 cannot spell and which docs/lang.md has always forbidden
// ("Identifiers cannot start with a number").
//
// Root cause is the one recorded in lexer.readFunRef for "#'0": isWord is the
// CONTINUATION rune class and admits digits, and readSymbol relies on having
// been entered after isWordStart or ':' already matched. Matching ':' is what
// admits the digit here.
func TestQualifiedSymbolHalvesMustBeNames(t *testing.T) {
	t.Parallel()

	// Rejected: the local half is not a name. Every one of these is a symbol
	// nothing else in the language can write.
	//
	// "a:1abc" and "a:0x10" are the ones a naive check misses: neither is a
	// number by strconv, but both start with a digit, so the reader gives them
	// back as an INT followed by a separate symbol rather than as one symbol.
	for _, src := range []string{
		"a:1", "a:1b", "a:-1", "a:1.5", "a:1e5", "a:0x10", "a:1abc", "a:1_",
		"a:9", "(a:1)", "'a:1", "#'a:1", "[a:1]", "(f a:1 b)",
	} {
		t.Run("rejected/"+src, func(t *testing.T) {
			t.Parallel()
			_, err := parseOne(t, src)
			require.Error(t, err, "%q must be refused: its local part is not a name", src)
			assert.Equal(t, lisp.CondInvalidSymbol, parseCondition(t, src),
				"rejection of %q should report the invalid-symbol condition", src)
		})
	}

	// Accepted: both halves are names. This half of the table is the guard
	// against over-tightening.
	//
	//   "a:+1" / "a:.1" / "a:*1"  -- '+', '.' and '*' are ordinary symbol
	//                                characters, so these ARE writable bare.
	//   "-a:b" / "a:-" / "a:--"   -- these lex as NEGATIVE and only become
	//                                symbols once ParseNegative merges them,
	//                                so a token-level check rejects them
	//                                wrongly.
	for _, src := range []string{
		"a:b", "lisp:set", "xyz:abc?", "a:+1", "a:.1", "a:*1", "a:_1",
		"a:true", "a:-", "a:--", "a:-a", "-a:b", "a:<=", "a:set!", "a:->",
	} {
		t.Run("accepted/"+src, func(t *testing.T) {
			t.Parallel()
			expr, err := parseOne(t, src)
			require.NoError(t, err, "%q is a valid qualified symbol", src)
			require.Equal(t, lisp.LSymbol, expr.Type)
			assert.Equal(t, src, expr.Str)

			// And it still round trips: printing and re-reading gives the
			// same symbol back.
			rt, err := parseOne(t, expr.String())
			require.NoError(t, err, "printed form %q must re-parse", expr.String())
			assert.Equal(t, lisp.LSymbol, rt.Type)
			assert.Equal(t, expr.Str, rt.Str)
		})
	}
}

// TestKeywordNamesAreNotIdentifiers pins the deliberate exemption for
// keywords, the other half of issue #319.
//
// ":1" is admitted by the same rune-class reuse as "a:1", but it is NOT the
// same thing. A leading ':' with no package yields a keyword: a self-evaluating
// literal that names no binding and that PutGlobal explicitly refuses to bind.
// The identifier rule governs names of bindings, so it has nothing to say
// about a keyword, and keywords are the only interned-literal facility the
// language has. Rejecting ":1" would remove a usable datum to buy uniformity.
//
// The one consequence worth knowing is pinned below: a keyword whose name is
// not a legal identifier can never match a &key formal, because no formal can
// be spelled that way.
func TestKeywordNamesAreNotIdentifiers(t *testing.T) {
	t.Parallel()

	for _, src := range []string{":1", ":1.5", ":-1", ":+1", ":1a", ":b", ":9"} {
		t.Run("accepted/"+src, func(t *testing.T) {
			t.Parallel()
			expr, err := parseOne(t, src)
			require.NoError(t, err, "%q is a keyword, not a qualified symbol", src)
			require.Equal(t, lisp.LSymbol, expr.Type)
			assert.Equal(t, src, expr.Str)
		})
	}

	// An empty name is still not a keyword, and neither is a doubled colon.
	for _, src := range []string{":", "::", "a:", "a:b:c"} {
		t.Run("rejected/"+src, func(t *testing.T) {
			t.Parallel()
			_, err := parseOne(t, src)
			assert.Error(t, err, "%q is not a symbol", src)
		})
	}
}

// TestDigitBeforeColonSplitsTokens records that "1:1" was never one symbol to
// begin with: the lexer stops the number at the ':' and starts a fresh token,
// so it reads as the INT 1 followed by the keyword :1. Pinned because it looks
// like a qualified symbol and is not one -- tightening the qualified-symbol
// rule does not (and should not) change it.
func TestDigitBeforeColonSplitsTokens(t *testing.T) {
	t.Parallel()

	exprs, err := rdparser.New(token.NewScannerString("test", "1:1")).ParseProgram()
	require.NoError(t, err)
	require.Len(t, exprs, 2)
	assert.Equal(t, lisp.LInt, exprs[0].Type)
	assert.Equal(t, 1, exprs[0].Int)
	assert.Equal(t, lisp.LSymbol, exprs[1].Type)
	assert.Equal(t, ":1", exprs[1].Str)
}

// TestQualifiedSymbolCheckIsMemoizedPerName guards the cache behind the check.
//
// Parser.readsBackAsSymbol memoizes per NAME, and both halves of every
// qualified symbol go through it, so a program that mentions a package
// repeatedly seeds the cache with entries that must not be mistaken for a
// verdict on the local half. "(a:b a:1)" is the shape that would break if the
// cache were keyed on the whole symbol, or consulted once per symbol rather
// than once per half: "a" is already cached as good when "a:1" is reached.
func TestQualifiedSymbolCheckIsMemoizedPerName(t *testing.T) {
	t.Parallel()

	_, err := rdparser.New(token.NewScannerString("test", "(a:b a:1)")).ParseProgram()
	require.Error(t, err, "a cached verdict for the package half must not excuse the local half")
	assert.Contains(t, err.Error(), `"1" does not read back as a symbol`)

	// ...and the mirror: a name cached as BAD must not condemn a later good
	// one that merely shares a package.
	exprs, err := rdparser.New(token.NewScannerString("test", "(a:b a:b a:c)")).ParseProgram()
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	require.Len(t, exprs[0].Cells, 3)
	assert.Equal(t, "a:c", exprs[0].Cells[2].Str)
}

// parseCondition returns the lisp condition reported by the parse error for
// src, or "" when src parses cleanly.
func parseCondition(t *testing.T, src string) string {
	t.Helper()
	_, err := rdparser.New(token.NewScannerString("test", src)).ParseProgram()
	if err == nil {
		return ""
	}
	var ev *lisp.ErrorVal
	require.ErrorAs(t, err, &ev)
	return ev.Condition()
}
