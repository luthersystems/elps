// Copyright © 2024 The ELPS authors

package lexer

import (
	"runtime"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

// escapedQuoteString returns a string literal that holds n escaped quotes.
func escapedQuoteString(n int) string {
	return `"` + strings.Repeat(`\"`, n) + `"`
}

// lexCount lexes src to the end and returns the number of tokens.
func lexCount(tb testing.TB, src string) int {
	lex := New(token.NewScannerString("bench.lisp", src))
	n := 0
	for {
		tok := lex.ReadToken()[0]
		if tok.Type == token.EOF {
			return n
		}
		if tok.Type == token.ERROR || tok.Type == token.INVALID {
			tb.Fatalf("lex error: %s", tok.Text)
		}
		n++
	}
}

// BenchmarkLexerEscapedQuotes lexes one string literal that holds 50000
// escaped quotes. The lexer used to copy the token scanned so far at each
// quote, so this input cost O(n^2) (luthersystems/elps#944).
func BenchmarkLexerEscapedQuotes(b *testing.B) {
	src := escapedQuoteString(50000)
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for b.Loop() {
		lexCount(b, src)
	}
}

// BenchmarkLexerStrings lexes ordinary source text that is rich in string
// literals with no escapes. Half of them are longer than 32 bytes, as a
// docstring is: the copy the lexer used to make of each string reached the
// heap only past that size.
func BenchmarkLexerStrings(b *testing.B) {
	src := strings.Repeat(`(defun f (x) "Return the value of x plus one, as an int." (g "name" "x"))
(error 'bad-argument "the first argument must be a sorted-map or an array")
`, 500)
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	for b.Loop() {
		lexCount(b, src)
	}
}

// TestStringLexingIsLinear pins luthersystems/elps#944 without a clock. One
// string literal is one token, so the number of allocations and the bytes
// copied must not grow with the number of escaped quotes in it. Before the
// fix, each escaped quote copied the token scanned so far: 10000 quotes cost
// 10000 more allocations and 100 MB.
func TestStringLexingIsLinear(t *testing.T) {
	// measure returns the allocations and the bytes allocated to lex one
	// literal with n escaped quotes.
	measure := func(n int) (int, int64) {
		src := escapedQuoteString(n)
		require.Equal(t, 1, lexCount(t, src))
		allocs := int(testing.AllocsPerRun(20, func() { lexCount(t, src) }))
		var before, after runtime.MemStats
		runtime.ReadMemStats(&before)
		lexCount(t, src)
		runtime.ReadMemStats(&after)
		return allocs, int64(after.TotalAlloc - before.TotalAlloc) //nolint:gosec // a byte count of one lexer run
	}
	smallAllocs, smallBytes := measure(100)
	largeAllocs, largeBytes := measure(10000)
	require.Equal(t, smallAllocs, largeAllocs, "allocations must not depend on the number of escaped quotes")
	// The source is 100 times longer. The lexer holds it in a buffer and
	// copies it into the token one time, so a linear lexer stays far below
	// the bound. The quadratic lexer needed 5000 times the source size.
	require.Less(t, largeBytes, 8*int64(len(escapedQuoteString(10000)))+smallBytes)
}
