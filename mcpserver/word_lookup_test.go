// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/symtext"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWordAtPositionAmpersand pins the argument-marker symbols. "&" is a
// symbol character in ELPS -- (defun f (&rest xs) ...) names the symbol
// "&rest", not "rest" -- and the language server has always treated it as
// one. The MCP server's private copy of the alphabet omitted it, so a cursor
// anywhere in "&rest" answered for a symbol that does not exist (elps#654).
func TestWordAtPositionAmpersand(t *testing.T) {
	const content = "(defun f (&rest xs) xs)"
	amp := strings.Index(content, "&rest")
	require.GreaterOrEqual(t, amp, 0)

	// Every column of "&rest", including the "&" itself and the end of the
	// word, names the whole symbol.
	for _, off := range []int{0, 1, 2, 5} {
		t.Run("&rest+"+string(rune('0'+off)), func(t *testing.T) {
			assert.Equal(t, "&rest", wordAtPosition(content, 0, amp+off))
		})
	}

	t.Run("&optional", func(t *testing.T) {
		src := "(defun f (&optional x) x)"
		assert.Equal(t, "&optional", wordAtPosition(src, 0, strings.Index(src, "&optional")+1))
	})
	t.Run("&key", func(t *testing.T) {
		src := "(defun f (&key x) x)"
		assert.Equal(t, "&key", wordAtPosition(src, 0, strings.Index(src, "&key")+1))
	})
	t.Run("lone ampersand", func(t *testing.T) {
		assert.Equal(t, "&", wordAtPosition("(& 1)", 0, 1))
	})
}

// TestWordAtPositionAllocs is the allocation half of elps#654. The MCP copy
// split the whole document on "\n" for every hover and definition request, so
// the cost of one lookup was linear in the document rather than in the line.
// The language server fixed the same thing in elps#641/#653; sharing the
// helper brings the MCP server to the same zero.
//
// The word is asserted non-empty so that an implementation that always
// returned "" -- which allocates nothing at all -- cannot pass this test.
func TestWordAtPositionAllocs(t *testing.T) {
	const lines = 1 << 14
	content := strings.Repeat("target\n", lines)

	for _, tc := range []struct {
		name string
		line int
	}{
		{"first line", 0},
		{"last line", lines - 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var got string
			allocs := testing.AllocsPerRun(10, func() {
				got = wordAtPosition(content, tc.line, 3)
			})
			assert.Equal(t, "target", got, "the lookup must actually find the word")
			assert.Zero(t, allocs, "word lookup must not allocate")
		})
	}
}

// TestSymbolAlphabetParity checks that the MCP server and the shared helper
// agree about which bytes make up a symbol, for every byte value there is.
//
// The MCP alphabet is probed through wordAtPosition rather than read from a
// private function, because after elps#654 the MCP server has no alphabet of
// its own: the point of the test is that the two cannot drift apart again.
func TestSymbolAlphabetParity(t *testing.T) {
	// The documented ELPS symbol alphabet, as lsp has always spelled it.
	const punct = "-_!?+*/<>=:.#^&"
	want := func(c byte) bool {
		switch {
		case c >= 'a' && c <= 'z', c >= 'A' && c <= 'Z', c >= '0' && c <= '9':
			return true
		}
		return strings.IndexByte(punct, c) >= 0
	}

	assert.True(t, symtext.IsSymbolChar('&'),
		"& is a symbol character: (defun f (&rest xs)) names the symbol &rest")

	for i := range 256 {
		c := byte(i)
		assert.Equalf(t, want(c), symtext.IsSymbolChar(c),
			"symtext.IsSymbolChar(%q)", c)

		// wordAtPosition finds a one-byte word exactly when that byte is a
		// symbol character.
		found := wordAtPosition(string([]byte{c}), 0, 0) != ""
		assert.Equalf(t, symtext.IsSymbolChar(c), found,
			"mcpserver word lookup disagrees with symtext on %q", c)
	}
}

// TestWordAtPositionBoundaries pins every out-of-range and edge input. These
// are the answers the language server gives today; sharing the helper must
// not change one of them.
//
// COLUMNS ARE BYTES throughout, not runes and not UTF-16 code units: a
// multi-byte character to the left of the cursor shifts the column by its
// byte width. The MCP protocol surface documents byte columns, so this is
// the contract, not an accident.
func TestWordAtPositionBoundaries(t *testing.T) {
	const doc = "(defun add (x y)\n  (+ x y))"

	tests := []struct {
		name    string
		content string
		line    int
		col     int
		want    string
	}{
		{"line -1", doc, -1, 0, ""},
		{"line == line count", doc, 2, 0, ""},
		{"line far beyond end", doc, 9999, 0, ""},
		{"col -1", doc, 0, -1, ""},
		{"col == len(line)", doc, 0, len("(defun add (x y)"), ""},
		{"col > len(line)", doc, 0, 100, ""},
		{"empty content", "", 0, 0, ""},
		{"empty content, line 1", "", 1, 0, ""},

		// A trailing newline makes one more (empty) line, which holds no word.
		{"trailing newline, last line", "abc\n", 1, 0, ""},
		{"trailing newline, word line", "abc\n", 0, 1, "abc"},

		// CRLF: lines are split on "\n", so the "\r" stays on the end of the
		// line. It is not a symbol character, so it bounds the word -- and a
		// cursor at end of line therefore sits ON the "\r" and finds nothing,
		// where the same cursor on an LF line finds the word. This is what
		// lsp does today and is asserted, not fixed, here.
		{"CRLF word", "abc\r\ndef", 0, 1, "abc"},
		{"CRLF cursor on CR", "abc\r\ndef", 0, 3, "abc"},
		{"CRLF cursor at end of line", "abc\r\ndef", 0, 4, ""},
		{"CRLF second line", "abc\r\ndef", 1, 0, "def"},

		// Invalid UTF-8 is just a non-symbol byte; it bounds a word and is
		// never part of one.
		{"invalid utf8 at cursor", "\xff foo", 0, 0, ""},
		{"invalid utf8 bounds word", "ab\xffcd", 0, 0, "ab"},
		{"invalid utf8 after word", "ab\xffcd", 0, 3, "cd"},

		// Byte columns: "é" is two bytes, so "foo" starts at byte 3.
		{"multibyte before cursor", "é foo", 0, 3, "foo"},
		{"multibyte before cursor, end of word", "é foo", 0, 6, "foo"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assert.Equal(t, tt.want, wordAtPosition(tt.content, tt.line, tt.col))
		})
	}
}

// TestHoverAmpersandQualifiedSymbol is the real-path half: "&" is legal in
// ELPS package and symbol names, so the missing alphabet entry did not only
// mis-name argument markers -- it broke the textual fallback that hover and
// definition fall back on when analysis resolves no symbol at the cursor.
//
// The reference is quoted, which is a shape analysis records no reference
// for, so both tools reach wordAtPosition. With "&" absent from the alphabet
// the cursor in "p&q:a&b" yielded "q:a" -- a package that does not exist and
// a symbol that does not exist -- so both answered found=false.
func TestHoverAmpersandQualifiedSymbol(t *testing.T) {
	tmp := t.TempDir()
	libPath := filepath.Join(tmp, "lib.lisp")
	mainPath := filepath.Join(tmp, "main.lisp")
	writeTestFile(t, libPath, "(in-package 'p&q)\n(export 'a&b)\n(defun a&b (x) (+ x 1))")
	mainContent := "(defun run () 'p&q:a&b)"
	writeTestFile(t, mainPath, mainContent)

	srv := New(WithWorkspaceRoot(tmp))
	col := strings.Index(mainContent, "a&b")
	require.GreaterOrEqual(t, col, 0)

	doc, _, err := srv.service.loadDocument(mainPath, nil, &tmp)
	require.NoError(t, err)
	sym, _ := symbolAtPosition(doc, 0, col)
	require.Nil(t, sym, "the test is only meaningful when the textual fallback is reached")

	_, hover, err := srv.service.hoverTool(context.Background(), nil, FileQueryInput{
		Path:      mainPath,
		Line:      0,
		Character: col,
	})
	require.NoError(t, err)
	require.True(t, hover.Found, "hover must resolve the &-bearing qualified symbol")
	assert.Equal(t, "a&b", hover.SymbolName)

	_, def, err := srv.service.definitionTool(context.Background(), nil, FileQueryInput{
		Path:      mainPath,
		Line:      0,
		Character: col,
	})
	require.NoError(t, err)
	require.True(t, def.Found, "definition must resolve the &-bearing qualified symbol")
	require.NotNil(t, def.Location)
	assert.Equal(t, libPath, def.Location.Path)
}
