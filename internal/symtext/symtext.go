// Copyright © 2026 The ELPS authors

// Package symtext is the single definition of what an ELPS symbol looks like
// as TEXT: which bytes make up one, where the one under a cursor starts and
// ends, and how to reach a line of a document without taking the document
// apart.
//
// Two servers answer cursor questions about a document -- the language server
// (package lsp) and the MCP server (package mcpserver) -- and each carried its
// own copy of this code. The copies drifted, in both of the ways a duplicated
// helper drifts (elps#654):
//
//   - CORRECTNESS. "&" is a symbol character in ELPS: (defun f (&rest xs) ...)
//     names the symbol "&rest", and "&" is legal inside package and symbol
//     names besides. The lsp copy knew that; the MCP copy did not, so a cursor
//     in "&rest" answered for "rest", and a cursor in "p&q:a&b" answered for
//     "q:a".
//   - COST. The lsp copy walks to the requested line and stops (elps#641,
//     elps#653); the MCP copy split the whole document on "\n" first, so one
//     hover on a 112 KiB file allocated 270 KiB to read one line.
//
// This package is the lsp code MOVED, byte for byte, so the language server's
// behaviour is unchanged and the MCP server adopts it. It is under internal/
// deliberately: an ELPS embedder gets the servers, not their cursor
// arithmetic, and keeping it closed leaves the definition free to follow the
// lexer without an exported API to hold still.
//
// COLUMNS ARE BYTES everywhere here, which is what token.Location counts and
// what both servers work in internally. A column taken off an LSP wire is in
// the negotiated position encoding and must be converted before it reaches
// this package -- lsp does that at its inbound boundary (elps#464).
package symtext

import "strings"

// LineAt returns the text of the 0-based line within content, without its
// trailing newline, or "" when the line does not exist.
//
// Lines are split on "\n" exactly as both servers do elsewhere, so a CRLF
// document leaves the "\r" on the end of the line. That is harmless for
// column arithmetic: "\r" is ASCII, so it changes neither a byte column nor a
// UTF-16 column.
//
// It walks to the line rather than splitting the document, so the cost of a
// lookup is the text ahead of the cursor and the allocation is nothing at all.
func LineAt(content string, line int) string {
	if line < 0 {
		return ""
	}
	for range line {
		nl := strings.IndexByte(content, '\n')
		if nl < 0 {
			return ""
		}
		content = content[nl+1:]
	}
	if nl := strings.IndexByte(content, '\n'); nl >= 0 {
		return content[:nl]
	}
	return content
}

// WordBoundsInLine returns the byte bounds of the symbol-like word containing
// the 0-based byte column col within ln, and whether there is one. The cursor
// may sit inside the word or immediately after its last byte; both name the
// whole word.
//
// A missing line and an empty line both contain no word.
func WordBoundsInLine(ln string, col int) (start, end int, ok bool) {
	if col < 0 || col > len(ln) {
		return 0, 0, false
	}
	// Clamp col to the line length (cursor can be at end of line).
	if col >= len(ln) {
		col = len(ln)
	}
	// Scan backwards from cursor.
	start = col
	for start > 0 && IsSymbolChar(ln[start-1]) {
		start--
	}
	// Scan forwards from cursor.
	end = col
	for end < len(ln) && IsSymbolChar(ln[end]) {
		end++
	}
	if start == end {
		return 0, 0, false
	}
	return start, end, true
}

// WordAt extracts the symbol-like word at the given 0-based line and 0-based
// byte column of content, or "" when there is none.
func WordAt(content string, line, col int) string {
	ln := LineAt(content, line)
	start, end, ok := WordBoundsInLine(ln, col)
	if !ok {
		return ""
	}
	return ln[start:end]
}

// IsSymbolChar reports whether c can appear in an ELPS symbol name.
//
// "&" is on the list and belongs there: the argument markers "&rest",
// "&optional" and "&key" are ordinary symbols whose first byte it is, and it
// is accepted inside package and symbol names generally.
func IsSymbolChar(c byte) bool {
	if c >= 'a' && c <= 'z' {
		return true
	}
	if c >= 'A' && c <= 'Z' {
		return true
	}
	if c >= '0' && c <= '9' {
		return true
	}
	switch c {
	case '-', '_', '!', '?', '+', '*', '/', '<', '>', '=', ':', '.', '#', '^', '&':
		return true
	}
	return false
}
