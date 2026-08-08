// Copyright © 2026 The ELPS authors

package formatter

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

// `elps fmt` deleting a comment is not visible to any AST-shaped check.
// Comments are not part of the tree, so a dropped one leaves the parsed
// program identical AND leaves Format idempotent -- the output is a perfectly
// stable fixed point that is simply missing a line of the user's file.  Five
// separate defects presented exactly that way, and every one of them was found
// by comparing the COMMENT TOKENS on either side of a format rather than the
// trees.
//
// The tests below pin each shape with its exact expected output (via
// runFormatTests, which also asserts idempotency), and commentsPreserved
// re-checks the whole set token-by-token so a future defect in one of these
// paths cannot hide behind a plausible-looking expectation.

// commentTokens returns the text of every comment in src, in order.  A
// hash-bang lexes as HASH_BANG plus a COMMENT holding the rest of the line.
func commentTokens(t *testing.T, src string) []string {
	t.Helper()
	lex := lexer.New(token.NewScanner("test", bytes.NewReader([]byte(src))))
	out := []string{}
	for range 1 << 12 {
		for _, tok := range lex.ReadToken() {
			switch tok.Type {
			case token.COMMENT, token.HASH_BANG:
				out = append(out, tok.Text)
			case token.EOF, token.ERROR, token.INVALID:
				return out
			}
		}
	}
	t.Fatalf("lexer did not terminate on %q", src)
	return nil
}

// requireCommentsPreserved asserts that formatting src neither drops nor
// rewrites a comment.
func requireCommentsPreserved(t *testing.T, src string) {
	t.Helper()
	out, err := Format([]byte(src), nil)
	require.NoError(t, err, "Format failed on %q", src)
	require.Equal(t, commentTokens(t, src), commentTokens(t, string(out)),
		"comments changed\n--- input ---\n%s\n--- output ---\n%s", src, out)
}

// TestCommentBeforePrefixForm covers a comment written on the line(s) before a
// prefix form.
//
// tokenLVal drains the parser's pending comments onto the first LVal it
// builds, and for a prefix form that is the OPERAND, not the node the comments
// belong to.  ' survived by accident (lisp.Quote shallow-copies an unquoted
// operand, Meta pointer included), but #' and #^ build a fresh two-cell
// s-expression and the printer's shorthand path writes only the prefix and the
// operand -- so "; c\n#'a" formatted to "#'a".
//
// The matrix is the whole point.  The original version of this test had no row
// with a BLANK LINE between the comment and the form, and none with a
// PRECEDING top-level form, and the fix for the deletion introduced a
// regression visible only in that corner: #' inherits its prefix token's
// PrecedingNewlines onto its operand (readFunRef emits FUN_REF and the symbol
// from one readToken, with no skipWhitespace between them), so the node
// reported a blank line before the comment that the source did not have and
// `elps fmt` inserted one.  ' and #^ scan separately and report zero, which is
// why they need to be here as controls: a fix broad enough to break them has
// to fail.
func TestCommentBeforePrefixForm(t *testing.T) {
	var tests []formatTest
	for _, form := range []struct{ name, text string }{
		{"quote", "'f"},
		{"funref", "#'f"},
		{"unbound", "#^f"},
		{"plain", "f"}, // control: no prefix at all
	} {
		for _, ctx := range []struct{ name, prefix string }{
			{"first", ""},
			{"after-form", "(a)\n"},
		} {
			for _, gap := range []struct{ name, text string }{
				{"adjacent", ""},
				{"blank-line", "\n"},
			} {
				src := ctx.prefix + "; c\n" + gap.text + form.text + "\n"
				tests = append(tests, formatTest{
					name:     form.name + "/" + ctx.name + "/" + gap.name,
					input:    src,
					expected: src, // every shape is already an exact fixed point
				})
			}
		}
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
	}
}

// TestCommentBeforeNestedPrefixForm is the same defect one level in, where the
// comment belongs to a child rather than a top-level form.
func TestCommentBeforeNestedPrefixForm(t *testing.T) {
	var tests []formatTest
	for _, form := range []string{"'g", "#'g", "#^g", "g"} {
		for _, gap := range []struct{ name, text string }{
			{"adjacent", ""},
			{"blank-line", "\n"},
		} {
			src := "(f\n  ; c\n" + gap.text + "  " + form + ")\n"
			tests = append(tests, formatTest{
				name:     form + "/" + gap.name,
				input:    src,
				expected: src,
			})
		}
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
	}
}

// TestCommentBetweenPrefixAndOperand covers a comment written AFTER the prefix
// token, in the gap before the thing it applies to.
//
// Those comments are collected while the operand is parsed and land on the
// operand, which the printer reaches through writeQuote or tryPrefixForm --
// neither of which writes a child's leading comments.  Quoting an
// already-quoted operand or a list builds an LQuote node with a Meta of its
// own, so "'\n; c\n[a]" lost the comment entirely; quoting a plain symbol
// shared the operand's Meta and only moved it.  Both now move it above the
// prefix, which is where the comment can be written without changing what the
// form means.
func TestCommentBetweenPrefixAndOperand(t *testing.T) {
	tests := []formatTest{
		{name: "quote/symbol", input: "'\n; c\na\n", expected: "; c\n'a\n"},
		{name: "quote/glued", input: "'; c\na\n", expected: "; c\n'a\n"},
		{name: "quote/list", input: "'\n; c\n[a]\n", expected: "; c\n'[a]\n"},
		{name: "quote/sexpr", input: "'\n; c\n(a)\n", expected: "; c\n'(a)\n"},
		{name: "quote/quote", input: "'\n; c\n'a\n", expected: "; c\n''a\n"},
		{name: "unbound", input: "#^; c\na\n", expected: "; c\n#^a\n"},
		{name: "both-sides", input: "; a\n'; b\nX\n", expected: "; a\n; b\n'X\n"},
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
	}
}

// TestCommentInEmptySExpr covers a comment between the brackets of a form with
// no children.
//
// writeSExpr took a shortcut for that case -- write "()", return -- which
// skipped both writeInnerTrailingComments and the closing-bracket newline, so
// "(\n; c\n)" formatted to "()".  Only the unquoted-paren path was affected:
// "[...]", "'(...)" and every non-empty form reach writeListInner, which
// handles both.  Those are the controls.
func TestCommentInEmptySExpr(t *testing.T) {
	tests := []formatTest{
		{name: "paren", input: "(\n; c\n)\n", expected: "(\n ; c\n )\n"},
		{name: "paren/two", input: "(\n; a\n; b\n)\n", expected: "(\n ; a\n ; b\n )\n"},
		{name: "brace", input: "[\n; c\n]\n", expected: "[\n ; c\n ]\n"},
		{name: "quoted-paren", input: "'(\n; c\n)\n", expected: "'(\n  ; c\n  )\n"},
		{name: "unbound-paren", input: "#^(\n; c\n)\n", expected: "#^(\n   ; c\n   )\n"},
		{name: "nested", input: "(f (\n; c\n))\n", expected: "(f (\n    ; c\n    ))\n"},
		// Controls: no comment involved.
		{name: "empty-paren", input: "()\n", expected: "()\n"},
		{name: "empty-brace", input: "[]\n", expected: "[]\n"},
		// The closing bracket on its own line is now honoured for an empty
		// form too, matching what "[" has always done.
		{name: "paren-newline", input: "(\n)\n", expected: "(\n )\n"},
		{name: "brace-newline", input: "[\n]\n", expected: "[\n ]\n"},
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
	}
}

// TestCommentBeforeSExprHead covers a comment between an opening bracket and
// the head of the form.
//
// writeSExpr wrote the head with no leading-comment handling at all -- it only
// ever considered the head's NEWLINE, and only for a data list -- so "(\n; c\n
// f x)" formatted to "(f x)".  writeListInner has always written a first
// child's leading comments, which is why the bracketed forms are the controls.
func TestCommentBeforeSExprHead(t *testing.T) {
	tests := []formatTest{
		{name: "call", input: "(\n; c\nf x)\n", expected: "(\n ; c\n f x)\n"},
		{name: "call/two", input: "(\n; a\n; b\nf x)\n", expected: "(\n ; a\n ; b\n f x)\n"},
		{name: "data-list", input: "(\n; c\n1 2)\n", expected: "(\n ; c\n 1 2)\n"},
		{name: "brace", input: "[\n; c\nf x]\n", expected: "[\n ; c\n f x]\n"},
		{name: "nested", input: "(g (\n; c\nf x))\n", expected: "(g (\n    ; c\n    f x))\n"},
		// Controls: no comment, so the head keeps being pulled up next to the
		// bracket for a call and left in place for a data list.
		{name: "call-newline", input: "(\nf x)\n", expected: "(f x)\n"},
		{name: "data-newline", input: "(\n1 2)\n", expected: "(\n 1 2)\n"},
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
	}
}

// TestNegativeLiteralAfterComment covers a signed literal that starts a line
// following a comment.
//
// The sign and the digits are two tokens merged into one literal by
// ParseNegative, which carried over the sign token's Source but not its
// whitespace bookkeeping.  PrecedingNewlines collapsed to zero, the literal
// was printed back on the previous line, and when that line ended in a comment
// the comment SWALLOWED it: "(a ; c\n-1)" formatted to "(a ; c -1)", which
// re-parses as "(a)".  `elps fmt` deleting a term from the program it was
// asked to tidy.
func TestNegativeLiteralAfterComment(t *testing.T) {
	tests := []formatTest{
		{name: "int", input: "(a ; c\n-1)\n", expected: "(a ; c\n  -1)\n"},
		{name: "float", input: "(a ; c\n-1.5)\n", expected: "(a ; c\n  -1.5)\n"},
		{name: "symbol", input: "(a ; c\n-x)\n", expected: "(a ; c\n  -x)\n"},
		{name: "exponent", input: "(a ; c\n-1e5)\n", expected: "(a ; c\n  -1e5)\n"},
		{name: "leading-comment", input: "(a\n; c\n-1)\n", expected: "(a\n  ; c\n  -1)\n"},
		{name: "top-level", input: "; c\n-1\n", expected: "; c\n-1\n"},
		// Controls: an unsigned literal in the same position, and a signed one
		// with no comment in play.
		{name: "unsigned", input: "(a ; c\n1)\n", expected: "(a ; c\n  1)\n"},
		{name: "same-line", input: "(a -1)\n", expected: "(a -1)\n"},
	}
	runFormatTests(t, tests)
	for _, tt := range tests {
		requireCommentsPreserved(t, tt.input)
		// The literal must still be there.  This is the assertion the AST
		// checks make: a swallowed term changes the program.
		out, err := Format([]byte(tt.input), nil)
		require.NoError(t, err)
		roundTripEqual(t, tt.input, string(out))
	}
}

// TestFormatRejectsNothingItPrints pins the shape that made `elps fmt` produce
// output it could not read back: a hash-bang followed by a #' whose operand
// cannot be re-sugared.  The comment landed on the operand, the longhand
// printer wrote it INSIDE the form, and a hash-bang is only legal at the start
// of a program.
func TestFormatRejectsNothingItPrints(t *testing.T) {
	srcs := []string{
		"#!/usr/bin/env elps\n#':%\n",
		"#!/usr/bin/env elps\n#'f\n",
		"#!/usr/bin/env elps\n; c\n#'f\n",
		"#!\n#^a\n",
	}
	for _, src := range srcs {
		t.Run(strings.ReplaceAll(src, "\n", "\\n"), func(t *testing.T) {
			out, err := Format([]byte(src), nil)
			require.NoError(t, err, "Format rejected %q", src)
			again, err := Format(out, nil)
			require.NoError(t, err, "Format rejected its own output %q", out)
			require.Equal(t, string(out), string(again), "not idempotent")
			require.Equal(t, commentTokens(t, src), commentTokens(t, string(out)),
				"comments changed\n--- in ---\n%s\n--- out ---\n%s", src, out)
			// roundTripEqual drives a bare Parse loop, which reports a
			// hash-bang as an unexpected token -- only ParseProgram consumes
			// one.  Format re-reading its own output above is the equivalent
			// check for these inputs.
		})
	}
}
