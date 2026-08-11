// Copyright © 2026 The ELPS authors

package rdparser

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// TestSealNonFormatParse pins that the ordinary (evaluation) parse path seals
// its completed top-level expressions — the set-point the copy-on-write seal
// design depends on (lisp/seal.go).
func TestSealNonFormatParse(t *testing.T) {
	p := New(token.NewScannerString("t", "(foo bar) ;; c\n"))
	expr, err := p.Parse()
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if !expr.IsSealed() {
		t.Fatalf("non-format parse produced an unsealed top-level expression")
	}
}

// TestFormatParseTrailingCommentNotSealedWrite is a regression test: the
// format-preserving Parse() attaches a same-line trailing comment to
// expr.Meta AFTER the top-level expression is otherwise complete.  When
// ParseExpression sealed at depth 1 unconditionally, that attach was a write
// to an already-sealed node — a violation of the "sealed bytes never change
// after parse" invariant and, in checked builds, a stale-fingerprint
// false positive.  Format parses are now left unsealed (they are never
// evaluated or shared), so the trailing-comment metadata still attaches
// (the formatter reads it) without writing a sealed node.
func TestFormatParseTrailingCommentNotSealedWrite(t *testing.T) {
	p := NewFormatting(token.NewScannerString("t", "(foo) ;; trailing\n"))
	expr, err := p.Parse()
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if expr.IsSealed() {
		t.Fatalf("format-preserving parse sealed a node whose Meta is still being" +
			" constructed by Parse(); the trailing-comment attach would write a sealed node")
	}
	// The formatter contract must still hold: the trailing comment is attached.
	if expr.Meta == nil || expr.Meta.TrailingComment == nil {
		t.Fatalf("format parse dropped the trailing comment; Meta=%v", expr.Meta)
	}
	if got := expr.Meta.TrailingComment.Text; got != ";; trailing" {
		t.Fatalf("trailing comment text = %q, want %q", got, ";; trailing")
	}
}
