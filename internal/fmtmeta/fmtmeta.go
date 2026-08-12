// Copyright © 2026 The ELPS authors

// Package fmtmeta holds the formatting metadata attached to lisp values by
// the format-preserving parser (comments, blank lines, bracket style,
// original literal text) and consumed by the formatter and comment-directive
// tooling.
//
// The struct used to be lisp.SourceMeta, carried in the exported LVal.Meta
// field.  Both went unexported in issue #382: every value in a parse tree
// can be shared across environments once the tree is sealed, and the
// post-seal leak fixes (macroexpand &rest, format Meta) were exactly
// metadata writes landing on shared nodes.  The metadata now lives in an
// unexported LVal field typed by this internal package, so code outside
// this module cannot read or write it at all — the compile-time closure
// holds at the module boundary.  In-repo tooling (parser/rdparser,
// formatter, analysis/perf) reaches it through internal/fmtraw.
//
// Format-preserving parse trees are never sealed and never evaluated (see
// parser/rdparser's seal tests): the parser owns the tree it stamps, and
// the formatter only reads.  Keep it that way — metadata writes on a
// sealed or shared tree are the corruption class this design closes.
package fmtmeta

import (
	"github.com/luthersystems/elps/parser/token"
)

// Meta holds formatting metadata for a lisp value, populated only when
// parsing in format-preserving mode.  Nil in normal parsing — zero cost.
type Meta struct {
	TrailingComment         *token.Token   // inline comment on same line after this node
	OriginalText            string         // original token text for literals (preserves escapes, numeric bases)
	LeadingComments         []*token.Token // comment tokens preceding this node
	InnerTrailingComments   []*token.Token // comments between last child and closing bracket
	BlankLinesBefore        int            // blank lines (newline count - 1) before this node (or before its leading comments)
	BlankLinesAfterComments int            // blank lines between last leading comment and the expression
	PrecedingSpaces         int            // spaces before this token on the same line (for column alignment)
	BracketType             rune           // '(' or '[' for LSExpr nodes
	NewlineBefore           bool           // true if at least one newline preceded this node in source
	ClosingBracketNewline   bool           // true if closing bracket was on its own line in source
}
