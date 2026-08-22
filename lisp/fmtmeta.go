// Copyright © 2026 The ELPS authors

package lisp

import (
	"github.com/luthersystems/elps/internal/fmtmeta"
	fmtrawhook "github.com/luthersystems/elps/internal/fmtraw/hook"
	"github.com/luthersystems/elps/parser/token"
)

func init() {
	// Inject the formatting-metadata accessors for in-repo format tooling
	// (parser/rdparser writes, formatter and analysis/perf read).  The
	// typed surface lives in internal/fmtraw; the untyped slots in
	// internal/fmtraw/hook exist only to break the import cycle (fmtraw
	// needs lisp's types, so lisp cannot import fmtraw).  This is
	// deliberately the ONLY way to reach an LVal's formatting metadata
	// (issue #382), and internal/ visibility limits it to this module.
	//
	// Ownership contract: the format-preserving parser writes only nodes
	// it produced during the current parse, and format trees are never
	// evaluated or shared.
	fmtrawhook.Meta = func(v *LVal) *fmtmeta.Meta { return v.meta }
	fmtrawhook.SetMeta = func(v *LVal, m *fmtmeta.Meta) {
		v.meta = m
	}
}

// detachMeta deep-copies format-preserving metadata, including the comment
// tokens and their locations.
func detachMeta(m *fmtmeta.Meta) *fmtmeta.Meta {
	if m == nil {
		return nil
	}
	cp := *m
	cp.TrailingComment = copyToken(m.TrailingComment)
	cp.LeadingComments = copyTokens(m.LeadingComments)
	cp.InnerTrailingComments = copyTokens(m.InnerTrailingComments)
	return &cp
}

func copyTokens(toks []*token.Token) []*token.Token {
	if toks == nil {
		return nil
	}
	out := make([]*token.Token, len(toks))
	for i := range toks {
		out[i] = copyToken(toks[i])
	}
	return out
}

func copyToken(t *token.Token) *token.Token {
	if t == nil {
		return nil
	}
	cp := *t
	cp.Source = copyLocation(t.Source)
	return &cp
}

func copyLocation(loc *token.Location) *token.Location {
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}
