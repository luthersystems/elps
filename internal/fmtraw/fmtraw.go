// Copyright © 2026 The ELPS authors

// Package fmtraw grants in-repo tooling access to the formatting metadata
// stored in lisp values' unexported meta field (see internal/fmtmeta for
// the metadata itself and the history of why the field is unexported —
// issue #382).
//
// lisp.LVal deliberately has no exported metadata accessors: the metadata
// only exists on format-preserving parse trees, which are produced and
// owned by parser/rdparser, read by the formatter, and never evaluated or
// shared.  This package is the sanctioned path between those
// tools and the field: it lives under internal/, so the Go compiler limits
// it to this module — an embedder importing elps cannot reach the field at
// all.
//
// The accessors are injected by package lisp's init through the untyped
// slots in the hook subpackage (see hook's doc comment for the cycle it
// breaks).  Writers MUST own the tree they stamp: the parser writes only
// nodes it produced during the current parse, before anything else can see
// them.
package fmtraw

import (
	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/internal/fmtraw/hook"
	"github.com/luthersystems/elps/lisp"
)

// Meta returns v's formatting metadata, or nil when v carries none (always
// nil outside format-preserving parses).  The returned struct is v's own —
// the format-preserving parser mutates it through this pointer while it
// still owns the tree; everyone else treats it as read-only.  Injected by
// package lisp's init; importing this package imports lisp, so the
// accessors are always non-nil by the time user code runs.
var Meta func(v *lisp.LVal) *fmtmeta.Meta

// SetMeta attaches m as v's formatting metadata, replacing any existing
// metadata.  Only the format-preserving parser should call it, on nodes it
// produced during the current parse.
var SetMeta func(v *lisp.LVal, m *fmtmeta.Meta)

// EnsureMeta returns v's formatting metadata, attaching a fresh empty Meta
// first when v has none.  The write path has the same ownership contract
// as SetMeta.
func EnsureMeta(v *lisp.LVal) *fmtmeta.Meta {
	m := Meta(v)
	if m == nil {
		m = &fmtmeta.Meta{}
		SetMeta(v, m)
	}
	return m
}

func init() {
	get, ok := hook.Meta.(func(*lisp.LVal) *fmtmeta.Meta)
	if !ok {
		// Unreachable: importing fmtraw imports lisp, whose init stores the
		// accessors before this init runs.
		panic("fmtraw: package lisp did not inject the Meta accessor")
	}
	set, ok := hook.SetMeta.(func(*lisp.LVal, *fmtmeta.Meta))
	if !ok {
		panic("fmtraw: package lisp did not inject the SetMeta accessor")
	}
	Meta, SetMeta = get, set
}
