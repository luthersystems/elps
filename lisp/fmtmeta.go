// Copyright © 2026 The ELPS authors

package lisp

import (
	"github.com/luthersystems/elps/internal/fmtmeta"
	fmtrawhook "github.com/luthersystems/elps/internal/fmtraw/hook"
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
	// sealed, evaluated, or shared (see parser/rdparser's seal tests), so
	// these accessors need no seal guard.
	//
	// The write is in place on the node's own Location (see nodeAux): the
	// parser stamps a node right after it gives the node a Location no
	// other node holds, and it keeps that pointer to write end positions
	// through later, so the node must keep the same Location object.
	fmtrawhook.Meta = func(v *LVal) *fmtmeta.Meta { return v.fmtMeta() }
	fmtrawhook.SetMeta = func(v *LVal, m *fmtmeta.Meta) {
		v.setFmtMeta(m, true)
	}
}
