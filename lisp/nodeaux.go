// Copyright © 2026 The ELPS authors

package lisp

import (
	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/internal/locaux"
	"github.com/luthersystems/elps/parser/token"
)

// nodeAux is the tooling record of one node that has the debugger's
// macro-expansion record, with the formatter's metadata beside it.  A node
// with metadata only has the *fmtmeta.Meta as its record (see newNodeAux).
//
// WHY IT IS NOT TWO FIELDS OF LVal.  Both are nil in production, and with
// them LVal was 112 bytes.  Without them it is 96, one allocator size class
// down, for every value the interpreter allocates (luthersystems/elps#944).
// The record therefore rides on the node's Location (token.Location's aux
// slot), and LVal keeps one pointer, source, for all three.
//
// WHO OWNS A RECORD.  A Location can be shared by nodes that are unrelated
// (every node of one macro expansion shares the call site), so the slot
// alone cannot say whose record it holds.  The hasAux flag on the LVal
// does: a node reads the record on its Location only when its own flag is
// set.  A header copy (`*cp = *v`: Quote, Splice, a function reference)
// copies the flag with the source pointer and so shares the record, exactly
// as it shared the two pointers before.  A node that only borrows the
// Location (`x.source = y.source`) has no flag and sees no record, also as
// before.
//
// A RECORD IS IMMUTABLE once it is on a Location.  A header copy cannot
// change its own record through the shared Location without changing its
// sibling's, so every change gives the node a new record on a new Location
// (setNodeAux).  The one exception is the format-preserving parser, which
// must keep the Location pointer it handed to SetSource: it writes the
// record onto that Location in place, while it still owns the tree.
//
// A NODE WITHOUT A LOCATION can still carry a record (a macro expanded from
// Go-built forms under a debugger).  It needs a Location to hold the slot,
// so it gets an empty carrier and the auxNoLoc flag, and (*LVal).loc reports
// nil for it.  Kernel code reads a node's location through loc for this
// reason, never through the source field.
type nodeAux struct {
	meta           *fmtmeta.Meta
	macroExpansion *macroExpansionInfo
}

// locAux and setLocAux read and write a Location's aux slot.  Package token
// injects them through internal/locaux; see that package for why.
var (
	locAux    = mustLocAuxHook[func(*token.Location) any](locaux.Get)
	setLocAux = mustLocAuxHook[func(*token.Location, any)](locaux.Set)
)

// mustLocAuxHook recovers one typed accessor from its untyped slot.
//
// NOT LISP-REACHABLE (luthersystems/elps#367): package lisp imports package
// token, whose init fills the slots before this package's variables are
// initialized.  A panic here is a broken build, not program input.
func mustLocAuxHook[F any](slot any) F {
	f, ok := slot.(F)
	if !ok {
		panic("lisp: package token did not inject the Location aux accessors")
	}
	return f
}

// loc returns v's location, or nil when v has none.  It differs from the
// source field only for a node whose source is a record carrier (see
// nodeAux).
func (v *LVal) loc() *token.Location {
	if v.auxNoLoc {
		return nil
	}
	return v.source
}

// nodeAuxRecord returns v's tooling record as the slot holds it, or nil.
// The flag test comes first so that the production path is one byte
// compare.
func (v *LVal) nodeAuxRecord() any {
	if !v.hasAux {
		return nil
	}
	return locAux(v.source)
}

// newNodeAux returns the record for the two parts.  A record with only
// formatter metadata is the *fmtmeta.Meta itself, not a nodeAux that points
// to it: the format-preserving parser gives most nodes metadata and none a
// macro-expansion record, and a second object per node made that parse
// allocate 7 to 8% more objects (BenchmarkMutationChecksScaling in lint).
// Both parts nil is no record.
func newNodeAux(meta *fmtmeta.Meta, mexp *macroExpansionInfo) any {
	switch {
	case mexp != nil:
		return &nodeAux{meta: meta, macroExpansion: mexp}
	case meta != nil:
		return meta
	default:
		return nil
	}
}

// nodeAuxParts splits a record that newNodeAux made into its two parts.
func nodeAuxParts(rec any) (*fmtmeta.Meta, *macroExpansionInfo) {
	switch rec := rec.(type) {
	case *fmtmeta.Meta:
		return rec, nil
	case *nodeAux:
		return rec.meta, rec.macroExpansion
	default:
		return nil, nil
	}
}

// fmtMeta returns v's formatter metadata, or nil.
func (v *LVal) fmtMeta() *fmtmeta.Meta {
	meta, _ := nodeAuxParts(v.nodeAuxRecord())
	return meta
}

// macroExp returns v's macro-expansion record, or nil.
func (v *LVal) macroExp() *macroExpansionInfo {
	_, mexp := nodeAuxParts(v.nodeAuxRecord())
	return mexp
}

// setNodeAux replaces v's tooling record.  v must be a header the caller
// may write.  A header that shares v's Location keeps the record it had.
//
// inPlace asks for the record to go on v's current Location object.  Only
// the format-preserving parser passes it: the parser keeps that pointer and
// writes end positions through it after the node exists.  It is honored
// only when the Location carries no record yet.
func (v *LVal) setNodeAux(meta *fmtmeta.Meta, mexp *macroExpansionInfo, inPlace bool) {
	loc := v.loc()
	if meta == nil && mexp == nil {
		// The Location may still hold the record for a sibling header,
		// so the record stays where it is and v moves to a plain copy.
		// Clearing the flag alone would stop v reading the record but
		// keep it reachable from v: a template header that drops its
		// debugger record must not keep the source VM's values alive.
		if loc != nil && locAux(loc) != nil {
			loc = loc.Copy()
		}
		v.source = loc     //elps:mutates the audited setter for the tooling record; callers pass a header they own
		v.hasAux = false   //elps:mutates the audited setter for the tooling record; callers pass a header they own
		v.auxNoLoc = false //elps:mutates the audited setter for the tooling record; callers pass a header they own
		return
	}
	noLoc := false
	switch {
	case loc == nil:
		loc = &token.Location{}
		noLoc = true
	case inPlace && !v.hasAux && locAux(loc) == nil:
	default:
		loc = loc.Copy()
	}
	setLocAux(loc, newNodeAux(meta, mexp))
	v.source = loc     //elps:mutates the audited setter for the tooling record; callers pass a header they own
	v.hasAux = true    //elps:mutates the audited setter for the tooling record; callers pass a header they own
	v.auxNoLoc = noLoc //elps:mutates the audited setter for the tooling record; callers pass a header they own
}

// setFmtMeta sets v's formatter metadata and keeps its macro-expansion
// record.
func (v *LVal) setFmtMeta(m *fmtmeta.Meta, inPlace bool) {
	if !v.hasAux && m == nil {
		return
	}
	v.setNodeAux(m, v.macroExp(), inPlace)
}

// setMacroExp sets v's macro-expansion record and keeps its formatter
// metadata.
func (v *LVal) setMacroExp(mexp *macroExpansionInfo) {
	if !v.hasAux && mexp == nil {
		return
	}
	v.setNodeAux(v.fmtMeta(), mexp, false)
}

// setLoc sets v's location and keeps its tooling record.  v must be a
// header the caller may write.  For a node without a record, which is every
// node in production, it is the plain field write.
func (v *LVal) setLoc(loc *token.Location) {
	if !v.hasAux {
		v.source = loc //elps:mutates the audited location setter; callers pass a header they own
		return
	}
	v.moveNodeAux(loc)
}

// moveNodeAux is the slow half of setLoc: it gives v the location loc and
// moves v's record onto it.
//
// The record goes onto loc itself when it can, because SetSource promises
// to store the pointer it is given and the format-preserving parser relies
// on that.  loc is copied first when it already holds another node's
// record, and when the record is a debugger's: a location under a debugger
// may be shared with nodes other goroutines read, and a write to its slot
// would race with them.
func (v *LVal) moveNodeAux(loc *token.Location) {
	aux := v.nodeAuxRecord()
	if aux == nil {
		v.source = loc     //elps:mutates the audited location setter; callers pass a header they own
		v.hasAux = false   //elps:mutates the audited location setter; callers pass a header they own
		v.auxNoLoc = false //elps:mutates the audited location setter; callers pass a header they own
		return
	}
	noLoc := false
	switch {
	case loc == nil:
		loc = &token.Location{}
		noLoc = true
	case locAux(loc) != nil:
		loc = loc.Copy()
	default:
		if _, mexp := nodeAuxParts(aux); mexp != nil {
			loc = loc.Copy()
		}
	}
	setLocAux(loc, aux)
	v.source = loc     //elps:mutates the audited location setter; callers pass a header they own
	v.auxNoLoc = noLoc //elps:mutates the audited location setter; callers pass a header they own
}
