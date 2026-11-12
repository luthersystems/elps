// Copyright © 2026 The ELPS authors

package lisp

import (
	"unsafe"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
)

// A node's tooling record is its formatter metadata and the debugger's
// macro-expansion record.  Both are nil in production.
//
// WHY THE RECORD HAS NO FIELD OF ITS OWN.  With the two records as pointer
// fields LVal was 112 bytes.  Without them it is 96, one allocator size
// class down, for every value the interpreter allocates
// (luthersystems/elps#944).  So LVal has one pointer word, source, for the
// location and the record together.
//
// WHY THE RECORD IS NOT ON THE LOCATION.  #944 first kept the record in a
// slot on token.Location.  That made every Location 96 bytes instead of 80,
// and code that allocates or copies many locations (analysis, lint, a cold
// durable function name) paid for it with no record anywhere in sight
// (luthersystems/elps#947).  Locations stay 80 bytes now, and the record
// lives in a carrier that only a node with a record has.
//
// THE SOURCE WORD.  source is an unsafe.Pointer, and auxKind says what it
// points to:
//
//	auxNone      a *token.Location, or nil (every node in production)
//	auxMeta      a *metaAux: the location and the metadata in one object
//	auxCarrier   a *nodeAux: the location and two record pointers
//
// The word is untyped because Go has no one-word field that holds one of
// two pointer types; an interface is two words, and a second pointer field
// is the 112-byte LVal again.  The garbage collector scans an
// unsafe.Pointer as a pointer, so the pointee stays alive whatever its
// type.  The only conversions are in loc, metaAuxOf, nodeAuxOf and the
// setters in this file, and each is chosen by auxKind, which the same
// statement that writes source writes.  No other code writes source or
// auxKind (TestNodeAuxSourceWritesStayInNodeAux checks that), so a
// carrier is never read as a Location or a Location as a carrier.
//
// WHY TWO CARRIERS.  The format-preserving parser gives most nodes
// metadata and none a macro-expansion record.  Before #944 that cost one
// metadata allocation per node, so a carrier and a separate metadata object
// would be a second allocation per node (#944 measured 7 to 8% more
// objects in BenchmarkMutationChecksScaling).  metaAux holds the metadata
// by value: one allocation, as before.  It is in the 128-byte class, 16
// bytes above the metadata alone, which is what the Location slot cost the
// same node, and it leaves out the macro-expansion pointer to stay there.
// Every other shape of record (a debugger record, metadata the caller
// allocated) takes the 24-byte nodeAux.
//
// WHO OWNS A RECORD.  A carrier is reachable only from the node it was made
// for and from that node's header copies (`*cp = *v`: Quote, Splice, a
// function reference), which copy source and auxKind together and so share
// the record, exactly as they shared the two pointer fields.  A node that
// only borrows another's location (`x.setPlainLoc(y.loc())`) gets the
// Location and not the record, also as before.
//
// A CARRIER IS IMMUTABLE once a node holds it.  A header copy cannot change
// its record or location in a shared carrier without changing its
// sibling's, so every change gives the node a new carrier.  The metadata a
// carrier points to is not frozen: it is the same object the two fields
// shared, and the format-preserving parser writes it while it owns the
// tree.  The Location is never written by this file, so SetSource stores
// the pointer it is given even for a node with a record, and the parser's
// later end-position writes through that pointer reach the node.
//
// A NODE WITHOUT A LOCATION can carry a record (a macro expanded from
// Go-built forms under a debugger); its carrier's loc is nil.  Kernel code
// reads a node's location through loc for this reason, never through the
// source word.
type auxKind uint8

const (
	auxNone auxKind = iota
	auxMeta
	auxCarrier
)

// metaAux is the carrier of a node whose only record is formatter metadata
// that package lisp allocated (ensureFmtMeta).  The metadata is part of the
// carrier, so the node's record costs one allocation (see auxKind).
type metaAux struct {
	loc  *token.Location
	meta fmtmeta.Meta
}

// nodeAux is the carrier of every other record: a macro-expansion record,
// or metadata the caller allocated (Copy's private copy is built in place
// instead; the macro stamp shares the original's metadata).
type nodeAux struct {
	loc            *token.Location
	meta           *fmtmeta.Meta
	macroExpansion *macroExpansionInfo
}

// metaAuxOf and nodeAuxOf read the carrier v.source points to.  The caller
// has tested v.auxKind; the conversion is valid only for the kind named.
func (v *LVal) metaAuxOf() *metaAux {
	return (*metaAux)(v.source)
}

func (v *LVal) nodeAuxOf() *nodeAux {
	return (*nodeAux)(v.source)
}

// loc returns v's location, or nil when v has none.  The production path
// is one byte compare and a conversion.
//
// It is kept this small on purpose: loc is inlined into Source, and Source
// into callers such as astutil.SourceLoc.  With the carrier switch inline,
// SourceLoc grew past the inlining budget, its result Location escaped to
// the heap, and the lint mutation checks allocated 3% more objects.
func (v *LVal) loc() *token.Location {
	if v.auxKind == auxNone {
		return (*token.Location)(v.source)
	}
	return v.carrierLoc()
}

// carrierLoc is the slow half of loc, for a node with a tooling record.
// It must not be inlined into loc (see loc); only tooling builds take it.
//
//go:noinline
func (v *LVal) carrierLoc() *token.Location {
	if v.auxKind == auxMeta {
		return v.metaAuxOf().loc
	}
	return v.nodeAuxOf().loc
}

// locWord is the source word for the location loc and no tooling record.
// A composite literal that gives an LVal a location without a record sets
// source to locWord(loc) and leaves auxKind at its zero, auxNone; that
// keeps the literal one statement on the hot paths that build one (a
// lambda, an error).  Every other write goes through a setter below.
func locWord(loc *token.Location) unsafe.Pointer {
	return unsafe.Pointer(loc) //nolint:gosec // G103: the conversion auxNone names (see auxKind)
}

// hasAux reports that v carries a tooling record.
func (v *LVal) hasAux() bool {
	return v.auxKind != auxNone
}

// fmtMeta returns v's formatter metadata, or nil.
func (v *LVal) fmtMeta() *fmtmeta.Meta {
	switch v.auxKind {
	case auxMeta:
		return &v.metaAuxOf().meta
	case auxCarrier:
		return v.nodeAuxOf().meta
	default:
		return nil
	}
}

// macroExp returns v's macro-expansion record, or nil.
func (v *LVal) macroExp() *macroExpansionInfo {
	if v.auxKind != auxCarrier {
		return nil
	}
	return v.nodeAuxOf().macroExpansion
}

// setPlainLoc gives v the location loc and no tooling record.  It is the
// field write for a header the caller may write; kernel code that used to
// assign the source field calls it, because the source word must not be
// written apart from auxKind (see auxKind).
func (v *LVal) setPlainLoc(loc *token.Location) {
	checkNotSharedInt(v)
	v.source = locWord(loc) //elps:mutates the audited location setter; callers pass a header they own
	v.auxKind = auxNone     //elps:mutates the audited location setter; callers pass a header they own
}

// setNodeAux gives v the location loc and the record (meta, mexp).  v must
// be a header the caller may write.  A header that shares v's carrier keeps
// the one it had.
func (v *LVal) setNodeAux(loc *token.Location, meta *fmtmeta.Meta, mexp *macroExpansionInfo) {
	if meta == nil && mexp == nil {
		v.setPlainLoc(loc)
		return
	}
	checkNotSharedInt(v)
	//elps:mutates the audited setter for the tooling record; callers pass a header they own
	v.source = unsafe.Pointer(&nodeAux{loc: loc, meta: meta, macroExpansion: mexp}) //nolint:gosec // G103: the conversion auxCarrier names (see auxKind)
	v.auxKind = auxCarrier                                                          //elps:mutates the audited setter for the tooling record; callers pass a header they own
}

// setFmtMeta sets v's formatter metadata to m and keeps its location and
// its macro-expansion record.
func (v *LVal) setFmtMeta(m *fmtmeta.Meta) {
	if !v.hasAux() && m == nil {
		return
	}
	v.setNodeAux(v.loc(), m, v.macroExp()) //elps:aliases v keeps its own location; only the record changes
}

// ensureFmtMeta returns v's formatter metadata, first giving v empty
// metadata if it has none.  The new metadata is part of a metaAux, so a
// node the format-preserving parser stamps costs one allocation (see
// auxKind).  v must be a header the caller may write.
func (v *LVal) ensureFmtMeta() *fmtmeta.Meta {
	if m := v.fmtMeta(); m != nil {
		return m
	}
	if mexp := v.macroExp(); mexp != nil {
		// Metadata on a debugger-stamped node: rare enough (a format
		// tree is never evaluated) that a separate object is fine.
		m := &fmtmeta.Meta{}
		v.setNodeAux(v.loc(), m, mexp) //elps:aliases v keeps its own location; only the record changes
		return m
	}
	checkNotSharedInt(v)
	c := &metaAux{loc: v.loc()}
	//elps:mutates the audited setter for the tooling record; callers pass a header they own
	v.source = unsafe.Pointer(c) //nolint:gosec // G103: the conversion auxMeta names (see auxKind)
	v.auxKind = auxMeta          //elps:mutates the audited setter for the tooling record; callers pass a header they own
	return &c.meta
}

// setMacroExp sets v's macro-expansion record and keeps its location and
// its formatter metadata.
func (v *LVal) setMacroExp(mexp *macroExpansionInfo) {
	if !v.hasAux() && mexp == nil {
		return
	}
	v.setNodeAux(v.loc(), v.fmtMeta(), mexp) //elps:aliases v keeps its own location; only the record changes
}

// setLoc sets v's location and keeps its tooling record.  v must be a
// header the caller may write.  For a node without a record, which is every
// node in production, it is the plain field write.
//
// The record moves to a new carrier rather than having its loc rewritten:
// a header copy may share the carrier (see auxKind).  The new carrier
// points to the same metadata, which is what a header copy that took a
// location of its own shared before (the parser's Quote path relies on it).
// That is the one place this design allocates where the Location slot did
// not: a quoted form in a format-preserving parse costs a 24-byte nodeAux,
// about 1.4% more objects in BenchmarkMutationChecksScaling, against 2 to
// 4% fewer bytes there and 16 fewer bytes on every Location everywhere.
func (v *LVal) setLoc(loc *token.Location) {
	if !v.hasAux() {
		v.setPlainLoc(loc)
		return
	}
	v.setNodeAux(loc, v.fmtMeta(), v.macroExp())
}
