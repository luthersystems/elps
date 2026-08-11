// Slice-alias laundering shapes that require more than ident-assignment
// tracking: taint through plain var declarations and through slice type
// conversions (the mapEntriesByKey sort-carrier shape from lisp/maps.go).
package a

import (
	"sort"

	"github.com/luthersystems/elps/lisp"
)

// varDeclAlias: `var cells = ...` is a ValueSpec, not an AssignStmt — the
// taint source must be tracked there too or the write below is invisible.
func varDeclAlias(list, x *lisp.LVal) {
	var cells = list.Cells
	cells[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// varDeclTypedAlias: same, with an explicit type on the declaration.
func varDeclTypedAlias(list, x *lisp.LVal) {
	var cells []*lisp.LVal = seqCells(list)
	cells[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// cellSlice mirrors lisp.mapEntriesByKey: a named slice of *LVal that
// implements sort.Interface so the raw backing can be handed to sort.Sort.
type cellSlice []*lisp.LVal

func (s cellSlice) Len() int           { return len(s) }
func (s cellSlice) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s cellSlice) Less(i, j int) bool { return s[i].Int < s[j].Int }

// convertedAlias: a slice type conversion shares backing, so it must carry
// taint — otherwise `cellSlice(cells)` launders it.
func convertedAlias(list, x *lisp.LVal) {
	cs := cellSlice(seqCells(list))
	cs[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// convertedSortCarrier is the in-tree shape from lisp/maps.go
// (sort.Sort(mapEntriesByKey(buf))) with a tainted slice: the conversion is
// itself the sort.Interface value.
func convertedSortCarrier(list *lisp.LVal) {
	cells := seqCells(list)
	sort.Sort(cellSlice(cells)) // want `sort\.Sort over a local slice alias of lisp\.LVal backing storage`
}

// convertedRoundTrip: converting back to the plain slice type must not
// launder either.
func convertedRoundTrip(list, x *lisp.LVal) {
	cs := cellSlice(seqCells(list))
	back := []*lisp.LVal(cs)
	back[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// convertedFresh: conversions of fresh backing stay clean — no diagnostic.
func convertedFresh(list, x *lisp.LVal) {
	cells := seqCells(list)
	fresh := make([]*lisp.LVal, len(cells))
	copy(fresh, cells)
	cs := cellSlice(fresh)
	cs[0] = x
	sort.Sort(cellSlice(fresh))
	var alsoFresh = make([]*lisp.LVal, 1)
	alsoFresh[0] = x
}
