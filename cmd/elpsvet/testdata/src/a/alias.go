// Package a — slice-alias fixtures for the elpsfreshness analyzer's
// laundering checks (issue #371, closing the #369 gap).
package a

import (
	"sort"

	"github.com/luthersystems/elps/lisp"
)

// seqCells mirrors lisp.seqCells: a function returning []*lisp.LVal fed
// from an LVal is assumed to return an alias of its backing storage.
func seqCells(v *lisp.LVal) []*lisp.LVal { return v.Cells }

// aliasIndexWrite is the basic laundered write: the #369 shape reduced to
// one assignment.
func aliasIndexWrite(list, x *lisp.LVal) {
	cells := seqCells(list)
	cells[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// aliasFieldSource taints directly from the .Cells field, not a call.
func aliasFieldSource(list, x *lisp.LVal) {
	cells := list.Cells
	cells[1] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// aliasOneDeep tracks taint through an intermediate assignment: a := .Cells;
// b := a; b[i] = x is flagged at any depth.
func aliasOneDeep(list, x *lisp.LVal) {
	a := list.Cells
	b := a
	b[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// aliasSubslice: reslicing shares backing, so taint survives it.
func aliasSubslice(list, x *lisp.LVal) {
	rest := seqCells(list)[1:]
	rest[0] = x // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// aliasBytes: the .Bytes() backing array reached through a local alias.
func aliasBytes(v *lisp.LVal) {
	b := v.Bytes()
	b[0] = 1 // want `index write through a local slice alias of lisp\.LVal backing storage`
	b[1]++   // want `index write through a local slice alias of lisp\.LVal backing storage`
}

// retroAppendVector is #369 mechanism 2: append 'vector writes vals into
// spare capacity retained by a slice of the literal's backing array.
func retroAppendVector(seq *lisp.LVal, vals []*lisp.LVal) *lisp.LVal {
	cells := seqCells(seq)
	return lisp.Array(nil, append(cells, vals...)) // want `append into a local slice alias of lisp\.LVal backing storage`
}

// lvalByFun mirrors lisp's stable-sort comparator carrier.
type lvalByFun struct {
	cells []*lisp.LVal
}

func (s *lvalByFun) Len() int           { return len(s.cells) }
func (s *lvalByFun) Swap(i, j int)      { s.cells[i], s.cells[j] = s.cells[j], s.cells[i] }
func (s *lvalByFun) Less(i, j int) bool { return s.cells[i].Int < s.cells[j].Int }

// retroStableSort is #369 mechanism 1: sort.Stable over a sort.Interface
// whose composite literal captured an alias of the list's backing array.
func retroStableSort(list *lisp.LVal) *lisp.LVal {
	cells := seqCells(list)
	sortCells := &lvalByFun{cells: cells}
	sort.Stable(sortCells) // want `sort\.Stable over a local slice alias of lisp\.LVal backing storage`
	return list
}

// sortSliceDirect: the sort helpers that take the slice itself.
func sortSliceDirect(list *lisp.LVal) {
	cells := seqCells(list)
	sort.Slice(cells, func(i, j int) bool { // want `sort\.Slice over a local slice alias of lisp\.LVal backing storage`
		return cells[i].Int < cells[j].Int
	})
}

// copyInto: copy with a tainted destination rewrites the shared backing.
func copyInto(dst, src *lisp.LVal) {
	cells := seqCells(dst)
	copy(cells, src.Cells) // want `copy into a local slice alias of lisp\.LVal backing storage`
}

// freshSlice: a slice with backing allocated in this function never flags —
// make, copy into the fresh slice, append from nil.
func freshSlice(list, x *lisp.LVal) []*lisp.LVal {
	cells := seqCells(list)
	fresh := make([]*lisp.LVal, len(cells))
	copy(fresh, cells) // reads cells, writes fresh: ok
	fresh[0] = x
	var out []*lisp.LVal
	out = append(out, cells...)
	out[0] = x
	sort.Slice(out, func(i, j int) bool { return out[i].Int < out[j].Int })
	return append(cells[:0:0], cells...) // zero-cap rebase must reallocate: ok
}

// freshReassign: reassigning the alias to fresh backing clears its taint
// (the copy-on-write guard idiom from the #369 fix).
func freshReassign(list, x *lisp.LVal) {
	cells := seqCells(list)
	fresh := make([]*lisp.LVal, len(cells))
	copy(fresh, cells)
	cells = fresh
	cells[0] = x // ok: cells now has backing this function allocated
}

// freshRootNoTaint: an alias of an LVal constructed in this function is this
// function's own storage.
func freshRootNoTaint(x *lisp.LVal) *lisp.LVal {
	v := lisp.QExpr([]*lisp.LVal{x, x})
	cells := seqCells(v)
	cells[0] = x
	sort.Stable(&lvalByFun{cells: v.Cells})
	return v
}

// selfAppendGrow: the grow idiom is covered by the field-write diagnostic
// alone — no second alias diagnostic on the same line.
func selfAppendGrow(v, x *lisp.LVal) {
	v.Cells = append(v.Cells, x) // want `write to LVal field \.Cells on a lisp\.LVal this function did not construct`
}

// annotatedAlias: //elps:mutates escapes the alias checks like any other.
func annotatedAlias(list, x *lisp.LVal) {
	cells := seqCells(list)
	cells[0] = x //elps:mutates deliberate in-place rewrite for the fixture
	//elps:mutates deliberate append into shared backing for the fixture
	cells = append(cells, x)
	_ = cells
}

//elps:mutates — fixture: the whole function deliberately mutates its input.
func annotatedAliasFunc(list, x *lisp.LVal) {
	cells := seqCells(list)
	cells[0] = x
}
