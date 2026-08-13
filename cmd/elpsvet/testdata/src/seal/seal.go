// Package seal holds elpsseal fixtures from OUTSIDE package
// lisp — the libelpspath / analysis position, where the `sealed` field is
// unexported and IsSealed() plus a copy is the only available fix.
package seal

import (
	"github.com/luthersystems/elps/lisp"
)

// toCells mirrors libelpspath.toCells: a TWO-RESULT taint source.  The
// original alias tracker only recognised single-result []*LVal functions, so
// every `cells, err := toCells(in)` laundered the alias — which is how #392
// stayed invisible.
func toCells(in *lisp.LVal) ([]*lisp.LVal, error) { return in.Cells, nil }

// toList and toVector mirror libelpspath's one-line wrappers.  Neither is
// named in the analyzer: both are INFERRED as borrowing constructors from
// lisp.SExpr's and lisp.Array's facts.
func toList(cells []*lisp.LVal) *lisp.LVal   { return lisp.SExpr(cells) }      // want toList:"borrowsLValBacking\\(0\\)"
func toVector(cells []*lisp.LVal) *lisp.LVal { return lisp.Array(nil, cells) } // want toVector:"borrowsLValBacking\\(0\\)"

// copyCells is the freshening helper the fixed code uses.
func copyCells(cells []*lisp.LVal) []*lisp.LVal {
	cp := make([]*lisp.LVal, len(cells))
	copy(cp, cells)
	return cp
}

// issue392 is the shipped defect, reduced: a fresh unsealed header over a
// possibly-sealed list's live backing, handed back to the value domain.
func issue392(in *lisp.LVal, from, to int) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	cells = cells[from:to]
	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(cells) // want `toVector constructs a lisp\.LVal over backing storage borrowed from .in. without carrying its sealed constraint`
	} else {
		newVal = toList(cells) // want `toList constructs a lisp\.LVal over backing storage borrowed from .in. without carrying its sealed constraint`
	}
	return newVal, nil
}

// issue392Fixed is the copy-on-write fix: consulting in.IsSealed() both
// freshens the slice and makes the function seal-aware about `in`.
func issue392Fixed(in *lisp.LVal, from, to int) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	cells = cells[from:to]
	if in.IsSealed() {
		cells = copyCells(cells)
	}
	return toList(cells), nil
}

// directConstructor: no wrapper, no helper — the primitive straight over a
// parameter's cells.
func directConstructor(v *lisp.LVal) *lisp.LVal {
	return lisp.SExpr(v.Cells[1:]) // want `lisp\.SExpr constructs a lisp\.LVal over backing storage borrowed from .v. without carrying its sealed constraint`
}

// arrayBorrowsSecondArgument: lisp.Array's borrowing position is 1, not 0.
// The dims argument is an LVal and must not be mistaken for the backing.
func arrayBorrowsSecondArgument(v *lisp.LVal) *lisp.LVal {
	return lisp.Array(nil, v.Cells) // want `lisp\.Array constructs a lisp\.LVal over backing storage borrowed from .v. without carrying its sealed constraint`
}

// bytesBorrow: the .Bytes() backing array is a borrow too.
func bytesBorrow(v *lisp.LVal) *lisp.LVal {
	return lisp.Bytes(v.Bytes()[1:]) // want `lisp\.Bytes constructs a lisp\.LVal over backing storage borrowed from .v. without carrying its sealed constraint`
}

// THE BOUNDARY.  A fresh slice that happens to hold borrowed ELEMENT
// pointers is not a borrow: sharing elements is normal and safe, sharing the
// array is the bug.  This is libelpspath's iterPath.Get, and it must stay
// clean or the rule is a nuisance.
func freshSliceBorrowedElements(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	var results []*lisp.LVal
	for _, item := range horizon {
		results = append(results, item)
	}
	return toList(results), nil
}

// makeThenCopy is the other freshening idiom: explicit make + copy.
func makeThenCopy(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	fresh := make([]*lisp.LVal, len(cells))
	copy(fresh, cells)
	return toList(fresh), nil
}

// zeroCapRebaseIsFresh: append(x[:0:0], x...) must reallocate.
func zeroCapRebaseIsFresh(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	return toList(append(cells[:0:0], cells...)), nil
}

// appendOntoBorrowedBase is NOT fresh: append may write into the source's
// spare capacity and the result may stay a window onto it.  This is #369
// mechanism 2's shape.
func appendOntoBorrowedBase(in *lisp.LVal, x *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	return toVector(append(cells, x)), nil // want `toVector constructs a lisp\.LVal over backing storage borrowed from .in. without carrying its sealed constraint`
}

// ownStorageIsNotABorrow: aliasing an LVal this function constructed is this
// function's own business.
func ownStorageIsNotABorrow(x *lisp.LVal) *lisp.LVal {
	mine := lisp.QExpr([]*lisp.LVal{x})
	return lisp.SExpr(mine.Cells)
}

// sealAnnotatedLine: the escape hatch on the flagged line.
func sealAnnotatedLine(v *lisp.LVal) *lisp.LVal {
	return lisp.SExpr(v.Cells) //elps:unsealed fixture: asserts the backing can never belong to a sealed value
}

// sealAnnotatedLineAbove: the escape hatch on the line above.
func sealAnnotatedLineAbove(v *lisp.LVal) *lisp.LVal {
	//elps:unsealed fixture: asserts the backing can never belong to a sealed value
	return lisp.SExpr(v.Cells)
}

//elps:unsealed fixture: whole-function suppression via the doc comment
func sealAnnotatedFunction(v *lisp.LVal) *lisp.LVal {
	return lisp.SExpr(v.Cells)
}

// aliasCarriesSeal mirrors libelpspath's `alias(in, cells)` — the helper the
// #392 fix introduced.  It is a borrowing constructor (it hands `cells`
// straight to lisp.SExpr) but it also propagates the constraint itself, so
// the discipline lives in ONE place and its call sites are clean.  A rule
// that could not see this would report every caller of the fix.
func aliasCarriesSeal(in *lisp.LVal, cells []*lisp.LVal) *lisp.LVal { // want aliasCarriesSeal:"borrowsLValBacking\\(1\\)\\+carriesSeal"
	out := lisp.SExpr(cells)
	out.InheritSeal(in)
	return out
}

// callsCarryingConstructor is the shape of every fixed libelpspath op: the
// backing is borrowed, and there is no seal handling here at all — because
// the callee did it.
func callsCarryingConstructor(in *lisp.LVal, from, to int) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	return aliasCarriesSeal(in, cells[from:to]), nil
}

// aliasWithoutInherit is the same helper with the propagation removed: still
// a borrowing constructor, no longer a carrying one, so its callers flag.
func aliasWithoutInherit(cells []*lisp.LVal) *lisp.LVal { // want aliasWithoutInherit:"borrowsLValBacking\\(0\\)"
	return lisp.SExpr(cells)
}

func callsNonCarryingConstructor(in *lisp.LVal, from, to int) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	return aliasWithoutInherit(cells[from:to]), nil // want `aliasWithoutInherit constructs a lisp\.LVal over backing storage borrowed from .in. without carrying its sealed constraint`
}

// inheritSealAtTheCallSite: the out-of-package propagation spelled inline.
func inheritSealAtTheCallSite(in *lisp.LVal, from, to int) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	out := toList(cells[from:to])
	out.InheritSeal(in)
	return out, nil
}
