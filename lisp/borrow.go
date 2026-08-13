// Copyright © 2026 The ELPS authors

package lisp

// In-kernel borrow helpers.
//
// A borrowed view is an LVal minted over another value's live Go backing
// storage: a second header onto storage a first header already governs.
// The constraints that follow that STORAGE — today exactly one, `sealed`
// (lisp/seal.go) — must travel with it, or the view is a laundering step
// and the next write through it corrupts the original (elps#392).
//
// The kernel had this propagation written out by hand at three sites
// (builtinCdr, builtinRest, builtinSlice) and missing at a fourth
// (opCond, found by the detector).  These helpers make the source a
// PARAMETER instead of a rule to remember: there is no way to spell a
// borrowed view here without naming what it was borrowed from, and the
// transfer happens in one function, so a future storage-scoped flag is
// added once.  They are kept small enough to inline, so the kernel pays
// nothing for the indirection.
//
// THEY ARE DELIBERATELY UNEXPORTED, and that is the whole shape of this
// arm.  Exporting them would be a six-function public API for a property
// the language cannot enforce anyway — a producer can always write
// `lisp.SExpr(src.Cells[i:j])` instead.  Inside package lisp they are
// worth having for two reasons that do not need an API:
//
//   - PREVENTION where prevention is possible.  A new kernel producer that
//     reaches for mintBorrowedCells cannot forget the transfer: the source
//     is the first parameter and there is no overload without it.
//   - COST.  A helper sets the constraint BEFORE the checked-build
//     detector looks at the header, so an in-kernel borrow is discharged
//     structurally and never enters the detector's pending table.  That
//     removes the per-mint stack capture from every cdr, rest and slice in
//     a checked run — measured at roughly a 2x difference on the tightest
//     borrow microbenchmarks.
//
// Code OUTSIDE package lisp cannot use these and does not need to: it
// discharges through the exported (*LVal).InheritSeal on the statement
// after the mint, which the detector accepts because discharge is defined
// by the post-condition rather than by the route (see
// lisp/borrow_check_elpscheck.go).
//
// These helpers change no behaviour.  They are the propagation the kernel
// already performed, moved behind one name; in particular they do NOT
// capacity-clamp the borrowed slice.  Clamping would close the #373
// spare-capacity shape, which is prevention, and this arm is the detector.

// borrowFrom transfers to dst every constraint that follows src's backing
// STORAGE rather than src's header.  It is the single point of truth for
// what "inheriting provenance" means: a new storage-scoped flag on LVal is
// added here and nowhere else.
//
// Today that is exactly one flag.  `sealed` marks a node of a parsed
// program whose storage may be shared by every environment evaluating the
// same parse, and the kernel's copy-on-write mutation sites consult it
// before writing.
//
// Header-scoped state is deliberately NOT transferred: `quoted` and
// `spliced` describe how a particular header is to be read, not what may
// be done to the bytes behind it.
func borrowFrom(dst, src *LVal) *LVal {
	if dst == nil || src == nil {
		return dst
	}
	//elps:mutates -- seal provenance for a freshly minted header over src's backing; monotone flag only, set before the header is published
	dst.sealed = src.sealed
	return dst
}

// mintBorrowedCells builds a list header over cells, which the caller
// declares to be (a subslice of) src's live cell backing.
//
// The header is built directly rather than through QExpr so the constraint
// is in place before the checked-build detector inspects it; going through
// QExpr would enter every legitimate borrow into the detector's pending
// table and pay a stack capture for each.  The detector is still consulted
// afterwards, so a caller that passes the WRONG src — cells belonging to
// some other sealed value — is still reported.
func mintBorrowedCells(src *LVal, cells []*LVal) *LVal {
	return mintBorrowed(src, cells, true)
}

// mintBorrowedSExpr is mintBorrowedCells for an UNQUOTED header — a form
// about to be evaluated rather than a list value.  `quoted` is header
// state, not storage state, so borrowFrom does not carry it and the caller
// must say which it wants.
func mintBorrowedSExpr(src *LVal, cells []*LVal) *LVal {
	return mintBorrowed(src, cells, false)
}

func mintBorrowed(src *LVal, cells []*LVal, quoted bool) *LVal {
	r := &LVal{Type: LSExpr, quoted: quoted, Cells: cells}
	borrowFrom(r, src)
	noteMintOverConstrainedCells(r, cells)
	return r
}

// mintBorrowedBytes is mintBorrowedCells for LBytes.
func mintBorrowedBytes(src *LVal, b []byte) *LVal {
	r := &LVal{Type: LBytes, Native: &b}
	borrowFrom(r, src)
	noteMintOverConstrainedBytes(r, b)
	return r
}

