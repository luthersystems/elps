// Copyright © 2026 The ELPS authors

package lisp

// Borrowed views: minting an LVal over another value's live Go backing.
//
// THE BUG CLASS.  An LVal minted over borrowed backing storage is a second
// header onto storage a first header already governs.  Whatever the first
// header's constraints are — today: `sealed`, the parse-cache no-write flag
// — they are properties of the STORAGE, not of the header, so the new
// header must inherit them.  A producer that forgets hands the value domain
// an unconstrained window onto constrained storage, and the next write
// through the window corrupts the original.  luthersystems/elps#392 is that
// with a sealed program literal on the far side (a read-only elpspath range
// query laundered a literal's cells into an unsealed list, and `append
// 'vector` then wrote the literal); #373 is the capacity half of the same
// shape on bytes.
//
// WHY A CONSTRUCTOR AND NOT A DISCIPLINE.  The propagation itself is one
// line — `r.sealed = v.sealed` — and the kernel already had it in three
// places (builtinCdr, builtinRest, builtinSlice).  It has now been
// forgotten twice: once in the #369 audit, once in libelpspath.  A rule
// that must be remembered at every new producer has a defect rate that
// scales with the number of producers, and the producers keep arriving.
//
// So the borrow is a CONSTRUCTOR whose source is a parameter.  There is no
// way to spell a borrowed view without naming what it was borrowed from:
//
//	v.SliceCells(i, j)          // receiver IS the provenance
//	v.SliceBytes(i, j)
//	BorrowCells(src, cells)     // when the caller already has the subslice
//	BorrowVector(src, cells)
//	BorrowBytes(src, b)
//
// and the transfer of constraints happens in ONE function, borrowFrom.  A
// future flag ("owned by runtime R", "immutable", "tainted") is added to
// borrowFrom once and every borrowed view in the tree inherits it — the
// property the hand-maintained discipline never had.
//
// CAPACITY IS NOT THE INVARIANT, BUT IT IS FREE.  Every borrow here is a
// three-index slice, so a view can never see spare capacity it does not
// own.  That closes the #373 mode (a write PAST the view's length) at zero
// cost.  It does nothing at all for the #392 stable-sort mode (a write
// WITHIN the view's length), which is why the flag transfer above, not the
// clamp, is the class fix.  Both are needed; only one of them generalises.
//
// WHAT THIS DOES NOT DO.  A producer can still write `lisp.SExpr(src.Cells[i:j])`
// and get an unconstrained view; Go has no way to make that a compile error
// short of unexporting LVal.Cells.  Two backstops cover the residual: the
// elpsvet freshness rule flags the pattern in-repo, and checked builds
// (-tags elpscheck) fingerprint every sealed parse and fail the run when
// one drifts.  Neither is prevention; the constructor is.

// borrowFrom transfers to dst every constraint that follows src's backing
// STORAGE rather than src's header.  It is the single point of truth for
// what "inheriting provenance" means: a new storage-scoped flag on LVal is
// added here and nowhere else.
//
// Today that is exactly one flag.  `sealed` marks a node of a parsed
// program whose storage may be shared by every environment evaluating the
// same parse (lisp/seal.go); a view onto sealed storage is sealed, and the
// kernel's copy-on-write mutation sites then see the flag and copy instead
// of writing.
//
// Header-scoped state is deliberately NOT transferred: `quoted` and
// `spliced` describe how a particular header is to be read, not what may
// be done to the bytes behind it.
func borrowFrom(dst, src *LVal) *LVal {
	if src == nil || dst == nil {
		return dst
	}
	dst.sealed = src.sealed
	return dst
}

// SliceCells returns a list header over the cells of v in the half-open
// range [i, j).  The result SHARES v's backing array — no copy is made —
// and therefore inherits every constraint v carries (see borrowFrom).
//
// v may be a list or a vector; the cells taken are the sequence's cells in
// either case, and the result is always a list.  Callers wanting a vector
// view must use BorrowVector, because a vector is mutable through append!
// and wrapping constrained storage in one needs a deliberate decision at
// the call site.
//
// The returned slice is capacity-clamped: appending to it allocates rather
// than writing storage beyond j that v's owner still governs.
//
// Bounds are the caller's responsibility, exactly as for a Go slice
// expression: SliceCells panics on an out-of-range index.  Every in-kernel
// caller validates first and reports a lisp error.
func (v *LVal) SliceCells(i, j int) *LVal {
	cells := seqCells(v)
	return mintBorrowedCells(v, cells[i:j:j])
}

// mintBorrowedCells builds the list header directly rather than through
// QExpr, so the constraint is in place before the checked-build mint
// detector looks at it (lisp/borrow_check_elpscheck.go).  Going through
// QExpr would report every legitimate borrow.
func mintBorrowedCells(src *LVal, cells []*LVal) *LVal {
	r := &LVal{Type: LSExpr, quoted: true, Cells: cells}
	borrowFrom(r, src)
	checkMintedOverConstrained(r, cells)
	return r
}

// mintBorrowedBytes is mintBorrowedCells for LBytes.
func mintBorrowedBytes(src *LVal, b []byte) *LVal {
	r := &LVal{Type: LBytes, Native: &b}
	borrowFrom(r, src)
	checkMintedOverConstrainedBytes(r, b)
	return r
}

// SliceBytes returns an LBytes over v's bytes in the half-open range
// [i, j).  The result SHARES v's backing array and inherits v's
// constraints.
//
// The slice is capacity-clamped, which is what stops the #373 shape: a
// later (append-bytes view ...) allocates instead of writing the bytes at
// [j, cap) that still belong to v.
//
// v must be an LBytes; SliceBytes panics otherwise (LVal.Bytes' contract).
func (v *LVal) SliceBytes(i, j int) *LVal {
	b := v.Bytes()
	return mintBorrowedBytes(v, b[i:j:j])
}

// BorrowCells returns a list header over cells, which the caller declares
// to be (a subslice of) src's live backing array.
//
// Use it where the subslice has already been computed — an embedder's path
// walker, say — and SliceCells' index form does not fit.  The result
// inherits src's constraints; passing the wrong src is a correctness bug
// the type system cannot catch, but omitting src is impossible.
//
// cells is capacity-clamped on the way in.
func BorrowCells(src *LVal, cells []*LVal) *LVal {
	return mintBorrowedCells(src, cells[:len(cells):len(cells)])
}

// BorrowVector returns a vector over cells, which the caller declares to
// be (a subslice of) src's live backing array.
//
// A vector is MUTABLE — append! writes its backing in place — so this
// constructor is the one place in the borrow API where the caller is
// handing out a write window.  When src is sealed that is never
// admissible, so the cells are copied instead and the result is an
// independent vector: the same copy-on-write bargain builtinSlice already
// strikes for (slice 'vector <sealed>).
func BorrowVector(src *LVal, cells []*LVal) *LVal {
	if src != nil && src.sealed {
		fresh := make([]*LVal, len(cells))
		copy(fresh, cells)
		cells = fresh
	}
	return Vector(cells[:len(cells):len(cells)])
}

// BorrowBytes returns an LBytes over b, which the caller declares to be (a
// subslice of) src's live backing array.  The result inherits src's
// constraints and b is capacity-clamped.
func BorrowBytes(src *LVal, b []byte) *LVal {
	return mintBorrowedBytes(src, b[:len(b):len(b)])
}

// BorrowSortedMap returns a sorted-map header over data, which the caller
// declares to be src's live map backing.
//
// SortedMapFromData is the other mint-over-borrowed-backing constructor in
// the exported surface, and it is the one the #392 shape reaches next: it
// takes a *MapData and no provenance, so `SortedMapFromData(other.Map())`
// produces two headers over one map with nothing recording that fact.  No
// in-kernel producer does this — assoc/dissoc copy the MapData first and
// only the documented ! variants write in place — but the constructor is
// exported, so an embedder's path walker can, exactly as libelpspath's
// did for cells.
//
// The map is copied when src is sealed: a sorted-map is mutable through
// assoc!, and a value that may be shared by every environment evaluating
// one parse must not be handed a write window.  Sealed maps cannot arise
// from the parser today; the guard is here so that the day an embedder
// seals a value tree containing one, this constructor is already correct.
func BorrowSortedMap(src *LVal, data *MapData) *LVal {
	if src != nil && src.sealed {
		cp, err := src.copyMapData()
		if err == nil {
			return SortedMapFromData(cp)
		}
	}
	v := SortedMapFromData(data)
	borrowFrom(v, src)
	return v
}
