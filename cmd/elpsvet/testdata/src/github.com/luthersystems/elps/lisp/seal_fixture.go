// Seal-provenance fixtures from INSIDE package lisp, where the `sealed`
// field is reachable and propagation — not copying — is the kernel's idiom.
//
// These live in the stub lisp package rather than in package a because
// `sealed` is unexported: the propagate-the-flag discharge is only
// expressible here, which is exactly the asymmetry the real tree has between
// lisp/builtins.go and lisp/lisplib/libelpspath.
package lisp

// cdrPreFix is builtinCDR as it shipped before the seal work: a new header
// over v's backing array with nothing carrying the constraint across.
func cdrPreFix(v *LVal) *LVal {
	return QExpr(v.Cells[1:]) // want `QExpr constructs a lisp\.LVal over backing storage borrowed from .v. without carrying its sealed constraint`
}

// cdrFixed is the shipped builtinCDR: propagate the flag onto the new header.
func cdrFixed(v *LVal) *LVal {
	r := QExpr(v.Cells[1:])
	r.sealed = v.sealed
	return r
}

// restFixed is builtinRest: same discharge, taint arriving through seqCells
// rather than a direct .Cells selection.
func restFixed(v *LVal) *LVal {
	cells := seqCells(v)
	r := QExpr(cells[1:])
	r.sealed = v.sealed
	return r
}

// sliceFixed is builtinSlice's intermediate: save the flag, rebind the same
// variable to the new header, restore.
func sliceFixed(list *LVal, i, j int) *LVal {
	sealed := list.sealed
	list = QExpr(seqCells(list)[i:j])
	list.sealed = sealed
	return list
}

// appendPreFix is #369 mechanism 2: append into a sealed sequence's spare
// backing capacity, then hand the result out as a mutable vector.
func appendPreFix(seq *LVal, vals []*LVal) *LVal {
	cells := seqCells(seq)
	return Array(nil, append(cells, vals...)) // want `Array constructs a lisp\.LVal over backing storage borrowed from .seq. without carrying its sealed constraint`
}

// appendFixed is the shipped builtinAppend: copy-on-write on the seal.  A
// vector can never be sealed, so propagation is wrong here and copying is
// the fix — the rule accepts either, because both are seal-awareness about
// the source.
func appendFixed(seq *LVal, vals []*LVal) *LVal {
	cells := seqCells(seq)
	if seq.sealed {
		fresh := make([]*LVal, len(cells), len(cells)+len(vals))
		copy(fresh, cells)
		cells = fresh
	}
	return Array(nil, append(cells, vals...))
}

// arglistIsFresh: a builtin's `args` is the per-call list evalSExprCells
// allocates, so slicing its cells is not a borrow.  Without this the rule
// reports every builtin in the kernel.
func arglistIsFresh(env *LEnv, args *LVal) *LVal {
	return SExpr(args.Cells[1:])
}

// arglistFreshnessIsShallow: args.Cells[i] is a CALLER's value — for a
// special operator, a sealed subform straight out of the parse — so a header
// over ITS backing is still a borrow.  This is lisp/op.go's opCond shape.
func arglistFreshnessIsShallow(env *LEnv, args *LVal) *LVal {
	for _, branch := range args.Cells {
		return SExpr(branch.Cells[1:]) // want `SExpr constructs a lisp\.LVal over backing storage borrowed from .branch. without carrying its sealed constraint`
	}
	return args
}

// switchCaseScoping is builtinSlice's shape, and the reason this rule's walk
// is branch-scoped.  Walked flat, `list = String(...)` in the first case
// marks list as storage this function constructed, and the borrow in the
// default case becomes invisible — which made the rule silently vacuous over
// the very function whose discipline it exists to protect.
func switchCaseScoping(list *LVal, i, j int) *LVal {
	switch list.Type {
	case LBytes:
		list = Bytes(list.Bytes()[i:j]) // want `Bytes constructs a lisp\.LVal over backing storage borrowed from .list. without carrying its sealed constraint`
	default:
		list = QExpr(seqCells(list)[i:j]) // want `QExpr constructs a lisp\.LVal over backing storage borrowed from .list. without carrying its sealed constraint`
	}
	return list
}

// guardMustPrecede: a seal read AFTER the construction is not a guard.  It is
// the shape that made the anti-vacuity probe pass vacuously.
func guardMustPrecede(seq *LVal, vals []*LVal) *LVal {
	cells := seqCells(seq)
	out := Array(nil, append(cells, vals...)) // want `Array constructs a lisp\.LVal over backing storage borrowed from .seq. without carrying its sealed constraint`
	if seq.sealed {
		_ = out
	}
	return out
}

// borrowedResultIsNotFreshStorage: QExpr mints a fresh HEADER, but over
// someone else's array.  A second borrow from that header is still a borrow —
// the constraint travels with the storage.
func borrowedResultIsNotFreshStorage(list *LVal, i, j int) *LVal {
	mid := QExpr(seqCells(list)[i:j])
	mid.sealed = list.sealed
	return Array(nil, mid.Cells) // want `Array constructs a lisp\.LVal over backing storage borrowed from .mid. without carrying its sealed constraint`
}
