// Copyright © 2026 The ELPS authors

package lisp

// formalsCopier gives every definition registered through LEnv.AddBuiltins,
// LEnv.AddSpecialOps or LEnv.AddMacros its own formal argument list, carved
// out of two blocks sized for the whole registration call.
//
// Why a copy at all:
//
// Almost every LBuiltinDef in the process comes out of a package-level table
// built ONCE, at Go package initialization: lisp's own langBuiltins,
// langSpecialOps and langMacros, a `var builtins = []*libutil.Builtin{...}` in
// libtime, libregexp, libhelp, libschema, libmath, libbase64 and libstring, and
// the equivalent tables embedders hand to elpsutil.  Each entry's formals were
// constructed by lisp.Formals at that moment and never again.  Installing that
// *LVal directly into the function value -- what the Add* methods used to do --
// gave every LEnv in the process the SAME formals object per definition: an
// embedder running sixty-four concurrent environments had sixty-four `lisp:map`
// bindings whose Cells[0] was one object.  A single in-place write to a formals
// cell, the exact shape of issue #362 where a shared-value assumption did get
// written to, would be visible in every unrelated environment and would be a
// data race besides.  libjson was the accidental control: it builds its table
// from a function called per load, so its formals were already private.  See
// issue #363.
//
// Why it is blocked rather than one LVal.Copy per definition:
//
// The copy lands once per definition per environment load -- never on the
// evaluation path -- but environment construction is a real cost for embedders
// that build an environment per request, and a plain formals.Copy() per
// definition costs one allocation for the list, one per cell and one for the
// cell slice.  Measured interleaved over BenchmarkEnvConstructionCore and
// BenchmarkEnvConstructionFull in lisp/lisplib (n=12, 100ms), a per-definition
// Copy() was +27.97% / +24.95% on allocs/op; carving the copies out of one
// []LVal and one []*LVal per registration call is +0.50% / +5.88% instead, for
// two allocations per call rather than three or more per definition.
//
// What blocking does NOT buy is time: +12.90% / +18.56% on sec/op against
// +14.43% / +20.23% for the per-definition Copy(), which is within a point or
// two of the same number.  The cost of this fix is the BYTES -- +28% on B/op
// either way -- and those are irreducible, because a formals list private to
// each environment is what the fix IS.  Building the tables per load instead
// (the shape libjson already has) allocates the same bytes or more.  The block
// is here to keep the allocation COUNT off the interpreter's ledger, not to
// pretend the memory is free.
//
// A block keeps its LVals alive as a unit.  That is harmless here: every value
// carved out of it is installed in the environment's package registry in the
// same loop and lives exactly as long as the environment does.
type formalsCopier struct {
	vals []LVal
	ptrs []*LVal
}

// newFormalsCopier sizes the blocks for defs.  It returns a value rather than a
// pointer so that the stdlib loaders, which call the Add* methods once per
// definition, do not pay a heap allocation for the copier itself.  Definitions
// whose formals are not block-copyable are skipped here and fall back to
// LVal.Copy in copy.
func newFormalsCopier(defs []LBuiltinDef) formalsCopier {
	var c formalsCopier
	nval, nptr := 0, 0
	for _, def := range defs {
		formals := def.Formals()
		if !blockCopyableFormals(formals) {
			continue
		}
		nval += 1 + len(formals.Cells)
		nptr += len(formals.Cells)
	}
	if nval > 0 {
		c.vals = make([]LVal, nval)
	}
	if nptr > 0 {
		c.ptrs = make([]*LVal, nptr)
	}
	return c
}

// copy returns a private copy of formals, equivalent to formals.Copy().
//
// The block is sized from the same defs slice copy is called for, but a
// definition is free to build its formals afresh on every Formals() call, so
// the sizes are re-checked rather than assumed: any shortfall, and any formals
// shape the block path does not reproduce exactly, falls back to LVal.Copy.
func (c *formalsCopier) copy(formals *LVal) *LVal {
	if !blockCopyableFormals(formals) {
		return formals.Copy()
	}
	n := len(formals.Cells)
	if len(c.vals) < n+1 || len(c.ptrs) < n {
		return formals.Copy()
	}
	// EVERY WRITE BELOW LANDS IN THE BLOCK, never in `formals`.  cp, syms and
	// cells are all carved out of c.vals / c.ptrs, which newFormalsCopier
	// allocated for this registration call and which nothing else can reach
	// yet -- the values are installed into the environment's registry later
	// in the same loop.  `formals` is READ ONLY here, and that is the whole
	// point of the function: it is what makes the shared (and sealed)
	// definition table safe to register from.  elpsvet cannot see that the
	// destination is block storage rather than a caller's value, so each
	// write is annotated rather than the rule relaxed.
	cp := &c.vals[0]
	//elps:mutates initialises a fresh LVal carved from this call's own block; `formals` is only read
	*cp = *formals
	c.vals = c.vals[1:]
	if n == 0 {
		// LVal.copyCells returns nil for an empty list; match it.
		//elps:mutates initialises the block-allocated copy above, not the caller's list
		cp.Cells = nil
		return cp
	}
	syms := c.vals[:n]
	c.vals = c.vals[n:]
	cells := c.ptrs[:n:n]
	c.ptrs = c.ptrs[n:]
	for i, cell := range formals.Cells {
		syms[i] = *cell
		cells[i] = &syms[i]
	}
	//elps:mutates attaches the block-allocated cell slice to the block-allocated copy; neither is the caller's
	cp.Cells = cells
	return cp
}

// blockCopyableFormals reports whether v is a formal argument list the block
// path reproduces exactly: a list of leaf symbols, none of which carries the
// per-node mutable state LVal.Copy has to duplicate separately -- a source
// location of its own (issue #446), SourceMeta or MacroExpansionInfo (issue
// #466).  lisp.Formals builds precisely this, so it is the path every
// registration in this repository takes; anything else defers to LVal.Copy,
// which handles those fields and the LArray/LSortMap cases the block path does
// not model.
func blockCopyableFormals(v *LVal) bool {
	if v == nil || v.Type != LSExpr || !nativeLeafLVal(v) {
		return false
	}
	for _, cell := range v.Cells {
		if cell == nil || cell.Type != LSymbol || len(cell.Cells) != 0 || !nativeLeafLVal(cell) {
			return false
		}
	}
	return true
}

// nativeLeafLVal reports whether a shallow struct copy of v is a complete copy
// of v's own fields -- that is, whether v carries none of the per-node mutable
// state LVal.Copy duplicates explicitly.
//
// A nil source is what "natively constructed" looks like now: nothing stamps
// a shared "<native code>" Location any more, so this reads `source == nil`
// where it used to read `Source == defaultSourceLocation` (issue #362).  A
// formals list built by lisp.Formals satisfies it; anything the parser
// produced does not, and falls back to LVal.Copy, which duplicates the
// location.
func nativeLeafLVal(v *LVal) bool {
	return v.source == nil && v.meta == nil && v.macroExpansion == nil
}
