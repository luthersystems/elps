// Copyright © 2026 The ELPS authors

package lisp

// AST sealing: copy-on-write protection for parsed program literals.
//
// A host that caches parser output and evaluates the same expression tree in
// many environments (substrate's parse cache is the motivating consumer)
// relies on a structural invariant for safety: elps literal syntax can only
// express lists, symbols, strings and numbers, and (almost) every mutating
// builtin type-errors on those types, so (almost) no lisp-reachable mutation
// can touch an AST node.  That invariant was implicit — and it had holes:
// stable-sort swaps a list's cells in place, and append 'vector writes into
// a sequence's spare backing capacity, both reachable from pure lisp code
// (see lisp/cow_seal_test.go for the reproductions).  A single such builtin
// silently corrupts the shared AST for every environment in the process
// (luthersystems/substrate#378 was exactly this class).
//
// The eager alternative (the exp-ast-leakpoints experiment) copied the
// subtree at every point where evaluation hands an AST node to the value
// domain (quote, quasiquote literals, macro arguments, lambda bodies).  That
// buys an airtight pointer-disjointness guarantee but costs a deep copy per
// quoted literal per evaluation — measured at +1.5% to +15.5% per warm
// transaction on a production workload.
//
// This file takes the lazy route: AST nodes are MARKED instead of copied.
//
//   - The parser seals its output: every node of a completed top-level
//     expression gets the sealed flag (SealAST, called by
//     rdparser.ParseExpression at nesting depth zero).  The flag lives in an
//     existing padding byte of LVal — the struct does not grow.
//   - The former leak points do nothing at all.  Quote evaluation, macro
//     argument binding, quasiquote literal leaves and lambda body embedding
//     hand out the sealed nodes directly, exactly as the unsealed evaluator
//     always has.  Zero copies, zero allocations, zero per-evaluation cost.
//   - The kernel's mutation sites check the flag and copy first
//     (copy-on-write) instead of writing shared storage:
//
//       stable-sort        sorts a fresh copy of a sealed list and returns
//                          it (builtinSortStable); the documented in-place
//                          effect on a program literal was never useful —
//                          observing it WAS the corruption.
//       append 'vector     copies a sealed sequence's cells before
//                          appending within spare capacity (builtinAppend).
//       slice 'vector      copies instead of wrapping sealed backing in a
//                          mutable vector (builtinSlice).
//
//     assoc!/dissoc!/append!/append-bytes! need no guard: they type-error on
//     lists, and maps/vectors/bytes cannot be produced by the parser, so a
//     sealed value can never legally reach their mutation path.
//   - The kernel sites that create a NEW header over a sealed node's
//     backing array propagate the flag so the constraint follows the
//     storage: car/cdr-style slicing (builtinCdr, builtinRest,
//     builtinSlice) set it explicitly; header copies (Quote, Splice,
//     shallowUnquote) inherit it through the struct copy.
//   - The evaluator's own metadata writes respect the flag:
//     stampMacroExpansion skips sealed subtrees (their location is the real
//     parse location; stamping a shared node would be a cross-environment
//     write and a data race), and SetSource is a no-op on sealed values.
//   - Copy and detach clear the flag on the fresh storage they create;
//     Copy-then-mutate is the sanctioned pattern for Go code that needs to
//     restructure a value that IsSealed reports as shared.
//
// What the flag cannot do: stop Go code from assigning to exported LVal
// fields directly (v.Cells[0] = x compiles no matter what).  That residual
// is the same one the eager-copy design carries — it ensures values handed
// to embedders are not shared, but does nothing about an embedder that
// reaches the AST itself — and it is covered by the surrounding kernel
// model: the parse/cache boundary exposes no raw AST (lisp.Program, the
// sealed package registry), in-repo Go code is under the elpsvet freshness
// rule which flags exactly this write pattern, and embedder libraries that
// restructure values (substrate's libelpspath) are expected to migrate into
// this module where the rule sees them.  IsSealed gives that code the
// check it needs: refuse ("cannot modify a program literal") or copy.
//
// Atoms are sealed along with containers.  Unlike the eager design — where
// copying atoms was a measured +56% geomean regression and was disabled —
// marking an atom costs one bit at parse time, so there is no reason to
// leave atoms out: the flag makes IsSealed meaningful on every node a
// program literal can produce.

// SealAST marks v and every node reachable through its Cells as sealed
// program-literal nodes.  The parser calls it on each completed top-level
// expression; embedders that build expression trees by hand and share them
// across environments may call it for the same protection.
//
// Only parser-producible node types are marked: s-expressions, quote forms,
// symbols, strings and numbers.  Any other type (functions, arrays, maps,
// bytes, errors, natives) was necessarily constructed at runtime; SealAST
// stops without descending rather than freeze storage the evaluator
// legitimately mutates.  Singletons (Nil/true/false) are skipped: they are
// already immutable by decree and writing even a flag to one would race.
//
// Sealing is idempotent, and an already-sealed node terminates the walk —
// a sealed node's descendants are always sealed, so revisiting them is
// pointless and the check doubles as cycle protection.
func (v *LVal) SealAST() {
	v.sealAST()
	// Checked builds record the tree's fingerprint here — SealAST is the
	// one point every parse path (Reader, LoadString, ParseProgram, the
	// REPL) exits through, so recording at the root of each seal covers
	// all of them.  A no-op in production builds; see
	// lisp/seal_check_elpscheck.go.
	recordSealedRoot(v)
}

func (v *LVal) sealAST() {
	if v == nil || v.sealed || isSingleton(v) {
		return
	}
	switch v.Type {
	case LSExpr, LQuote, LSymbol, LQSymbol, LString, LInt, LFloat:
	default:
		return
	}
	// The write below is the one sanctioned non-fresh LVal write in the
	// sealing design: it happens exactly once per node, after parsing
	// completes and before the tree can be shared, and it only sets the
	// monotone flag that forbids all further writes.
	v.sealed = true //elps:mutates -- parse-completion sealing; single-threaded, pre-sharing, sets the flag that freezes the node
	for _, c := range v.Cells {
		c.sealAST()
	}
}

// IsSealed reports whether v is a sealed program-literal node (or a header
// sharing storage with one).  Go code holding a value that reports true
// must not modify the value in place — modifying it could corrupt the
// program for every environment sharing the parse — and should either
// return an error ("cannot modify a program literal") or work on a Copy
// (whose fresh storage IsSealed reports false).
func (v *LVal) IsSealed() bool {
	return v != nil && v.sealed
}

// InheritSeal marks v sealed when src is.  It is for a v that was just
// minted over storage BORROWED from src — a fresh LVal header wrapping a
// (sub)slice of src's backing array — where the seal has to travel with the
// storage, not with the header that happens to point at it.
//
// The kernel already does this by hand at every such site, and the comment
// it repeats is the whole rule: "a two-index slice keeps the original
// backing array (and its spare capacity), so a sealed input's constraint
// travels with the intermediate value" (builtinSlice; builtinCdr and
// builtinRest say the same).  Those sites live in package lisp and assign
// the unexported field directly.  This method is that same assignment,
// exported, for the value-restructuring libraries the seal design expects
// to migrate into this module — libelpspath being the one named in the
// commentary above, and the one that shipped without it (issue #392).
//
// It is deliberately NOT a general "seal this value" setter:
//
//   - Nothing happens unless src is already sealed.  A caller cannot invent
//     a constraint, only propagate one that exists.
//   - Only the node types SealAST itself marks are eligible.  In particular
//     an array is never marked, because the kernel's mutating vector
//     builtins (append!, assoc!) do not consult the flag at all — a "sealed"
//     vector would be a lie that reads as protection.  Code wrapping sealed
//     backing in a vector must copy instead, exactly as builtinSlice does
//     for (slice 'vector …).
//   - Singletons are skipped: they are born sealed, and writing even a flag
//     to a value shared process-wide would race.
//
// Sealing is monotone, so this is idempotent and safe to call on a value
// that is already sealed.
func (v *LVal) InheritSeal(src *LVal) {
	if v == nil || v.sealed || !src.IsSealed() || isSingleton(v) {
		return
	}
	switch v.Type {
	case LSExpr, LQuote, LSymbol, LQSymbol, LString, LInt, LFloat:
	default:
		return
	}
	// Same sanctioned write as sealAST's: v is a header the caller minted a
	// moment ago and has not published, and the only thing being set is the
	// monotone flag that forbids all further writes.
	v.sealed = true //elps:mutates -- seal provenance for a freshly minted header over src's backing; monotone flag only
}
