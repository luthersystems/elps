// Copyright © 2026 The ELPS authors

package lisp

// AST→value leak-point sealing (hermetic-sealing experiment F).
//
// A host that caches parser output and evaluates the same expression tree in
// many runtimes (substrate's parse cache is the motivating consumer) relies
// today on a structural invariant for safety: elps literal syntax can only
// express lists, symbols, strings and numbers, and every mutating builtin
// (append!, assoc!, ...) type-errors on those types, so no lisp-reachable
// mutation can touch an AST node.  That invariant is real but implicit — a
// single future builtin that mutates a list in place would silently corrupt
// the shared AST for every runtime in the process.
//
// This file makes the guarantee explicit at the evaluator level: every path
// where evaluation hands a node of the input expression tree to the value
// domain (returns it as a result, or binds it where lisp code can obtain it
// as a first-class value) copies the subtree instead of aliasing it.  The
// sealed leak points are:
//
//   - quote evaluation: eval's Quoted fast path ('x, '(1 2 3), and LQuote
//     chains) and the quote special operator (op.go opQuote).
//   - quasiquote: the literal (non-unquoted) leaves of a quasiquoted form
//     (macro.go findAndUnquote); the skeleton was already rebuilt fresh.
//   - macro arguments: macros receive their arguments UNEVALUATED, and the
//     macro body sees them as ordinary first-class values (it can store them,
//     not just splice them into the expansion), so macroCall seals the
//     argument list before binding (env.go macroCall).
//   - function objects: env.Lambda copies the formals list and body
//     expressions it embeds in the created LFun, so a function value never
//     retains the expression tree it was defined from (env.go Lambda; covers
//     lambda, expr/#^, flet, labels, macrolet, and — via the sealed macro
//     arguments — defun/defmacro/deftype).
//   - self-evaluating atoms and keywords (eval's default and keyword
//     branches, env.get's keyword branch, opQualifiedSymbol): sealed only
//     when copyAtomsAtLeakPoints is true; see that constant for the measured
//     decision.
//
// With these seals in place, mutating any value reachable from an
// environment can no longer corrupt the expression tree the value was
// evaluated from.  This is the load-bearing mechanism of the proposed
// ast.Node/LVal bifurcation, implemented without the type split: the type
// split would make the same boundary a compile-time property (ast.Node has
// no mutable fields) instead of a runtime copy discipline.
//
// lisp/ast_leak_test.go proves the property: it simulates the shared parse
// cache, evaluates through it, and asserts zero pointer intersection between
// the cached tree and everything reachable from the runtime's value domain.

// copyAtomsAtLeakPoints selects whether leaf atoms (symbols, keywords,
// strings, numbers) are also copied at the leak points, or only container
// nodes (s-expressions and quote forms — anything with Cells storage).
//
// This is a measured decision, not an oversight:
//
//   - Copying only containers costs nothing on the hottest paths: a
//     self-evaluating literal (the 1 in (+ x 1)) and a quoted symbol (the 'k
//     in (get m 'k)) evaluate with zero allocations, exactly as before the
//     seals.  Atom copying turns every constant reference in every function
//     body into an allocation per call; measured on the quote/atom stress
//     benchmarks (BenchmarkSealQuotedAtom) it is the difference between 0
//     and 1 alloc on the per-literal fast path, and it is visible in the
//     arithmetic-heavy repo benchmarks.
//   - The safety loss is bounded and covered elsewhere: an atom has no Cells,
//     so the only way to corrupt a shared atom is a direct Go field write
//     (.Str/.Int/.Float/.Quoted) on a value that was not freshly constructed
//     — exactly the class the elpsvet freshness rule flags at vet time
//     (issue #333/#334 lineage), and unreachable from lisp code, which has
//     no atom-mutating builtin at all.
//
// The container/atom boundary is also exactly the boundary of today's
// structural invariant: containers are corruptible through Cells surgery by
// a hypothetical buggy builtin, atoms are not.
const copyAtomsAtLeakPoints = false

// astToValue returns a copy of v suitable for handing to the value domain
// when v is (or may be) a node of a cached expression tree.
//
// Only AST-representable nodes are copied: s-expressions and quote forms are
// copied with fresh Cells storage, and leaf atoms are copied or shared per
// copyAtomsAtLeakPoints.  Any other node type (LFun, LNative, LArray,
// LSortMap, LBytes, LError, LTaggedVal, internal marks) cannot be produced
// by the parser; encountering one means the subtree was synthesized at
// runtime (e.g. a macro expansion embedding a function or native value), so
// the node is already value-domain and is returned as-is — copying it would
// break pointer-identity semantics (natives are compared by identity;
// detaching an array or sorted-map would silently change aliasing
// behavior).  Because such nodes are returned without descent, a value
// embedded in an expression keeps its within-runtime sharing exactly as
// before sealing.
//
// The copy shares source locations (immutable once parsing completes, and
// shared across LVals by design — see SetSource) and Meta (format-preserving
// metadata, read-only after parse).  Parser output is a strict tree, so no
// visited-set is needed; synthesized cyclic structures cannot occur in the
// descended types because no lisp operation mutates list cells after
// construction (the same structural invariant the seals exist to make
// explicit).
func astToValue(v *LVal) *LVal {
	if isSingleton(v) {
		// Shared by design, immutable by decree; copying one would defeat
		// the singleton corruption checks without any safety gain.
		return v
	}
	switch v.Type {
	case LSExpr, LQuote:
		cp := &LVal{}
		*cp = *v
		if len(v.Cells) > 0 {
			cells := make([]*LVal, len(v.Cells))
			for i := range v.Cells {
				cells[i] = astToValue(v.Cells[i])
			}
			cp.Cells = cells
		}
		return cp
	case LSymbol, LQSymbol, LString, LInt, LFloat:
		if !copyAtomsAtLeakPoints {
			return v
		}
		cp := &LVal{}
		*cp = *v
		return cp
	default:
		return v
	}
}

// sealMacroArgs returns args with every argument expression sealed by
// astToValue.  args itself is replaced (never mutated) because callers hand
// macroCall a wrapper whose Cells may be shared with other views of the
// call.
func sealMacroArgs(args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return args
	}
	cells := make([]*LVal, len(args.Cells))
	for i := range args.Cells {
		cells[i] = astToValue(args.Cells[i])
	}
	return SExpr(cells)
}
