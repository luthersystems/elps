// Copyright © 2018 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"io"
)

type Loader func(*LEnv) *LVal

// Reader abstracts a parser implementation so that it may be implemented in a
// separate package as an optional/swappable component.
type Reader interface {
	// Read the contents of r and return the sequence of LVals that it
	// contains.  The returned LVals should be executed as if inside a progn.
	Read(name string, r io.Reader) ([]*LVal, error)
}

// LocationReader is like Reader but assigns physical locations to the tokens
// from r.
type LocationReader interface {
	// ReadLocation the contents of r, associated with physical location loc,
	// and return the sequence of LVals that it contains.  The returned LVals
	// should be executed as if inside a progn.
	ReadLocation(name string, loc string, r io.Reader) ([]*LVal, error)
}

// LoaderMust returns its first argument when err is nil.  If err is nil
// LoaderMust panics.
//
// NOT LISP-REACHABLE (#367): this is the Go `Must` idiom, for an embedder
// building a Loader from a source it controls (a //go:embed constant, a
// literal) at start-up.  Nothing in the interpreter calls it, so no evaluated
// program can reach it; an embedder wrapping a source a program supplies
// should handle the error instead.
func LoaderMust(fn Loader, err error) Loader {
	if err != nil {
		panic(err)
	}
	return fn
}

// TextLoader parses a text stream using r and returns a Loader which evaluates
// the stream's expressions when called.  The reader will be invoked only once.
//
// TextLoader returns an error if r produces any reference type (bytes, map,
// array, native, etc), if any node carries a Native payload on a type the seal
// would otherwise mark (see admitExpr), or if r's output is not finite:
// a cycle, or nesting past loaderWalkMaxDepth.  It does NOT reject node
// SHARING and imposes no node budget — a Reader that interns symbols, or one
// that returns a single very large expression, loads exactly as it always has.
// Those two extra rules exist only for Runtime.LoadCache, whose entries are
// aliased into unboundedly many environments, and they are applied only there
// (see admitExpr, and the strict walk newLoaderWalk builds for it).
func TextLoader(r Reader, name string, stream io.Reader) (Loader, error) {
	exprs, err := r.Read(name, stream)
	if err != nil {
		return nil, err
	}
	// One walk state for the whole stream, not one per expression: it costs a
	// single map for the file instead of len(exprs) of them, and it lets the
	// cycle guard see a cycle that closes across two top-level expressions.
	w := newLoaderWalk(false)
	for _, expr := range exprs {
		err := admitExpr(expr, w)
		if err != nil {
			lerr := Error(err)
			// Copied, not aliased: the error escapes to the embedder through
			// GoError while expr stays part of the loaded program, so the
			// two must not share a *token.Location (cold path; the copy is
			// free in practice).
			lerr.source = copyLocation(expr.source)
			return nil, GoError(lerr)
		}
	}

	// THE PER-LOAD COPY STAYS, and the sealing work is why it is worth
	// saying so rather than leaving the Copy() unremarked.
	//
	// The seal makes sharing this parse SAFE -- sealed nodes are frozen
	// storage under the seal's write protection, and the ownership checker
	// exempts them for exactly that reason -- and elpstest.RunBenchmark
	// takes that share, because its consumer is in this repository and its
	// contract is about measurement rather than about what a caller may do
	// with the tree.
	//
	// TextLoader is different on both counts.  It is PUBLIC API whose
	// documented behaviour is that every load gets its own tree: issue #446
	// is specifically about that guarantee reaching positions as well as
	// cells, and TestTextLoaderEvaluationsGetPrivatePositions pins it.
	// Taking the share would move an embedder that mutates what a Loader
	// handed it -- legal, if unwise, under the current contract -- from
	// ownership to the seal's refusal.  And it would buy nothing today: TextLoader
	// has no callers in this repository, and the downstream sweep for issue
	// #379 found that embedders reach elps through the Reader path, which
	// never had this copy.  A public contract should not change for a
	// speculative consumer, so the optimization is left on the table with
	// its measurements recorded (#379 item 4: -72.3% sec/op, -61.9%
	// allocs/op on a 50 KB source) for whoever has a caller to justify it.
	fn := func(env *LEnv) *LVal {
		var lval *LVal
		for _, expr := range exprs {
			lval = env.Eval(expr.Copy())
			if lval.Type == LError {
				return lval
			}
		}
		if lval == nil {
			return Nil()
		}
		return lval
	}

	return fn, nil
}

// errReaderTreeUnbounded marks reader output that is not a finite tree: a
// cycle, nesting past the admission depth cap, or — on the cache path only —
// an interned (shared) subtree.  A real parse is a strict tree well within
// these bounds — rdparser caps parse depth at 10,000 and shares no
// non-singleton composite node — so output that trips them did not come from a
// parser and is refused.
//
// It is a distinct sentinel because, unlike an ordinary admission refusal (a
// reference type, or a node no seal can cover), such output is not merely
// un-cacheable: it is unsafe to hand to the evaluator too — evaluating an
// interned subtree is exponential in the sharing depth, and a cycle is stopped
// only by the eval nesting cap, after doing that work.  (*LEnv).readCached
// fails the load on this sentinel instead of falling back to an uncached eval.
var errReaderTreeUnbounded = errors.New("reader output is not a finite tree (cycle, shared subtree, or too deep to admit)")

// errReaderTreeTooLarge marks reader output that is finite, strict and legal
// but larger than the cache admission's node budget.
//
// It is deliberately NOT errReaderTreeUnbounded, and the difference is the
// whole point: a cycle or an interned subtree is unsafe to evaluate, but a
// node COUNT is not — a single enormous top-level expression is an ordinary
// program.  Refusing to cache it must therefore behave exactly as it does
// with no cache installed, so (*LEnv).readCached falls back to an uncached
// load on this sentinel rather than failing the load (see readCached's
// fall-back list).  Nothing outside the cache path imposes the budget at all.
var errReaderTreeTooLarge = errors.New("reader output exceeds the cache admission node budget")

const (
	// loaderWalkMaxNodes bounds the total distinct nodes the CACHE admission
	// walk visits.  It mirrors sealFPMaxNodes (lisp/sealfp.go): orders of
	// magnitude beyond any top-level expression a parser emits, present only
	// to bound hand-built or adversarial reader output.  It applies to the
	// cache path alone — ReadProgram, ParseProgram and TextLoader have no node
	// budget, so a legal 1.2M-node expression still loads through them.
	loaderWalkMaxNodes = sealFPMaxNodes
	// loaderWalkMaxDepth bounds admission recursion depth so a very deep tree
	// cannot exhaust the Go stack before anything else notices.  It mirrors
	// sealFPMaxDepth; parse depth caps at 10,000 (rdparser), leaving wide
	// headroom.  Unlike the node budget this DOES apply on every path,
	// because the alternative is not a refusal but an unrecoverable Go stack
	// overflow.
	loaderWalkMaxDepth = sealFPMaxDepth
	// loaderWalkNoBudget disables the node budget (the non-cache paths).
	loaderWalkNoBudget = -1
)

// admitExpr reports whether v is admissible reader output, under the rules
// w carries.  It is the single admission walk: TextLoader, ReadProgram,
// ReadLocationProgram and ParseProgram pass a walk from newLoaderWalk(false),
// and Runtime.LoadCache passes newLoaderWalk(true), which adds two rules.
//
// What EVERY path rejects, and why each rule is safe to apply to public API:
//
//   - REFERENCE TYPES (bytes, map, array, native), whose mutable backing
//     every copy of the tree would share.  This is TextLoader's historical
//     rule, unchanged.
//   - A NATIVE PAYLOAD on a type SealAST marks (issue #368 review, finding
//     9).  The seal freezes a node's LVal fields; it does not and cannot
//     freeze whatever an embedder hung off Native, the fingerprint oracle
//     does not hash Native (lisp/sealfp.go says so explicitly), and
//     firstUnsealed's flag+type conjunction admits such a node as-is because
//     its TYPE is sealable.  So a mutable box riding on a sealed-looking
//     LInt would cross environments aliased, unfingerprinted and unreported.
//     No parser produces one — the standard parser sets Native on zero nodes
//     — so the rule costs nothing real and closes the gap at the boundary
//     rather than one level further in.
//   - A CYCLE: a node reached from itself.  Detected with an on-path set, so
//     ordinary node sharing is not mistaken for one.
//   - Nesting past loaderWalkMaxDepth.
//
// What a NON-STRICT walk deliberately does NOT reject, because those paths
// are not the cache and a rule that is right for an aliased process-wide
// entry is not automatically right for public API (issue #368 review,
// blockers 2 and 3):
//
//   - NODE SHARING.  A Reader that interns symbols returns a DAG, which is an
//     ordinary memory optimization and has always loaded.  The walk memoises
//     validated nodes so a DAG still costs O(distinct nodes) rather than
//     O(paths).
//   - SIZE.  There is no node budget, so one very large legal expression is
//     admitted.
//
// Because newProgram runs this pass FIRST, the walks after it (firstUnsealed
// and (*LVal).Copy) see output already known to be acyclic and depth-bounded.
// firstUnsealed memoises for the same reason this does; (*LVal).Copy does not,
// so a DAG it copies is unfolded — that is the pre-existing behaviour of the
// copy path and is why the cache, which cannot afford it, forbids sharing.
//
// The two rules a STRICT walk adds exist only for Runtime.LoadCache, whose
// admitted entry is aliased into unboundedly many environments instead of
// being handed to one load:
//
//   - STRICT TREE among composite nodes.  A node WITH CELLS reached twice is
//     an interned subtree, and the copy path unfolds it and the evaluator
//     re-evaluates it once per path — exponential in the sharing depth (a
//     depth-40 DAG is ~10^12 paths).  Refused with errReaderTreeUnbounded,
//     which readCached turns into a hard load error, because handing it to an
//     uncached eval instead would not terminate either.  A repeated LEAF is
//     explicitly allowed: symbol interning is the common case, it cannot
//     unfold anything (no children to re-descend), and Copy duplicates it in
//     linear time.
//   - A NODE BUDGET.  Exceeding it yields errReaderTreeTooLarge, which
//     readCached treats as "do not cache this one" — the load proceeds
//     uncached, exactly as it would with no cache installed.
func admitExpr(v *LVal, w *loaderWalk) error {
	return w.check(v, 0)
}

// newLoaderWalk builds the admission walk state.  strict selects the cache
// path's extra rules (no shared composite nodes, node budget); one walk is
// shared across all the top-level expressions of one stream so the cycle
// guard spans them and the state costs one map per stream rather than one
// per expression.
func newLoaderWalk(strict bool) *loaderWalk {
	w := &loaderWalk{state: make(map[*LVal]uint8), budget: loaderWalkNoBudget}
	if strict {
		w.strict = true
		w.budget = loaderWalkMaxNodes
	}
	return w
}

// Node states in loaderWalk.state.
const (
	loaderNodeOnPath uint8 = 1 // on the current root->node path: a revisit is a cycle
	loaderNodeDone   uint8 = 2 // fully validated: a revisit is sharing, not a cycle
)

// loaderWalk carries the admission walk's cycle guard, its validated-node
// memo, and (cache path only) its node budget.  See admitExpr.
type loaderWalk struct {
	state  map[*LVal]uint8
	budget int  // loaderWalkNoBudget for the non-cache paths
	strict bool // cache path: no repeated composite nodes
}

func (w *loaderWalk) check(v *LVal, depth int) error {
	if v == nil {
		return nil
	}
	// Singletons (Nil/true/false) are shared by design and immutable, so a
	// parse may legitimately reach one from many positions.  They are exempt
	// from every repeat rule and terminate the walk at once.
	if isSingleton(v) {
		return nil
	}
	if depth > loaderWalkMaxDepth {
		return errReaderTreeUnbounded
	}
	switch w.state[v] {
	case loaderNodeOnPath:
		// Reached from itself: a cycle, on every path.
		return errReaderTreeUnbounded
	case loaderNodeDone:
		if w.strict && len(v.Cells) > 0 {
			// An interned SUBTREE.  Cache path only; see admitExpr for
			// why a repeated leaf is fine and a repeated composite is not.
			return errReaderTreeUnbounded
		}
		return nil
	}
	if w.budget == 0 {
		return errReaderTreeTooLarge
	}
	if w.budget > 0 {
		w.budget--
	}

	switch v.Type {
	case LBytes, LSortMap, LArray, LNative:
		// Reference types share mutable state with every copy of the cached
		// expression, so a cached loader would hand the same backing store to
		// each caller.
		return fmt.Errorf("cannot cache reference type expression: %v", v.Type)
	case LInvalid, LInt, LFloat, LError, LSymbol, LQSymbol, LSExpr, LFun,
		LQuote, LString, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
		// Value types are safe to cache; composite ones (LSExpr, LQuote,
		// LTaggedVal) are covered by the recursion over Cells below.  Listed
		// explicitly because this switch is a denylist: a new LType that
		// wraps shared state would otherwise be cached silently.
	}
	// A Native payload on a type the seal MARKS is not covered by anything
	// downstream: SealAST freezes LVal fields only, the fingerprint oracle
	// skips Native by design, and both the admission conjunction
	// (firstUnsealed) and the ownership exemption key off the TYPE, which is
	// sealable here.  See admitExpr's doc comment.  Types the seal does
	// not mark are unaffected: an LFun's funData, an LError's CallStack and
	// LNative's own payload reach their own rejections.
	if v.Native != nil && sealableNodeType(v.Type) {
		return fmt.Errorf("cannot cache %v expression carrying a native payload", v.Type)
	}

	w.state[v] = loaderNodeOnPath
	for _, cell := range v.Cells {
		if err := w.check(cell, depth+1); err != nil {
			return err
		}
	}
	w.state[v] = loaderNodeDone
	return nil
}
