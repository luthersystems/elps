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
// TextLoader will return an error if r produces any reference types (bytes,
// map, array, native, etc).
func TextLoader(r Reader, name string, stream io.Reader) (Loader, error) {
	exprs, err := r.Read(name, stream)
	if err != nil {
		return nil, err
	}
	for _, expr := range exprs {
		err := checkLoaderExpr(expr)
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

// errReaderTreeUnbounded marks reader output that is not a finite, strict,
// bounded-depth tree: a cycle, an interned (shared) subtree, or nesting past
// the admission budget.  A real parse is a strict tree well within these
// bounds — rdparser caps parse depth at 10,000 and shares no non-singleton
// node — so output that trips them did not come from a parser and is refused.
//
// It is a distinct sentinel because, unlike an ordinary admission refusal (a
// reference type, or a node no seal can cover), such output is not merely
// un-cacheable: it is unsafe to hand to the evaluator too — evaluating an
// interned subtree is exponential, and a cycle is stopped only by the eval
// nesting cap, after doing that work.  (*LEnv).readCached fails the load on
// this sentinel instead of falling back to an uncached eval.
var errReaderTreeUnbounded = errors.New("reader output is not a finite strict tree (cycle, shared subtree, or too deep to admit)")

const (
	// loaderWalkMaxNodes bounds the total distinct nodes the admission walk
	// visits.  It mirrors sealFPMaxNodes (lisp/sealfp.go): orders of magnitude
	// beyond any top-level expression a parser emits, present only to bound
	// hand-built or adversarial reader output.
	loaderWalkMaxNodes = sealFPMaxNodes
	// loaderWalkMaxDepth bounds admission recursion depth so a very deep strict
	// tree cannot exhaust the Go stack before the node budget notices.  It
	// mirrors sealFPMaxDepth; parse depth caps at 10,000 (rdparser), leaving
	// wide headroom.
	loaderWalkMaxDepth = sealFPMaxDepth
)

// checkLoaderExpr reports whether v is admissible reader output — safe both to
// cache (share across environments) and, for the load path, to evaluate.
//
// It rejects reference types (whose mutable backing every copy of the tree
// would share) and, since issue #368 put this walk on the `load-file` path via
// Runtime.LoadCache, it also BOUNDS the walk.  Before that the recursion had no
// cycle guard, depth cap, or memo: a Reader returning a cyclic graph made it
// recurse until the Go stack overflowed (unrecoverable), and one returning an
// interned shared subtree made it re-descend that subtree once per path —
// exponential.  Both were newly reachable from `load-file` the moment a cache
// was installed.  The bounded walk (loaderWalk) rejects a node reached twice
// (cycle or sharing), output past the depth cap, and output past the node
// budget, all with errReaderTreeUnbounded, and runs in O(distinct nodes)
// time and space.  Singletons are exempt from the no-repeat rule: they are
// shared by design and immutable, and a parse may reference them from many
// positions.
//
// Because newProgram runs this pass FIRST, the walks after it (firstUnsealed
// and (*LVal).Copy, neither of which memoises) only ever see a finite strict
// tree and inherit its bound.
func checkLoaderExpr(v *LVal) error {
	w := loaderWalk{seen: make(map[*LVal]struct{}), budget: loaderWalkMaxNodes}
	return w.check(v, 0)
}

// loaderWalk carries the admission walk's cycle/sharing memo and its node
// budget.  See checkLoaderExpr.
type loaderWalk struct {
	seen   map[*LVal]struct{}
	budget int
}

func (w *loaderWalk) check(v *LVal, depth int) error {
	if v == nil {
		return nil
	}
	// Singletons (Nil/true/false) are shared by design and immutable, so a
	// parse may legitimately reach one from many positions.  They are exempt
	// from the no-repeat rule and terminate the walk at once.
	if isSingleton(v) {
		return nil
	}
	if depth > loaderWalkMaxDepth {
		return errReaderTreeUnbounded
	}
	if _, ok := w.seen[v]; ok {
		// A non-singleton node reached twice is a cycle or an interned shared
		// subtree — not a strict parser tree.
		return errReaderTreeUnbounded
	}
	if w.budget <= 0 {
		return errReaderTreeUnbounded
	}
	w.budget--
	w.seen[v] = struct{}{}

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
	for _, cell := range v.Cells {
		if err := w.check(cell, depth+1); err != nil {
			return err
		}
	}
	return nil
}
