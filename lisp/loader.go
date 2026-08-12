// Copyright © 2018 The ELPS authors

package lisp

import (
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
			lerr.source = expr.source
			return nil, GoError(lerr)
		}
	}

	// The Loader is called once per environment, and the SAME parsed exprs
	// are what every call evaluates.  That used to require a deep copy per
	// load, because an LVal shared by two Runtimes was unsafe under any
	// circumstances (issue #365).
	//
	// The seal changes the premise.  A Reader that seals its output (every
	// parse path in this repo does — see lisp/seal.go) hands back frozen
	// program-literal storage under copy-on-write protection: kernel
	// mutation sites copy before writing, the evaluator's metadata writes
	// skip sealed nodes, and checked builds fingerprint every sealed parse
	// so a hole in that protection is a test failure rather than a
	// silently-trusted assumption.  #365's rule narrows to "no MUTABLE
	// cross-runtime sharing", and the ownership checker exempts sealed
	// nodes accordingly.  This is the formals precedent (#374) applied to
	// the loader.
	//
	// A Reader that does NOT seal — an embedder's own implementation of the
	// interface, which this package cannot constrain — keeps #365's
	// per-load copy.  The decision is made once, here, rather than per load.
	//
	// The question must be asked of the WHOLE tree, not of the roots.  The
	// Copy() this replaces was deep, and a sealed root does not imply a
	// sealed tree: SealAST marks only parser-producible shapes and stops
	// WITHOUT DESCENDING at anything else, while checkLoaderExpr above
	// explicitly admits two of those shapes into a cached loader (LFun and
	// LTaggedVal).  A Reader that seals its roots can therefore hand back a
	// tree with unsealed, mutable storage underneath it, and sharing that
	// storage would give every environment the same buffer — exactly the
	// #365 hazard the seal narrows but does not remove.
	allSealed := true
	for _, expr := range exprs {
		if !sealedThroughout(expr, 0) {
			allSealed = false
			break
		}
	}

	fn := func(env *LEnv) *LVal {
		var lval *LVal
		for _, expr := range exprs {
			if !allSealed {
				expr = expr.Copy()
			}
			lval = env.Eval(expr)
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

// sealedThroughout reports whether v and every value reachable through its
// Cells is sealed — the deep question TextLoader must answer before it may
// share one parse with every environment.
//
// Native payloads are not walked and do not need to be: checkLoaderExpr
// rejects every type that carries mutable state in Native (LBytes, LSortMap,
// LArray, LNative) before this runs, so Cells is the whole reachable graph
// of a cacheable expression.
//
// Bounded by sealFPMaxDepth, the same cap the fingerprint walk uses, so a
// Reader returning a cyclic or pathologically deep tree answers "not sealed
// throughout" — and gets the per-load copy — rather than overflowing the
// stack.
func sealedThroughout(v *LVal, depth int) bool {
	if v == nil {
		return true
	}
	if !v.sealed || depth > sealFPMaxDepth {
		return false
	}
	for _, c := range v.Cells {
		if !sealedThroughout(c, depth+1) {
			return false
		}
	}
	return true
}

func checkLoaderExpr(v *LVal) error {
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
		err := checkLoaderExpr(cell)
		if err != nil {
			return err
		}
	}
	return nil
}
