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
			// Copied, not aliased: the error escapes to the embedder through
			// GoError while expr stays part of the loaded program, so the
			// two must not share a *token.Location (cold path; the copy is
			// free in practice).
			lerr.source = copyLocation(expr.source)
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
			// Two requirements meet here, and the allSealed answer above
			// satisfies both without either giving anything up.
			//
			// What every load path owes the rest of the kernel is that the
			// tree it evaluates is SEALED.  Reader, LoadString, Program and
			// the REPL all already do; TextLoader was the one seam that did
			// not, and code loaded through a Loader was invisibly
			// second-class for it — a defmacro evaluated here bound an
			// UNSEALED formals node, disqualifying the macro from
			// per-callsite expansion caching in every environment (#381).
			//
			// What #365 owes is that no MUTABLE storage is shared between
			// runtimes.  When the tree is sealed throughout, nothing here
			// is mutable and sharing it is the point of the seal, so the
			// per-load deep copy is pure cost and goes away (#379/#380);
			// when it is not, the copy stays.
			//
			// So: sealed throughout means share as-is, already sealed and
			// already safe.  Not sealed throughout means take #365's
			// private copy AND seal it, which is where the sealing
			// obligation actually bites — the copy is parser-shaped content
			// (checkLoaderExpr above rejects reference types), it is
			// private to this environment, and nothing mutates loader
			// output in place, so it keeps the immutability contract of the
			// parse it was copied from.
			if !allSealed {
				expr = expr.Copy()
				expr.SealAST()
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
