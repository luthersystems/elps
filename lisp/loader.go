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
