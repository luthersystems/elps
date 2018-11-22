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
			lerr.Source = expr.Source
			return nil, GoError(lerr)
		}
	}

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
		return fmt.Errorf("cannot cache reference type expression: %v", v.Type)
	}
	for _, cell := range v.Cells {
		err := checkLoaderExpr(cell)
		if err != nil {
			return err
		}
	}
	return nil
}
