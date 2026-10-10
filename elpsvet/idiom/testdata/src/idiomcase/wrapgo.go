package idiomcase

import (
	"bytes"
	"compress/gzip"
	"fmt"
	"io"

	"github.com/luthersystems/elps/lisp"
)

// gunzip is scoped (builtinGunzip returns its error), but its errors are Go
// errors, so a wrap of them hides no Lisp condition.
func gunzip(b []byte) ([]byte, error) {
	r, err := gzip.NewReader(bytes.NewReader(b))
	if err != nil {
		return nil, fmt.Errorf("gunzip: %w", err)
	}
	out, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	return out, nil
}

var builtinGunzip = lisp.Func1E(func(env *lisp.LEnv, b []byte) ([]byte, error) {
	out, err := gunzip(b)
	if err != nil {
		return nil, fmt.Errorf("decompress: %w", err)
	}
	return out, nil
})

// lispSource returns a Lisp error through a variable, so a wrap of its
// error is reported.
func lispSource(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
	out, err := lisp.Result(v)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// errorValSource returns a *lisp.ErrorVal as its error.
func errorValSource(v *lisp.LVal) (*lisp.LVal, error) {
	if v.IsError() {
		return nil, (*lisp.ErrorVal)(v)
	}
	return v, nil
}

var builtinSources = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	v, err := lispSource(env, args)
	if err != nil {
		return nil, fmt.Errorf("source: %w", err) // want `fmt.Errorf over a Lisp error`
	}
	if v == nil {
		return v, nil
	}
	w, err2 := errorValSource(v)
	if err2 != nil {
		return nil, fmt.Errorf("source: %w", err2) // want `fmt.Errorf over a Lisp error`
	}
	return w, nil
})
