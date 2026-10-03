// Copyright © 2026 The ELPS authors

package libjson

// Error values: an LError is written as ["~#error",["CONDITION",[DATA...]]].
//
// CONDITION is the error's condition type (LVal.Str), the name handler-bind
// matches.  DATA is the error's data (LVal.Cells), each a durable value: for
// an error raised by (error 'c "message" x), the message string and x.  The
// message renders from the data, so it is equal after a load.  An error is
// an object: written once and referenced after, so it can sit in a cycle.
//
// Not saved: the call stack (LVal.Native) and the source location, which
// would make the bytes depend on where the error was raised, and the Go
// error a host error wraps (errors.Unwrap), which a string cell carries
// alongside its text.  A restored error has no stack and no location.  An
// internal panic is never saved: its marker is evidence of a host fault and
// must not be forged by a load.

import (
	"errors"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// tagError is the error extension tag.
const tagError = "~#error"

// errorKey identifies an error by its header.
type errorKey struct{ p *lisp.LVal }

// checkError refuses an error DumpDurable cannot write: an internal panic,
// or a condition that is empty or not valid UTF-8.
func checkError(v *lisp.LVal) error {
	switch {
	case v.Str == lisp.CondInternalPanic:
		return errors.New("durable json: cannot encode an internal panic")
	case v.Str == "" || !utf8.ValidString(v.Str):
		return errors.New("durable json: cannot encode an error whose condition is empty or not UTF-8")
	}
	return nil
}

// scanError visits an error's data.
func (e *durableEncoder) scanError(v *lisp.LVal, key any, depth int) error {
	if err := checkError(v); err != nil {
		return err
	}
	if err := e.cfg.depthError(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	for _, c := range v.Cells {
		if err := e.scan(c, depth+1); err != nil {
			return err
		}
	}
	e.closeNode(i)
	return nil
}

// errorBody writes ["~#error",["CONDITION",[DATA...]]].
func (e *durableEncoder) errorBody(v *lisp.LVal, depth int) error {
	if err := checkError(v); err != nil {
		return err
	}
	if err := e.container(depth); err != nil {
		return err
	}
	if err := e.reserve(jsonStringLen(v.Str) + len(tagError) + 6); err != nil {
		return err
	}
	e.buf = append(e.buf, `["`+tagError+`",[`...)
	e.buf = appendJSONString(e.buf, v.Str)
	e.buf = append(e.buf, ',')
	if err := e.cells(v.Cells, depth); err != nil {
		return err
	}
	e.buf = append(e.buf, ']', ']')
	return nil
}

// errorValue reads ["CONDITION",[DATA...]] after "~#error",[ and rebuilds
// the error, with no stack and no source location.
func (d *durableDecoder) errorValue(depth int) (*lisp.LVal, error) {
	s, err := d.rawString()
	if err != nil {
		return nil, err
	}
	switch cond := string(s); {
	case cond == lisp.CondInternalPanic:
		return nil, d.errorf("an error with condition %s", cond)
	case cond == "" || !utf8.ValidString(cond):
		return nil, d.errorf("an error with an empty or invalid condition")
	}
	v := &lisp.LVal{Type: lisp.LError, Str: string(s)}
	d.define(v)
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if err = d.expect('['); err != nil {
		return nil, err
	}
	cells, err := d.elements(depth)
	if err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	v.Cells = cells
	return v, nil
}
