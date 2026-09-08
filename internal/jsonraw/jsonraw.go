// Copyright © 2026 The ELPS authors

// Package jsonraw lets the JSON decoder wrap its map storage in an
// interpreter-owned map without exporting the backing implementation.
package jsonraw

import (
	"github.com/luthersystems/elps/internal/jsonraw/hook"
	"github.com/luthersystems/elps/lisp"
)

// Wrap makes a distinct Lisp map wrapper around data without copying it.
// The decoder must replace every value with an *lisp.LVal before calling Wrap.
// The backing is writable, accepts only string keys, and retains data's
// identity. Callers must not share it between independent runtimes.
var Wrap func(data map[string]any) *lisp.LVal

func init() {
	fn, ok := hook.Wrap.(func(map[string]any) *lisp.LVal)
	if !ok {
		// Importing this package initializes lisp and its hook first.
		panic("jsonraw: package lisp did not inject the Wrap constructor")
	}
	Wrap = fn
}
