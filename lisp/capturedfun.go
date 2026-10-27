// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// CapturedFun describes a Go function value whose VM state is explicit.  An
// embedder uses it to give Lisp code an object that dispatches in Go, where
// it would otherwise return a Lisp closure over the object's state.
//
// Eval receives the call's arguments and the capture graph of the VM that
// calls it.  Template.NewVM remaps the graph with the rest of the VM, so a
// value that a template holds gets a private copy of its state in each
// fork, as a Lisp closure does.  Eval must keep every mutable value it uses
// in Captures; it may close over immutable, concurrency-safe Go data only.
// Captures is held by reference, so aliases to other values in the same VM
// stay intact.  A nil Captures declares no VM state.
//
// Lisp code cannot read Captures: the state is as private as a closure's
// environment.  A durable dump saves the value only through a codec
// registered for its Package and FID (libjson.DurableFunCodec).
type CapturedFun struct {
	// Formals is the formal argument list, as for FunInPackage.
	Formals *LVal
	// Captures is the value's VM state.
	Captures *LVal
	// Eval runs a call.  It must not be nil.
	Eval func(env *LEnv, args, captures *LVal) *LVal
	// Package and FID identify the function, as for FunInPackage.  All the
	// values that one constructor makes share them.
	Package string
	FID     string
	// Name is the display name: the value prints as #<Name>.  An empty
	// Name prints #<builtin>, as any Go function does.  A Name must be
	// valid UTF-8 with no space, control character or '>'.
	Name string
}

// NewCapturedFun returns the function value that spec describes.  It
// returns an error value when spec.Eval is nil.
func NewCapturedFun(spec CapturedFun) *LVal {
	if i := strings.IndexFunc(spec.Name, badNameRune); i >= 0 || !utf8.ValidString(spec.Name) {
		// A name prints inside #<...>: one with a space or a '>' could pass
		// for other printed values.
		return Errorf("captured function %s:%s: display name %q is not printable as one word without '>'", spec.Package, spec.FID, spec.Name)
	}
	return newCapturedBuiltin(spec)
}

func badNameRune(r rune) bool {
	return r == '>' || unicode.IsSpace(r) || !unicode.IsGraphic(r)
}

// capturedName returns the display name of a CapturedFun value, or "".
func (v *LVal) capturedName() string {
	if fd := v.funData(); fd != nil && fd.captures != nil {
		return fd.captures.name
	}
	return ""
}
