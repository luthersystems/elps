// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// DurableCodec is the durable codec of the native payloads of Go type T.
// Declare it as a package-level value (not a pointer) in the package that
// builds the natives, or export it from a package that package imports.
// The elpsdurablenative analyzer (elpsvet/nativepayload) reads these values
// to decide that a native payload type is durable.  Pass the value to
// NewFrozenDurableRegistry to register it.
//
// Name is written into every saved document and must never change for T.
// Version is the payload version Save writes, starting at 1.  Load reads
// every version from 1 to Version.  Save and Load follow the NativeCodec
// contract.  Charge and SharedPayload are the WithNativeCharge and
// WithSharedPayload options.
type DurableCodec[T any] struct {
	Save          func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	Load          func(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
	Name          string
	Version       int
	Charge        int
	SharedPayload bool
}

// WithName returns a copy of c registered under name.  Use it to keep the
// name an embedder already writes into its documents for a codec that elps
// declares.
func (c DurableCodec[T]) WithName(name string) DurableCodec[T] {
	c.Name = name
	return c
}

func (c DurableCodec[T]) registerDurable(r *DurableRegistry) error {
	return ForeignCodec{
		Type: reflect.TypeFor[T](), Save: c.Save, Load: c.Load,
		Name: c.Name, Version: c.Version, Charge: c.Charge, SharedPayload: c.SharedPayload,
	}.registerDurable(r)
}

// ForeignCodec is the durable codec of a native payload type that the
// declaring package cannot name, such as an unexported type of another
// module.  Type is the payload's reflect.Type.  The other fields are those
// of DurableCodec.  The elpsdurablenative analyzer cannot read a
// reflect.Type, so a ForeignCodec makes no payload type durable to it.
type ForeignCodec struct {
	Type          reflect.Type
	Save          func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	Load          func(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
	Name          string
	Version       int
	Charge        int
	SharedPayload bool
}

func (c ForeignCodec) registerDurable(r *DurableRegistry) error {
	if c.Save == nil || c.Load == nil {
		return fmt.Errorf("durable json: codec %q needs both a Save and a Load function", c.Name)
	}
	opts := []NativeOption{WithNativeCharge(c.Charge)}
	if c.SharedPayload {
		opts = append(opts, WithSharedPayload())
	}
	return r.Register(c.Type, c.Name, c.Version, NativeFuncs{Save: c.Save, Load: c.Load}, opts...)
}

// DurableFunCodec is the durable codec of the Go function values that one
// constructor makes with lisp.NewCapturedFun: the values whose Package and
// FID are the codec's.  A dump writes such a value as a native,
// ["~#native",[NAME,VERSION,CAPTURES]], where CAPTURES is the value's
// capture graph, saved with sharing like any other value.  A load decodes
// CAPTURES and calls Load, which must return a function value of the same
// Package and FID over exactly the captures it was given, so that sharing is
// kept.  Load must only assemble the value, as a native codec's LoadNative
// does: it must run no Lisp code and read no state outside its arguments.
//
// A function value with no codec is refused, as before: a durable dump
// cannot save a Go function's code.  Name and Version follow the rules of
// DurableRegistry.Register; the Fingerprint lists the codec as the function
// it rebuilds.
type DurableFunCodec struct {
	// Load rebuilds the value over captures, saved at version.
	Load func(env *lisp.LEnv, version int, captures *lisp.LVal) (*lisp.LVal, error)
	// Package and FID name the function values the codec saves.
	Package string
	FID     string
	// Name is written into every document; it must never change.
	Name string
	// Version is the captures layout DumpDurable writes, starting at 1.
	Version int
	// Charge is the WithNativeCharge contract.
	Charge int
}

func (c DurableFunCodec) registerDurable(r *DurableRegistry) error {
	if err := r.checkNew(c.Name, c.Version); err != nil {
		return err
	}
	fun := funKey{c.Package, c.FID}
	switch {
	case c.Load == nil:
		return fmt.Errorf("durable json: function codec %q needs a Load function", c.Name)
	case c.Package == "" || c.FID == "":
		return fmt.Errorf("durable json: function codec %q needs a package and an FID", c.Name)
	case c.Charge < 0:
		return fmt.Errorf("durable json: native %q: negative charge %d", c.Name, c.Charge)
	}
	if prev, ok := r.byFun[fun]; ok {
		return fmt.Errorf("durable json: function %s is already registered as %q", fun, prev.name)
	}
	load := c.Load
	e := &nativeEntry{
		fun: fun, name: c.Name, version: c.Version, charge: c.Charge,
		// Save returns the captures Load was given, so sharing is kept.
		shared: true,
		codec: NativeFuncs{
			Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return funraw.Captures(v), nil },
			Load: load,
		},
	}
	r.byFun[fun] = e
	r.byName[c.Name] = e
	return nil
}

// DurableEntry is one codec of NewFrozenDurableRegistry: a DurableCodec of
// any type, a ForeignCodec or a DurableFunCodec.  Only this package implements it.
type DurableEntry interface {
	registerDurable(r *DurableRegistry) error
}

// NewFrozenDurableRegistry registers every codec in a new registry and
// freezes it.  The Fingerprint does not depend on the order of the codecs,
// and equals the Fingerprint of a registry built with RegisterNative or
// Register and the same names, versions and options.  It returns an error
// for a codec with no Save or no Load function, and for a codec that
// Register refuses.
func NewFrozenDurableRegistry(codecs ...DurableEntry) (*DurableRegistry, error) {
	r := NewDurableRegistry()
	for _, c := range codecs {
		if c == nil {
			return nil, errors.New("durable json: cannot register a nil codec entry")
		}
		if err := c.registerDurable(r); err != nil {
			return nil, err
		}
	}
	r.Freeze()
	return r, nil
}

// TransientNative is a native payload type that is never saved by a
// durable dump.  Declare the method on the payload's named type, with a doc
// comment that says why the type is never saved.  The method does nothing;
// the elpsdurablenative analyzer reads it.  A value receiver marks T and
// *T; a pointer receiver marks only *T.  A method promoted from an embedded
// field does not mark the type.  DumpDurable refuses a transient native
// like any native with no codec.
type TransientNative interface {
	TransientNative()
}
