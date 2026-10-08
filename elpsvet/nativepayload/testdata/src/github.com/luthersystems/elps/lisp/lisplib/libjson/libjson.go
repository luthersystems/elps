// Package libjson is a minimal stub of elps's libjson for analysistest.
// Only the names elpsdurablenative reads matter: DurableCodec,
// ForeignCodec, NewFrozenDurableRegistry and TransientNative.
package libjson

import (
	"reflect"

	"github.com/luthersystems/elps/lisp"
)

type DurableCodec[T any] struct {
	Save    func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error)
	Load    func(env *lisp.LEnv, version int, payload *lisp.LVal) (*lisp.LVal, error)
	Name    string
	Version int
}

func (c DurableCodec[T]) registerDurable() error { return nil }

func (c DurableCodec[T]) WithName(name string) DurableCodec[T] {
	c.Name = name
	return c
}

type ForeignCodec struct {
	Type    reflect.Type
	Name    string
	Version int
}

func (c ForeignCodec) registerDurable() error { return nil }

type DurableEntry interface{ registerDurable() error }

type DurableRegistry struct{}

func NewFrozenDurableRegistry(codecs ...DurableEntry) (*DurableRegistry, error) {
	return &DurableRegistry{}, nil
}

type TransientNative interface{ TransientNative() }
