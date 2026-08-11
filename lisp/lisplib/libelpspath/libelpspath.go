// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "elpspath"

const packageDoc = `Path-based access and mutation for nested data structures.

Paths are given as positional ELPS values — one argument per path
step, no string DSL. Path step types:

  string    map key         "foo"
  int       array index     0, -1
  symbol    iterate all     '*
  list      array slice     '(range 1 3)

Functions ending in "!" mutate in place; those without "!" return
a copy and leave the original unchanged. For ?set! and ?set, the
last argument is the new value; all preceding arguments are path
steps.`

// LoadPackage adds the elpspath package to env.
//
// DefinePackage is get-or-create, and AddBuiltins refuses only exact
// symbol-name collisions, so an embedder may compose additional builtins
// into the same lisp-visible "elpspath" package with its own loader (as
// substrate does with its legacy jq-string operations).
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(packageDoc)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// builtins are functions exported to elps.
//
// The formal lists below are read-only templates: libutil seals them at
// construction, and AddBuiltins (registrationFormals) aliases the sealed
// lists into each environment under copy-on-write protection, so no Runtime
// can mutate these package-level LVals.
//
//elpsvet:allow read-only formal templates; sealed by libutil at construction and shared via registrationFormals
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("?", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryGet,
		`Get value at a path specified by positional args.

		Each arg is a path step: string for map key, int for array index,
		'* to iterate all elements, '(range from to) for array slice.

		(? obj "foo" 0 "bar")     => value at foo[0].bar
		(? users '* "name")       => list of all user names
		(? scores '(range 1 3))   => elements [1,3)
		(? obj)                   => obj itself (no path steps)`),
	libutil.FunctionDoc("?set!", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySetMutate,
		`Set value at a path specified by positional args, mutating the original.

		The last argument is the new value; all preceding arguments are path
		steps. Returns the mutated original.

		(?set! obj "foo" "bar" "new")   => obj with foo.bar="new" (mutated)
		(?set! obj "items" 0 "x")       => obj with items[0]="x" (mutated)
		(?set! data '* "active" true)   => set active=true on all elements`),
	libutil.FunctionDoc("?set", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySet,
		`Set value at a path specified by positional args, returning a copy.

		The last argument is the new value; all preceding arguments are path
		steps. The original is not modified.

		(?set obj "foo" "bar" "new")   => new obj with foo.bar="new"
		(?set obj "items" 0 "x")       => new obj with items[0]="x"`),
	libutil.FunctionDoc("?del!", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryDeleteMutate,
		`Delete value at a path specified by positional args, mutating the original.

		(?del! obj "foo")              => obj with foo removed (mutated)
		(?del! obj "items" 1)          => obj with items[1] removed (mutated)
		(?del! records '* "cache")     => remove cache key from all elements`),
	libutil.FunctionDoc("?del", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryDelete,
		`Delete value at a path specified by positional args, returning a copy.

		(?del obj "foo")               => new obj with foo removed
		(?del obj "items" 1)           => new obj with items[1] removed`),
	libutil.FunctionDoc("?nil!", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryNilMutate,
		`Set value at a path to nil, mutating the original. The key is kept.

		(?nil! obj "foo")              => obj with foo=nil (mutated)
		(?nil! rows '* "cached")       => nil out cached on all elements`),
	libutil.FunctionDoc("?nil", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryNil,
		`Set value at a path to nil, returning a copy. The key is kept.

		(?nil obj "foo")               => new obj with foo=nil
		(?nil patient "ssn")           => new obj with ssn=nil`),
}

// okSimpleContainerType ensures that lval is a valid container that only
// contains "simple" types compatible with `elpspath`.
// It sucks that we have to traverse the entire object checking the type,
// but better to be safe.
func okSimpleContainerType(in *lisp.LVal) error {
	if in.IsNil() {
		return errors.New("nil container type invalid")
	}
	switch in.Type {
	case lisp.LSortMap:
		m0 := in.Map()
		entries := sortedMapEntries(m0)
		if lisp.GoError(entries) != nil {
			return lisp.GoError(entries)
		}
		for _, ent := range entries.Cells {
			v := ent.Cells[1]
			err := okSimpleType(v)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LArray:
		if in.Cells[0].Len() > 1 {
			return errors.New("cannot index multi-dimensional array")
		}
		cells := in.Cells[1].Cells
		for _, v := range cells {
			err := okSimpleType(v)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LSExpr:
		cells := in.Cells
		for _, v := range cells {
			err := okSimpleType(v)
			if err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
}

// okSimpleType ensures that the lval is a valid simple type compatible with
// elpspath.
// It sucks that we have to traverse the entire object checking the type,
// but better to be safe.
func okSimpleType(in *lisp.LVal) error {
	if in.IsNil() {
		// allow nil as a placeholder for removed elements
		return nil
	}
	switch in.Type {
	case lisp.LString:
		return nil
	case lisp.LInt:
		return nil
	case lisp.LFloat:
		return nil
	case lisp.LSymbol:
		if in.Str == lisp.TrueSymbol || in.Str == lisp.FalseSymbol {
			return nil
		}
		return okSimpleContainerType(in)
	default:
		return okSimpleContainerType(in)
	}
}
