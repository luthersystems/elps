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
  list      array slice     '(range 1 3), '(range 1)

Functions ending in "!" mutate in place; those without "!" return
a copy and leave the original unchanged. For ?set! and ?set, the
last argument is the new value; all preceding arguments are path
steps.

parse-path is the one function that takes a string: it converts a
jq-style path into a list of the steps above, for a path that
arrives as a string and is used more than once.

  (apply ? (cons obj (parse-path ".items[0].id")))`

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
// IMPORTANT: the formal lists below are read-only templates built once at
// package initialisation and handed to AddBuiltins by every environment that
// loads this package, exactly as every other lisplib package here does.
// Nothing may mutate them after construction.
//
// Two mechanisms hold that, and the annotation below rests on both.  libutil
// SEALS each formals list at construction, so the shared template is under
// the kernel's sealed-write guards; and AddBuiltins gives every environment
// a PRIVATE copy of it (formalsCopier, lisp/defformals.go, issue #513), so no
// two Runtimes hold one list in the first place.  See the long comment above
// AddMacros in lisp/env.go for why both exist.
//
//elpsvet:allow read-only formal templates; sealed by libutil at construction, and copied per environment by formalsCopier (lisp/env.go AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("?", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryGet,
		`Get value at a path specified by positional args.

		Each arg is a path step: string for map key, int for array index,
		'* to iterate all elements, '(range from to) for array slice.
		The slice end is optional: '(range from) runs to the end.

		(? obj "foo" 0 "bar")     => value at foo[0].bar
		(? users '* "name")       => list of all user names
		(? scores '(range 1 3))   => elements [1,3)
		(? scores '(range 1))     => elements [1,end)
		(? scores '(range -2))    => the last two elements
		(? obj)                   => obj itself (no path steps)`),
	libutil.FunctionDoc("parse-path", lisp.Formals("selector"), BuiltinParsePath,
		`Convert a jq-style path string into a list of positional path steps.

		The steps are what the ? family takes, so the result applies
		straight into any of them:

		(parse-path ".items[0].id")   => '("items" 0 "id")
		(parse-path ".items[].id")    => '("items" * "id")
		(parse-path ".items[1:3]")    => '("items" '(range 1 3))
		(parse-path ".items[1:]")     => '("items" '(range 1))
		(parse-path ".")              => '()

		(apply ? (cons obj (parse-path sel)))
		(apply ?set (concat 'list (list obj) (parse-path sel) (list v)))

		This is for a path that ARRIVES AS A STRING and is used more than
		once -- convert it once, keep the steps, and every later operation
		skips the parse.

		KEY SYNTAX. A bare .key is the classic identifier rule,
		[A-Za-z_][A-Za-z_0-9]*, which is jq's rule too. Underscores and
		digits are fine, so the snake_case keys these paths usually
		address need nothing special:

		(parse-path ".field_mask.paths")  => '("field_mask" "paths")

		Anything else -- a hyphen, a leading digit, "$", non-ASCII --
		MUST be bracketed and quoted, as it must in jq:

		(parse-path ".my-key")        => error: failed to parse: -key
		                                 (the error explains this rule)
		(parse-path ".[\"my-key\"]")   => '("my-key")
		(parse-path ".[\"\"]")         => '("")

		The jq optional-selector suffix "?" is accepted and DISCARDED --
		".a?" is exactly ".a" -- because nothing in the engine suppresses
		errors per step.

		A malformed selector RAISES. It does not return an empty list:
		no steps is the identity path, so a swallowed error would make a
		bad selector address the whole document.

		A selector may not SPAN LINES, and that raises for the same
		reason:

		(parse-path ".[0]
		.password")   => error: selector may not span lines

		The jq-string grammar this shares cuts a bracket-led selector at
		its first newline and DISCARDS the rest, which would convert the
		selector above to the single step 0 -- a live path to the whole
		element. (apply ?set ...) through it would overwrite the record
		instead of its "password" field, silently.

		Converting on each call is slower than one string-path operation,
		since it parses AND builds a list. How much the caching is worth
		depends on document size -- every operation first walks the whole
		document to validate it, so on a large document the parse is
		noise and on a small one it dominates.`),
	libutil.FunctionDoc("?set!", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySetMutate,
		`Set value at a path specified by positional args, mutating the original.

		The last argument is the new value; all preceding arguments are path
		steps. Returns the mutated original.

		The new value is stored BY REFERENCE, not copied: after the call it is
		reachable and mutable through the result, and a later write through
		either name is visible through the other.

		(?set! obj "foo" "bar" "new")   => obj with foo.bar="new" (mutated)
		(?set! obj "items" 0 "x")       => obj with items[0]="x" (mutated)
		(?set! data '* "active" true)   => set active=true on all elements`),
	libutil.FunctionDoc("?set", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySet,
		`Set value at a path specified by positional args, returning a copy.

		The last argument is the new value; all preceding arguments are path
		steps. The original is not modified.

		The copy is independent of the SOURCE document only. The new value is
		stored BY REFERENCE, not copied: the value you supply becomes
		reachable and mutable through the result, so a later in-place write
		through the result (?set!, ?del!, append!) reaches the caller's
		value, and vice versa. Pass a copy if the result must not alias it.

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

// errCyclicValue reports a value that contains itself.  append! and assoc!
// mutate a container in place, so a program can put a container inside
// itself, and elpspath has no answer for one: a path into a cyclic value has
// no finite result and a copy of one has no finite representation.  The
// walkers below refuse it with an ordinary error, which the builtins turn
// into a condition handler-bind can catch, rather than recursing until the
// goroutine stack overflows and the runtime kills the process -- a failure
// recover() cannot intercept.  See cycle.go and issue #393.
var errCyclicValue = errors.New("cannot operate on a value that contains itself")

// okSimpleContainerTypeGuarded ensures that lval is a valid container that
// only contains "simple" types compatible with `elpspath`.
// It sucks that we have to traverse the entire object checking the type,
// but better to be safe.
//
// It continues the walk g is already on rather than starting a fresh one.
// Every nested check must pass g down; starting a new walk per level resets
// the bound on every lap and it never fires.  okSimpleType is the entry
// point that begins a walk.
//
// With okSimpleType this is the gate every builtin runs before touching a
// value, and the rest of the package relies on what it rejects: copyLVal's
// multi-dimensional array branch cannot construct a copy and says so, and
// stays unreachable only because this refuses such an array first.  A cycle
// is refused here for the same reason, and the copy walk is guarded too
// because the exported Path interface lets a Go embedder reach a copy
// without coming through this gate.
func okSimpleContainerTypeGuarded(in *lisp.LVal, g cycleGuard) error {
	if in.IsNil() {
		return errors.New("nil container type invalid")
	}
	switch in.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr:
		// The three types that reach other values, and so the only ones
		// entered on the guard's path.  Handled below.
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
	g, cyclic := g.descend(in)
	if cyclic {
		return errCyclicValue
	}
	err := okSimpleContainerContents(in, g)
	if g.tracking() {
		g.ascend(in)
	}
	return err
}

// okSimpleContainerContents checks the values a container reaches.  It is
// only ever called through okSimpleContainerTypeGuarded, which has already
// established that in is a container and put it on g's path.
func okSimpleContainerContents(in *lisp.LVal, g cycleGuard) error {
	switch in.Type {
	case lisp.LSortMap:
		m0 := in.Map()
		entries := sortedMapEntries(m0)
		if lisp.GoError(entries) != nil {
			return lisp.GoError(entries)
		}
		for _, ent := range entries.Cells {
			v := ent.Cells[1]
			err := okSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LArray:
		// Exactly one dimension, matching toCells -- see the reason there.
		// The gate and the accessor have to agree about what "indexable"
		// means, or a shape this admits still fails downstream.
		if n := in.Cells[0].Len(); n != 1 {
			if n > 1 {
				return errors.New("cannot index multi-dimensional array")
			}
			return errors.New("cannot index zero-dimensional array")
		}
		cells := in.Cells[1].Cells
		for _, v := range cells {
			err := okSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LSExpr:
		cells := in.Cells
		for _, v := range cells {
			err := okSimpleTypeGuarded(v, g)
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
//
// Every builtin in this package runs it before touching a value. It is not a
// nicety: it refuses a value that CONTAINS ITSELF, and every unguarded
// recursive walk over such an LVal -- Get, a copy, a String() -- grows the
// goroutine stack until the Go runtime kills the process, an abort recover()
// cannot intercept and handler-bind never sees (issue #393). It also refuses
// the multi-dimensional array copyLVal has no answer for, which is what
// keeps that branch's "cannot construct a copy" unreachable.
func okSimpleType(in *lisp.LVal) error {
	var st cycleState
	return okSimpleTypeGuarded(in, newCycleGuard(&st))
}

// okSimpleTypeGuarded is okSimpleType continuing a walk already in progress.
// The guard is threaded through rather than re-created because the recursion
// that overflows the stack is okSimpleType <-> okSimpleContainerType, so a
// bound either survives the round trip or does nothing.
func okSimpleTypeGuarded(in *lisp.LVal, g cycleGuard) error {
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
		return okSimpleContainerTypeGuarded(in, g)
	default:
		return okSimpleContainerTypeGuarded(in, g)
	}
}
