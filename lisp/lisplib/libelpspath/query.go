// elpspath — positional-arg path operations
//
// The elpspath API addresses locations inside nested data structures with
// positional path steps: each step is an ordinary ELPS value — no
// mini-language to learn and no runtime string parsing. (A legacy jq-string
// path DSL is still spoken by builtins downstream in
// luthersystems/substrate, over the parser in selector.go; see below.)
//
// # Builtins
//
// All functions take a data structure as the first argument, followed by
// zero or more path steps. Functions ending in "!" mutate in place;
// those without "!" return a copy and leave the original unchanged.
//
//	(elpspath:?      val &rest steps)             ; get
//	(elpspath:?set!  val &rest steps-and-value)   ; set (mutating)
//	(elpspath:?set   val &rest steps-and-value)   ; set (copy)
//	(elpspath:?del!  val &rest steps)             ; delete (mutating)
//	(elpspath:?del   val &rest steps)             ; delete (copy)
//	(elpspath:?nil!  val &rest steps)             ; nil (mutating)
//	(elpspath:?nil   val &rest steps)             ; nil (copy)
//
// For ?set! and ?set the last variadic argument is always the new value;
// everything before it is treated as path steps.
//
// # Path step types
//
//	Type             Meaning        Example        jq analogue
//	─────────────    ───────        ───────        ───────────
//	string           map key        "foo"          .foo
//	int              array index    0, -1          [0], [-1]
//	symbol '*        iterate all    '*             []
//	list (range a b) array slice    '(range 1 3)   [1:3]
//
// # Examples
//
// Basic access:
//
//	(elpspath:? obj "name")                   ; get a key
//	(elpspath:? obj "address" "city")         ; nested keys
//	(elpspath:? obj "items" 0)                ; array index
//	(elpspath:? obj "items" -1)               ; last element
//
// Iterators:
//
//	(elpspath:? users '* "name")              ; all user names
//	(elpspath:? obj '* "tags" 0)              ; first tag from each item
//	(elpspath:? org "teams" '* "members" '* "name")  ; double iterate (flattens)
//
// Ranges:
//
//	(elpspath:? scores '(range 1 3))          ; elements [1,3)
//
// Mutations:
//
//	(elpspath:?set! user "profile" "bio" "hello")    ; mutating set
//	(set new (elpspath:?set user "profile" "bio" "hello"))  ; copy set
//	(elpspath:?set! data "users" '* "active" true)   ; set via iterator
//
// Deletes:
//
//	(elpspath:?del! user "tmp-field")                 ; mutating delete
//	(set clean (elpspath:?del obj "metadata" "internal-id"))  ; copy delete
//	(elpspath:?del! records '* "cache")               ; delete via iterator
//
// Nils:
//
//	(elpspath:?nil! record "deprecated")              ; null a field in place
//	(set redacted (elpspath:?nil patient "ssn"))       ; null with copy
//
// Dynamic paths (steps are plain values, so no string splicing is needed):
//
//	(defun get-field (obj field)
//	  (elpspath:? obj field))
//
//	(defun get-nth-result (resp n)
//	  (elpspath:? resp "results" n "value"))
//
// # Legacy jq-string DSL
//
// The deprecated legacy BUILTINS (get-path, set-path!, etc.), which encode
// paths as jq-style strings, did not move here: they remain downstream in
// luthersystems/substrate, whose loader composes them into this same
// lisp-visible elpspath package. The positional-arg API is ~3-4x faster
// because it skips regex-based string parsing — path steps are dispatched
// by type switch.
//
// The PARSER those builtins are built on does live here, as the Go-level
// ParseSelector in selector.go (issue #564): it is pure translation of a
// selector string into the exported Path constructors, so leaving it
// downstream meant one repository owning the syntax of a path language whose
// semantics live in another. Nothing lisp-visible reaches it.
package libelpspath

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

// ArgsToPath converts positional ELPS args into a Path.
// Each arg is dispatched by type:
//   - LString → Dot(key)
//   - LInt → Index(i)
//   - LSymbol "*" → Iter()
//   - LSExpr (range from to) → Range(from, to, false)
func ArgsToPath(args []*lisp.LVal) (Path, error) {
	if len(args) == 0 {
		return Root(Chain()), nil
	}
	steps := make([]Path, 0, len(args))
	for i, arg := range args {
		step, err := argToStep(arg)
		if err != nil {
			return nil, fmt.Errorf("step %d: %w", i, err)
		}
		steps = append(steps, step)
	}
	return Root(Chain(steps...)), nil
}

func argToStep(arg *lisp.LVal) (Path, error) {
	switch arg.Type {
	case lisp.LString:
		return Dot(arg.Str), nil
	case lisp.LInt:
		return Index(arg.Int), nil
	case lisp.LSymbol:
		if arg.Str == "*" {
			return Iter(), nil
		}
		return nil, fmt.Errorf("unsupported symbol: %s (only '* is supported)", arg.Str)
	case lisp.LSExpr:
		return parseSExprStep(arg)
	default:
		return nil, fmt.Errorf("unsupported path step type: %v", arg.Type)
	}
}

// parseSExprStep parses an s-expression path step like (range 1 3).
func parseSExprStep(expr *lisp.LVal) (Path, error) {
	cells := expr.Cells
	if len(cells) == 0 {
		return nil, errors.New("empty path expression")
	}
	head := cells[0]
	if head.Type != lisp.LSymbol {
		return nil, fmt.Errorf("path expression head must be a symbol, got %v", head.Type)
	}
	switch head.Str {
	case "range":
		// One argument is the open-ended slice: (range from) means
		// [from, len). Two is the explicit half-open [from, to).
		//
		// The engine has always been able to express the open end --
		// rangePath carries an implicitTo flag and validateRange resolves
		// it against the input length -- but until issue #563 no elps
		// surface could construct one, so the capability was reachable
		// only by an embedder building Path values directly.
		if len(cells) != 2 && len(cells) != 3 {
			return nil, fmt.Errorf("range requires 1 or 2 arguments (from [to]), got %d", len(cells)-1)
		}
		from := cells[1]
		if from.Type != lisp.LInt {
			return nil, fmt.Errorf("range 'from' must be an integer, got %v", from.Type)
		}
		if len(cells) == 2 {
			// to is ignored when implicitTo is set; validateRange
			// overwrites it with the input length.
			return Range(from.Int, 0, true), nil
		}
		to := cells[2]
		if to.Type != lisp.LInt {
			return nil, fmt.Errorf("range 'to' must be an integer, got %v", to.Type)
		}
		return Range(from.Int, to.Int, false), nil
	default:
		return nil, fmt.Errorf("unsupported path expression: %s", head.Str)
	}
}

// BuiltinQueryGet implements (elpspath:? val &rest steps).
func BuiltinQueryGet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	steps := args.Cells[1:]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.Get(val)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQuerySetMutate implements (elpspath:?set! val &rest steps-and-value).
// The last vararg is the new value; all preceding varargs are path steps.
func BuiltinQuerySetMutate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	rest := args.Cells[1:]
	if len(rest) < 1 {
		return env.Errorf("?set! requires at least a value argument")
	}
	steps, newVal := rest[:len(rest)-1], rest[len(rest)-1]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	if err := OKSimpleType(newVal); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.SetMutate(val, newVal)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQuerySet implements (elpspath:?set val &rest steps-and-value).
// The last vararg is the new value; all preceding varargs are path steps.
func BuiltinQuerySet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	rest := args.Cells[1:]
	if len(rest) < 1 {
		return env.Errorf("?set requires at least a value argument")
	}
	steps, newVal := rest[:len(rest)-1], rest[len(rest)-1]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	if err := OKSimpleType(newVal); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.Set(val, newVal)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQueryDeleteMutate implements (elpspath:?del! val &rest steps).
func BuiltinQueryDeleteMutate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	steps := args.Cells[1:]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.DeleteMutate(val)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQueryDelete implements (elpspath:?del val &rest steps).
func BuiltinQueryDelete(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	steps := args.Cells[1:]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.Delete(val)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQueryNilMutate implements (elpspath:?nil! val &rest steps).
func BuiltinQueryNilMutate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	steps := args.Cells[1:]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.NilMutate(val)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}

// BuiltinQueryNil implements (elpspath:?nil val &rest steps).
func BuiltinQueryNil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := args.Cells[0]
	steps := args.Cells[1:]
	if err := OKSimpleType(val); err != nil {
		return env.Errorf("%s", err)
	}
	path, err := ArgsToPath(steps)
	if err != nil {
		return env.Errorf("%s", err)
	}
	data, err := path.Nil(val)
	if err != nil {
		return env.Errorf("%s", err)
	}
	return data
}
