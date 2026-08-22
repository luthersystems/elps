// Copyright © 2021 The ELPS authors
// This package provides schema validation for ELPS types
// Author: Reuben Thompson
package libschema

import (
	"fmt"
	"regexp"
	"sync/atomic"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "s"

// These are our types. We don't use the `LType`s in the lisp package as we have
// some extras and we don't want some of those
const (
	String    = "string"
	Number    = "number"
	Int       = "int"
	Float     = "float"
	Fun       = "fun"
	Bytes     = "bytes"
	Error     = "error"
	SortedMap = "sorted-map"
	Array     = "array"
	Bool      = "bool"
	TaggedVal = "tagged-value"
	Any       = "any"
)

var symbols = []string{
	String,
	Number,
	Int,
	Float,
	Fun,
	Bytes,
	Error,
	SortedMap,
	Array,
	Bool,
	TaggedVal,
	Any,
}

// These are the errors we may produce
const (
	BadArgs          = "bad-arguments"
	FailedConstraint = "failed-constraint"
	WrongType        = "wrong-type"
)

// LoadPackage adds the schema package to env.
//
// The package name is hardcoded to DefaultPackageName. If a future
// caller wants to load libschema under a different name, also update
// newValidator/newNamedValidator (below) to thread the chosen name
// through instead of using DefaultPackageName directly — otherwise
// validator LFuns will carry stale "s" labels in stack frames and
// profiler attributes.
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
	env.SetPackageDoc(`Schema validation: define typed validators with constraints and
		check values against them at runtime. Validators compose to
		describe complex data structures.`)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	// We export the type symbols to the package to make writing code using it less messy
	for _, symbol := range symbols {
		env.PutGlobal(lisp.Symbol(symbol), lisp.String(symbol))
	}
	env.Runtime.Package.Exports(symbols...)
	env.SetSymbolDoc("string", "Type name for string values.")
	env.SetSymbolDoc("number", "Type name for numeric values (int or float).")
	env.SetSymbolDoc("int", "Type name for integer values.")
	env.SetSymbolDoc("float", "Type name for floating-point values.")
	env.SetSymbolDoc("fun", "Type name for function values.")
	env.SetSymbolDoc("bytes", "Type name for byte-sequence values.")
	env.SetSymbolDoc("error", "Type name for error values.")
	env.SetSymbolDoc("sorted-map", "Type name for sorted-map (associative array) values.")
	env.SetSymbolDoc("array", "Type name for array values.")
	env.SetSymbolDoc("bool", "Type name for boolean values (true or false).")
	env.SetSymbolDoc("tagged-value", "Type name for user-defined tagged values (created with deftype/new).")
	env.SetSymbolDoc("any", "Type name matching any value type (no type constraint).")
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("deftype", lisp.Formals("name", "type", lisp.VarArgSymbol, "constraints"), builtinDefType,
		`Defines a named schema type and binds it as a global symbol.
		name is a string used as both the type name and symbol binding.
		type is a type string ("string", "int", "float", "number",
		"bool", "array", "sorted-map", "fun", "tagged-value", "any").
		Additional constraint functions may be passed to further
		restrict valid values. Use with s:validate to check values.`),
	libutil.FunctionDoc("make-validator", lisp.Formals("name", "type", lisp.VarArgSymbol, "constraints"), builtinMakeValidator,
		`Creates and returns a validator function without binding it.
		Like deftype but returns the validator instead of creating a
		global binding. name may be a string or a typedef (tagged
		value). When name is a typedef, the type argument is treated
		as a constraint on the user-data and "tagged-value" is implied.`),
	libutil.FunctionDoc("in", lisp.Formals("&rest", "allowed-values"), builtinAllowedValues,
		`Returns a constraint that checks if the input is equal to one
		of the allowed values. Useful for creating enum-like types.
		Example: (s:in "red" "green" "blue").`),
	libutil.FunctionDoc("gt", lisp.Formals("allowed-value"), builtinGreaterThan,
		`Returns a constraint that checks if the input is strictly
		greater than allowed-value. Works with numeric types.`),
	libutil.FunctionDoc("gte", lisp.Formals("allowed-value"), builtinGreaterThanOrEqual,
		`Returns a constraint that checks if the input is greater than
		or equal to allowed-value. Works with numeric types.`),
	libutil.FunctionDoc("lt", lisp.Formals("allowed-value"), builtinLessThan,
		`Returns a constraint that checks if the input is strictly less
		than allowed-value. Works with numeric types.`),
	libutil.FunctionDoc("lte", lisp.Formals("allowed-value"), builtinLessThanOrEqual,
		`Returns a constraint that checks if the input is less than or
		equal to allowed-value. Works with numeric types.`),
	libutil.FunctionDoc("positive", lisp.Formals(), builtinPositive,
		`Returns a constraint that checks if the input is strictly
		greater than zero.`),
	libutil.FunctionDoc("negative", lisp.Formals(), builtinNegative,
		`Returns a constraint that checks if the input is strictly
		less than zero.`),
	libutil.FunctionDoc("validate", lisp.Formals("type", "input"), builtinValidate,
		`Validates input against a type validator function (created by
		deftype or make-validator). Returns nil on success or an error
		with a condition string describing the validation failure.`),
	libutil.FunctionDoc("len", lisp.Formals("allowed-value"), builtinLen,
		`Returns a constraint that checks if the length of the input
		equals allowed-value. Works with strings, bytes, and arrays.`),
	libutil.FunctionDoc("lengt", lisp.Formals("allowed-value"), builtinLenGreaterThan,
		`Returns a constraint that checks if the length of the input is
		strictly greater than allowed-value. Works with strings, bytes,
		and arrays.`),
	libutil.FunctionDoc("lengte", lisp.Formals("allowed-value"), builtinLenGreaterThanOrEqual,
		`Returns a constraint that checks if the length of the input is
		greater than or equal to allowed-value. Works with strings,
		bytes, and arrays.`),
	libutil.FunctionDoc("lenlt", lisp.Formals("allowed-value"), builtinLenLessThan,
		`Returns a constraint that checks if the length of the input is
		strictly less than allowed-value. Works with strings, bytes,
		and arrays.`),
	libutil.FunctionDoc("lenlte", lisp.Formals("allowed-value"), builtinLenLessThanOrEqual,
		`Returns a constraint that checks if the length of the input is
		less than or equal to allowed-value. Works with strings, bytes,
		and arrays.`),
	libutil.FunctionDoc("of", lisp.Formals("&rest", "allowed-types"), builtinArrayOf,
		`Returns a constraint for arrays that checks each element matches
		one of the allowed types. Each allowed-type is a type string
		passed to the type handler. Example: (s:of "string") checks all
		elements are strings.`),
	libutil.FunctionDoc("has-key", lisp.Formals("key", "&rest", "allowed-types"), builtinHasKey,
		`Returns a constraint for sorted-maps that checks the map has
		the specified key and its value matches one of the allowed
		types. key is a string. Returns the key name on success (used
		by no-other-keys).`),
	libutil.FunctionDoc("may-have-key", lisp.Formals("key", "&rest", "allowed-types"), builtinMayHaveKey,
		`Returns a constraint for sorted-maps that checks if the key
		exists, and if so, validates its value matches one of the
		allowed types. If the key is absent, validation passes.
		key is a string, matching has-key. Returns the key name on
		success (used by no-other-keys).`),
	libutil.FunctionDoc("no-other-keys", lisp.Formals("&rest", "constraints"), builtinNoOtherKeys,
		`Returns a constraint for sorted-maps that rejects maps with
		keys not declared by the given has-key or may-have-key
		constraints. Pass all key constraints as arguments.`),
	libutil.FunctionDoc("when", lisp.Formals("key", "constraint", "matchKey", "&rest", "constraints"), builtinWhen,
		`Returns a conditional constraint for sorted-maps. When the value
		at key satisfies constraint, the value at matchKey must satisfy
		all additional constraints. If the condition is not met, the
		constraint passes.`),
	libutil.FunctionDoc("is-true", lisp.Formals(), builtinIsTrue,
		`Returns a constraint that checks if the input is the boolean
		true symbol.`),
	libutil.FunctionDoc("is-false", lisp.Formals(), builtinIsFalse,
		`Returns a constraint that checks if the input is the boolean
		false symbol.`),
	libutil.FunctionDoc("is-truthy", lisp.Formals(), builtinIsTruthy,
		`Returns a constraint that checks if the input is truthy.
		Truthy values include: true, non-empty strings (not "false"),
		non-empty arrays/maps/bytes, and positive numbers.`),
	libutil.FunctionDoc("is-falsy", lisp.Formals(), builtinIsFalsy,
		`Returns a constraint that checks if the input is falsy (the
		logical negation of is-truthy).`),
	libutil.FunctionDoc("not", lisp.Formals("constraint"), builtinIsNot,
		`Returns a constraint that negates another constraint. The
		input passes if the inner constraint fails, and fails if the
		inner constraint passes.`),
	libutil.FunctionDoc("regexp", lisp.Formals("pattern"), builtinRegexp,
		`Returns a constraint that checks if a string input matches
		the given regular expression pattern. Uses Go RE2 syntax.
		Returns an error if the pattern is invalid.`),
}

// This is the `s:validate` keyword. It checks its input matches the type
// validator specified.
func builtinValidate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	val := args.Cells[0]
	input := args.Cells[1]
	if val.Type != lisp.LFun {
		return lisp.ErrorConditionf(BadArgs, "First argument is not a type")
	}
	return applyConstraint(env, val, input)
}

// This is the `s:deftype` keyword. It defines a type and associated
// constraints.  s:deftype creates a symbol binding for the type validator
// which can be used with s:validate.
//
// s:deftype cannot be used with the core language deftype macro because they
// would bind different values to the same symbol.  s:make-validator should be
// used with tagged-values instead.
func builtinDefType(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lname := args.Cells[0]
	typeValidator := args.Cells[1]
	constraints := args.Cells[2:]
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	var name string
	switch lname.Type {
	case lisp.LString:
		name = lname.Str
	default:
		return lisp.ErrorConditionf(BadArgs, "First argument must resolve to a string")
	}
	exists := env.Get(lname)
	if !exists.IsNil() {
		return lisp.ErrorConditionf(BadArgs, "Symbol %s is already defined", lname)
	}
	res := getHandler(env, typeValidator, name, constraints)
	if res != nil && res.Type == lisp.LError {
		return res
	}
	if res != nil {
		// BUG:  A regular function should not call PutGlobal in this way
		// because functions aren't supposed to operate in the caller's lexical
		// environment, but builtins don't get a lexical environment currently.
		res = env.PutGlobal(lisp.Symbol(lname.Str), res)
		if res != nil && res.Type == lisp.LError {
			return res
		}
	}
	return lisp.Nil()
}

// This is the `s:make-validator` keyword.  It returns a reference to a
// validation handler constructed from the given name, type, and constraints.
// The type name may be a string or a typedef.  Passing a typedef implies the
// "tagged-value" typename and in that case the typename argument is used to
// constrain the user-data of validated objects.
func builtinMakeValidator(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lname := args.Cells[0]
	typeValidator := args.Cells[1]
	constraints := args.Cells[2:]
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	var name string
	switch lname.Type {
	case lisp.LString:
		name = lname.Str
	case lisp.LTaggedVal:
		if lname.Str != "lisp:typedef" {
			return lisp.ErrorConditionf(BadArgs, "First argument must resolve to a string or typedef")
		}
		name = lname.UserData().Cells[0].Str
		taggedConstraints := []*lisp.LVal{typeValidator}
		taggedConstraints = append(taggedConstraints, constraints...)
		constraints = taggedConstraints
		typeValidator = lisp.String(TaggedVal)
	default:
		return lisp.ErrorConditionf(BadArgs, "First argument must resolve to a string or typedef")
	}
	res := getHandler(env, typeValidator, name, constraints)
	if res != nil {
		return res
	}
	return lisp.Nil()
}

// finds the correct validation handler for the type
func getHandler(env *lisp.LEnv, in *lisp.LVal, name string, constraints []*lisp.LVal) *lisp.LVal {
	lType, _ := lisp.GoString(in)
	var res *lisp.LVal
	switch lType {
	case String:
		res = builtinCheckString(env, name, constraints)
	case Int:
		res = builtinCheckInt(env, name, constraints)
	case Float:
		res = builtinCheckFloat(env, name, constraints)
	case Number:
		res = builtinCheckNumber(env, name, constraints)
	case Array:
		res = builtinCheckArray(env, name, constraints)
	case SortedMap:
		res = builtinCheckMap(env, name, constraints)
	case Fun:
		res = builtinCheckFun(env, name, constraints)
	case Bool:
		res = builtinCheckBool(env, name, constraints)
	case TaggedVal:
		res = builtinCheckTaggedVal(env, name, constraints)
	case Any:
		res = builtinCheckAny(env, constraints)
	default:
		// BUG:  It is not correct to evaluate `in` here, it has already been
		// evaluated as part of the function application process.
		if in.Type == lisp.LSExpr {
			in = env.Eval(in)
		}
		if in.Type == lisp.LSymbol {
			in = env.Get(in)
		}
		if in.Type == lisp.LFun {
			// The choke point. Every composite constraint (s:has-key,
			// s:may-have-key, s:of, s:no-other-keys, s:when) and both
			// entry points (s:deftype, s:make-validator) reach an
			// already-constructed constraint through here, so refusing a
			// foreign function HERE refuses it at construction time for all
			// of them -- before any inverting caller (s:not, s:when) can
			// misread the refusal as "the constraint failed".
			if !isValidator(in) {
				return lisp.ErrorConditionf(BadArgs,
					"Bad input type: an ordinary function is not usable as a constraint (%v). Constraints must be built by the s package (s:int, s:has-key, s:gt, ...) or by libschema.NewValidator.",
					in)
			}
			return in
		}
		res = lisp.ErrorConditionf(BadArgs, "Bad input type: %s is not usable as a constraint (%v)", in.Type.String(), in)
	}
	return res
}

// symcounter names anonymous validators.  It is ATOMIC, and it has to be
// (issue #364).
//
// Every validator this package mints takes its FID from here: NewValidator
// does, and so does every s: constructor reachable from ordinary ELPS source
// -- s:int and s:gt each mint one, s:deftype mints another around them.  So
// two runtimes evaluating schema code on two goroutines both land in
// GenSymbol, and while it was a plain int++ that was a data race with no Go
// embedder involvement required.  substrate runs dozens of environments
// evaluating concurrently, so that is the normal case, not an exotic one.
//
// The damage from losing an increment is only a duplicate name in a stack
// frame; the undefined behaviour of the unsynchronised read-modify-write is
// the actual defect, and it is why this was invisible for so long.
//
// The counter is process-global rather than per-Runtime because NewValidator
// has no runtime to hang it off: its signature is (formals, fn), and giving it
// one would be an API break for an extension point whose whole point is that
// the value it returns is runtime-independent.  Global-and-atomic also gives
// FIDs that stay distinct across runtimes, which is what you want when reading
// a stack trace out of a process running many.  lisp.Runtime.gensym is the
// in-tree precedent for the atomic counter.
var symcounter atomic.Uint64

// GenSymbol returns a fresh name for an anonymous validator.  It is safe to
// call from multiple goroutines.
func GenSymbol() string {
	return fmt.Sprintf("_validation_fun_%d", symcounter.Add(1))
}

// validatorTag is a private zero-size type whose ADDRESS identifies a schema
// validator.  Nothing outside this package can obtain the pointer, and
// lisp.Native of an identical struct value is a different pointer, so the
// marker cannot be forged -- not from ELPS source, and not from a Go caller
// that has not gone through NewValidator.
type validatorTag struct{}

// validatorMarker is the single marker cell every validator LFun carries in
// Cells[validatorMarkerIndex].  Its identity is the credential.
//
// Deliberately process-wide: the marker is an identity-only credential —
// compared by pointer in isValidator, never evaluated, never bound into a
// scope, and never written after init.  A per-runtime marker would break
// nothing but would also credential nothing: its whole value is that every
// runtime recognizes the same pointer.
var validatorMarker = lisp.Native(&validatorTag{})

// A validator LFun's cells are [formals, docstring, marker].  The first two
// come from lisp.FunInPackage; newValidator appends the third.  For a builtin
// LVal.Docstring reads only Cells[1], and nothing else in the interpreter
// walks an LFun's cells, so the extra cell is invisible.
const (
	validatorMarkerIndex = 2
	validatorCellCount   = 3
)

// isValidator reports whether v is a schema constraint minted by this package.
//
// WHY THIS EXISTS.  Every constraint in libschema is invoked through a PRIVATE
// calling convention: the validator LFun's Go closure takes the value under
// test DIRECTLY, not an argument list, so constraints are called by reaching
// into the Builtin accessor rather than through LEnv.FunCall.  That convention is
// unenforceable by the type system -- s:validate, s:has-key, s:not, s:when and
// the rest accepted ANY lisp.LFun -- so an ordinary function landing in a
// constraint slot was invoked with a bare value:
//
//	(s:validate identity 1)        -> index out of range [0] with length 0
//	(s:validate (lambda (x) x) 1)  -> nil pointer dereference (Builtin is nil)
//
// Both are reachable from phylum source, which in substrate is
// customer-supplied. The marker turns a whole CLASS of crash into a lisp-level
// error, rather than patching the instances one at a time.
//
// The Builtin check is not redundant with the marker check: it is what makes
// the credential unforgeable even if the marker value ever leaked into a
// user's hands, since a user lambda always has a nil Builtin.
func isValidator(v *lisp.LVal) bool {
	return v != nil &&
		v.Type == lisp.LFun &&
		len(v.Cells) == validatorCellCount &&
		v.Cells[validatorMarkerIndex] == validatorMarker &&
		v.Builtin() != nil
}

// applyConstraint is the ONE place a schema constraint is invoked.
//
// All sixteen former call sites in this file route through it, and
// TestNoUnroutedConstraintInvocation fails if a second raw invocation ever
// appears.  The check has to live in one place: sixteen copies of it would be
// sixteen chances to write the next one without it, which is exactly how the
// original defect looked.
func applyConstraint(env *lisp.LEnv, constraint *lisp.LVal, input *lisp.LVal) *lisp.LVal {
	if constraint == nil {
		return lisp.ErrorConditionf(BadArgs, "Missing constraint")
	}
	// A constructor that already failed propagates its own error unchanged;
	// re-wrapping it as "not a constraint" would hide the real cause.
	if constraint.Type == lisp.LError {
		return constraint
	}
	if !isValidator(constraint) {
		return lisp.ErrorConditionf(BadArgs,
			"Value is not a schema constraint: %v. Constraints must be built by the s package (s:int, s:has-key, s:gt, ...) or by libschema.NewValidator; an ordinary function cannot be used as one.",
			constraint)
	}
	return constraint.Builtin()(env, input)
}

// newValidator constructs an anonymous schema validator LFun bound to the
// libschema package. Without the package binding, the LFun reaching
// funCall / MacroCall / SpecialOpCall would trigger "BUG: GetFunName" log
// spam (issue #271).
func newValidator(formals *lisp.LVal, fn lisp.LBuiltin) *lisp.LVal {
	return markValidator(lisp.FunInPackage(DefaultPackageName, GenSymbol(), formals, fn))
}

// newNamedValidator is like newValidator but uses the given FID instead of
// an auto-generated one (so call-stack frames carry the type name).
func newNamedValidator(name string, formals *lisp.LVal, fn lisp.LBuiltin) *lisp.LVal {
	return markValidator(lisp.FunInPackage(DefaultPackageName, name, formals, fn))
}

// markValidator stamps the credential onto fun's cells.
//
// The cell slice is allocated at EXACTLY its final length, so cap == len.
// That is the issue #373 clamp applied to an LFun: a slice handed out with
// spare capacity is a slice something else can append into, and that append
// writes through into cells the owner still considers its own.  Before the
// clamp, appending the marker onto the two-cell slice lisp.FunInPackage
// returns grew the backing array to capacity 4, so every validator carried a
// spare slot for life; append(v.Cells[:2], x) would have overwritten the
// marker in place, silently revoking the credential of a value that -- per
// NewValidator's contract below -- may be shared by every runtime in the
// process.
//
// Nothing does that today.  The clamp is what stops "nothing does that" from
// being load-bearing, and it is not a cost: one exact-length allocation
// replaces one grow-on-append allocation, and allocates less.
func markValidator(fun *lisp.LVal) *lisp.LVal {
	cells := make([]*lisp.LVal, 0, len(fun.Cells)+1)
	cells = append(cells, fun.Cells...)
	cells = append(cells, validatorMarker)
	fun.Cells = cells
	return fun
}

// NewValidator returns a schema constraint implemented in Go.
//
// This is the extension point the marker would otherwise have closed.  Before
// the marker existed, a Go embedder could pass any lisp.LFun where libschema
// expected a constraint and it worked by accident -- the call sites simply
// invoked the raw builtin closure.  Requiring the marker ends that, so the capability
// is restored deliberately and with a documented contract instead of being
// dropped silently.  Nothing in this repository, and nothing in substrate, uses
// it today; it exists so that closing the crash does not also remove a
// capability someone might be relying on.
//
// CONTRACT: fn is called with the value under test as its second argument --
// the value itself, NOT an argument list.  It must return lisp.Nil() when the
// value satisfies the constraint and an LError (see lisp.ErrorConditionf, with
// FailedConstraint or WrongType) when it does not.  It must not panic: a panic
// here surfaces as an internal-panic condition, which handler-bind is
// documented not to catch.
//
// Handing an ELPS lambda to a constraint slot is still refused, by design.
// There is no way to call one with libschema's convention, and the whole point
// of the marker is that the refusal is a lisp-level error rather than a nil
// dereference.
//
// RUNTIME SCOPE (issue #364): the returned value may be bound into ANY number
// of lisp.LEnv / lisp.Runtime pairs, including concurrently.  That is the
// natural reading of an extension point -- build the constraint set once at
// process start, install it into every environment you create -- and it is now
// a guarantee rather than something that happened to work:
//
//   - NewValidator is itself safe to call from multiple goroutines.
//   - The returned value owns all of its own state.  The formals are COPIED,
//     so a caller that keeps its formals list and writes through it later
//     cannot reach into validators already built from it; and the cell slice
//     is capacity-clamped, so no append through a view of it can overwrite the
//     validator credential.
//   - The interpreter does not write into a validator while running it.
//     TestSharedValidatorIsNotMutatedByEvaluation pins that, so this line does
//     not quietly become false.
//
// Two obligations stay with the caller, because they cannot be enforced here:
// fn must be safe to call from multiple goroutines if the validator is shared
// across them, and any value fn captures is shared on exactly the same terms.
func NewValidator(formals *lisp.LVal, fn lisp.LBuiltin) *lisp.LVal {
	// Copy the caller's formals: see RUNTIME SCOPE above.  This is done at
	// the exported boundary only.  The package-internal callers of
	// newValidator all build a fresh lisp.Formals(...) inline on the call
	// and drop the reference immediately, so making the copy unconditional
	// would put a second allocation on the path every s: constructor takes
	// -- and buy nothing, since there is no other holder to defend against.
	return newValidator(formals.Copy(), fn)
}

// Checks constraints and type for boolean values
func builtinCheckBool(_ *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.TrueSymbol && input.Str != lisp.FalseSymbol {
			return lisp.ErrorConditionf(WrongType, "Input was not a boolean for type %s", name)
		}
		return applyConstraint(env, builtinCheckAny(env, constraints), input)
	})
}

// Checks constraints and type for values with a user-defined type.
func builtinCheckTaggedVal(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	var rest *lisp.LVal
	if len(constraints) == 0 || constraints[0].Type != lisp.LString {
		rest = builtinCheckAny(env, constraints)
	} else {
		subtype := constraints[0]
		constraints = constraints[1:]
		rest = getHandler(env, subtype, name, constraints)
		if rest.Type == lisp.LError {
			return rest
		}
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LTaggedVal {
			return lisp.ErrorConditionf(WrongType, "Input was not a tagged-value for type %s", name)
		}
		return applyConstraint(env, rest, input.UserData())
	})
}

// Checks constraints and type for untyped values
func builtinCheckAny(_ *lisp.LEnv, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		for _, constraint := range constraints {
			if v := applyConstraint(env, constraint, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

// Checks constraints and type for functions
func builtinCheckFun(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFun {
			return lisp.ErrorConditionf(WrongType, "Input was not a function for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks constraints and type for sorted-map values
func builtinCheckMap(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input was not a sorted map for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks constraints and type for array values
func builtinCheckArray(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Input was not an array for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks constraints and type for string values
func builtinCheckString(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LString {
			return lisp.ErrorConditionf(WrongType, "Input was not a string for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks values are within the allowed set. Good for making enums.
func builtinAllowedValues(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		for _, v := range args.Cells {
			if eq := input.Equal(v); lisp.True(eq) {
				return lisp.Nil()
			}
		}
		return lisp.ErrorConditionf(FailedConstraint, "Supplied value was not in the list of allowed values")
	})
}

func builtinRegexp(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	pattern, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(BadArgs, "You must specify a pattern")
	}
	compiled, err := regexp.Compile(pattern)
	if err != nil {
		return lisp.ErrorConditionf(BadArgs, "You must specify a valid pattern: %s", err.Error())
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		match, readable := lisp.GoString(input)
		if readable && compiled.MatchString(match) {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Supplied value did not match the pattern %s", pattern)
	})
}

// constraintLen returns the length of input for the three types the s:len*
// constraints know how to measure, and false for every other type.
//
// The false case is what the constraints have always done for, say, an int or
// a list: the constraint simply passes.  That is preserved here, but the
// switch now names every LType so that adding one forces a decision instead
// of quietly joining the "no length, always passes" group.
//
// Worth a second look: LSExpr (a list) and LSortMap are in that group today
// even though both have an obvious length, so (s:len 4) accepts a list of any
// size.  Deliberately left alone -- tightening it turns schema validations
// that pass today into failures for existing callers.  See
// TestLenConstraintUnmeasuredTypes, which pins the current behaviour.
func constraintLen(input *lisp.LVal) (int, bool) {
	switch input.Type {
	case lisp.LString:
		return len(input.Str), true
	case lisp.LBytes:
		return len(input.Bytes()), true
	case lisp.LArray:
		return input.Len(), true
	case lisp.LInvalid, lisp.LInt, lisp.LFloat, lisp.LError, lisp.LSymbol,
		lisp.LQSymbol, lisp.LSExpr, lisp.LFun, lisp.LQuote, lisp.LSortMap,
		lisp.LNative, lisp.LTaggedVal, lisp.LMarkTerminal, lisp.LMarkTailRec,
		lisp.LMarkMacExpand, lisp.LTypeMax:
		return 0, false
	}
	return 0, false
}

// lenConstraint builds a validator that fails when input has a measurable
// length and cmp reports that length as out of bounds.  Inputs with no
// measurable length pass.
func lenConstraint(args *lisp.LVal, cmp func(length, comparison int) bool) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		length, ok := constraintLen(input)
		if ok && cmp(length, comparison) {
			return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
		}
		return lisp.Nil()
	})
}

// Checks length of a string, bytes or array
func builtinLen(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lenConstraint(args, func(length, comparison int) bool { return length != comparison })
}

// Checks length of a string, bytes or array
func builtinLenGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lenConstraint(args, func(length, comparison int) bool { return length <= comparison })
}

// Checks length of a string, bytes or array
func builtinLenGreaterThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lenConstraint(args, func(length, comparison int) bool { return length < comparison })
}

// Checks length of a string, bytes or array
func builtinLenLessThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lenConstraint(args, func(length, comparison int) bool { return length >= comparison })
}

// Checks length of a string, bytes or array
func builtinLenLessThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lenConstraint(args, func(length, comparison int) bool { return length > comparison })
}

// Checks value is greater than specified value
func builtinGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if comparison >= compareTo {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value was less than the allowed value")
		}
		return lisp.Nil()
	})
}

// Checks value is greater or equal than specified value
func builtinGreaterThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if comparison > compareTo {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value %v was less than the allowed value %v", compareTo, comparison)
		}
		return lisp.Nil()
	})
}

// Checks value is less than specified value
func builtinLessThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if comparison <= compareTo {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value was greater than the allowed value")
		}
		return lisp.Nil()
	})
}

// Checks value is less than or equal specified value
func builtinLessThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if comparison < compareTo {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value was greater than the allowed value")
		}
		return lisp.Nil()
	})
}

// Checks array members are of correct type
func builtinArrayOf(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	compares := make([]*lisp.LVal, 0)
	for _, v := range args.Cells {
		c := getHandler(env, v, "x", []*lisp.LVal{})
		// Propagate at CONSTRUCTION. Left to be discovered at application
		// time the error is swallowed by the "did any allowed type match?"
		// loop below and reported as "Item N was of wrong type" -- a
		// misconfigured schema masquerading as bad data.
		if c.Type == lisp.LError {
			return c
		}
		compares = append(compares, c)
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Invalid input for 'of' - need an array")
		}
		for k, v := range input.Cells[1].Cells {
			matched := false
			for _, compare := range compares {
				if applyConstraint(env, compare, v).IsNil() {
					matched = true
					break
				}
			}
			if !matched {
				return lisp.ErrorConditionf(WrongType, "Item %d was of wrong type", k)
			}
		}
		return lisp.Nil()
	})
}

// Checks value is greater than 0
func builtinPositive(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if compareTo <= 0 {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value was not positive")
		}
		return lisp.Nil()
	})
}

// Checks value is less than zero
func builtinNegative(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		compareTo, ok := lisp.GoFloat64(input)
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Value cannot be compared")
		}
		if compareTo >= 0 {
			return lisp.ErrorConditionf(FailedConstraint, "Supplied value was not negative")
		}
		return lisp.Nil()
	})
}

// Checks type and constraints for integers
func builtinCheckInt(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newNamedValidator(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt {
			return lisp.ErrorConditionf(WrongType, "Input was not an integer for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks type and constraints for floats
func builtinCheckFloat(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newNamedValidator(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a float for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// Checks type and constraints for numbers
func builtinCheckNumber(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return newNamedValidator(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt && input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a number for type %s", name)
		}
		return applyConstraint(env, rest, input)
	})
}

// schemaKey is the ONE place a schema key string is turned into the LVal used
// to look it up in a sorted-map.  Every `.Get(` in this file goes through it,
// and TestSchemaKeysAreLookedUpAsStrings fails if a raw key LVal ever appears
// at a lookup again.
//
// THE KEY'S LVAL TYPE IS PART OF THE LOOKUP, NOT A LABEL ON IT.  lisp.Map is
// an interface with more than one implementation and they do NOT agree:
//
//   - the built-in sortedmap (lisp/maps.go) keys on LVal.Str for both LString
//     and LSymbol, so there a string lookup and a symbol lookup are the same
//     lookup -- and a string lookup also finds a symbol-keyed entry; but
//   - libjson.SortedMap -- what json:load-string returns -- REJECTS any key
//     whose Type is not LString, returning (LError, false).
//
// s:may-have-key looked its key up as lisp.Symbol(key) while s:has-key,
// s:when and s:no-other-keys all used strings.  On a JSON-decoded map that
// made Get return "not found" unconditionally, and may-have-key's "key is
// absent, so pass" branch swallowed it: the constraint was a silent no-op on
// exactly the maps it is most often written for.  Wrong answer, not a crash,
// so no fuzz target could see it.  See issue #325.
//
// STRING IS CANONICAL: it is what the other three constraints already used,
// it is the only key type a strict Map accepts, and it loses nothing on the
// built-in map where it matches symbol-keyed entries too.
func schemaKey(key string) *lisp.LVal {
	return lisp.String(key)
}

// Checks sorted-map has specified key and its type and constraints
func builtinHasKey(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	key, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a key")
	}
	compares := make([]*lisp.LVal, 0)
	for _, v := range args.Cells[1:] {
		c := getHandler(env, v, "x", []*lisp.LVal{})
		// Propagate at CONSTRUCTION -- see builtinArrayOf.
		if c.Type == lisp.LError {
			return c
		}
		compares = append(compares, c)
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input is not sorted map")
		}
		matched := false
		// The !ok branch here is already the LOUD one: a map that cannot be
		// searched for this key and a map that simply lacks it both fail.
		// s:may-have-key's equivalent branch PASSES, which is why it needs the
		// extra distinction the sibling below draws.
		val, ok := input.Map().Get(schemaKey(key))
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Map does not have key %s", key)
		}
		for _, compare := range compares {
			if applyConstraint(env, compare, val).IsNil() {
				matched = true
				break
			}
		}
		if !matched {
			return lisp.ErrorConditionf(WrongType, "Key %s was of wrong type", key)
		}
		return lisp.String(key)
	})
}

// Checks if sorted-map has specified key and if so, its type and constraints
func builtinMayHaveKey(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	key, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a key")
	}
	compares := make([]*lisp.LVal, 0)
	for _, v := range args.Cells[1:] {
		c := getHandler(env, v, "x", []*lisp.LVal{})
		// Propagate at CONSTRUCTION -- see builtinArrayOf.
		if c.Type == lisp.LError {
			return c
		}
		compares = append(compares, c)
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input is not sorted map")
		}
		matched := false
		val, ok := input.Map().Get(schemaKey(key))
		if !ok {
			// Get signals two different things through the same false: "no
			// such key" (val is nil-ish) and "this map cannot hold a key of
			// that type at all" (val is an LError).  Only the first means
			// absent.  Treating the second as absent is precisely how #325
			// hid -- the constraint passed on every input it was given.  No
			// in-tree Map reaches this branch now that the key is always an
			// LString; it is here so a future strict Map implementation
			// cannot silently reproduce the same no-op.
			if val != nil && val.Type == lisp.LError {
				return lisp.ErrorConditionf(WrongType,
					"Map cannot be searched for key %s: %v", key, val)
			}
			return lisp.String(key)
		}
		for _, compare := range compares {
			if applyConstraint(env, compare, val).IsNil() {
				matched = true
				break
			}
		}
		if !matched {
			return lisp.ErrorConditionf(WrongType, "Key %s was of wrong type", key)
		}
		return lisp.String(key)
	})
}

// Checks only the keys specified in its children exist on the sorted map
func builtinNoOtherKeys(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	constraints := make([]*lisp.LVal, 0)
	for _, v := range args.Cells {
		c := getHandler(env, v, "x", []*lisp.LVal{})
		// Propagate at CONSTRUCTION -- see builtinArrayOf.
		if c.Type == lisp.LError {
			return c
		}
		constraints = append(constraints, c)
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		allowedKeys := make(map[string]bool)
		for _, c := range constraints {
			val := applyConstraint(env, c, input)
			if val.Type == lisp.LError { //nolint:staticcheck // not a tagged switch context
				return val
			} else if val.Type == lisp.LString {
				allowedKeys[val.Str] = true
			}
		}
		for _, mapKey := range input.Map().Keys().Cells {
			if mapKey.Type != lisp.LString && mapKey.Type != lisp.LSymbol {
				return lisp.ErrorConditionf(FailedConstraint, "Map is not allowed to have key '%s'", mapKey)
			}
			if _, ok := allowedKeys[mapKey.Str]; !ok {
				return lisp.ErrorConditionf(FailedConstraint, "Map is not allowed to have key '%s'", mapKey)
			}
		}
		return lisp.Nil()
	})
}

// Checks sorted-map key when a condition on another key is met
//
// THE GUARD CONSTRAINT IS VALIDATED AT CONSTRUCTION, NOT AT APPLICATION.
// s:when reads an error from its guard as "the condition is not met, skip
// these checks".  That is correct for a real constraint and catastrophic for
// anything else: an error meaning "this is not a constraint", raised at
// application time, would silently skip the WHOLE CLAUSE.
//
//	(s:deftype "M" s:sorted-map (s:has-key "a" s:int) (s:has-key "b" s:int)
//	                            (s:when "a" identity "b" (s:gt 100)))
//	(s:validate M (sorted-map "a" 1 "b" 1))
//
// Before any of this work that expression raised [INTERNAL-PANIC] -- loud, and
// obviously broken.  Routing the guard through applyConstraint and stopping
// there would make it return () -- VALIDATION PASSES, with b = 1 against a
// constraint demanding b > 100.  A crash in a validator is bad; a validator
// that quietly approves invalid data is worse.  Rejecting at construction, the
// way s:not does, is the only placement where the inversion cannot bite.
func builtinWhen(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// Both keys are captured as Go strings at CONSTRUCTION and re-minted
	// through schemaKey at application, rather than the argument LVals being
	// handed to Get directly.  Behaviour is unchanged -- GoString only
	// succeeds for an LString, so these were already string lookups -- but it
	// puts s:when under the same single funnel as its siblings, which is what
	// makes the drift guard able to see every lookup in the file.  See #325.
	whenKey, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a key")
	}
	matchKey, ok := lisp.GoString(args.Cells[2])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a match key")
	}
	whenConstraint := getHandler(env, args.Cells[1], "x", []*lisp.LVal{})
	if whenConstraint.Type == lisp.LError {
		return whenConstraint
	}
	if !isValidator(whenConstraint) {
		return lisp.ErrorConditionf(BadArgs, "Guard value is not a constraint")
	}
	constraints := make([]*lisp.LVal, 0)
	for _, v := range args.Cells[3:] {
		c := getHandler(env, v, "x", []*lisp.LVal{})
		if c.Type == lisp.LError {
			return c
		}
		constraints = append(constraints, c)
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		// input.Map() panics ("not sorted-map: int") on anything else. s:when
		// is reachable under s:any, where no earlier constraint has checked
		// the type: (s:deftype "T" "any" (s:when "a" s:int "b" s:int)) then
		// (s:validate T 1).
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input is not sorted map")
		}
		lMap := input.Map()
		whenVal, _ := lMap.Get(schemaKey(whenKey))
		testVal, _ := lMap.Get(schemaKey(matchKey))
		val := applyConstraint(env, whenConstraint, whenVal)
		if val.Type == lisp.LError {
			// Guard not satisfied: this clause does not apply. Safe only
			// because whenConstraint was proven to be a real constraint at
			// construction -- see the doc comment.
			return lisp.Nil()
		}
		for _, c := range constraints {
			val := applyConstraint(env, c, testVal)
			if val.Type == lisp.LError {
				return val
			}
		}
		return lisp.Nil()
	})
}

// Checks if value is false
func builtinIsFalse(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.FalseSymbol {
			return lisp.ErrorConditionf(FailedConstraint, "Value %v is not false", input)
		}
		return lisp.Nil()
	})
}

// Checks if value is true
func builtinIsTrue(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.TrueSymbol {
			return lisp.ErrorConditionf(FailedConstraint, "Value %v is not true", input)
		}
		return lisp.Nil()
	})
}

// Checks if value can reasonably be considered to be false
func builtinIsFalsy(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		val := applyConstraint(env, builtinIsTruthy(env, nil), input)
		if val.Type == lisp.LError {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Value %v is not falsy", input)
	})
}

// Checks if value can reasonably be considered to be true
func builtinIsTruthy(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str == lisp.TrueSymbol {
			return lisp.Nil()
		}
		switch input.Type {
		case lisp.LArray:
			if input.Cells[0].Cells[0].Int > 0 {
				return lisp.Nil()
			}
		case lisp.LSortMap, lisp.LBytes:
			if len(input.Cells) > 0 {
				return lisp.Nil()
			}
		case lisp.LString:
			if len(input.Str) > 0 && input.Str != lisp.FalseSymbol {
				return lisp.Nil()
			}
		case lisp.LInt:
			if input.Int > 0 {
				return lisp.Nil()
			}
		case lisp.LFloat:
			if input.Float > 0.0 {
				return lisp.Nil()
			}
		case lisp.LInvalid, lisp.LError, lisp.LSymbol, lisp.LQSymbol,
			lisp.LSExpr, lisp.LFun, lisp.LQuote, lisp.LNative, lisp.LTaggedVal,
			lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
			lisp.LTypeMax:
			// Not truthy.  Symbols other than 'true are handled by the
			// TrueSymbol check above; everything else here has no truthiness
			// rule and is rejected.  Enumerated so a new LType has to pick a
			// side rather than defaulting to "not truthy".
		}
		return lisp.ErrorConditionf(FailedConstraint, "Value %v is not truthy", input)
	})
}

// Reverses a constraint
func builtinIsNot(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	constraint := args.Cells[0]
	// Rejected at CONSTRUCTION, not at application. s:not inverts its inner
	// constraint's result, so a "not a schema constraint" error raised at
	// application time would be read as "the inner constraint failed" and
	// s:not would PASS -- turning a loud crash into a silent wrong answer.
	// Same inversion hazard as s:when; see builtinWhen.
	if !isValidator(constraint) {
		return lisp.ErrorConditionf(BadArgs, "Value is not a constraint")
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return newValidator(lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		val := applyConstraint(env, constraint, input)
		if val.Type == lisp.LError {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Inner constraint did not return an error")
	})
}
