// Copyright © 2021 The ELPS authors
// This package provides schema validation for ELPS types
// Author: Reuben Thompson
package libschema

import (
	"fmt"
	"regexp"

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

// LoadPackage adds the schema package to env
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
		Returns the key name on success (used by no-other-keys).`),
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
	return val.FunData().Builtin(env, input)
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
			return in
		}
		res = lisp.ErrorConditionf(BadArgs, "Bad input type: %s is not usable as a constraint (%v)", in.Type.String(), in)
	}
	return res
}

var symcounter = 0

// Keep our symbols clean
func GenSymbol() string {
	symcounter++
	return fmt.Sprintf("_validation_fun_%d", symcounter)
}

// Checks constraints and type for boolean values
func builtinCheckBool(_ *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.TrueSymbol && input.Str != lisp.FalseSymbol {
			return lisp.ErrorConditionf(WrongType, "Input was not a boolean for type %s", name)
		}
		return builtinCheckAny(env, constraints).FunData().Builtin(env, input)
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LTaggedVal {
			return lisp.ErrorConditionf(WrongType, "Input was not a tagged-value for type %s", name)
		}
		return rest.FunData().Builtin(env, input.UserData())
	})
}

// Checks constraints and type for untyped values
func builtinCheckAny(_ *lisp.LEnv, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		for _, constraint := range constraints {
			if constraint.Type != lisp.LFun {
				return lisp.ErrorConditionf(BadArgs, "Invalid type received for constraint %v", constraint)
			}
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFun {
			return lisp.ErrorConditionf(WrongType, "Input was not a function for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks constraints and type for sorted-map values
func builtinCheckMap(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input was not a sorted map for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks constraints and type for array values
func builtinCheckArray(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Input was not an array for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks constraints and type for string values
func builtinCheckString(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LString {
			return lisp.ErrorConditionf(WrongType, "Input was not a string for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks values are within the allowed set. Good for making enums.
func builtinAllowedValues(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		match, readable := lisp.GoString(input)
		if readable && compiled.MatchString(match) {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Supplied value did not match the pattern %s", pattern)
	})
}

// Checks length of a string, bytes or array
func builtinLen(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		switch input.Type {
		case lisp.LString:
			if len(input.Str) != comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LBytes:
			if len(input.Bytes()) != comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LArray:
			if input.Len() != comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		}
		return lisp.Nil()
	})
}

// Checks length of a string, bytes or array
func builtinLenGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		switch input.Type {
		case lisp.LString:
			if len(input.Str) <= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LBytes:
			if len(input.Bytes()) <= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LArray:
			if input.Len() <= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		}
		return lisp.Nil()
	})
}

// Checks length of a string, bytes or array
func builtinLenGreaterThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		switch input.Type {
		case lisp.LString:
			if len(input.Str) < comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LBytes:
			if len(input.Bytes()) < comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LArray:
			if input.Len() < comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		}
		return lisp.Nil()
	})
}

// Checks length of a string, bytes or array
func builtinLenLessThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		switch input.Type {
		case lisp.LString:
			if len(input.Str) >= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LBytes:
			if len(input.Bytes()) >= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LArray:
			if input.Len() >= comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		}
		return lisp.Nil()
	})
}

// Checks length of a string, bytes or array
func builtinLenLessThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		switch input.Type {
		case lisp.LString:
			if len(input.Str) > comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LBytes:
			if len(input.Bytes()) > comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		case lisp.LArray:
			if input.Len() > comparison {
				return lisp.ErrorConditionf(FailedConstraint, "Length was not %d", comparison)
			}
		}
		return lisp.Nil()
	})
}

// Checks value is greater than specified value
func builtinGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
		compares = append(compares, getHandler(env, v, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Invalid input for 'of' - need an array")
		}
		for k, v := range input.Cells[1].Cells {
			matched := false
			for _, compare := range compares {
				val := builtinValidate(env, &lisp.LVal{
					Cells: []*lisp.LVal{
						compare,
						v,
					},
				})
				if val.IsNil() {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt {
			return lisp.ErrorConditionf(WrongType, "Input was not an integer for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks type and constraints for floats
func builtinCheckFloat(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a float for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks type and constraints for numbers
func builtinCheckNumber(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt && input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a number for type %s", name)
		}
		return rest.FunData().Builtin(env, input)
	})
}

// Checks sorted-map has specified key and its type and constraints
func builtinHasKey(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	key, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a key")
	}
	compares := make([]*lisp.LVal, 0)
	for _, v := range args.Cells[1:] {
		compares = append(compares, getHandler(env, v, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input is not sorted map")
		}
		matched := false
		val, ok := input.Map().Get(lisp.String(key))
		if !ok {
			return lisp.ErrorConditionf(FailedConstraint, "Map does not have key %s", key)
		}
		for _, compare := range compares {
			val := builtinValidate(env, &lisp.LVal{
				Cells: []*lisp.LVal{
					compare,
					val,
				},
			})
			if val.IsNil() {
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
		compares = append(compares, getHandler(env, v, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input is not sorted map")
		}
		matched := false
		val, ok := input.Map().Get(lisp.Symbol(key))
		if !ok {
			return lisp.String(key)
		}
		for _, compare := range compares {
			val := builtinValidate(env, &lisp.LVal{
				Cells: []*lisp.LVal{
					compare,
					val,
				},
			})
			if val.IsNil() {
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
		constraints = append(constraints, getHandler(env, v, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		allowedKeys := make(map[string]bool)
		for _, c := range constraints {
			if c.Type != lisp.LFun {
				return lisp.ErrorConditionf(BadArgs, "Invalid type received for constraint %v", c)
			}
			val := c.FunData().Builtin(env, input)
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
func builtinWhen(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if _, ok := lisp.GoString(args.Cells[0]); !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a key")
	}
	if _, ok := lisp.GoString(args.Cells[2]); !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You must specify a match key")
	}
	whenConstraint := args.Cells[1]
	constraints := make([]*lisp.LVal, 0)
	for _, v := range args.Cells[3:] {
		constraints = append(constraints, getHandler(env, v, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		lMap := input.Map()
		whenVal, _ := lMap.Get(args.Cells[0])
		testVal, _ := lMap.Get(args.Cells[2])
		val := whenConstraint.FunData().Builtin(env, whenVal)
		if val.Type == lisp.LError {
			return lisp.Nil()
		}
		for _, c := range constraints {
			if c.Type == lisp.LError {
				return c
			} else if c.Type != lisp.LFun {
				return lisp.ErrorConditionf(BadArgs, "Invalid type received for constraint %v", c)
			}
			val := c.FunData().Builtin(env, testVal)
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
	return lisp.Fun(GenSymbol(), lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.FalseSymbol {
			return lisp.ErrorConditionf(FailedConstraint, "Value %v is not false", input)
		}
		return lisp.Nil()
	})
}

// Checks if value is true
func builtinIsTrue(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.TrueSymbol {
			return lisp.ErrorConditionf(FailedConstraint, "Value %v is not true", input)
		}
		return lisp.Nil()
	})
}

// Checks if value can reasonably be considered to be false
func builtinIsFalsy(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		val := builtinIsTruthy(env, nil).Builtin()(env, input)
		if val.Type == lisp.LError {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Value %v is not falsy", input)
	})
}

// Checks if value can reasonably be considered to be true
func builtinIsTruthy(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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
		}
		return lisp.ErrorConditionf(FailedConstraint, "Value %v is not truthy", input)
	})
}

// Reverses a constraint
func builtinIsNot(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	constraint := args.Cells[0]
	if constraint.Type != lisp.LFun {
		return lisp.ErrorConditionf(BadArgs, "Value is not a constraint")
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals(), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		val := constraint.FunData().Builtin(env, input)
		if val.Type == lisp.LError {
			return lisp.Nil()
		}
		return lisp.ErrorConditionf(FailedConstraint, "Inner constraint did not return an error")
	})
}
