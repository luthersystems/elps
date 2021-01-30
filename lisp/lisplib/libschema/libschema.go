// Copyright © 2021 The ELPS authors
// This package provides schema validation for ELPS types
// Author: Reuben Thompson
package libschema

import (
	"fmt"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
	"regexp"
)

// DeafultPackageName is the package name used by LoadPackage.
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
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	// We export the type symbols to the package to make writing code using it less messy
	for _, symbol := range symbols {
		env.PutGlobal(lisp.Symbol(symbol), lisp.String(symbol))
		env.Runtime.Package.Externals = append(env.Runtime.Package.Externals, symbol)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("deftype", lisp.Formals("name", "type", "&rest", "constraints"), builtinDefType),
	libutil.Function("in", lisp.Formals("&rest", "allowed-values"), builtinAllowedValues),
	libutil.Function("gt", lisp.Formals("allowed-value"), builtinGreaterThan),
	libutil.Function("gte", lisp.Formals("allowed-value"), builtinGreaterThanOrEqual),
	libutil.Function("lt", lisp.Formals("allowed-value"), builtinLessThan),
	libutil.Function("lte", lisp.Formals("allowed-value"), builtinLessThanOrEqual),
	libutil.Function("positive", lisp.Formals(), builtinPositive),
	libutil.Function("negative", lisp.Formals(), builtinNegative),
	libutil.Function("validate", lisp.Formals("type", "input"), builtinValidate),
	libutil.Function("len", lisp.Formals("allowed-value"), builtinLen),
	libutil.Function("lengt", lisp.Formals("allowed-value"), builtinLenGreaterThan),
	libutil.Function("lengte", lisp.Formals("allowed-value"), builtinLenGreaterThanOrEqual),
	libutil.Function("lenlt", lisp.Formals("allowed-value"), builtinLenLessThan),
	libutil.Function("lenlte", lisp.Formals("allowed-value"), builtinLenLessThanOrEqual),
	libutil.Function("of", lisp.Formals("&rest", "allowed-types"), builtinArrayOf),
	libutil.Function("has-key", lisp.Formals("key", "&rest", "allowed-types"), builtinHasKey),
	libutil.Function("may-have-key", lisp.Formals("key", "&rest", "allowed-types"), builtinMayHaveKey),
	libutil.Function("no-other-keys", lisp.Formals("&rest", "constraints"), builtinNoOtherKeys),
	libutil.Function("when", lisp.Formals("key", "constraint", "matchKey", "&rest", "constraints"), builtinWhen),
	libutil.Function("is-true", lisp.Formals(), builtinIsTrue),
	libutil.Function("is-false", lisp.Formals(), builtinIsFalse),
	libutil.Function("is-truthy", lisp.Formals(), builtinIsTruthy),
	libutil.Function("is-falsy", lisp.Formals(), builtinIsFalsy),
	libutil.Function("not", lisp.Formals("constraint"), builtinIsNot),
	libutil.Function("regexp", lisp.Formals("pattern"), builtinRegexp),
}

// This is the `s:validate` keyword. It checks its input matches the type specified
func builtinValidate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	if args.Cells[0].Type != lisp.LFun {
		return lisp.ErrorConditionf(BadArgs, "First argument is not a type")
	}
	return args.Cells[0].FunData().Builtin(env, args.Cells[1])
}

// This is the `s:deftype` keyword. It defines a type and associated constraints
func builtinDefType(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	name, ok := lisp.GoString(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(BadArgs, "First argument must resolve to a string")
	}
	if exists := env.Get(args.Cells[0]); !exists.IsNil() {
		return lisp.ErrorConditionf(BadArgs, "Symbol %s is already defined", args.Cells[0])
	}
	constraints := make([]*lisp.LVal, 0)
	if len(args.Cells) > 2 {
		constraints = args.Cells[2:]
	}
	res := getHandler(env, args.Cells[1], name, constraints)
	if res != nil && res.Type == lisp.LError {
		return res
	}
	if res != nil {
		res = env.PutGlobal(lisp.Symbol(args.Cells[0].Str), res)
		if res != nil && res.Type == lisp.LError {
			return res
		}
	}
	return lisp.Nil()
}

// finds the correct validation handler for the type
func getHandler(env *lisp.LEnv, in *lisp.LVal, name string, constraints []*lisp.LVal) *lisp.LVal {
	lType, _ := lisp.GoString(in)
	var res *lisp.LVal
	switch lType {
	case String:
		res = builtinCheckString(name, constraints)
	case Int:
		res = builtinCheckInt(name, constraints)
	case Float:
		res = builtinCheckFloat(name, constraints)
	case Number:
		res = builtinCheckNumber(name, constraints)
	case Array:
		res = builtinCheckArray(name, constraints)
	case SortedMap:
		res = builtinCheckMap(name, constraints)
	case Fun:
		res = builtinCheckFun(name, constraints)
	case Bool:
		res = builtinCheckBool(name, constraints)
	case Any:
		res = builtinCheckAny(constraints)
	default:
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
func builtinCheckBool(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Str != lisp.TrueSymbol && input.Str != lisp.FalseSymbol {
			return lisp.ErrorConditionf(WrongType, "Input was not a boolean for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks constraints and type for untyped values
func builtinCheckAny(constraints []*lisp.LVal) *lisp.LVal {
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
func builtinCheckFun(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFun {
			return lisp.ErrorConditionf(WrongType, "Input was not a function for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks constraints and type for sorted-map values
func builtinCheckMap(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input was not a sorted map for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks constraints and type for array values
func builtinCheckArray(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Input was not an array for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks constraints and type for string values
func builtinCheckString(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(GenSymbol(), lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LString {
			return lisp.ErrorConditionf(WrongType, "Input was not a string for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
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
func builtinCheckInt(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt {
			return lisp.ErrorConditionf(WrongType, "Input was not an integer for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks type and constraints for floats
func builtinCheckFloat(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a float for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
	})
}

// Checks type and constraints for numbers
func builtinCheckNumber(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt && input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a number for type %s", name)
		}
		return builtinCheckAny(constraints).FunData().Builtin(env, input)
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
		lMap := input.Map()
		val, ok := lMap[lisp.MapSymbol(key)]
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
		lMap := input.Map()
		val, ok := lMap[lisp.MapSymbol(key)]
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
			if val.Type == lisp.LError {
				return val
			} else if val.Type == lisp.LString {
				allowedKeys[val.Str] = true
			}
		}
		lMap := input.Map()
		for mapKey, _ := range lMap {
			if _, ok := allowedKeys[string(mapKey.(lisp.MapSymbol))]; !ok {
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
		whenVal := lMap[lisp.MapSymbol(args.Cells[0].Str)]
		testVal := lMap[lisp.MapSymbol(args.Cells[2].Str)]
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
