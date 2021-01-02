package libschema

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "s"

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
)

const (
	BadArgs          = "bad-arguments"
	FailedConstraint = "failed-constraint"
	WrongType        = "wrong-type"
)

// LoadPackage adds the math package to env
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
	for _, symbol := range symbols {
		env.PutGlobal(lisp.Symbol(symbol), lisp.String(symbol))
		env.Runtime.Package.Externals = append(env.Runtime.Package.Externals, symbol)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("deftype", lisp.Formals("name", "type", "&rest", "constraints"), builtinDefType),
	libutil.Function("in", lisp.Formals("&rest", "allowed-values"), builtinAllowedValues),
	libutil.Function("gt", lisp.Formals("&rest", "allowed-values"), builtinGreaterThan),
	libutil.Function("gte", lisp.Formals("&rest", "allowed-values"), builtinGreaterThanOrEqual),
	libutil.Function("lt", lisp.Formals("&rest", "allowed-values"), builtinLessThan),
	libutil.Function("lte", lisp.Formals("&rest", "allowed-values"), builtinLessThanOrEqual),
	libutil.Function("positive", lisp.Formals("&rest", "allowed-values"), builtinPositive),
	libutil.Function("negative", lisp.Formals("&rest", "allowed-values"), builtinNegative),
	libutil.Function("validate", lisp.Formals("type", "input"), builtinValidate),
}

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
}

func builtinValidate(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return lisp.ErrorConditionf(BadArgs, "Not enough arguments")
	}
	if args.Cells[0].Type != lisp.LFun {
		return lisp.ErrorConditionf(BadArgs, "First argument is not a type")
	}
	return args.Cells[0].FunData().Builtin(env, args.Cells[1])
}

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
	lType, _ := lisp.GoString(args.Cells[1])
	constraints := []*lisp.LVal{}
	if len(args.Cells) > 2 {
		constraints = args.Cells[2:]
	}
	var res *lisp.LVal
	switch lType {
	case String:
		res = env.PutGlobal(lisp.Symbol(args.Cells[0].Str), builtinCheckString(name, constraints))
	case Int:
		res = env.PutGlobal(lisp.Symbol(args.Cells[0].Str), builtinCheckInt(name, constraints))
	case Float:
		res = env.PutGlobal(lisp.Symbol(args.Cells[0].Str), builtinCheckFloat(name, constraints))
	case Number:
		res = env.PutGlobal(lisp.Symbol(args.Cells[0].Str), builtinCheckNumber(name, constraints))
	}
	if res != nil && res.Type == lisp.LError {
		return res
	}
	return lisp.Nil()
}

func builtinCheckString(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LString {
			return lisp.ErrorConditionf(WrongType, "Input was not a string for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

func builtinAllowedValues(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		for _, v := range args.Cells {
			if eq := input.Equal(v); lisp.True(eq) {
				return lisp.Nil()
			}
		}
		return lisp.ErrorConditionf(FailedConstraint, "Supplied value was not in the list of allowed values")
	})
}

func builtinGreaterThan(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinGreaterThanOrEqual(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLessThan(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLessThanOrEqual(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoFloat64(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinPositive(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinNegative(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinCheckInt(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt {
			return lisp.ErrorConditionf(WrongType, "Input was not an integer for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

func builtinCheckFloat(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a float for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

func builtinCheckNumber(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun(name, lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LInt && input.Type != lisp.LFloat {
			return lisp.ErrorConditionf(WrongType, "Input was not a number for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}
