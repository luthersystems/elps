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
	libutil.Function("len", lisp.Formals("&rest", "allowed-values"), builtinLen),
	libutil.Function("lengt", lisp.Formals("&rest", "allowed-values"), builtinLenGreaterThan),
	libutil.Function("lengte", lisp.Formals("&rest", "allowed-values"), builtinLenGreaterThanOrEqual),
	libutil.Function("lenlt", lisp.Formals("&rest", "allowed-values"), builtinLenLessThan),
	libutil.Function("lenlte", lisp.Formals("&rest", "allowed-values"), builtinLenLessThanOrEqual),
	libutil.Function("of", lisp.Formals("&rest", "allowed-values"), builtinArrayOf),
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
	constraints := make([]*lisp.LVal, 0)
	if len(args.Cells) > 2 {
		constraints = args.Cells[2:]
	}
	res := getHandler(lType, name, constraints)
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

func getHandler(lType string, name string, constraints []*lisp.LVal) *lisp.LVal {
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
	default:
		// TODO handle nested types using env lookup
		res = lisp.ErrorConditionf(BadArgs, "Bad input type")
	}
	return res
}

func builtinCheckFun(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LFun {
			return lisp.ErrorConditionf(WrongType, "Input was not a function for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

func builtinCheckMap(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Input was not a sorted map for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
}

func builtinCheckArray(name string, constraints []*lisp.LVal) *lisp.LVal {
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Input was not an array for type %s", name)
		}
		for _, constraint := range constraints {
			if v := constraint.FunData().Builtin(env, input); v.Type == lisp.LError {
				return v
			}
		}
		return lisp.Nil()
	})
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

func builtinAllowedValues(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
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

func builtinLen(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLenGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLenGreaterThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLenLessThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinLenLessThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	comparison, ok := lisp.GoInt(args.Cells[0])
	if !ok {
		return lisp.ErrorConditionf(FailedConstraint, "You cannot compare %v to a number", args.Cells[0])
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinGreaterThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
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

func builtinGreaterThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
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

func builtinLessThan(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
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

func builtinLessThanOrEqual(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
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

func builtinArrayOf(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	compares := make([]*lisp.LVal, 0)
	for _, v := range args.Cells {
		compares = append(compares, getHandler(v.Str, "x", []*lisp.LVal{}))
	}
	// NB these aren't normal functions - they aren't looking for an array of args
	return lisp.Fun("allowed-values", lisp.Formals("input"), func(env *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
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

func builtinPositive(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
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

func builtinNegative(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
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
