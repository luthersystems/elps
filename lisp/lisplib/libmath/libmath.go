// Copyright © 2018 The ELPS authors

package libmath

import (
	"math"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "math"

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
	env.PutGlobal(lisp.Symbol("pi"), lisp.Float(math.Pi))
	env.PutGlobal(lisp.Symbol("inf"), lisp.Float(math.Inf(1)))
	env.PutGlobal(lisp.Symbol("-inf"), lisp.Float(math.Inf(-1)))
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("nan?", lisp.Formals("number"), builtinIsNaN,
		`Returns true if number is IEEE 754 NaN (not-a-number). Integers
		always return false. Returns an error if the argument is not
		a number.`),
	libutil.FunctionDoc("abs", lisp.Formals("number"), builtinAbs,
		`Returns the absolute value of number. Preserves the type: an int
		argument returns an int, a float returns a float. Returns an
		error on integer overflow (min-int).`),
	libutil.FunctionDoc("ceil", lisp.Formals("number"), builtinCeil,
		`Returns the smallest float not less than number (rounds toward
		positive infinity). Integers are returned unchanged. The result
		is always a float for float input.`),
	libutil.FunctionDoc("floor", lisp.Formals("number"), builtinFloor,
		`Returns the largest float not greater than number (rounds toward
		negative infinity). Integers are returned unchanged. The result
		is always a float for float input.`),
	libutil.FunctionDoc("sqrt", lisp.Formals("number"), builtinSqrt,
		`Returns the square root of number as a float. Accepts int or
		float arguments.`),
	libutil.FunctionDoc("exp", lisp.Formals("number"), builtinExp,
		`Returns e raised to the power of number (e^x) as a float.
		Accepts int or float arguments.`),
	libutil.FunctionDoc("ln", lisp.Formals("number"), builtinLn,
		`Returns the natural logarithm (base e) of number as a float.
		Accepts int or float arguments.`),
	libutil.FunctionDoc("log", lisp.Formals("base", "number"), builtinLog,
		`Returns the logarithm of number in the given base as a float.
		Both arguments must be numbers. Computed as ln(number)/ln(base).`),
	libutil.FunctionDoc("sin", lisp.Formals("radians"), builtinSin,
		`Returns the sine of radians as a float.`),
	libutil.FunctionDoc("sinh", lisp.Formals("radians"), builtinSinh,
		`Returns the hyperbolic sine of radians as a float.`),
	libutil.FunctionDoc("asin", lisp.Formals("radians"), builtinAsin,
		`Returns the arcsine (inverse sine) in radians as a float.
		The argument must be in the range [-1, 1].`),
	libutil.FunctionDoc("asinh", lisp.Formals("radians"), builtinAsinh,
		`Returns the inverse hyperbolic sine as a float.`),
	libutil.FunctionDoc("cos", lisp.Formals("radians"), builtinCos,
		`Returns the cosine of radians as a float.`),
	libutil.FunctionDoc("cosh", lisp.Formals("radians"), builtinCosh,
		`Returns the hyperbolic cosine of radians as a float.`),
	libutil.FunctionDoc("acos", lisp.Formals("radians"), builtinAcos,
		`Returns the arccosine (inverse cosine) in radians as a float.
		The argument must be in the range [-1, 1].`),
	libutil.FunctionDoc("acosh", lisp.Formals("radians"), builtinAcosh,
		`Returns the inverse hyperbolic cosine as a float. The argument
		must be >= 1.`),
	libutil.FunctionDoc("tan", lisp.Formals("radians"), builtinTan,
		`Returns the tangent of radians as a float.`),
	libutil.FunctionDoc("tanh", lisp.Formals("radians"), builtinTanh,
		`Returns the hyperbolic tangent of radians as a float.`),
	libutil.FunctionDoc("atan", lisp.Formals("radians", lisp.OptArgSymbol, "quotient"), builtinAtan,
		`Returns the arctangent as a float. With one argument, returns
		atan(radians). With two arguments, returns atan2(radians,
		quotient), computing the angle in the correct quadrant.`),
	libutil.FunctionDoc("atanh", lisp.Formals("radians"), builtinAtanh,
		`Returns the inverse hyperbolic tangent as a float. The argument
		must be in the range (-1, 1).`),
}

func builtinIsNaN(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	switch x.Type {
	case lisp.LInt:
		return lisp.Bool(false)
	case lisp.LFloat:
		return lisp.Bool(math.IsNaN(x.Float))
	default:
		// Umm.. This is a strange error message.  Should this be true??
		return env.Errorf("argument is not a number: %v", x.Type)
	}
}

func builtinAbs(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	switch x.Type {
	case lisp.LFloat:
		return lisp.Float(math.Abs(x.Float))
	case lisp.LInt:
		if x.Int > 0 {
			return x
		}
		abs := -x.Int
		if abs < 0 {
			return env.Errorf("integer overflow: absolute value overflows int")
		}
		return lisp.Int(abs)
	default:
		return env.Errorf("argument is not a number: %v", x.Type)
	}
}

func builtinCeil(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	if x.Type == lisp.LInt {
		return x
	}
	return lisp.Float(math.Ceil(x.Float))
}

func builtinFloor(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	if x.Type == lisp.LInt {
		return x
	}
	return lisp.Float(math.Floor(x.Float))
}

func builtinLog(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	b, x := args.Cells[0], args.Cells[1]
	if !b.IsNumeric() {
		return env.Errorf("argument is not a number: %v", b.Type)
	}
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	return lisp.Float(math.Log(toFloat(x)) / math.Log(toFloat(b)))
}

var builtinSqrt = realFunc(math.Sqrt).builtin

var builtinExp = realFunc(math.Exp).builtin

var builtinLn = realFunc(math.Log).builtin

var builtinSin = realFunc(math.Sin).builtin

var builtinSinh = realFunc(math.Sinh).builtin

var builtinAsin = realFunc(math.Asin).builtin

var builtinAsinh = realFunc(math.Asinh).builtin

var builtinCos = realFunc(math.Cos).builtin

var builtinCosh = realFunc(math.Cosh).builtin

var builtinAcos = realFunc(math.Acos).builtin

var builtinAcosh = realFunc(math.Acosh).builtin

var builtinTan = realFunc(math.Tan).builtin

var builtinTanh = realFunc(math.Tanh).builtin

// builtinAtan does not have the same signature as other trigonometric
// functions and must be implemented specially.
func builtinAtan(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x, q := args.Cells[0], args.Cells[1]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	if q.IsNil() {
		return lisp.Float(math.Atan(toFloat(x)))
	}
	if !q.IsNumeric() {
		return env.Errorf("second argument is not a number: %v", q.Type)
	}
	return lisp.Float(math.Atan2(toFloat(x), toFloat(q)))
}

var builtinAtanh = realFunc(math.Atanh).builtin

// realFunc is a function of the real number line (potentially with special
// cases for NaN and -Inf/+Inf)
type realFunc func(float64) float64

func (fn realFunc) builtin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	x := args.Cells[0]
	if !x.IsNumeric() {
		return env.Errorf("argument is not a number: %v", x.Type)
	}
	y := fn(toFloat(x))
	return lisp.Float(y)
}

func toFloat(x *lisp.LVal) float64 {
	if x.Type == lisp.LFloat {
		return x.Float
	}
	return float64(x.Int)
}
