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
	libutil.Function("nan?", lisp.Formals("number"), builtinIsNaN),
	libutil.Function("abs", lisp.Formals("number"), builtinAbs),
	libutil.Function("ceil", lisp.Formals("number"), builtinCeil),
	libutil.Function("floor", lisp.Formals("number"), builtinFloor),
	libutil.Function("sqrt", lisp.Formals("number"), builtinSqrt),
	libutil.Function("exp", lisp.Formals("number"), builtinExp),
	libutil.Function("ln", lisp.Formals("number"), builtinLn),
	libutil.Function("log", lisp.Formals("base", "number"), builtinLog),
	libutil.Function("sin", lisp.Formals("radians"), builtinSin),
	libutil.Function("sinh", lisp.Formals("radians"), builtinSinh),
	libutil.Function("asin", lisp.Formals("radians"), builtinAsin),
	libutil.Function("asinh", lisp.Formals("radians"), builtinAsinh),
	libutil.Function("cos", lisp.Formals("radians"), builtinCos),
	libutil.Function("cosh", lisp.Formals("radians"), builtinCosh),
	libutil.Function("acos", lisp.Formals("radians"), builtinAcos),
	libutil.Function("acosh", lisp.Formals("radians"), builtinAcosh),
	libutil.Function("tan", lisp.Formals("radians"), builtinTan),
	libutil.Function("tanh", lisp.Formals("radians"), builtinTanh),
	libutil.Function("atan", lisp.Formals("radians", lisp.OptArgSymbol, "quotient"), builtinAtan),
	libutil.Function("atanh", lisp.Formals("radians"), builtinAtanh),
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
