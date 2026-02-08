// Copyright © 2018 The ELPS authors

package libgolang

import (
	"reflect"
	"unicode"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "golang"

// LoadPackage adds the time package to env
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
	return lisp.Nil()
}

type builtin struct {
	formals *lisp.LVal
	fun     lisp.LBuiltin
	name    string
	docs    string
}

func (fun *builtin) Name() string {
	return fun.name
}

func (fun *builtin) Formals() *lisp.LVal {
	return fun.formals
}

func (fun *builtin) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return fun.fun(env, args)
}

func (fun *builtin) Docstring() string {
	return fun.docs
}

var builtins = []*builtin{
	{lisp.Formals("native-value"), BuiltinString, "string",
		`Extracts a Go string from a native value and returns it as an
		ELPS string. The native value must hold a Go string or a type
		with underlying kind string. Does not convert non-string types
		to strings (use to-string for that).`},
	{lisp.Formals("native-value"), BuiltinInt, "int",
		`Extracts a Go integer from a native value and returns it as an
		ELPS int. Accepts any native value with an underlying integer
		kind (signed or unsigned). Returns an error on overflow.`},
	{lisp.Formals("native-value"), BuiltinFloat, "float",
		`Extracts a Go float from a native value and returns it as an
		ELPS float. Accepts native values with underlying kind float32
		or float64.`},
	{lisp.Formals("native-struct", "field-name"), BuiltinStructField, "struct-field",
		`Reads an exported field from a native Go struct value using
		reflection. field-name is a string or symbol naming the field
		(must start with an uppercase letter). Returns the field value
		as a native value. Automatically dereferences pointers.`},
}

// BuiltinString returns a string held by the native value (it does not convert
// non-string values to strings).
func BuiltinString(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lval := args.Cells[0]
	s, ok := lval.Native.(string)
	if !ok {
		v := reflect.ValueOf(lval.Native)
		if v.Kind() == reflect.String {
			s = v.String()
		} else {
			return env.Errorf("first argument is not a go string")
		}
	}
	return lisp.String(s)
}

// BuiltinFloat returns a floating point number held by the native value.
// Any type with underlying kind float32 or float64 can be converted.
func BuiltinFloat(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lval := args.Cells[0]
	if lval.Type != lisp.LNative {
		return env.Errorf("first argument is not a go float: %v", lval.Type)
	}
	x, ok := lval.Native.(float64)
	if !ok {
		v := reflect.ValueOf(lval.Native)
		if v.Kind() == reflect.Float64 || v.Kind() == reflect.Float32 {
			x = v.Float()
		} else {
			return env.Errorf("first argument is not a go float: %T", lval.Native)
		}
	}
	return lisp.Float(x)
}

// BuiltinInt returns a integer held by the native value.  Any type with
// underlying integer kind (possibly unsigned) can be converted.  An error will
// be returned if any overflow occurs during conversion.
func BuiltinInt(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lval := args.Cells[0]
	if lval.Type != lisp.LNative {
		return env.Errorf("first argument is not a go integer: %v", lval.Type)
	}
	x, ok := lval.Native.(int)
	x64 := int64(x)
	if !ok {
		v := reflect.ValueOf(lval.Native)
		switch v.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			x64 = v.Int()
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			x64 = int64(v.Uint()) //nolint:gosec // overflow checked on next line
			if x64 < 0 {
				return env.Errorf("unsigned integer overflow")
			}
		default:
			return env.Errorf("first argument is not a go integer: %T", lval.Native)
		}
	}
	x = int(x64)
	if int64(x) != x64 {
		return env.Errorf("integer overflow")
	}
	return lisp.Int(x)
}

// BuiltinStructField reflects the first argument and returns the named struct
// field.
func BuiltinStructField(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s, field := args.Cells[0], args.Cells[1]
	if s.Type != lisp.LNative {
		return env.Errorf("first argument is not a go struct: %v", s.Type)
	}
	v := reflect.ValueOf(s.Native)
	v = derefPtr(v)
	if v.Kind() != reflect.Struct {
		return env.Errorf("first argument is not a go struct: %v", s)
	}
	if field.Type != lisp.LString && field.Type != lisp.LSymbol {
		return env.Errorf("second argument is not a string or a symbol: %v", field.Type)
	}
	if !nameIsExported(field.Str) {
		return env.Errorf("cannot access unexported field name: %v", field.Str)
	}
	x := v.FieldByName(field.Str)
	if !x.IsValid() {
		return env.Errorf("struct has no field: %v", field.Str)
	}
	if !x.CanInterface() {
		return env.Errorf("cannot return struct field: %v", field.Str)
	}
	return lisp.Native(x.Interface())
}

func nameIsExported(name string) bool {
	c, n := utf8.DecodeRuneInString(name)
	if c == utf8.RuneError && n == 1 {
		return false
	}
	if unicode.IsUpper(c) {
		return true
	}
	return false
}

func derefPtr(v reflect.Value) reflect.Value {
	if v.Kind() != reflect.Ptr {
		return v
	}
	if v.IsNil() {
		return v
	}
	return reflect.Indirect(v)
}
