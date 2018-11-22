// Copyright © 2018 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"reflect"
	"sort"
	"strconv"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

func DefaultSerializer() *Serializer {
	return &Serializer{
		Null: lisp.Symbol("json:null"),
	}
}

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "json"

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
	env.PutGlobal(lisp.Symbol("null"), lisp.Symbol("json:null"))
	s := DefaultSerializer()
	for _, fn := range Builtins(s) {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Builtins takes the default serializer for a lisp environment and returns a
// set of package builtin functions that use it.
func Builtins(s *Serializer) []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("message-bytes", lisp.Formals("json-message"), s.MessageBytesBuiltin),
		libutil.Function("dump-message", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpMessageBuiltin),
		libutil.Function("load-message", lisp.Formals("json-message", lisp.KeyArgSymbol, "string-numbers"), s.LoadMessageBuiltin),
		libutil.Function("dump-bytes", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpBytesBuiltin),
		libutil.Function("load-bytes", lisp.Formals("json-bytes", lisp.KeyArgSymbol, "string-numbers"), s.LoadBytesBuiltin),
		libutil.Function("dump-string", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpStringBuiltin),
		libutil.Function("load-string", lisp.Formals("json-string", lisp.KeyArgSymbol, "string-numbers"), s.LoadStringBuiltin),
		libutil.Function("use-string-numbers", lisp.Formals("bool"), s.UseStringNumbersBuiltin),
	}
}

// Dump serializes the structure of v as a JSON formatted byte slice.
func Dump(v *lisp.LVal, stringNums bool) ([]byte, error) {
	return DefaultSerializer().Dump(v, stringNums)
}

// Load parses b as JSON and returns an equivalent LVal.
func Load(b []byte, stringNums bool) *lisp.LVal {
	return DefaultSerializer().Load(b, stringNums)
}

// Serializer defines JSON serialization rules for lisp values.
type Serializer struct {
	UseStringNumbers bool
	True             *lisp.LVal
	False            *lisp.LVal
	Null             *lisp.LVal
}

// Load parses b and returns an LVal representing its structure.
func (s *Serializer) Load(b []byte, stringNums bool) *lisp.LVal {
	var x interface{}
	err := s.jsonDecode(b, &x, stringNums)
	switch err.(type) {
	case nil:
		break
	case *json.SyntaxError:
		lerr := lisp.Error(err)
		lerr.Str = "json:syntax-error"
		return lerr
	default:
		return lisp.Error(err)
	}
	return s.loadInterface(x)
}

func (s *Serializer) jsonDecode(b []byte, dst interface{}, stringNums bool) error {
	if !stringNums {
		return json.Unmarshal(b, dst)
	}
	d := json.NewDecoder(bytes.NewReader(b))
	d.UseNumber()
	err := d.Decode(dst)
	// Check for trailing bytes in b.  RawMessage is not ideal because it
	// performs allocation when there is another object in b, but its OK for
	// now.
	var rest json.RawMessage
	if d.Decode(&rest) != io.EOF {
		return fmt.Errorf("not a valid json object")
	}
	return err
}

func (s *Serializer) loadInterface(x interface{}) *lisp.LVal {
	if x == nil {
		return lisp.Nil()
	}
	switch x := x.(type) {
	case bool:
		return lisp.Bool(x)
	case string:
		return lisp.String(x)
	case float64:
		return lisp.Float(x)
	case json.Number:
		// This can only show up if stringNums was true.
		return lisp.String(string(x))
	case map[string]interface{}:
		m := lisp.SortedMap()
		for k, v := range x {
			err := m.MapSet(k, s.loadInterface(v))
			if err.Type == lisp.LError {
				return err
			}
		}
		return m
	case []interface{}:
		lis := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(len(x))}), nil)
		cells := lis.Cells[1].Cells
		for i, v := range x {
			cells[i] = s.loadInterface(v)
			if cells[i].Type == lisp.LError {
				return cells[i]
			}
		}
		return lis
	default:
		return lisp.Errorf("unable to load json type: %T", x)
	}
}

func (s *Serializer) attachStack(env *lisp.LEnv, lerr *lisp.LVal) *lisp.LVal {
	if lerr.Type != lisp.LError {
		return lerr
	}
	lerr.SetCallStack(env.Runtime.Stack.Copy())
	return lerr
}

func (s *Serializer) UseStringNumbersBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	confirm := args.Cells[0]
	s.UseStringNumbers = lisp.True(confirm)
	return lisp.Nil()
}

func (s *Serializer) useStringNumbers(env *lisp.LEnv) *lisp.LVal {
	return lisp.Bool(s.UseStringNumbers)
}

// Dump serializes v as JSON and returns any error.
func (s *Serializer) Dump(v *lisp.LVal, stringNums bool) ([]byte, error) {
	m := s.GoValue(v, stringNums)
	_, badnews := m.(*lisp.LVal)
	if badnews {
		return nil, fmt.Errorf("type cannot be converted to json: %v", v.Type)
	}
	return json.Marshal(m)
}

func (s *Serializer) MessageBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lmsg := args.Cells[0]
	if lmsg.Type != lisp.LNative {
		return env.Errorf("argument is not a raw json-message: %v", lmsg.Type)
	}
	msg, ok := lmsg.Native.(*json.RawMessage)
	if !ok {
		return env.Errorf("argument is not a raw json-message: %v", msg)
	}
	return lisp.Bytes([]byte(*msg))
}

func (s *Serializer) DumpMessageBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	val := s.DumpBytesBuiltin(env, args)
	if val.Type == lisp.LError {
		return val
	}
	if val.Type != lisp.LBytes {
		panic("unexpected lval: " + val.Type.String())
	}
	b := val.Bytes()
	msg := (*json.RawMessage)(&b)
	var _ json.Marshaler = msg
	return lisp.Native(msg)
}

func (s *Serializer) DumpBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj, stringNums := args.Cells[0], args.Cells[1]
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return stringNums
		}
	}
	b, err := s.Dump(obj, lisp.True(stringNums))
	if err != nil {
		return env.Error(err)
	}
	return lisp.Bytes(b)
}

func (s *Serializer) LoadMessageBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lmsg, stringNums := args.Cells[0], args.Cells[1]
	if lmsg.Type != lisp.LNative {
		return env.Errorf("argument is not a raw json-message: %v", lmsg.Type)
	}
	msg, ok := lmsg.Native.(*json.RawMessage)
	if !ok {
		return env.Errorf("argument is not a raw json-message: %v", msg)
	}
	return s.LoadBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{lisp.Bytes([]byte(*msg)), stringNums}))
}

func (s *Serializer) LoadBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js, stringNums := args.Cells[0], args.Cells[1]
	if js.Type != lisp.LBytes {
		return env.Errorf("argument is not bytes: %v", js.Type)
	}
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return stringNums
		}
	}
	return s.attachStack(env, s.Load(js.Bytes(), lisp.True(stringNums)))
}

func (s *Serializer) DumpStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj, stringNums := args.Cells[0], args.Cells[1]
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return stringNums
		}
	}
	b, err := s.Dump(obj, lisp.True(stringNums))
	if err != nil {
		return env.Error(err)
	}
	return lisp.String(string(b))
}

func (s *Serializer) LoadStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js, stringNums := args.Cells[0], args.Cells[1]
	if js.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", js.Type)
	}
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return stringNums
		}
	}
	return s.attachStack(env, s.Load([]byte(js.Str), lisp.True(stringNums)))
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
func (s *Serializer) GoValue(v *lisp.LVal, stringNums bool) interface{} {
	if v.IsNil() {
		return nil
	}
	switch v.Type {
	case lisp.LError:
		return (error)((*lisp.ErrorVal)(v))
	case lisp.LSymbol, lisp.LString:
		if v.Type == lisp.LSymbol {
			switch v.Str {
			case lisp.TrueSymbol:
				return true
			case lisp.FalseSymbol:
				return false
			case s.Null.Str:
				return nil
			}
		}
		return v.Str
	case lisp.LBytes:
		return v.Bytes
	case lisp.LInt:
		if stringNums {
			return strconv.Itoa(v.Int)
		}
		return v.Int
	case lisp.LFloat:
		if stringNums {
			return strconv.FormatFloat(v.Float, 'g', -1, 64)
		}
		return v.Float
	case lisp.LNative:
		return v.Native
	case lisp.LQuote:
		return s.GoValue(v.Cells[0], stringNums)
	case lisp.LSExpr:
		s, _ := s.GoSlice(v, stringNums)
		return s
	case lisp.LArray:
		s, _ := s.GoSlice(v.Cells[1], stringNums)
		switch v.Cells[0].Len() {
		case 0:
			return s[0]
		case 1:
			return s
		default:
			return fmt.Errorf("cannot serialize array with dimensions: %v", v.Cells[0])
		}
	case lisp.LSortMap:
		m, _ := s.GoMap(v, stringNums)
		return m
	}
	return v
}

// GoError returns an error that represents v.  If v is not LError then nil is
// returned.
func (s *Serializer) GoError(v *lisp.LVal) error {
	if v.Type != lisp.LError {
		return nil
	}
	return (*lisp.ErrorVal)(v)
}

// GoString returns the string that v represents and the value true.  If v does
// not represent a string GoString returns a false second argument
func (s *Serializer) GoString(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LString {
		return "", false
	}
	return v.Str, true
}

// SymbolName returns the name of the symbol that v represents and the value
// true.  If v does not represent a symbol SymbolName returns a false second
// argument
func (s *Serializer) SymbolName(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LSymbol {
		return "", false
	}
	return v.Str, true
}

// GoInt converts the numeric value that v represents to and int and returns it
// with the value true.  If v does not represent a number GoInt returns a
// false second argument
func (s *Serializer) GoInt(v *lisp.LVal) (int, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return int(v.Float), true
	}
	return v.Int, true
}

// GoFloat64 converts the numeric value that v represents to a float64 and
// returns it with the value true.  If v does not represent a number GoFloat64
// returns a false second argument
func (s *Serializer) GoFloat64(v *lisp.LVal) (float64, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return v.Float, true
	}
	return float64(v.Int), true
}

// GoSlice returns the string that v represents and the value true.  If v does
// not represent a string GoSlice returns a false second argument
func (s *Serializer) GoSlice(v *lisp.LVal, stringNums bool) ([]interface{}, bool) {
	if v.Type != lisp.LSExpr {
		return nil, false
	}
	vs := make([]interface{}, len(v.Cells))
	for i := range vs {
		vs[i] = s.GoValue(v.Cells[i], stringNums)
	}
	return vs, true
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map json serializable map GoMap
// returns a false second argument
func (s *Serializer) GoMap(v *lisp.LVal, stringNums bool) (SortedMap, bool) {
	if v.Type != lisp.LSortMap {
		return nil, false
	}
	lmap := v.Map()
	m := make(SortedMap, len(lmap))
	for k, vlisp := range lmap {
		vgo := s.GoValue(vlisp, stringNums)
		kreflect := reflect.ValueOf(k)
		// This is really shitty
		switch kreflect.Kind() {
		case reflect.String:
			m[kreflect.String()] = vgo
		default:
			return nil, false
		}
	}
	return m, true
}

type SortedMap map[string]interface{}

func (m SortedMap) MarshalJSON() ([]byte, error) {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	var buf bytes.Buffer
	buf.WriteString("{")
	for i, k := range keys {
		b, err := json.Marshal(k)
		if err != nil {
			return nil, err
		}
		buf.Write(b)
		buf.WriteString(":")
		b, err = json.Marshal(m[k])
		if err != nil {
			log.Printf("bad value: %#v", m[k])
			return nil, err
		}
		buf.Write(b)
		if i < len(keys)-1 {
			buf.WriteString(",")
		}
	}
	buf.WriteString("}")
	return buf.Bytes(), nil
}
