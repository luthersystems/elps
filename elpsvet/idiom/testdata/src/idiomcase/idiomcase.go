package idiomcase

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

var (
	coreKeys = lisp.BuiltinFunc("keys")
	coreGet  = lisp.BuiltinFunc("get")
)

func isErr(v *lisp.LVal) bool {
	return v.Type == lisp.LError // want `use v.IsError\(\), which is the same compare`
}

func notErr(v *lisp.LVal) bool {
	return lisp.LError != v.Type // want `use !v.IsError\(\), which is the same compare`
}

func otherType(v *lisp.LVal) bool {
	return v.Type == lisp.LString
}

func list(a, b *lisp.LVal) *lisp.LVal {
	return lisp.QExpr([]*lisp.LVal{ // want `use lisp.Cells\{...\}.List\(\), which builds the same list`
		a, b,
	})
}

func listVar(cells []*lisp.LVal) *lisp.LVal {
	return lisp.QExpr(cells)
}

func alloc(env *lisp.LEnv, n int) *lisp.LVal {
	if msg := env.Runtime.CheckAlloc(n); msg != "" { // want `use env.CheckAlloc, which returns the same error`
		return env.Errorf("%s", msg)
	}
	return lisp.Nil()
}

func allocPair(env *lisp.LEnv, n int) (*lisp.LVal, *lisp.LVal) {
	if msg := env.Runtime.CheckAlloc(n); msg != "" { // want `use env.CheckAlloc, which returns the same error`
		return nil, env.Errorf("%s", msg)
	}
	return nil, nil
}

// A different message is not the same error.
func allocOtherMessage(env *lisp.LEnv, n int) *lisp.LVal {
	if msg := env.Runtime.CheckAlloc(n); msg != "" {
		return env.Errorf("join: %s", msg)
	}
	return lisp.Nil()
}

func keysAfterCheck(m *lisp.LVal) int {
	n := 0
	if m.Type == lisp.LSortMap {
		for range m.MapKeys().Cells { // want `range m.Keys\(\) walks the keys without building a list`
			n++
		}
	}
	return n
}

// Without a map check, Keys would hide MapKeys' panic: no hint.
func keysNoCheck(m *lisp.LVal) int {
	n := 0
	for range m.MapKeys().Cells {
		n++
	}
	return n
}

func field(desc *lisp.LVal) bool {
	in := false
	if desc.Type == lisp.LSortMap {
		if s := desc.MapGetString("status"); s.Type == lisp.LString && s.Str == "ok" { // want `lisp.Field\[string\]\(desc, "status"\) reads a string field`
			in = true
		}
	}
	return in
}

func argCells(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0] // want `an ArgReader read`
	if v.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", v.Type)
	}
	return v
}

func errorValResult() *lisp.ErrorVal { // want `a \*lisp.ErrorVal result is a typed nil`
	return nil
}

func resultAsVarying(env *lisp.LEnv, m, k *lisp.LVal) {
	_, _ = lisp.ResultAs[string](env.CallBuiltin(coreGet, m, k))     // want `the result type of get varies with the data` `use env.MapLookup`
	_, _ = lisp.ResultAs[string](env.MapLookup(m, k))                // want `the result type of get varies with the data`
	_, _ = lisp.ResultAs[*lisp.LVal](env.CallBuiltin(coreGet, m, k)) // want `use env.MapLookup`
	_, _ = lisp.ResultAs[lisp.Cells](env.CallBuiltin(coreKeys, m))
}

var builtinPad = lisp.Func2E(func(env *lisp.LEnv, s string, n int) (string, error) {
	return s, nil
})

func register() []*lisp.LVal {
	return []*lisp.LVal{
		lisp.FunInPackage("p", "pad", lisp.Formals("s", "n"), builtinPad),
		lisp.FunInPackage("p", "pad1", lisp.Formals("s"), builtinPad),                         // want `builtinPad takes exactly 2 arguments, but its formals declare 1`
		lisp.FunInPackage("p", "pad2", lisp.Formals("s", lisp.OptArgSymbol, "n"), builtinPad), // want `builtinPad is a Func\*E builtin, which takes 2 required arguments only`
		lisp.FunInPackage("p", "one", lisp.Formals("a", "b"), lisp.Func1E(func(env *lisp.LEnv, s string) (string, error) { // want `takes exactly 1 arguments, but its formals declare 2`
			return s, nil
		})),
	}
}

var builtinWrap = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	keys, err := lisp.Result(env.CallBuiltin(coreKeys, args))
	if err != nil {
		return nil, fmt.Errorf("keys: %w", err) // want `fmt.Errorf over a Lisp error gives the error condition "error"`
	}
	if err := lisp.GoError(env.CheckAlloc(3)); err != nil {
		return nil, errors.New(err.Error()) // want `errors.New\(err.Error\(\)\) over a Lisp error drops its condition`
	}
	v, err := helper(env, keys)
	if err != nil {
		return nil, fmt.Errorf("helper: %v", err) // want `fmt.Errorf over a Lisp error`
	}
	return v, nil
})

// helper is scoped: builtinWrap returns its error.
func helper(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
	out, err := lisp.Result(env.CallBuiltin(coreKeys, v))
	if err != nil {
		return nil, fmt.Errorf("inner: %w", err) // want `fmt.Errorf over a Lisp error`
	}
	return out, nil
}

// Outside a FuncE body a wrap is not reported.
func notScoped(env *lisp.LEnv, v *lisp.LVal) error {
	_, err := lisp.Result(v)
	return fmt.Errorf("x: %w", err)
}

// A wrap of a Go error is fine.
var builtinGoErr = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	err := errors.New("plain")
	return nil, fmt.Errorf("context: %w", err)
})
