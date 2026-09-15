// Copyright © 2026 The ELPS authors

package libgolang_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libgolang"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

type embeddedFields struct {
	X int
}

type embeddedMid struct {
	*embeddedFields
}

type embeddedOuter struct {
	*embeddedMid
}

func reflectionEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	require.NotEqual(t, lisp.LError, libgolang.LoadPackage(env).Type)
	return env
}

func TestStructFieldEmbeddedPointer(t *testing.T) {
	for _, tc := range []struct {
		payload any
		name    string
		wantErr bool
	}{
		{embeddedMid{}, "nil-value", true},
		{&embeddedMid{}, "nil-pointer", true},
		{embeddedOuter{}, "nil-two-level-outer", true},
		{embeddedOuter{&embeddedMid{}}, "nil-two-level-inner", true},
		{&embeddedOuter{&embeddedMid{}}, "nil-two-level-pointer", true},
		{embeddedMid{&embeddedFields{42}}, "non-nil-value", false},
		{&embeddedMid{&embeddedFields{42}}, "non-nil-pointer", false},
		{embeddedOuter{&embeddedMid{&embeddedFields{42}}}, "non-nil-two-level", false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := reflectionEnv(t)
			env.PutGlobal(lisp.Symbol("nv"), lisp.Native(tc.payload))
			for _, field := range []string{`"X"`, `'X`} {
				got := env.LoadString("embedded.lisp", `(golang:struct-field nv `+field+`)`)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				if tc.wantErr {
					require.Equal(t, lisp.LError, got.Type, "%v", got)
					require.Equal(t, "field X is reached through a nil embedded pointer", (*lisp.ErrorVal)(got).ErrorMessage())
				} else {
					require.Equal(t, lisp.LNative, got.Type, "%v", got)
					require.Equal(t, 42, got.Native)
				}
			}
			if tc.wantErr {
				got := env.LoadString("embedded.lisp", `(handler-bind ((condition (lambda (&rest args) "handled"))) (golang:struct-field nv "X"))`)
				require.Equal(t, lisp.LString, got.Type, "%v", got)
				require.Equal(t, "handled", got.Str)
			}
		})
	}
}

func TestBuiltinStringRequiresNative(t *testing.T) {
	env := reflectionEnv(t)
	for _, expr := range []string{`(golang:string "text")`, `(golang:string (to-bytes "text"))`} {
		got := env.LoadString("string.lisp", expr)
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		require.False(t, lisp.IsInternalPanic(got), "%v", got)
		require.Contains(t, (*lisp.ErrorVal)(got).ErrorMessage(), "first argument is not a go string:")
	}
}

// A payload assertion alone must not admit a value with the wrong Lisp type,
// even when the host has supplied string storage in its Native slot.
func TestBuiltinStringRejectsNonNativePayload(t *testing.T) {
	env := reflectionEnv(t)
	v := &lisp.LVal{Type: lisp.LString, Str: "text", Native: "payload"}
	got := libgolang.BuiltinString(env, lisp.SExpr([]*lisp.LVal{v}))
	require.Equal(t, lisp.LError, got.Type, "%v", got)
}

func TestStructFieldValidation(t *testing.T) {
	env := reflectionEnv(t)
	for _, tc := range []struct {
		value *lisp.LVal
		field string
		want  string
	}{
		{lisp.Native(nil), `"X"`, "first argument is not a go struct"},
		{lisp.Native((*embeddedFields)(nil)), `"X"`, "first argument is not a go struct"},
		{lisp.Native(42), `"X"`, "first argument is not a go struct"},
		{lisp.Int(42), `"X"`, "first argument is not a go struct"},
		{lisp.Native(embeddedFields{}), `"Missing"`, "struct has no field: Missing"},
		{lisp.Native(struct{ hidden int }{}), `"hidden"`, "cannot access unexported field name: hidden"},
		{lisp.Native(embeddedFields{}), `0`, "second argument is not a string or a symbol"},
	} {
		env.PutGlobal(lisp.Symbol("nv"), tc.value)
		got := env.LoadString("validation.lisp", `(golang:struct-field nv `+tc.field+`)`)
		require.False(t, lisp.IsInternalPanic(got), "%v", got)
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		require.Contains(t, (*lisp.ErrorVal)(got).ErrorMessage(), tc.want)
	}

	// A nil pointer at the end of the path is a valid field value.
	type leaf struct{ X *int }
	env.PutGlobal(lisp.Symbol("nv"), lisp.Native(struct{ *leaf }{&leaf{}}))
	got := env.LoadString("validation.lisp", `(golang:struct-field nv "X")`)
	require.Equal(t, lisp.LNative, got.Type, "%v", got)
	require.Equal(t, (*int)(nil), got.Native)
}

// Extend the registry sweep's evaluator/IsInternalPanic approach with native
// shapes here, keeping the extra cross product confined to Go interop.
func TestGoInteropNativeShapesNeverPanic(t *testing.T) {
	type namedString string
	type namedInt int64
	type namedFloat float32
	var ptr *embeddedFields
	payloads := []any{
		nil, ptr, &ptr, embeddedFields{42}, &embeddedFields{42},
		embeddedMid{}, &embeddedMid{}, embeddedOuter{&embeddedMid{}},
		embeddedOuter{&embeddedMid{&embeddedFields{42}}},
		struct{ X *int }{}, struct{ hidden int }{},
		"text", namedString("text"), int(42), namedInt(42), uint64(42),
		float64(1.5), namedFloat(1.5), true, complex(1, 2), uintptr(0),
		[]int(nil), []int{1}, [1]int{1}, map[string]int(nil),
		(chan int)(nil), (func())(nil),
	}
	env := reflectionEnv(t)
	pkg := env.Runtime.Registry.Package("golang")
	var checked int
	for _, name := range pkg.SymbolNames() {
		fn, _ := pkg.Symbol(name)
		if fn.Type != lisp.LFun {
			continue
		}
		checked++
		t.Run(name, func(t *testing.T) {
			for i, payload := range payloads {
				for _, field := range []*lisp.LVal{lisp.String("X"), lisp.Symbol("X"), lisp.String("Missing"), lisp.String("hidden"), lisp.String(""), lisp.String("\xff"), lisp.Int(0)} {
					args := []*lisp.LVal{lisp.Symbol("golang:" + name), lisp.Native(payload)}
					if name == "struct-field" {
						args = append(args, lisp.Quote(field))
					}
					got := env.Eval(lisp.SExpr(args))
					require.False(t, lisp.IsInternalPanic(got), "payload %d (%T), field %v: %v", i, payload, field, got)
				}
			}
		})
	}
	require.Equal(t, 4, checked, "sweep must cover every Go interop builtin")
	for _, tc := range []struct {
		payload any
		name    string
		want    string
	}{
		{namedString("text"), "string", `"text"`},
		{namedInt(42), "int", "42"},
		{uint64(42), "int", "42"},
		{namedFloat(1.5), "float", "1.5"},
	} {
		got := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("golang:" + tc.name), lisp.Native(tc.payload)}))
		require.Equal(t, tc.want, got.String(), "successful reflection conversion for %T", tc.payload)
	}
}
