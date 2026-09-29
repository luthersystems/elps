// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// typedFour is handWritten's first three checks plus its name key, as a
// Func4 builtin.
var typedFour = lisp.Func4(
	lisp.StringArg("first argument"),
	lisp.TypedArg(lisp.LSortMap, "second argument is not a map: %s"),
	lisp.IntArg("third argument"),
	lisp.OptStringArg("name", "default"),
	func(_ *lisp.LEnv, s string, m *lisp.LVal, n int, name string) *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{lisp.String(s), lisp.Int(m.Len()), lisp.Int(n), lisp.String(name), lisp.Int(10)})
	})

func TestFuncNMatchesHandWritten(t *testing.T) {
	env := newLimitTestEnv(t)
	env.AddBuiltins(false,
		&testBuiltinDef{name: "hand", formals: lisp.Formals("s", "m", "n", lisp.KeyArgSymbol, "name", "count"), fn: handWritten},
		&testBuiltinDef{name: "typed", formals: lisp.Formals("s", "m", "n", lisp.KeyArgSymbol, "name"), fn: typedFour})
	for _, al := range []string{
		`"s" (sorted-map "a" 1) 3`,
		`"s" (sorted-map) 3 :name "n"`,
		`1 (sorted-map) 3`,
		`"s" 2 3`,
		`"s" () "3"`,
		`"s" (sorted-map) "3"`,
		`"s" (sorted-map) 3 :name 5`,
		`1 2 3 :name 4`,
	} {
		want, ws := stepsOf(t, env, "(hand "+al+")")
		got, gs := stepsOf(t, env, "(typed "+al+")")
		require.Equal(t, want.Type, got.Type, al)
		assert.Equal(t, ws, gs, al)
		if want.Type == lisp.LError {
			assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), al)
		} else {
			assert.Equal(t, want.String(), got.String(), al)
		}
	}
}

func TestFuncNSmallArities(t *testing.T) {
	env := newLimitTestEnv(t)
	one := lisp.Func1(lisp.IntArg("argument"), func(_ *lisp.LEnv, n int) *lisp.LVal { return lisp.Int(n + 1) })
	two := lisp.Func2(lisp.ValueArg(), lisp.OptIntArg("step", 1), func(_ *lisp.LEnv, v *lisp.LVal, s int) *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{v, lisp.Int(s)})
	})
	three := lisp.Func3(lisp.MapArg("first argument"), lisp.OptArg(), lisp.StringArg("third argument"),
		func(_ *lisp.LEnv, m, o *lisp.LVal, s string) *lisp.LVal { return lisp.String(s) })
	for _, tc := range []struct {
		fn   lisp.LBuiltin
		args []*lisp.LVal
		want string
	}{
		{one, []*lisp.LVal{lisp.Int(1)}, "2"},
		{one, []*lisp.LVal{lisp.String("x")}, "argument is not an integer: string"},
		{two, []*lisp.LVal{lisp.Int(1), lisp.Nil()}, "'(1 1)"},
		{two, []*lisp.LVal{lisp.Int(1), lisp.Float(2)}, "step is not an integer: float"},
		{three, []*lisp.LVal{lisp.SortedMap(), lisp.Nil(), lisp.String("s")}, `"s"`},
		{three, []*lisp.LVal{lisp.Int(1), lisp.Nil(), lisp.Int(1)}, "first argument is not a map: int"},
		{three, []*lisp.LVal{lisp.SortedMap(), lisp.Nil()}, "missing required argument 2: this builtin reads at least 3 argument(s) but was bound to formals declaring only 2"},
	} {
		got := tc.fn(env, lisp.QExpr(tc.args))
		if got.Type == lisp.LError {
			assert.Equal(t, tc.want, (*lisp.ErrorVal)(got).ErrorMessage())
		} else {
			assert.Equal(t, tc.want, got.String())
		}
	}
}

func TestFuncNNoAlloc(t *testing.T) {
	env := newLimitTestEnv(t)
	result := lisp.Int(0)
	fn := lisp.Func2(lisp.StringArg("first argument"), lisp.OptIntArg("n", 3),
		func(_ *lisp.LEnv, s string, n int) *lisp.LVal { return result })
	args := lisp.QExpr([]*lisp.LVal{lisp.String("s"), lisp.Nil()})
	assert.Zero(t, testing.AllocsPerRun(100, func() {
		if fn(env, args) != result {
			t.Fatal("unexpected result")
		}
	}))
}

func TestFuncNExtendedDecoders(t *testing.T) {
	env := newLimitTestEnv(t)
	fn := lisp.Func4(lisp.StringOrSymbolArg("a: %v"), lisp.BytesArg("b: %v"),
		lisp.OneOfArg("c: %v", lisp.LInt, lisp.LFloat), lisp.ReqKeyArg("d is required"),
		func(_ *lisp.LEnv, a string, b []byte, c, d *lisp.LVal) *lisp.LVal {
			return lisp.String(a + string(b) + c.String() + d.String())
		})
	ok := fn(env, lisp.QExpr([]*lisp.LVal{lisp.Symbol("x"), lisp.String("y"), lisp.Float(1.5), lisp.Int(2)}))
	assert.Equal(t, `"xy1.52"`, ok.String())
	bad := fn(env, lisp.QExpr([]*lisp.LVal{lisp.Symbol("x"), lisp.String("y"), lisp.Float(1.5), lisp.Nil()}))
	require.Equal(t, lisp.LError, bad.Type)
	assert.Equal(t, "d is required", (*lisp.ErrorVal)(bad).ErrorMessage())
	g := lisp.Func2(lisp.StringArgf("s: %v"), lisp.IntArgf("n: %v"), func(_ *lisp.LEnv, s string, n int) *lisp.LVal { return lisp.Int(n) })
	bad = g(env, lisp.QExpr([]*lisp.LVal{lisp.String("s"), lisp.String("x")}))
	assert.Equal(t, "n: string", (*lisp.ErrorVal)(bad).ErrorMessage())
}
