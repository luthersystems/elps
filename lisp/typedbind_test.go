// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// handWritten2 is handWritten's first two checks: a string and a map.
func handWritten2(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s, m := args.Cells[0], args.Cells[1]
	if s.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", s.Type)
	}
	if m.Type != lisp.LSortMap {
		return env.Errorf("second argument is not a map: %s", m.Type)
	}
	return lisp.QExpr([]*lisp.LVal{lisp.String(s.Str), lisp.Int(m.Len())})
}

var typedTwo = lisp.Func2(
	lisp.StringArg("first argument"),
	lisp.TypedArg(lisp.LSortMap, "second argument is not a map: %s"),
	func(_ *lisp.LEnv, s string, m *lisp.LVal) *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{lisp.String(s), lisp.Int(m.Len())})
	})

func TestFuncMatchesHandWritten(t *testing.T) {
	env := newLimitTestEnv(t)
	env.AddBuiltins(false,
		&testBuiltinDef{name: "hand", formals: lisp.Formals("s", "m"), fn: handWritten2},
		&testBuiltinDef{name: "typed", formals: lisp.Formals("s", "m"), fn: typedTwo})
	for _, al := range []string{
		`"s" (sorted-map "a" 1)`,
		`"s" (sorted-map)`,
		`1 (sorted-map)`,
		`"s" 2`,
		`"s" ()`,
		`1 2`,
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

func TestFuncSmallArities(t *testing.T) {
	env := newLimitTestEnv(t)
	one := lisp.Func1(lisp.StringArg("argument"), func(_ *lisp.LEnv, s string) *lisp.LVal { return lisp.String(s + "!") })
	two := lisp.Func2(lisp.ValueArg(), lisp.TypedArg(lisp.LInt, "step is not an integer: %v"), func(_ *lisp.LEnv, v, s *lisp.LVal) *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{v, s})
	})
	for _, tc := range []struct {
		fn   lisp.LBuiltin
		args []*lisp.LVal
		want string
	}{
		{one, []*lisp.LVal{lisp.String("x")}, `"x!"`},
		{one, []*lisp.LVal{lisp.Int(1)}, "argument is not a string: int"},
		{two, []*lisp.LVal{lisp.Int(1), lisp.Int(2)}, "'(1 2)"},
		{two, []*lisp.LVal{lisp.Int(1), lisp.Float(2)}, "step is not an integer: float"},
		{two, []*lisp.LVal{lisp.Int(1)}, "missing required argument 1: this builtin reads at least 2 argument(s) but was bound to formals declaring only 1"},
	} {
		got := tc.fn(env, lisp.QExpr(tc.args))
		if got.Type == lisp.LError {
			assert.Equal(t, tc.want, (*lisp.ErrorVal)(got).ErrorMessage())
		} else {
			assert.Equal(t, tc.want, got.String())
		}
	}
}

func TestFuncNoAlloc(t *testing.T) {
	env := newLimitTestEnv(t)
	result := lisp.Int(0)
	fn := lisp.Func2(lisp.StringArg("first argument"), lisp.ValueArg(),
		func(_ *lisp.LEnv, s string, _ *lisp.LVal) *lisp.LVal { return result })
	args := lisp.QExpr([]*lisp.LVal{lisp.String("s"), lisp.Nil()})
	assert.Zero(t, testing.AllocsPerRun(100, func() {
		if fn(env, args) != result {
			t.Fatal("unexpected result")
		}
	}))
}
