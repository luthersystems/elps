// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The decoders are package variables and the builtins are built from them in
// package initialization, as a consumer package builds them.  The compiler
// cannot see which decoder a Func1 or Func2 closure calls, so this is the
// case where an indirect decoder call moves the ArgReader to the heap.
var (
	allocValue  = lisp.ValueArg()
	allocTyped  = lisp.TypedArg(lisp.LInt, "second argument is not an int: %v")
	allocString = lisp.StringArg("first argument")
	allocOpt    = lisp.OptArg()
	allocOptStr = lisp.OptStringArg("name", "none")
	allocOptInt = lisp.OptIntArg("count", 7)

	allocResult = lisp.Int(0)

	allocTyped1 = lisp.Func1(allocString, func(_ *lisp.LEnv, _ string) *lisp.LVal { return allocResult })
	allocTyped2 = lisp.Func2(allocValue, allocTyped, func(_ *lisp.LEnv, _, _ *lisp.LVal) *lisp.LVal { return allocResult })
	allocTypedS = lisp.Func2(allocString, allocOptStr, func(_ *lisp.LEnv, _, _ string) *lisp.LVal { return allocResult })
	allocTypedO = lisp.Func2(allocOpt, allocOptInt, func(_ *lisp.LEnv, _ *lisp.LVal, _ int) *lisp.LVal { return allocResult })
)

func allocHand1(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if args.Cells[0].Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", args.Cells[0].Type)
	}
	return allocResult
}

func allocHand2(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if args.Cells[1].Type != lisp.LInt {
		return env.Errorf("second argument is not an int: %v", args.Cells[1].Type)
	}
	return allocResult
}

func allocHandS(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if args.Cells[0].Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", args.Cells[0].Type)
	}
	if !args.Cells[1].IsNil() && args.Cells[1].Type != lisp.LString {
		return env.Errorf("name is not a string: %v", args.Cells[1].Type)
	}
	return allocResult
}

func allocHandO(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if !args.Cells[1].IsNil() && args.Cells[1].Type != lisp.LInt {
		return env.Errorf("count is not an integer: %v", args.Cells[1].Type)
	}
	return allocResult
}

// A typed builtin allocates exactly what the equivalent hand-written
// LBuiltin allocates.
func TestFuncAllocsMatchHandWritten(t *testing.T) {
	env := newLimitTestEnv(t)
	allocsOf := func(fn lisp.LBuiltin, args *lisp.LVal) float64 {
		return testing.AllocsPerRun(100, func() {
			if fn(env, args) != allocResult {
				t.Fatal("unexpected result")
			}
		})
	}
	for _, tc := range []struct {
		name        string
		hand, typed lisp.LBuiltin
		args        []*lisp.LVal
	}{
		{"Func1 StringArg", allocHand1, allocTyped1, []*lisp.LVal{lisp.String("s")}},
		{"Func2 ValueArg TypedArg", allocHand2, allocTyped2, []*lisp.LVal{lisp.Nil(), lisp.Int(1)}},
		{"Func2 StringArg OptStringArg", allocHandS, allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.String("n")}},
		{"Func2 StringArg OptStringArg absent", allocHandS, allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.Nil()}},
		{"Func2 OptArg OptIntArg", allocHandO, allocTypedO, []*lisp.LVal{lisp.Nil(), lisp.Int(3)}},
		{"Func2 OptArg OptIntArg absent", allocHandO, allocTypedO, []*lisp.LVal{lisp.Nil(), lisp.Nil()}},
	} {
		args := lisp.QExpr(tc.args)
		assert.Equal(t, int(allocsOf(tc.hand, args)), int(allocsOf(tc.typed, args)), tc.name)
	}
}

// The optional decoders return their default when the cell is nil and
// produce ArgReader's messages otherwise.
func TestFuncOptionalDecoders(t *testing.T) {
	env := newLimitTestEnv(t)
	for _, tc := range []struct {
		fn   lisp.LBuiltin
		args []*lisp.LVal
		want string
	}{
		{allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.String("n")}, ""},
		{allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.Nil()}, ""},
		{allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.Int(1)}, "name is not a string: int"},
		{allocTypedS, []*lisp.LVal{lisp.Int(1), lisp.Int(1)}, "first argument is not a string: int"},
		{allocTypedO, []*lisp.LVal{lisp.Nil(), lisp.Float(1)}, "count is not an integer: float"},
	} {
		got := tc.fn(env, lisp.QExpr(tc.args))
		if tc.want == "" {
			assert.Same(t, allocResult, got)
			continue
		}
		require.Equal(t, lisp.LError, got.Type, tc.want)
		assert.Equal(t, tc.want, (*lisp.ErrorVal)(got).ErrorMessage())
	}

	var gotStr string
	var gotInt int
	var gotOpt *lisp.LVal
	s := lisp.Func1(lisp.OptStringArg("name", "none"), func(_ *lisp.LEnv, v string) *lisp.LVal { gotStr = v; return allocResult })
	n := lisp.Func1(lisp.OptIntArg("count", 7), func(_ *lisp.LEnv, v int) *lisp.LVal { gotInt = v; return allocResult })
	o := lisp.Func1(lisp.OptArg(), func(_ *lisp.LEnv, v *lisp.LVal) *lisp.LVal { gotOpt = v; return allocResult })
	s(env, lisp.QExpr([]*lisp.LVal{lisp.Nil()}))
	assert.Equal(t, "none", gotStr)
	s(env, lisp.QExpr([]*lisp.LVal{lisp.String("x")}))
	assert.Equal(t, "x", gotStr)
	n(env, lisp.QExpr([]*lisp.LVal{lisp.Nil()}))
	assert.Equal(t, 7, gotInt)
	n(env, lisp.QExpr([]*lisp.LVal{lisp.Int(3)}))
	assert.Equal(t, 3, gotInt)
	o(env, lisp.QExpr([]*lisp.LVal{lisp.Nil()}))
	assert.True(t, gotOpt.IsNil())
	o(env, lisp.QExpr([]*lisp.LVal{lisp.Int(3)}))
	assert.Equal(t, 3, gotOpt.Int)
}

var allocCustom = lisp.Func1(
	lisp.CustomArg(func(a *lisp.ArgReader, i int) string { return a.String(i, "first argument") }),
	func(_ *lisp.LEnv, _ string) *lisp.LVal { return allocResult })

// A CustomArg decoder costs the one allocation its documentation states.
func TestFuncCustomArgAllocs(t *testing.T) {
	env := newLimitTestEnv(t)
	args := lisp.QExpr([]*lisp.LVal{lisp.String("s")})
	assert.Equal(t, 1, int(testing.AllocsPerRun(100, func() {
		if allocCustom(env, args) != allocResult {
			t.Fatal("unexpected result")
		}
	})))
}

// BenchmarkFuncDecode compares a typed builtin with the equivalent
// hand-written LBuiltin.  allocs/op must be equal for the built-in decoders.
func BenchmarkFuncDecode(b *testing.B) {
	env := lisp.NewEnv(nil)
	for _, bc := range []struct {
		name string
		fn   lisp.LBuiltin
		args []*lisp.LVal
	}{
		{"hand/1", allocHand1, []*lisp.LVal{lisp.String("s")}},
		{"typed/1", allocTyped1, []*lisp.LVal{lisp.String("s")}},
		{"custom/1", allocCustom, []*lisp.LVal{lisp.String("s")}},
		{"hand/2", allocHand2, []*lisp.LVal{lisp.Nil(), lisp.Int(1)}},
		{"typed/2", allocTyped2, []*lisp.LVal{lisp.Nil(), lisp.Int(1)}},
		{"hand/opt", allocHandS, []*lisp.LVal{lisp.String("s"), lisp.Nil()}},
		{"typed/opt", allocTypedS, []*lisp.LVal{lisp.String("s"), lisp.Nil()}},
	} {
		args := lisp.QExpr(bc.args)
		b.Run(bc.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				if bc.fn(env, args) != allocResult {
					b.Fatal("unexpected result")
				}
			}
		})
	}
}
