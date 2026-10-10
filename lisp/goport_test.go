// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestIsError(t *testing.T) {
	assert.True(t, lisp.Errorf("boom").IsError())
	for _, v := range []*lisp.LVal{lisp.Nil(), lisp.Int(1), lisp.String("x"), lisp.SortedMap()} {
		assert.False(t, v.IsError(), v.Type.String())
	}
}

func TestResult(t *testing.T) {
	v, err := lisp.Result(lisp.Int(3))
	assert.NoError(t, err)
	assert.Equal(t, 3, v.Int)

	lerr := lisp.ErrorConditionf("my-condition", "boom")
	v, err = lisp.Result(lerr)
	assert.Nil(t, v)
	var ev *lisp.ErrorVal
	if assert.ErrorAs(t, err, &ev) {
		assert.Same(t, lerr, (*lisp.LVal)(ev), "Result returns the error value itself")
	}
}

// TestConditionOfMatchesLisp checks that ConditionOf names the condition the
// error has once env.Error raises it, which is what handler-bind sees.
func TestConditionOfMatchesLisp(t *testing.T) {
	env := testEnv(t)
	env.PutGlobal(lisp.Symbol("host-panic"), lisp.FunInPackage(lisp.DefaultUserPackage, "host-panic", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { panic("host fault") }))
	panicked := env.LoadString("panic.lisp", `(host-panic)`)
	require.True(t, lisp.IsInternalPanic(panicked), "%v", panicked)
	budget := lisp.ErrorConditionf(lisp.CondStepBudgetExceeded, "step budget exceeded")
	for name, err := range map[string]error{
		"bare":           lisp.GoError(budget),
		"wrapped %w":     fmt.Errorf("load: %w", lisp.GoError(budget)),
		"wrapped %v":     fmt.Errorf("load: %v", lisp.GoError(budget)),
		"plain":          errors.New("plain"),
		"typed nil":      (*lisp.ErrorVal)(nil),
		"step limit":     lisp.GoError(lisp.ErrorConditionf(lisp.CondStepLimitExceeded, "x")),
		"internal panic": lisp.GoError(panicked),
		"wrapped panic":  fmt.Errorf("host: %w", lisp.GoError(panicked)),
	} {
		t.Run(name, func(t *testing.T) {
			raised := env.Error(err)
			assert.Equal(t, raised.Str, lisp.ConditionOf(err))
		})
	}
	assert.Equal(t, "", lisp.ConditionOf(nil))
}

func TestFuncE(t *testing.T) {
	env := testEnv(t)
	inner := lisp.ErrorConditionf("my-condition", "inner")
	bind := func(name string, f func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error)) {
		env.PutGlobal(lisp.Symbol(name), lisp.FunInPackage(lisp.DefaultUserPackage, name, lisp.Formals("x"), lisp.FuncE(f)))
	}
	bind("ok", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
		return lisp.Int(args.Cells[0].Int + 1), nil
	})
	bind("nothing", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return nil, nil })
	bind("bare", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(1), lisp.GoError(inner) })
	bind("plain", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) { return nil, errors.New("plain failure") })
	bind("wrapped", func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
		return nil, fmt.Errorf("context: %w", lisp.GoError(inner))
	})

	assert.Equal(t, "2", env.LoadString("t", `(ok 1)`).String())
	assert.Equal(t, "()", env.LoadString("t", `(nothing 1)`).String())

	got := env.LoadString("t", `(bare 1)`)
	assert.Same(t, inner, got, "a bare *ErrorVal is returned as itself")

	got = env.LoadString("t", `(plain 1)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Equal(t, "error", got.Str)
	assert.Contains(t, got.String(), "plain failure")

	got = env.LoadString("t", `(wrapped 1)`)
	assert.Equal(t, "error", got.Str, "a wrapped *ErrorVal gets condition error")

	caught := env.LoadString("t", `(handler-bind ((my-condition (lambda (c &rest _) "caught"))) (bare 1))`)
	assert.Equal(t, `"caught"`, caught.String())
}

func TestCells(t *testing.T) {
	a, b := lisp.String("a"), lisp.Int(2)
	l := lisp.Cells{a, b}.List()
	assert.Equal(t, lisp.QExpr([]*lisp.LVal{a, b}).String(), l.String())
	assert.Equal(t, `'("a" 2)`, l.String())
	assert.Equal(t, `(vector "a" 2)`, lisp.Cells{a, b}.Vector().String())

	// Cells and []*lisp.LVal assign to each other with no conversion.
	var plain []*lisp.LVal = lisp.Cells{a}
	var cells lisp.Cells = plain
	assert.Len(t, cells, 1)

	// List uses the receiver as storage.
	src := lisp.QExpr([]*lisp.LVal{a, b})
	shared := lisp.Cells(src.Cells).List()
	assert.Same(t, src.Cells[0], shared.Cells[0])
	assert.True(t, lisp.Cells(nil).List().IsNil())
}

type goportHandle struct{ name string }

type goportStatus string

func TestResultAs(t *testing.T) {
	s, err := lisp.ResultAs[string](lisp.String("x"))
	require.NoError(t, err)
	assert.Equal(t, "x", s)

	_, err = lisp.ResultAs[string](lisp.Symbol("x"))
	assert.Equal(t, "value is not a string: symbol", errMessage(t, err))
	assert.Equal(t, "error", lisp.ConditionOf(err))

	n, err := lisp.ResultAs[int](lisp.Int(4))
	require.NoError(t, err)
	assert.Equal(t, 4, n)
	_, err = lisp.ResultAs[int](lisp.Float(4.5))
	assert.Equal(t, "value is not an integer: float", errMessage(t, err))

	f, err := lisp.ResultAs[float64](lisp.Int(2))
	require.NoError(t, err)
	assert.InDelta(t, 2.0, f, 0)

	b, err := lisp.ResultAs[bool](lisp.Nil())
	require.NoError(t, err)
	assert.False(t, b)
	b, _ = lisp.ResultAs[bool](lisp.Int(0))
	assert.True(t, b)

	bs, err := lisp.ResultAs[[]byte](lisp.Bytes([]byte("ab")))
	require.NoError(t, err)
	assert.Equal(t, []byte("ab"), bs)

	list := lisp.Cells{lisp.Int(1), lisp.Int(2)}.List()
	cells, err := lisp.ResultAs[lisp.Cells](list)
	require.NoError(t, err)
	assert.Len(t, cells, 2)
	plain, err := lisp.ResultAs[[]*lisp.LVal](list)
	require.NoError(t, err)
	assert.Same(t, list.Cells[0], plain[0])

	v, err := lisp.ResultAs[*lisp.LVal](lisp.Int(1))
	require.NoError(t, err)
	assert.Equal(t, 1, v.Int)

	h := &goportHandle{name: "h"}
	got, err := lisp.ResultAs[*goportHandle](lisp.NativeOf(h))
	require.NoError(t, err)
	assert.Same(t, h, got)
	_, err = lisp.ResultAs[*goportHandle](lisp.Int(1))
	assert.Equal(t, "value is not a native *lisp_test.goportHandle: int", errMessage(t, err))

	// A named type reads a native, not a string.
	_, err = lisp.ResultAs[goportStatus](lisp.String("ok"))
	assert.Error(t, err)

	lerr := lisp.Errorf("boom")
	_, err = lisp.ResultAs[string](lerr)
	var ev *lisp.ErrorVal
	require.ErrorAs(t, err, &ev)
	assert.Same(t, lerr, (*lisp.LVal)(ev))
}

func TestField(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetString("status", lisp.String("in-service"))
	m.MapSetString("count", lisp.Int(3))
	m.MapSetLVal(lisp.Symbol("sym"), lisp.String("s"))
	h := &goportHandle{}
	m.MapSetString("def", lisp.NativeOf(h))

	s, ok := lisp.Field[string](m, "status")
	assert.True(t, ok)
	assert.Equal(t, "in-service", s)
	_, ok = lisp.Field[int](m, "status")
	assert.False(t, ok, "wrong type")
	_, ok = lisp.Field[string](m, "missing")
	assert.False(t, ok, "missing key")
	_, ok = lisp.Field[string](lisp.Int(1), "status")
	assert.False(t, ok, "not a map")
	_, ok = lisp.Field[string](lisp.Nil(), "status")
	assert.False(t, ok, "nil")
	n, ok := lisp.Field[int](m, "count")
	assert.True(t, ok)
	assert.Equal(t, 3, n)
	sym, ok := lisp.Field[string](m, "sym")
	assert.True(t, ok)
	assert.Equal(t, "s", sym)
	def, ok := lisp.Field[*goportHandle](m, "def")
	assert.True(t, ok)
	assert.Same(t, h, def)
}

func TestResultAsAllocations(t *testing.T) {
	v := lisp.String("x")
	allocs := testing.AllocsPerRun(100, func() {
		if _, err := lisp.ResultAs[string](v); err != nil {
			t.Fatal(err)
		}
	})
	assert.Zero(t, allocs, "ResultAs allocates nothing on success")
}

// errMessage returns the message of an *ErrorVal error, without location.
func errMessage(t *testing.T, err error) string {
	t.Helper()
	var ev *lisp.ErrorVal
	require.ErrorAs(t, err, &ev)
	return ev.ErrorMessage()
}
