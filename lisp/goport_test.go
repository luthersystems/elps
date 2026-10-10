// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"encoding/hex"
	"errors"
	"fmt"
	"strings"
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

func TestIsSymbol(t *testing.T) {
	assert.True(t, lisp.Symbol("true").IsSymbol(lisp.TrueSymbol))
	assert.True(t, lisp.Symbol("x").IsSymbol("x"))
	assert.False(t, lisp.Symbol("x").IsSymbol("y"))
	assert.False(t, lisp.String("x").IsSymbol("x"), "a string is not a symbol")
	assert.False(t, lisp.Nil().IsSymbol(""))
}

func TestResult(t *testing.T) {
	v, err := lisp.Result(lisp.Int(3))
	require.NoError(t, err)
	assert.Equal(t, 3, v.Int)

	lerr := lisp.ErrorConditionf("my-condition", "boom")
	v, err = lisp.Result(lerr)
	assert.Nil(t, v)
	var ev *lisp.ErrorVal
	if assert.ErrorAs(t, err, &ev) {
		assert.Same(t, (*lisp.LVal)(ev), lerr, "Result returns the error value itself")
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
		"wrapped %v":     fmt.Errorf("load: %v", lisp.GoError(budget)), //nolint:errorlint // the case checks a wrap that hides the condition
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
	assert.Empty(t, lisp.ConditionOf(nil))
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
	sx := lisp.Cells{a, b}.SExpr()
	assert.Equal(t, lisp.SExpr([]*lisp.LVal{a, b}).String(), sx.String())
	assert.Equal(t, `("a" 2)`, sx.String())
	assert.Equal(t, lisp.LSExpr, sx.Type)

	// Cells and []*lisp.LVal assign to each other with no conversion.
	var plain []*lisp.LVal = lisp.Cells{a}
	var cells lisp.Cells = plain
	assert.Len(t, cells, 1)

	// List uses the receiver as storage.
	src := lisp.QExpr([]*lisp.LVal{a, b})
	shared := lisp.Cells(src.Cells).List()
	assert.Same(t, src.Cells[0], shared.Cells[0])
	assert.Same(t, src.Cells[0], lisp.Cells(src.Cells).SExpr().Cells[0])
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
	require.Error(t, err)

	lerr := lisp.Errorf("boom")
	_, err = lisp.ResultAs[string](lerr)
	var ev *lisp.ErrorVal
	require.ErrorAs(t, err, &ev)
	assert.Same(t, (*lisp.LVal)(ev), lerr)
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

func TestArgReaderTypedReads(t *testing.T) {
	env := testEnv(t)
	fn := lisp.FunInPackage(lisp.DefaultUserPackage, "f", lisp.Formals(), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return args
	})
	m := lisp.SortedMap()
	vec := lisp.Cells{lisp.Int(7)}.Vector()
	args := lisp.Cells{lisp.Int(3), lisp.Bytes([]byte("b")), m, fn, vec}.List()

	a := lisp.ReadArgs(env, args)
	assert.Equal(t, 3, a.Int(0, "first argument"))
	assert.Equal(t, []byte("b"), a.Bytes(1, "second argument"))
	assert.Same(t, m, a.Map(2, "third argument"))
	assert.Same(t, fn, a.Fun(3, "fourth argument"))
	assert.Len(t, a.Seq(4, "fifth argument"), 1)
	assert.Equal(t, lisp.LSExpr, a.Err().Type)

	for _, tc := range []struct {
		read func(a *lisp.ArgReader)
		want string
	}{
		{func(a *lisp.ArgReader) { a.Int(1, "first argument") }, "first argument is not an integer: bytes"},
		{func(a *lisp.ArgReader) { a.Bytes(0, "first argument") }, "first argument is not bytes: int"},
		{func(a *lisp.ArgReader) { a.Map(0, "first argument") }, "first argument is not a map: int"},
		{func(a *lisp.ArgReader) { a.Fun(0, "first argument") }, "first argument is not a function: int"},
		{func(a *lisp.ArgReader) { a.Seq(0, "first argument") }, "first argument is not a proper sequence: int"},
		{func(a *lisp.ArgReader) { a.Seq(2, "first argument") }, "first argument is not a proper sequence: sorted-map"},
	} {
		r := lisp.ReadArgs(env, args)
		tc.read(&r)
		err := lisp.GoError(r.Err())
		require.Error(t, err, tc.want)
		assert.Equal(t, tc.want, errMessage(t, err))
	}

	// A multi-dimensional array is not a sequence.
	grid := lisp.Array(lisp.Cells{lisp.Int(1), lisp.Int(1)}.List(), []*lisp.LVal{lisp.Int(1)})
	a = lisp.ReadArgs(env, lisp.Cells{grid}.List())
	assert.Nil(t, a.Seq(0, "argument"))
	assert.Equal(t, "argument is not a proper sequence: array", errMessage(t, lisp.GoError(a.Err())))
}

var goportKeys = lisp.BuiltinFunc("keys")

// TestCheckAllocParity compares env.CheckAlloc with the allocation check of
// the keys builtin.
func TestCheckAllocParity(t *testing.T) {
	env := testEnv(t)
	env.Runtime.MaxAlloc = 2
	m := lisp.SortedMap()
	for _, k := range []string{"a", "b", "c"} {
		m.MapSetString(k, lisp.Int(1))
	}
	assert.Equal(t, lisp.LSExpr, env.CheckAlloc(2).Type)
	assert.True(t, env.CheckAlloc(2).IsNil())

	want := env.CallBuiltin(goportKeys, m)
	got := env.CheckAlloc(m.Len())
	require.True(t, want.IsError())
	require.True(t, got.IsError())
	assert.Equal(t, want.Str, got.Str, "condition")
	assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage())
	assert.Equal(t, "allocation size 3 exceeds maximum (2)", (*lisp.ErrorVal)(got).ErrorMessage())

	// No context check: a cancelled context does not change the result.
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		assert.True(t, env.CheckContext().IsError(), "the context is cancelled")
		assert.True(t, env.CheckAlloc(1).IsNil())
	})
}

// inCancelledBuiltin runs fn inside a builtin call under a context that is
// cancelled when fn starts, so fn sees what a builtin sees after a cancel.
func inCancelledBuiltin(t *testing.T, env *lisp.LEnv, fn func(env *lisp.LEnv)) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ran := false
	env.PutGlobal(lisp.Symbol("goport-probe"), lisp.FunInPackage(lisp.DefaultUserPackage, "goport-probe", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			cancel()
			ran = true
			fn(env)
			return lisp.Nil()
		}))
	env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{lisp.Symbol("goport-probe")}))
	require.True(t, ran, "the probe builtin did not run")
}

// assertSameResult checks that got is what the builtin call want returned:
// the same printed value, or an error with the same condition and message.
func assertSameResult(t *testing.T, want, got *lisp.LVal, msgAndArgs ...any) {
	t.Helper()
	require.Equal(t, want.Type, got.Type, msgAndArgs...)
	if want.IsError() {
		assert.Equal(t, want.Str, got.Str, msgAndArgs...)
		assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), msgAndArgs...)
		return
	}
	assert.Equal(t, want.String(), got.String(), msgAndArgs...)
}

var (
	goportToString     = lisp.BuiltinFunc("to-string")
	goportFormatString = lisp.BuiltinFunc("format-string")
)

func TestToStringParity(t *testing.T) {
	env := testEnv(t)
	values := []*lisp.LVal{
		lisp.Int(42), lisp.Float(1.5), lisp.String("s"), lisp.Symbol("sym"),
		lisp.Bytes([]byte("bytes")), lisp.SortedMap(), lisp.Nil(),
		lisp.Bytes(make([]byte, 64)), lisp.Int(123456789),
	}
	for _, limit := range []int{0, 4} {
		env.Runtime.MaxAlloc = limit
		for _, v := range values {
			assertSameResult(t, env.CallBuiltin(goportToString, v), env.ToString(v), "limit %d, %v", limit, v)
		}
	}
	env.Runtime.MaxAlloc = 0
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		got := env.ToString(lisp.Int(1))
		assertSameResult(t, env.CallBuiltin(goportToString, lisp.Int(1)), got)
		assert.Equal(t, lisp.CondContextCancelled, got.Str)
	})
}

func TestFormatStringParity(t *testing.T) {
	env := testEnv(t)
	long := lisp.String(strings.Repeat("x", 100))
	cases := []struct {
		format string
		vals   []*lisp.LVal
	}{
		{"plain", nil},
		{"{} and {}", []*lisp.LVal{lisp.Int(1), lisp.String("two")}},
		{"{1} {0}", []*lisp.LVal{lisp.Int(1), lisp.Symbol("b")}},
		{"{{literal}}", nil},
		{"unclosed {", nil},
		{"{} {}", []*lisp.LVal{lisp.Int(1)}},
		{"{0} {}", []*lisp.LVal{lisp.Int(1), lisp.Int(2)}},
		{"stray }", nil},
		{"{}", []*lisp.LVal{long}},
		{"{}", []*lisp.LVal{lisp.Cells{long, long}.List()}},
	}
	for _, limit := range []int{0, 16} {
		env.Runtime.MaxAlloc = limit
		for _, tc := range cases {
			args := append([]*lisp.LVal{lisp.String(tc.format)}, tc.vals...)
			want := env.CallBuiltin(goportFormatString, args...)
			assertSameResult(t, want, env.FormatString(tc.format, tc.vals...), "limit %d, %q", limit, tc.format)
		}
	}
	env.Runtime.MaxAlloc = 0
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		got := env.FormatString("{}", lisp.Int(1))
		assertSameResult(t, env.CallBuiltin(goportFormatString, lisp.String("{}"), lisp.Int(1)), got)
		assert.Equal(t, lisp.CondContextCancelled, got.Str)
	})
}

var (
	goportAssocMutate = lisp.BuiltinFunc("assoc!")
	goportGet         = lisp.BuiltinFunc("get")
)

func goportMap(n int) *lisp.LVal {
	m := lisp.SortedMap()
	for i := range n {
		m.MapSetString(fmt.Sprintf("k%d", i), lisp.Int(i))
	}
	return m
}

func TestMapPutParity(t *testing.T) {
	env := testEnv(t)
	type input struct {
		m    func() *lisp.LVal
		k, v *lisp.LVal
		name string
	}
	inputs := []input{
		{func() *lisp.LVal { return lisp.Nil() }, lisp.String("a"), lisp.Int(1), "nil map"},
		{func() *lisp.LVal { return lisp.Int(3) }, lisp.String("a"), lisp.Int(1), "not a map"},
		{func() *lisp.LVal { return goportMap(2) }, lisp.String("new"), lisp.Int(1), "new key"},
		{func() *lisp.LVal { return goportMap(2) }, lisp.String("k1"), lisp.Int(9), "existing key"},
		{func() *lisp.LVal { return goportMap(2) }, lisp.Symbol("k0"), lisp.Int(9), "symbol key"},
		{func() *lisp.LVal { return goportMap(2) }, lisp.Int(5), lisp.Int(9), "int key"},
		{func() *lisp.LVal { return goportMap(2) }, lisp.Float(1.5), lisp.Int(9), "float key"},
	}
	for _, limit := range []int{0, 2} {
		env.Runtime.MaxAlloc = limit
		for _, in := range inputs {
			want := env.CallBuiltin(goportAssocMutate, in.m(), in.k, in.v)
			got := env.MapPut(in.m(), in.k, in.v)
			assertSameResult(t, want, got, "limit %d, %s", limit, in.name)
		}
	}
	env.Runtime.MaxAlloc = 0
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		m := goportMap(1)
		got := env.MapPut(m, lisp.String("x"), lisp.Int(1))
		assertSameResult(t, env.CallBuiltin(goportAssocMutate, m, lisp.String("x"), lisp.Int(1)), got)
		assert.Equal(t, lisp.CondContextCancelled, got.Str)
		assert.Equal(t, 1, m.Len(), "a cancelled MapPut writes nothing")
	})
}

func TestMapLookupParity(t *testing.T) {
	env := testEnv(t)
	m := goportMap(3)
	for _, tc := range []struct {
		m, k *lisp.LVal
	}{
		{lisp.Nil(), lisp.String("k0")},
		{lisp.Int(1), lisp.String("k0")},
		{m, lisp.String("k1")},
		{m, lisp.Symbol("k2")},
		{m, lisp.String("missing")},
		{m, lisp.Float(1.5)},
		{m, lisp.Int(1)},
	} {
		assertSameResult(t, env.CallBuiltin(goportGet, tc.m, tc.k), env.MapLookup(tc.m, tc.k), "%v %v", tc.m, tc.k)
	}
	env.Runtime.MaxAlloc = 1
	assertSameResult(t, env.CallBuiltin(goportGet, m, lisp.String("k0")), env.MapLookup(m, lisp.String("k0")), "get makes no allocation check")
	env.Runtime.MaxAlloc = 0
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		got := env.MapLookup(m, lisp.String("k0"))
		assertSameResult(t, env.CallBuiltin(goportGet, m, lisp.String("k0")), got)
		assert.Equal(t, lisp.CondContextCancelled, got.Str)
	})
}

func TestSeqCells(t *testing.T) {
	list := lisp.Cells{lisp.Int(1), lisp.Int(2)}.List()
	cells, ok := list.SeqCells()
	assert.True(t, ok)
	assert.Same(t, list.Cells[0], cells[0], "the cells are the list's own")

	vec := lisp.Cells{lisp.Int(1)}.Vector()
	cells, ok = vec.SeqCells()
	assert.True(t, ok)
	assert.Equal(t, 1, cells[0].Int)

	cells, ok = lisp.Nil().SeqCells()
	assert.True(t, ok)
	assert.Empty(t, cells)

	grid := lisp.Array(lisp.Cells{lisp.Int(1), lisp.Int(1)}.List(), []*lisp.LVal{lisp.Int(1)})
	for _, v := range []*lisp.LVal{grid, lisp.Int(1), lisp.String("s"), lisp.SortedMap()} {
		cells, ok = v.SeqCells()
		assert.False(t, ok, v.Type.String())
		assert.Nil(t, cells)
	}
}

func TestFunInPackageDoc(t *testing.T) {
	env := testEnv(t)
	body := func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Int(7) }
	fun := lisp.FunInPackageDoc(lisp.DefaultUserPackage, "run-phylum", lisp.Formals(), body, "@trace{run-phylum}")
	assert.Equal(t, "@trace{run-phylum}", fun.Docstring())
	assert.Empty(t, lisp.FunInPackage(lisp.DefaultUserPackage, "f", lisp.Formals(), body).Docstring())

	env.PutGlobal(lisp.Symbol("run-phylum"), fun)
	assert.Equal(t, "7", env.LoadString("t", `(run-phylum)`).String())
}

func TestMapIterators(t *testing.T) {
	env := testEnv(t)
	m := lisp.SortedMap()
	m.MapSetString("b", lisp.Int(2))
	m.MapSetLVal(lisp.Symbol("a"), lisp.Int(1))
	m.MapSetLVal(lisp.Int(10), lisp.Int(10))
	m.MapSetLVal(lisp.Int(-1), lisp.Int(-1))

	var want []lisp.MapKey
	var wantVals []int
	require.True(t, env.MapRange(m, func(k lisp.MapKey, v *lisp.LVal) bool {
		want = append(want, k)
		wantVals = append(wantVals, v.Int)
		return true
	}).IsNil())

	var keys []lisp.MapKey
	for k := range m.Keys() {
		keys = append(keys, k)
	}
	assert.Equal(t, want, keys)

	var all []lisp.MapKey
	var vals []int
	for k, v := range m.All() {
		all = append(all, k)
		vals = append(vals, v.Int)
	}
	assert.Equal(t, want, all)
	assert.Equal(t, wantVals, vals)

	// The same order as MapKeys.
	for i, k := range m.MapKeys().Cells {
		if k.Type == lisp.LInt {
			assert.Equal(t, k.Int, keys[i].Int)
		} else {
			assert.Equal(t, k.Str, keys[i].Str)
		}
	}

	// Early break.
	n := 0
	for range m.Keys() {
		n++
		break
	}
	assert.Equal(t, 1, n)

	// A non-map yields nothing.
	for _, v := range []*lisp.LVal{lisp.Nil(), lisp.Int(1), lisp.String("x")} {
		for range v.Keys() {
			t.Fatal("Keys yielded for a non-map")
		}
		for range v.All() {
			t.Fatal("All yielded for a non-map")
		}
	}

	name, ok := keys[2].Name()
	assert.True(t, ok)
	assert.Equal(t, "a", name)
	assert.Equal(t, lisp.LSymbol, keys[2].Type)
	_, ok = keys[0].Name()
	assert.False(t, ok, "an int key has no name")
}

func TestSeqOf(t *testing.T) {
	names, ok := lisp.SeqOf[string](lisp.Cells{lisp.String("a"), lisp.String("b")}.List())
	assert.True(t, ok)
	assert.Equal(t, []string{"a", "b"}, names)

	ids, ok := lisp.SeqOf[int](lisp.Cells{lisp.Int(1), lisp.Int(2)}.Vector())
	assert.True(t, ok)
	assert.Equal(t, []int{1, 2}, ids)

	empty, ok := lisp.SeqOf[string](lisp.Nil())
	assert.True(t, ok)
	assert.Empty(t, empty)

	_, ok = lisp.SeqOf[string](lisp.Cells{lisp.String("a"), lisp.Symbol("b")}.List())
	assert.False(t, ok, "a symbol is not a string")
	_, ok = lisp.SeqOf[string](lisp.String("a"))
	assert.False(t, ok, "not a sequence")
	grid := lisp.Array(lisp.Cells{lisp.Int(1), lisp.Int(1)}.List(), []*lisp.LVal{lisp.String("a")})
	_, ok = lisp.SeqOf[string](grid)
	assert.False(t, ok, "a multi-dimensional array")

	h := &goportHandle{}
	hs, ok := lisp.SeqOf[*goportHandle](lisp.Cells{lisp.NativeOf(h)}.List())
	assert.True(t, ok)
	assert.Same(t, h, hs[0])
}

func TestStringList(t *testing.T) {
	l := lisp.StringList([]string{"a", "b"})
	assert.Equal(t, `'("a" "b")`, l.String())
	assert.Equal(t, lisp.LString, l.Cells[0].Type)
	assert.True(t, lisp.StringList(nil).IsNil())
	back, ok := lisp.SeqOf[string](l)
	assert.True(t, ok)
	assert.Equal(t, []string{"a", "b"}, back)
}

// callTyped binds fn as a builtin with formals and evaluates expr.
func callTyped(t *testing.T, env *lisp.LEnv, name string, formals *lisp.LVal, fn lisp.LBuiltin, expr string) *lisp.LVal {
	t.Helper()
	env.PutGlobal(lisp.Symbol(name), lisp.FunInPackage(lisp.DefaultUserPackage, name, formals, fn))
	return env.LoadString("t", expr)
}

func lvalMessage(v *lisp.LVal) string {
	if v.Type != lisp.LError {
		return "not an error: " + v.String()
	}
	return (*lisp.ErrorVal)(v).ErrorMessage()
}

func TestFunc1E(t *testing.T) {
	env := testEnv(t)
	encode := lisp.Func1E(func(env *lisp.LEnv, in lisp.Text) ([]byte, error) {
		return hex.AppendEncode(nil, in), nil
	})
	assert.Equal(t, `"6869"`, callTyped(t, env, "enc", lisp.Formals("x"), encode, `(to-string (enc "hi"))`).String())
	assert.Equal(t, `"6869"`, env.LoadString("t", `(to-string (enc (to-bytes "hi")))`).String())
	got := env.LoadString("t", `(enc 3)`)
	assert.Equal(t, "argument is not a string or bytes: int", lvalMessage(got))
	assert.Equal(t, "error", got.Str)

	// The arity check: a Func1E builtin bound to two formals fails.
	got = callTyped(t, env, "enc2", lisp.Formals("x", "y"), encode, `(enc2 "a" "b")`)
	assert.Equal(t, "invalid number of arguments: 2", lvalMessage(got))

	h := &goportHandle{name: "n"}
	env.PutGlobal(lisp.Symbol("handle"), lisp.NativeOf(h))
	name := lisp.Func1E(func(env *lisp.LEnv, h *goportHandle) (string, error) { return h.name, nil })
	assert.Equal(t, `"n"`, callTyped(t, env, "hname", lisp.Formals("h"), name, `(hname handle)`).String())
	assert.Equal(t, "argument is not a native *lisp_test.goportHandle: string", lvalMessage(env.LoadString("t", `(hname "x")`)))

	truthy := lisp.Func1E(func(env *lisp.LEnv, b bool) (bool, error) { return !b, nil })
	assert.Equal(t, "true", callTyped(t, env, "not2", lisp.Formals("b"), truthy, `(not2 ())`).String())
	assert.Equal(t, "false", env.LoadString("t", `(not2 0)`).String())

	half := lisp.Func1E(func(env *lisp.LEnv, f float64) (float64, error) { return f / 2, nil })
	assert.Equal(t, "1.5", callTyped(t, env, "half", lisp.Formals("f"), half, `(half 3)`).String())
	assert.Equal(t, "argument is not a number: string", lvalMessage(env.LoadString("t", `(half "x")`)))

	nothing := lisp.Func1E(func(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return nil, nil })
	assert.Equal(t, "()", callTyped(t, env, "nothing", lisp.Formals("v"), nothing, `(nothing 1)`).String())

	inner := lisp.ErrorConditionf("my-condition", "inner")
	fail := lisp.Func1E(func(env *lisp.LEnv, v *lisp.LVal) (int, error) { return 0, lisp.GoError(inner) })
	assert.Same(t, inner, callTyped(t, env, "fail", lisp.Formals("v"), fail, `(fail 1)`))

	list := lisp.Func1E(func(env *lisp.LEnv, n int) (lisp.Cells, error) { return lisp.Cells{lisp.Int(n)}, nil })
	assert.Equal(t, "'(4)", callTyped(t, env, "list1", lisp.Formals("n"), list, `(list1 4)`).String())
	plain := lisp.Func1E(func(env *lisp.LEnv, n int) ([]*lisp.LVal, error) { return []*lisp.LVal{lisp.Int(n)}, nil })
	assert.Equal(t, "'(5)", callTyped(t, env, "list2", lisp.Formals("n"), plain, `(list2 5)`).String())
	assert.Equal(t, "argument is not an integer: float", lvalMessage(env.LoadString("t", `(list2 5.5)`)))
}

func TestFunc2EAndFunc3E(t *testing.T) {
	env := testEnv(t)
	pad := lisp.Func2E(func(env *lisp.LEnv, s string, width int) (string, error) {
		if width <= len(s) {
			return s, nil
		}
		if err := lisp.GoError(env.CheckAlloc(width)); err != nil {
			return "", err
		}
		return strings.Repeat(" ", width-len(s)) + s, nil
	})
	assert.Equal(t, `"  ab"`, callTyped(t, env, "pad", lisp.Formals("s", "w"), pad, `(pad "ab" 4)`).String())
	assert.Equal(t, "second argument is not an integer: string", lvalMessage(env.LoadString("t", `(pad "ab" "4")`)))
	assert.Equal(t, "first argument is not a string: symbol", lvalMessage(env.LoadString("t", `(pad 'ab "4")`)), "the first failure wins")
	env.Runtime.MaxAlloc = 3
	assert.Equal(t, "allocation size 4 exceeds maximum (3)", lvalMessage(env.LoadString("t", `(pad "ab" 4)`)))
	env.Runtime.MaxAlloc = 0

	sum := lisp.Func3E(func(env *lisp.LEnv, a, b, c int) (int, error) { return a + b + c, nil })
	assert.Equal(t, "6", callTyped(t, env, "sum3", lisp.Formals("a", "b", "c"), sum, `(sum3 1 2 3)`).String())
	assert.Equal(t, "third argument is not an integer: string", lvalMessage(env.LoadString("t", `(sum3 1 2 "3")`)))

	join3 := lisp.Func3(lisp.StringArg("first argument"), lisp.StringArg("second argument"), lisp.StringArg("third argument"),
		func(env *lisp.LEnv, a, b, c string) *lisp.LVal { return lisp.String(a + b + c) })
	assert.Equal(t, `"abc"`, callTyped(t, env, "join3", lisp.Formals("a", "b", "c"), join3, `(join3 "a" "b" "c")`).String())
	assert.Equal(t, "third argument is not a string: int", lvalMessage(env.LoadString("t", `(join3 "a" "b" 1)`)))
}

func TestFunc2EAllocations(t *testing.T) {
	env := testEnv(t)
	add := lisp.Func2E(func(env *lisp.LEnv, s string, n int) (int, error) { return len(s) + n, nil })
	args := lisp.Cells{lisp.String("ab"), lisp.Int(1)}.List()
	allocs := testing.AllocsPerRun(100, func() {
		if v := add(env, args); v.Int != 3 {
			t.Fatal(v)
		}
	})
	assert.InDelta(t, 1, allocs, 0, "only the result value is allocated")
}

var goportSortedMap = lisp.BuiltinFunc("sorted-map")

func TestMapOfParity(t *testing.T) {
	env := testEnv(t)
	id := lisp.String("id-1")
	cases := []struct {
		name string
		kv   []any
		lv   []*lisp.LVal
	}{
		{"empty", nil, nil},
		{"mixed values", []any{"id", id, "n", 3, "f", 1.5, "ok", true, "b", []byte("x"), "l", lisp.Cells{lisp.Int(1)}, "s", "str", "nil", (*lisp.LVal)(nil)},
			[]*lisp.LVal{lisp.String("id"), id, lisp.String("n"), lisp.Int(3), lisp.String("f"), lisp.Float(1.5), lisp.String("ok"), lisp.Bool(true),
				lisp.String("b"), lisp.Bytes([]byte("x")), lisp.String("l"), lisp.Cells{lisp.Int(1)}.List(), lisp.String("s"), lisp.String("str"), lisp.String("nil"), lisp.Nil()}},
		{"lval keys", []any{lisp.Symbol("a"), 1, lisp.Int(2), 2}, []*lisp.LVal{lisp.Symbol("a"), lisp.Int(1), lisp.Int(2), lisp.Int(2)}},
		{"duplicate key", []any{"a", 1, "a", 2}, []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.String("a"), lisp.Int(2)}},
		{"odd count", []any{"a", 1, "b"}, []*lisp.LVal{lisp.String("a"), lisp.Int(1), lisp.String("b")}},
		{"float key", []any{lisp.Float(1.5), 1}, []*lisp.LVal{lisp.Float(1.5), lisp.Int(1)}},
	}
	many := make([]any, 0, 40)
	manyLV := make([]*lisp.LVal, 0, 40)
	for i := range 20 {
		many = append(many, fmt.Sprintf("k%02d", i), i)
		manyLV = append(manyLV, lisp.String(fmt.Sprintf("k%02d", i)), lisp.Int(i))
	}
	cases = append(cases, struct {
		name string
		kv   []any
		lv   []*lisp.LVal
	}{"more than the stack array", many, manyLV})
	for _, limit := range []int{0, 2} {
		env.Runtime.MaxAlloc = limit
		for _, tc := range cases {
			want := env.CallBuiltin(goportSortedMap, tc.lv...)
			assertSameResult(t, want, env.MapOf(tc.kv...), "limit %d, %s", limit, tc.name)
		}
	}
	env.Runtime.MaxAlloc = 0
	inCancelledBuiltin(t, env, func(env *lisp.LEnv) {
		got := env.MapOf("a", 1)
		assertSameResult(t, env.CallBuiltin(goportSortedMap, lisp.String("a"), lisp.Int(1)), got)
		assert.Equal(t, lisp.CondContextCancelled, got.Str)
	})
}

func TestMapOfPanicsOnBadTypes(t *testing.T) {
	env := testEnv(t)
	assert.PanicsWithValue(t, "lisp.MapOf: key of type int; a key is a string or an *LVal", func() { env.MapOf(1, 2) })
	assert.PanicsWithValue(t, "lisp.MapOf: value of type []string; a value is *LVal, string, int, float64, bool, []byte, []*LVal or Cells",
		func() { env.MapOf("a", []string{"x"}) })
	assert.Panics(t, func() { env.MapOf("a", &goportHandle{}) }, "MapOf never makes a native")
}

// TestMapOfAllocations checks that MapOf allocates what SortedMapOf with
// lisp.String keys allocates: the converted slice stays on the stack.
func TestMapOfAllocations(t *testing.T) {
	env := testEnv(t)
	id, typ, desc := lisp.String("id"), "kind", lisp.String("d")
	want := testing.AllocsPerRun(100, func() {
		env.SortedMapOf(lisp.String("id"), id, lisp.String("type"), lisp.String(typ), lisp.String("description"), desc)
	})
	got := testing.AllocsPerRun(100, func() {
		env.MapOf("id", id, "type", typ, "description", desc)
	})
	assert.InDelta(t, want, got, 0)
}
