// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// handWritten is the shape ArgReader replaces, spelled the way builtins in
// this repository and in substrate spell it.
func handWritten(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s, m, n, name, count := args.Cells[0], args.Cells[1], args.Cells[2], args.KeyArg(3), args.KeyArg(4)
	if s.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", s.Type)
	}
	if m.Type != lisp.LSortMap {
		return env.Errorf("second argument is not a map: %s", m.Type)
	}
	if n.Type != lisp.LInt {
		return env.Errorf("third argument is not an integer: %v", n.Type)
	}
	if !name.IsNil() && name.Type != lisp.LString {
		return env.Errorf("name is not a string: %v", name.Type)
	}
	c := 10
	if !count.IsNil() {
		if count.Type != lisp.LInt {
			return env.Errorf("count is not an integer: %v", count.Type)
		}
		c = count.Int
	}
	nm := "default"
	if !name.IsNil() {
		nm = name.Str
	}
	return lisp.QExpr([]*lisp.LVal{lisp.String(s.Str), lisp.Int(m.Len()), lisp.Int(n.Int), lisp.String(nm), lisp.Int(c)})
}

func withReader(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	a := lisp.ReadArgs(env, args)
	s := a.String(0, "first argument")
	m := a.Map(1, "second argument")
	n := a.Int(2, "third argument")
	nm := a.OptString(3, "name", "default")
	c := a.OptInt(4, "count", 10)
	if lerr := a.Err(); lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.QExpr([]*lisp.LVal{lisp.String(s), lisp.Int(m.Len()), lisp.Int(n), lisp.String(nm), lisp.Int(c)})
}

func TestArgReaderMatchesHandWritten(t *testing.T) {
	env := newLimitTestEnv(t)
	formals := func() *lisp.LVal { return lisp.Formals("s", "m", "n", lisp.KeyArgSymbol, "name", "count") }
	env.AddBuiltins(false,
		&testBuiltinDef{name: "hand", formals: formals(), fn: handWritten},
		&testBuiltinDef{name: "reader", formals: formals(), fn: withReader})
	argLists := []string{
		`"s" (sorted-map "a" 1) 3`,
		`"s" (sorted-map) 3 :name "n" :count 4`,
		`1 (sorted-map) 3`,
		`"s" 2 3`,
		`"s" () 3`,
		`"s" (sorted-map) "3"`,
		`"s" (sorted-map) 3 :name 5`,
		`"s" (sorted-map) 3 :count "x"`,
		`"s" (sorted-map) 3 :name 'sym :count 1.5`,
		`1 2 3 :name 4 :count 5`,
	}
	for _, al := range argLists {
		want, ws := stepsOf(t, env, "(hand "+al+")")
		got, gs := stepsOf(t, env, "(reader "+al+")")
		require.Equal(t, want.Type, got.Type, al)
		assert.Equal(t, ws, gs, al)
		if want.Type == lisp.LError {
			assert.Equal(t, want.Str, got.Str, al)
			assert.Equal(t, (*lisp.ErrorVal)(want).ErrorMessage(), (*lisp.ErrorVal)(got).ErrorMessage(), al)
		} else {
			assert.Equal(t, want.String(), got.String(), al)
		}
	}
}

func TestArgReaderTypedAndMissing(t *testing.T) {
	env := newLimitTestEnv(t)
	a := lisp.ReadArgs(env, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	assert.Equal(t, lisp.LInt, a.Typed(0, lisp.LInt, "x %s").Type)
	assert.True(t, a.Err().IsNil())
	a.Value(1)
	require.Equal(t, lisp.LError, a.Err().Type)
	assert.Equal(t, lisp.CondMissingArgument, a.Err().Str)
	first := a.Err()
	a.String(0, "later")
	assert.Same(t, first, a.Err(), "the first failure wins")

	b := lisp.ReadArgs(env, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	b.String(0, "100% odd")
	assert.Equal(t, "100% odd is not a string: int", (*lisp.ErrorVal)(b.Err()).ErrorMessage())
	c := lisp.ReadArgs(env, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	c.OptTyped(0, lisp.LString, "value is not a string: %s")
	assert.Equal(t, "value is not a string: int", (*lisp.ErrorVal)(c.Err()).ErrorMessage())
	assert.True(t, c.Opt(5).IsNil())
}

func TestArgReaderNoAllocOnSuccess(t *testing.T) {
	env := newLimitTestEnv(t)
	args := lisp.QExpr([]*lisp.LVal{lisp.String("s"), lisp.Int(3), lisp.Nil()})
	allocs := testing.AllocsPerRun(100, func() {
		a := lisp.ReadArgs(env, args)
		_ = a.String(0, "first argument")
		_ = a.Int(1, "second argument")
		_ = a.OptString(2, "name", "d")
		if a.Err().Type == lisp.LError {
			t.Fatal("unexpected error")
		}
	})
	assert.Zero(t, allocs)
}

// load-string decodes its arguments through ArgReader; its messages are
// pinned byte for byte as they were written by hand.
func TestLoadStringArgumentErrors(t *testing.T) {
	env := newLimitTestEnv(t)
	for src, want := range map[string]string{
		`(load-string 1)`:                  "first argument is not a string: int",
		`(load-string "1" :name 2)`:        "name is not a string: int",
		`(load-string 1 :name 2)`:          "first argument is not a string: int",
		`(load-string "(+ 1 2)" :name "")`: "",
	} {
		v := env.LoadString("test", src)
		if want == "" {
			assert.Equal(t, 3, v.Int, src)
			continue
		}
		require.Equal(t, lisp.LError, v.Type, src)
		assert.Equal(t, want, (*lisp.ErrorVal)(v).ErrorMessage(), src)
	}
}

func argErr(a *lisp.ArgReader) string {
	lerr := a.Err()
	if lerr.Type != lisp.LError {
		return ""
	}
	return (*lisp.ErrorVal)(lerr).ErrorMessage()
}

// The extension points: Check and Fail let an embedder write its own
// decoders with the first-failure-wins rule; StringOrSymbol, Bytes, OneOf,
// Stringf, Intf and ReqKey cover the shapes hand-written builtins check.
func TestArgReaderExtensions(t *testing.T) {
	env := newLimitTestEnv(t)
	args := func(vs ...*lisp.LVal) *lisp.LVal { return lisp.QExpr(vs) }
	bs := env.LoadString("test", `(to-bytes "hi")`)
	require.Equal(t, lisp.LBytes, bs.Type)

	a := lisp.ReadArgs(env, args(lisp.String("s"), lisp.Symbol("sym"), bs, lisp.Int(4), lisp.Nil(), lisp.String("k")))
	assert.Equal(t, "s", a.StringOrSymbol(0, "bad %v"))
	assert.Equal(t, "sym", a.StringOrSymbol(1, "bad %v"))
	assert.Equal(t, []byte("hi"), a.Bytes(2, "bad %v"))
	assert.Equal(t, []byte("s"), a.Bytes(0, "bad %v"))
	assert.Equal(t, lisp.LInt, a.OneOf(3, "bad %v", lisp.LString, lisp.LInt).Type)
	assert.Equal(t, "s", a.Stringf(0, "argument is not a date: %v"))
	assert.Equal(t, 4, a.Intf(3, "count is not an int: %v"))
	assert.Equal(t, "k", a.ReqKey(5, "key k is required").Str)
	assert.True(t, a.Check(true, "unused %d", 1))
	assert.Empty(t, argErr(&a))

	for _, tc := range []struct {
		read func(a *lisp.ArgReader)
		want string
	}{
		{func(a *lisp.ArgReader) { a.StringOrSymbol(0, "collection is not a string: %v") }, "collection is not a string: int"},
		{func(a *lisp.ArgReader) { a.Bytes(0, "data is not bytes: %v") }, "data is not bytes: int"},
		{func(a *lisp.ArgReader) { a.OneOf(0, "want string or map: %v", lisp.LString, lisp.LSortMap) }, "want string or map: int"},
		{func(a *lisp.ArgReader) { a.Stringf(0, "argument is not a date: %v") }, "argument is not a date: int"},
		{func(a *lisp.ArgReader) { a.Intf(1, "count is not an int: %v") }, "count is not an int: list"},
		{func(a *lisp.ArgReader) { a.ReqKey(1, "key 100% is required") }, "key 100% is required"},
		{func(a *lisp.ArgReader) { a.Check(false, "custom %d", 7) }, "custom 7"},
		{func(a *lisp.ArgReader) { a.Fail(env.Errorf("mine")) }, "mine"},
		// The first failure wins over a later Check or Fail.
		{func(a *lisp.ArgReader) {
			a.Stringf(0, "first: %v")
			assert.False(t, a.Check(false, "second"))
			a.Fail(env.Errorf("third"))
		}, "first: int"},
	} {
		r := lisp.ReadArgs(env, args(lisp.Int(1), lisp.Nil()))
		tc.read(&r)
		assert.Equal(t, tc.want, argErr(&r))
	}
}

// A custom decoder built on Check composes with FuncN.
func TestCustomDecoder(t *testing.T) {
	env := newLimitTestEnv(t)
	even := func(a *lisp.ArgReader, i int) int {
		n := a.Intf(i, "argument is not an int: %v")
		a.Check(n%2 == 0, "argument is odd: %d", n)
		return n
	}
	fn := lisp.Func1(even, func(_ *lisp.LEnv, n int) *lisp.LVal { return lisp.Int(n / 2) })
	assert.Equal(t, 2, fn(env, lisp.QExpr([]*lisp.LVal{lisp.Int(4)})).Int)
	v := fn(env, lisp.QExpr([]*lisp.LVal{lisp.Int(3)}))
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, "argument is odd: 3", (*lisp.ErrorVal)(v).ErrorMessage())
}
