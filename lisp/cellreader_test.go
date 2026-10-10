// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func readerMessage(t *testing.T, v *lisp.LVal) string {
	t.Helper()
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, "error", v.Str)
	return (*lisp.ErrorVal)(v).ErrorMessage()
}

func TestCellReader(t *testing.T) {
	env := testEnv(t)
	m := lisp.SortedMap()
	fn := env.GetGlobal(lisp.Symbol("car"))
	args := lisp.Cells{
		lisp.String("s"), lisp.Symbol("sym"), lisp.String("t"), lisp.Int(3),
		lisp.Float(1.5), lisp.Bytes([]byte("b")), m, fn,
		lisp.QExpr([]*lisp.LVal{lisp.Int(1)}), lisp.Nil(), lisp.Int(7), lisp.Int(8),
	}
	r := args.Read(env)
	assert.Equal(t, "s", r.Str())
	assert.Equal(t, "sym", r.Name())
	assert.Equal(t, []byte("t"), r.Text())
	assert.Equal(t, 3, r.Int())
	assert.InDelta(t, 1.5, r.Float(), 0)
	assert.Equal(t, []byte("b"), r.Bytes())
	assert.Same(t, m, r.Map().LVal())
	assert.Same(t, fn, r.Fun())
	assert.Len(t, r.Seq(), 1)
	assert.Equal(t, "dflt", r.OptStr("dflt"), "a nil cell is absent")
	assert.Equal(t, 7, r.OptInt(0))
	rest := r.Rest()
	require.Len(t, rest, 1)
	assert.Equal(t, 8, rest[0].Int)
	assert.Equal(t, 42, r.OptInt(42), "past the end is absent")
	assert.False(t, r.Err().IsError())
}

func TestCellReaderMessages(t *testing.T) {
	env := testEnv(t)
	one := lisp.Cells{lisp.Int(1)}
	r := one.Read(env)
	r.Str()
	assert.Equal(t, "argument is not a string: int", readerMessage(t, r.Err()))

	two := lisp.Cells{lisp.String("ok"), lisp.Int(1)}
	r = two.Read(env)
	r.Name()
	r.Name()
	assert.Equal(t, "second argument is not a string or symbol: int", readerMessage(t, r.Err()))

	// The first failure sticks; later reads return zero values.
	r = lisp.Cells{lisp.Int(1), lisp.Int(2)}.Read(env)
	assert.Empty(t, r.Str())
	assert.Equal(t, 0, r.Int())
	assert.Equal(t, "first argument is not a string: int", readerMessage(t, r.Err()))

	r = one.Read(env)
	r.Int()
	r.Int()
	assert.Equal(t, "invalid number of arguments: 1", readerMessage(t, r.Err()))

	r = lisp.Cells{lisp.Int(1), lisp.Int(1), lisp.Int(1), lisp.Int(1), lisp.Int(1),
		lisp.Int(1), lisp.Int(1), lisp.Int(1), lisp.Int(1), lisp.Int(1), lisp.Int(1)}.Read(env)
	for range 10 {
		r.Int()
	}
	r.Str()
	assert.Equal(t, "argument 11 is not a string: int", readerMessage(t, r.Err()))

	r = lisp.Cells{lisp.Int(1), lisp.Nil(), lisp.String("x")}.Read(env)
	r.Int()
	assert.Equal(t, 5, r.OptInt(5))
	assert.Equal(t, 0, r.OptInt(0))
	assert.Equal(t, "third argument is not an integer: string", readerMessage(t, r.Err()))

	for _, tc := range []struct {
		read func(r *lisp.CellReader)
		want string
	}{
		{func(r *lisp.CellReader) { r.Text() }, "argument is not a string or bytes: int"},
		{func(r *lisp.CellReader) { r.Bytes() }, "argument is not bytes: int"},
		{func(r *lisp.CellReader) { r.Float() }, "argument is not a number: string"},
		{func(r *lisp.CellReader) { r.Map() }, "argument is not a sorted-map: int"},
		{func(r *lisp.CellReader) { r.Fun() }, "argument is not a function: int"},
		{func(r *lisp.CellReader) { r.Seq() }, "argument is not a proper sequence: int"},
		{func(r *lisp.CellReader) { r.OptName("") }, "argument is not a string or symbol: int"},
	} {
		arg := lisp.Int(1)
		if tc.want == "argument is not a number: string" {
			arg = lisp.String("x")
		}
		r := lisp.Cells{arg}.Read(env)
		tc.read(&r)
		assert.Equal(t, tc.want, readerMessage(t, r.Err()))
	}
}

func TestCellReaderAllocs(t *testing.T) {
	env := testEnv(t)
	args := lisp.Cells{lisp.String("c"), lisp.Symbol("k"), lisp.Int(1), lisp.Nil()}
	allocs := testing.AllocsPerRun(100, func() {
		r := args.Read(env)
		_, _, _, _ = r.Name(), r.Name(), r.Int(), r.OptStr("x")
		if r.Err().IsError() {
			t.Fatal("unexpected failure")
		}
	})
	assert.Zero(t, allocs)
}

func TestFuncNameArgument(t *testing.T) {
	env := testEnv(t)
	name := lisp.Func1E(func(env *lisp.LEnv, n lisp.Name) (string, error) {
		return string(n), nil
	})
	assert.Equal(t, `"k"`, callTyped(t, env, "nm", lisp.Formals("x"), name, `(nm 'k)`).String())
	assert.Equal(t, `"k"`, env.LoadString("t", `(nm "k")`).String())
	got := env.LoadString("t", `(nm 3)`)
	assert.Equal(t, "argument is not a string or symbol: int", lvalMessage(got))
	assert.Equal(t, "error", got.Str)
}
