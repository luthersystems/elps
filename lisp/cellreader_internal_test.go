// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// TestCellReaderMatchesValueAs checks that each typed read accepts exactly
// the values valueAs accepts and returns what it returns, on success and on
// failure.  The reads check v.Type inline instead of calling valueAs
// (luthersystems/elps#841), so this keeps the two from drifting apart.
func TestCellReaderMatchesValueAs(t *testing.T) {
	env := NewEnv(nil)
	vals := []*LVal{
		String("s"), Symbol("sym"), Int(3), Float(1.5), Bytes([]byte("b")),
		Nil(), SortedMap(), QExpr([]*LVal{Int(1)}), Bool(true),
	}
	read := func(v *LVal, f func(r *CellReader) any) (any, bool) {
		r := Cells{v}.Read(env)
		got := f(&r)
		return got, !r.Err().IsError()
	}
	for _, v := range vals {
		name := v.Type.String()

		got, ok := read(v, func(r *CellReader) any { return r.Str() })
		want, wok := valueAs[string](v)
		assert.Equal(t, wok, ok, "Str %s", name)
		assert.Equal(t, want, got, "Str %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.Name() })
		wantName, wok := valueAs[Name](v)
		assert.Equal(t, wok, ok, "Name %s", name)
		assert.Equal(t, string(wantName), got, "Name %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.Text() })
		wantText, wok := valueAs[Text](v)
		assert.Equal(t, wok, ok, "Text %s", name)
		assert.Equal(t, []byte(wantText), got, "Text %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.Int() })
		wantInt, wok := valueAs[int](v)
		assert.Equal(t, wok, ok, "Int %s", name)
		assert.Equal(t, wantInt, got, "Int %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.Float() })
		wantFloat, wok := valueAs[float64](v)
		assert.Equal(t, wok, ok, "Float %s", name)
		assert.InDelta(t, wantFloat, got, 0, "Float %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.Bytes() })
		wantBytes, wok := valueAs[[]byte](v)
		assert.Equal(t, wok, ok, "Bytes %s", name)
		assert.Equal(t, wantBytes, got, "Bytes %s", name)

		// An optional read of () is absent and returns def; any other value
		// converts as the required read does, and returns def on failure.
		isNil := v.IsNil()
		got, ok = read(v, func(r *CellReader) any { return r.OptStr("d") })
		want, wok = valueAs[string](v)
		if isNil || !wok {
			want = "d"
		}
		assert.Equal(t, wok || isNil, ok, "OptStr %s", name)
		assert.Equal(t, want, got, "OptStr %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.OptName("d") })
		wantName, wok = valueAs[Name](v)
		if isNil || !wok {
			wantName = "d"
		}
		assert.Equal(t, wok || isNil, ok, "OptName %s", name)
		assert.Equal(t, string(wantName), got, "OptName %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.OptInt(9) })
		wantInt, wok = valueAs[int](v)
		if isNil || !wok {
			wantInt = 9
		}
		assert.Equal(t, wok || isNil, ok, "OptInt %s", name)
		assert.Equal(t, wantInt, got, "OptInt %s", name)

		got, ok = read(v, func(r *CellReader) any { return r.OptFloat(9) })
		wantFloat, wok = valueAs[float64](v)
		if isNil || !wok {
			wantFloat = 9
		}
		assert.Equal(t, wok || isNil, ok, "OptFloat %s", name)
		assert.InDelta(t, wantFloat, got, 0, "OptFloat %s", name)
	}
}
