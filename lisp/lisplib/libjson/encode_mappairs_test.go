// Copyright © 2018 The ELPS authors

package libjson

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// entriesDump is the encoder's map walk as it was before encodeSortMap
// stopped materialising pair lists: every map, at every depth, goes through
// MapEntries and writes its keys from the pair LVals.  Non-map leaves are
// delegated to Dump, which the change did not touch.
func entriesDump(t *testing.T, buf *bytes.Buffer, v *lisp.LVal) {
	t.Helper()
	switch {
	case v.Type == lisp.LSortMap:
		ents := v.MapEntries()
		require.NotEqual(t, lisp.LError, ents.Type, "%v", ents)
		buf.WriteByte('{')
		for i, pair := range ents.Cells {
			if i > 0 {
				buf.WriteByte(',')
			}
			k := pair.Cells[0]
			require.Contains(t, []lisp.LType{lisp.LString, lisp.LSymbol}, k.Type)
			entriesDump(t, buf, lisp.String(k.Str))
			buf.WriteByte(':')
			entriesDump(t, buf, pair.Cells[1])
		}
		buf.WriteByte('}')
	case v.Type == lisp.LSExpr && !v.IsNil():
		buf.WriteByte('[')
		for i, c := range v.Cells {
			if i > 0 {
				buf.WriteByte(',')
			}
			entriesDump(t, buf, c)
		}
		buf.WriteByte(']')
	default:
		b, err := Dump(v, false)
		require.NoError(t, err)
		buf.Write(b)
	}
}

// TestDumpMapsMatchEntriesPath pins that sorting (key, value) views of a
// built-in map writes the same bytes the pair-list walk did: the same key
// order, symbol and string keys spelled alike, for stock maps, decoded JSON
// maps, and the two nested in each other.
func TestDumpMapsMatchEntriesPath(t *testing.T) {
	mixed := lisp.SortedMap()
	mixed.MapSet(lisp.Symbol("zeta"), lisp.Int(1))
	mixed.MapSet(lisp.String("alpha"), lisp.String("x\"y"))
	mixed.MapSet(lisp.Symbol("Mid"), lisp.Float(2.5))
	mixed.MapSet(lisp.String("é-unicode"), lisp.Nil())
	mixed.MapSet(lisp.String(""), lisp.Symbol("true"))

	inner := lisp.SortedMap()
	inner.MapSet(lisp.Symbol("b"), lisp.Int(2))
	inner.MapSet(lisp.String("a"), lisp.SExpr([]*lisp.LVal{mixed, lisp.Int(3)}))
	nested := lisp.SortedMap()
	nested.MapSet(lisp.String("inner"), inner)
	nested.MapSet(lisp.Symbol("empty"), lisp.SortedMap())
	nested.MapSet(lisp.String("deep"), nestMaps(5, mixed))

	loaded := Load([]byte(`{"z":{"y":[1,{"b":true,"a":null}],"x":"s"},"a":2.5,"m":{},"k":"v"}`), false)
	require.Equal(t, lisp.LSortMap, loaded.Type, "%v", loaded)

	hybrid := lisp.SortedMap()
	hybrid.MapSet(lisp.Symbol("loaded"), loaded)
	hybrid.MapSet(lisp.String("stock"), nested)

	for _, tc := range []struct {
		name string
		v    *lisp.LVal
		want string
	}{
		{"mixed keys", mixed, `{"":true,"Mid":2.5,"alpha":"x\"y","zeta":1,"é-unicode":null}`},
		{"nested", nested, ""},
		{"json load backed", loaded, `{"a":2.5,"k":"v","m":{},"z":{"x":"s","y":[1,{"a":null,"b":true}]}}`},
		{"stock and loaded nested", hybrid, ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got, err := Dump(tc.v, false)
			require.NoError(t, err)
			var ref bytes.Buffer
			entriesDump(t, &ref, tc.v)
			assert.Equal(t, ref.String(), string(got))
			if tc.want != "" {
				assert.Equal(t, tc.want, string(got))
			}
		})
	}
}
