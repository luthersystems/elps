// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// intKeyMap is a minimal embedder Map whose only entry has an int key.
type intKeyMap struct{}

func (intKeyMap) Len() int { return 1 }
func (intKeyMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	if k.Type == lisp.LInt && k.Int == 7 {
		return lisp.String("seven"), true
	}
	return lisp.Nil(), false
}
func (intKeyMap) Set(*lisp.LVal, *lisp.LVal) *lisp.LVal { return lisp.Errorf("read-only") }
func (intKeyMap) Del(*lisp.LVal) *lisp.LVal             { return lisp.Errorf("read-only") }
func (intKeyMap) Keys() *lisp.LVal                      { return lisp.QExpr([]*lisp.LVal{lisp.Int(7)}) }
func (intKeyMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	buf[0] = lisp.QExpr([]*lisp.LVal{lisp.Int(7), lisp.String("seven")})
	return lisp.Int(1)
}

// TestEmbedderIntKeyCompatibility pins the Go-embedder-visible changes of
// #733 documented in docs/embed.md: json:dump of an embedder Map holding an
// int key writes the key's decimal spelling (it used to fail with an invalid
// key type error), and a stock map now accepts an int key through
// MapSetLVal and returns it as an LInt from Keys and Entries.
func TestEmbedderIntKeyCompatibility(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	env.PutGlobal(lisp.Symbol("emb"), lisp.SortedMapFromData(lisp.NewMapData(intKeyMap{})))
	got := env.LoadString("embedder.lisp", `(json:dump-string emb)`)
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	assert.Equal(t, `{"7":"seven"}`, got.Str)

	m := lisp.SortedMap()
	require.NotEqual(t, lisp.LError, m.MapSetLVal(lisp.Int(3), lisp.Int(1)).Type)
	m.MapSetString("a", lisp.Int(2))
	keys := m.MapKeys()
	require.Len(t, keys.Cells, 2)
	assert.Equal(t, lisp.LInt, keys.Cells[0].Type)
	assert.Equal(t, 3, keys.Cells[0].Int)
	ents := m.MapEntries()
	require.Len(t, ents.Cells, 2)
	assert.Equal(t, lisp.LInt, ents.Cells[0].Cells[0].Type)
}

// TestSerializerGoMapIntKeys pins the deprecated Serializer.GoMap/GoValue on
// int keys: they are converted to their decimal spelling, as json:dump
// writes them, instead of the map being silently dropped, and a map whose
// int key collides with a string key is refused as json:dump refuses it.
func TestSerializerGoMapIntKeys(t *testing.T) {
	s := &libjson.Serializer{}
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.Int(1), lisp.String("x"))
	got, ok := s.GoMap(m, false)
	require.True(t, ok)
	assert.Equal(t, map[string]any{"1": "x"}, got)
	outer := lisp.SortedMap()
	outer.MapSetString("a", m)
	assert.Equal(t, map[string]any{"a": map[string]any{"1": "x"}}, s.GoValue(outer, false))
	m.MapSetString("1", lisp.String("y"))
	_, ok = s.GoMap(m, false)
	assert.False(t, ok)
}
