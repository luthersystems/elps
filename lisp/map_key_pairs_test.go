// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"slices"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// AppendMapKeyPairs reports every entry with its key's type, int keys
// included, and appends past a prefix it leaves alone.
func TestAppendMapKeyPairs(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String("s"), lisp.Int(1))
	m.MapSetLVal(lisp.Symbol("y"), lisp.Int(2))
	m.MapSetLVal(lisp.Symbol(":k"), lisp.Int(3))
	m.MapSetLVal(lisp.Int(7), lisp.Int(4))
	prefix := lisp.MapKeyPair{Key: "prefix", Kind: lisp.LString}
	pairs, ok := m.AppendMapKeyPairs([]lisp.MapKeyPair{prefix})
	require.True(t, ok)
	require.Len(t, pairs, 5)
	assert.Equal(t, prefix, pairs[0])
	var got []string
	for _, p := range pairs[1:] {
		k := p.Key
		if p.Kind == lisp.LInt {
			k = strconv.Itoa(p.Int)
		}
		got = append(got, p.Kind.String()+" "+k+"="+p.Val.String())
	}
	slices.Sort(got)
	assert.Equal(t, []string{"int 7=4", "string s=1", "symbol :k=3", "symbol y=2"}, got)
	empty, ok := lisp.SortedMap().AppendMapKeyPairs(nil)
	assert.True(t, ok)
	assert.Empty(t, empty)
}
