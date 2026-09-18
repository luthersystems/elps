// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// SortedMapFromData(NewMapData(nil)) is a sorted-map value with no backing
// implementation at all. It is reachable through the documented embedder
// extension point, so every walk over one has to treat it as the empty map it
// is rather than call a method on a nil interface: a nil dereference inside
// the interpreter surfaces as an internal-panic condition, which is this
// repository's own marker for a defect.
func TestNilBackedSortedMapWalks(t *testing.T) {
	for _, expr := range []string{
		`(format-string "{}" m)`,
		`(debug-print m)`,
		`(equal? m m)`,
		`(equal? m (sorted-map))`,
		`(json:dump-string m)`,
		`(assoc m "a" 1)`,
		`(keys m)`,
		`(get m "a")`,
		`(dissoc m "a")`,
		`(length (keys m))`,
		`(copy m)`,
		`(to-string (sorted-map "k" m))`,
		`(format-string "{}" (sorted-map "k" m))`,
	} {
		t.Run(expr, func(t *testing.T) {
			env, err := lisplib.NewDocEnv()
			require.NoError(t, err)
			env.Runtime.Reader = parser.NewReader()
			env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
			m := lisp.SortedMapFromData(lisp.NewMapData(nil))
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("m"), m)))
			v := env.LoadString("nil-backed-map.lisp", expr)
			assert.False(t, lisp.IsInternalPanic(v), "internal panic: %v", v)
			if v.Type == lisp.LError {
				// An ordinary condition is a defensible answer for a
				// degenerate map; a panic never is.
				t.Logf("condition: %v", v)
			}
		})
	}
}

// The walkers reached from Go rather than from lisp source take the same view.
func TestNilBackedSortedMapGoWalkers(t *testing.T) {
	m := lisp.SortedMapFromData(lisp.NewMapData(nil))
	assert.NotPanics(t, func() { _ = m.String() })
	assert.NotPanics(t, func() { m.Copy() })
	assert.NotPanics(t, func() { lisp.GoValue(m) })
	assert.NotPanics(t, func() { m.Equal(m) })
	assert.NotPanics(t, func() { lisp.SExpr([]*lisp.LVal{m, m}).Equal(lisp.SExpr([]*lisp.LVal{m, m})) })
}
