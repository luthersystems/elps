// Copyright © 2026 The ELPS authors

package libschema

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func formals(names ...string) *lisp.LVal { return lisp.Formals(names...) }

// TestArityAccepts pins when a method's formals satisfy an interface's.
func TestArityAccepts(t *testing.T) {
	cases := []struct {
		name       string
		have, want *lisp.LVal
		ok         bool
	}{
		{"exact", formals("a"), formals("x"), true},
		{"optional widens", formals("a", "&optional", "b"), formals("x"), true},
		{"rest widens", formals("&rest", "r"), formals("x", "y"), true},
		{"too many required", formals("a", "b"), formals("x"), false},
		{"too few", formals(), formals("x"), false},
		{"rest wanted", formals("a", "b"), formals("&rest", "r"), false},
		// A keyword-only method cannot take a positional argument.
		{"key is not positional", formals("&key", "a"), formals("x"), false},
		{"keys present", formals("&key", "a", "b"), formals("&key", "a"), true},
		{"key missing", formals("&key", "b"), formals("&key", "a"), false},
		{"keys, rest", formals("&rest", "r"), formals("&key", "a"), true},
		{"keys, no keys", formals(), formals("&key", "a"), false},
		// An extra optional would bind the keyword argument.
		{"keys, extra optional", formals("&optional", "o", "&key", "a"), formals("&key", "a"), false},
		{"keys unused", formals("a", "&key", "k"), formals("x"), true},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			require.NoError(t, checkFormals(c.have))
			require.NoError(t, checkFormals(c.want))
			assert.Equal(t, c.ok, arityOf(c.have).accepts(arityOf(c.want)))
		})
	}
}

// TestCheckFormalsRefusesMalformed checks that formals a lambda refuses are
// refused, so their arity is never read wrong.
func TestCheckFormalsRefusesMalformed(t *testing.T) {
	for _, f := range []*lisp.LVal{
		formals("&rest"),
		formals("&rest", "a", "b"),
		formals("a", "a"),
		formals("a", "&key"),
		formals("&optional", "&optional", "a"),
		formals(":k"),
		lisp.Cells{lisp.Int(1)}.SExpr(),
	} {
		assert.Error(t, checkFormals(f), "%v", f)
	}
}

// TestLispMethodArity checks the receiver is taken from the full formals.
func TestLispMethodArity(t *testing.T) {
	fn := func(f *lisp.LVal) *lisp.LVal { return &lisp.LVal{Type: lisp.LFun, Cells: []*lisp.LVal{f}} }
	assert.Equal(t, arity{min: 1, max: 1}, lispMethodArity(fn(formals("o", "a"))))
	assert.Equal(t, arity{min: 0, max: -1}, lispMethodArity(fn(formals("&rest", "args"))))
	assert.Equal(t, arity{min: 0, max: 1}, lispMethodArity(fn(formals("&optional", "o", "a"))))
	assert.True(t, lispMethodArity(fn(formals())).none, "no receiver")
	assert.True(t, lispMethodArity(fn(formals("&key", "a"))).none, "no receiver")
	assert.False(t, lispMethodArity(fn(formals())).accepts(arity{}))
}

// TestArityCheckCall checks a Go method's call shape.
func TestArityCheckCall(t *testing.T) {
	a := arityOf(formals("x", "&key", "k"))
	sym, kw, n := lisp.Symbol, func(s string) *lisp.LVal { return lisp.Symbol(":" + s) }, lisp.Int(1)
	assert.Empty(t, a.checkCall([]*lisp.LVal{n}))
	assert.Empty(t, a.checkCall([]*lisp.LVal{n, kw("k"), n}))
	assert.NotEmpty(t, a.checkCall(nil))
	assert.NotEmpty(t, a.checkCall([]*lisp.LVal{n, n}), "odd keyword args")
	assert.NotEmpty(t, a.checkCall([]*lisp.LVal{n, kw("z"), n}), "unknown keyword")
	assert.NotEmpty(t, a.checkCall([]*lisp.LVal{n, sym("k"), n}), "not a keyword")
	assert.NotEmpty(t, arityOf(formals("x")).checkCall([]*lisp.LVal{n, n}))
	assert.Empty(t, arityOf(formals("&rest", "r")).checkCall([]*lisp.LVal{n, n, n}))
}
