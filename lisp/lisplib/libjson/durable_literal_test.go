// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// sortCondition runs (stable-sort < name) and returns the condition it
// raises, or "" when it sorts.
func sortCondition(t *testing.T, env *lisp.LEnv, name string) string {
	t.Helper()
	return evalString(t, env, `(handler-bind ((condition (lambda (c &rest _) (to-string c)))) (stable-sort < `+name+`) "")`)
}

// A program literal restores as a literal: the mutators that refuse a
// literal refuse it after the load too.  Its tail stays a literal.
func TestDurableRestoresLiterals(t *testing.T) {
	env := newTypedTestEnv(t)
	lit := env.LoadString("test", `(defun lit () '(3 2 1)) (lit)`)
	require.True(t, lit.IsSealed())
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("before"), lit)))
	require.Contains(t, sortCondition(t, env, "before"), "modify-literal-error")
	b, err := libjson.DumpDurable(env, lit, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#lit",["~#list",[3,2,1]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.True(t, back.IsSealed())
	assert.NotSame(t, lit, back, "a restored literal is a protected copy, not the program's cells")
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("after"), back)))
	assert.Contains(t, sortCondition(t, env, "after"), "modify-literal-error")
	assert.Contains(t, sortCondition(t, env, "(rest after)"), "modify-literal-error")
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))

	// A list built at run time stays mutable.
	plain := env.LoadString("test", `(list 3 2 1)`)
	b, err = libjson.DumpDurable(env, plain, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[3,2,1]]]]`, string(b))
	back, err = libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("after"), back)))
	assert.Equal(t, `""`, sortCondition(t, env, "after"))
}

// A literal and its tail share storage, and a literal held by two roots is
// one object; the marker is written once, inside the object.
func TestDurableLiteralSharing(t *testing.T) {
	env := newTypedTestEnv(t)
	lit := env.LoadString("test", `(defun lit () '(3 2 1)) (lit)`)
	b, err := libjson.DumpDurableRoots(env, []libjson.DurableRoot{{Name: "a", Value: lit}, {Name: "b", Value: lit}}, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",["a",["~#obj",[0,["~#lit",["~#list",[3,2,1]]]]],"b",["~#ref",0]]]]]`, string(b))
	roots, err := libjson.LoadDurableRoots(env, b, nil)
	require.NoError(t, err)
	assert.Same(t, roots[0].Value, roots[1].Value)
	assert.True(t, roots[1].Value.IsSealed())

	tail := env.LoadString("test", `(let ((l (lit))) (list l (rest l)))`)
	require.NoError(t, lisp.GoError(tail))
	b, err = libjson.DumpDurable(env, tail, nil)
	require.NoError(t, err)
	assert.Contains(t, string(b), `["~#lit",["~#view",`)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.True(t, back.Cells[0].IsSealed())
	assert.True(t, back.Cells[1].IsSealed())
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))
}

func TestLoadDurableRejectsLiterals(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct{ doc, want string }{
		{`["~#durable",[1,["~#lit",1]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit","s"]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",[1,2]]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",{}]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",null]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",["~#lit",["~#list",[1]]]]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",["~#obj",[0,["~#list",[1]]]]]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#list",[["~#obj",[0,["~#list",[1]]]],["~#lit",["~#ref",0]]]]]]`, "a literal marker must wrap a list or a view"},
		{`["~#durable",[1,["~#lit",["~#list",[]]]]]`, "empty list must be null"},
		{`["~#durable",[1,["~#lit",["~#list",[1]]],2]]`, "expected ']'"},
	} {
		t.Run(c.doc, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), nil)
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
}
