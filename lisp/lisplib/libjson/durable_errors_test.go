// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// An error value saves its condition and data, and restores as an error
// that handler-bind matches by the same condition, with the same message
// and data, and with no stack or source location.
func TestDurableErrorValues(t *testing.T) {
	env := newTypedTestEnv(t)
	orig := env.LoadString("test", `(error 'my-condition "boom" 42 (list 1 "two"))`)
	require.Equal(t, lisp.LError, orig.Type)
	_, hasSource := (*lisp.ErrorVal)(orig).Source()
	require.True(t, hasSource, "the raised error has a location the document leaves out")
	doc, back := roundTrip(t, env, orig, nil)
	assert.Equal(t, `["~#durable",[1,["~#error",["my-condition",["boom",42,["~#list",[1,"two"]]]]]]]`, doc)
	require.Equal(t, lisp.LError, back.Type)
	assert.Equal(t, "my-condition", back.Str)
	assert.Equal(t, (*lisp.ErrorVal)(orig).ErrorMessage(), (*lisp.ErrorVal)(back).ErrorMessage())
	assert.Equal(t, lisp.QExpr(orig.Cells).String(), lisp.QExpr(back.Cells).String())
	assert.Nil(t, back.Native, "no call stack")
	_, hasSource = (*lisp.ErrorVal)(back).Source()
	assert.False(t, hasSource, "no source location")

	// Raising the restored error reaches the handler for its condition,
	// with its data.
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("saved"), back)))
	assert.Equal(t, `'('my-condition "boom" 42 '(1 "two"))`,
		evalString(t, env, `(handler-bind ((my-condition (lambda (c &rest data) (cons c data)))) saved)`))
	assert.Equal(t, `"matched"`,
		evalString(t, env, `(handler-bind ((other (lambda (c &rest _) "other")) (my-condition (lambda (c &rest _) "matched"))) saved)`))

	// An error with no data.
	bare := env.LoadString("test", `(error 'empty-condition)`)
	doc, back = roundTrip(t, env, bare, nil)
	assert.Equal(t, `["~#durable",[1,["~#error",["empty-condition",[]]]]]`, doc)
	assert.Empty(t, back.Cells)
}

// An error is an object: shared, it restores as one value, and it can sit
// in a cycle through its own data.
func TestDurableErrorSharing(t *testing.T) {
	env := newTypedTestEnv(t)
	e := lisp.ErrorConditionf("c", "boom")
	doc, back := roundTrip(t, env, lisp.QExpr([]*lisp.LVal{e, e}), nil)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#error",["c",["boom"]]]]],["~#ref",0]]]]]`, doc)
	assert.Same(t, back.Cells[0], back.Cells[1])

	holder := lisp.QExpr([]*lisp.LVal{nil})
	cyc := &lisp.LVal{Type: lisp.LError, Str: "c", Cells: []*lisp.LVal{holder}}
	holder.Cells[0] = cyc
	doc, back = roundTrip(t, env, cyc, nil)
	assert.Equal(t, `["~#durable",[1,["~#obj",[0,["~#error",["c",[["~#list",[["~#ref",0]]]]]]]]]]`, doc)
	assert.Same(t, back, back.Cells[0].Cells[0])
}

// A host error's text is its message; the Go error it wraps is not saved.
func TestDurableHostError(t *testing.T) {
	env := newTypedTestEnv(t)
	inner := errors.New("disk full")
	orig := lisp.ErrorCondition("io-error", fmt.Errorf("write: %w", inner))
	require.ErrorIs(t, lisp.GoError(orig), inner)
	doc, back := roundTrip(t, env, orig, nil)
	assert.Equal(t, `["~#durable",[1,["~#error",["io-error",["write: disk full"]]]]]`, doc)
	assert.Equal(t, (*lisp.ErrorVal)(orig).ErrorMessage(), (*lisp.ErrorVal)(back).ErrorMessage())
	assert.NotErrorIs(t, lisp.GoError(back), inner)
}

// An internal panic, and an error with an empty or invalid condition, are
// refused.
func TestDurableErrorRefusals(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct {
		v    *lisp.LVal
		want string
	}{
		{&lisp.LVal{Type: lisp.LError, Str: lisp.CondInternalPanic}, "durable json: cannot encode an internal panic"},
		{lisp.QExpr([]*lisp.LVal{{Type: lisp.LError, Str: lisp.CondInternalPanic}}), "durable json: cannot encode an internal panic"},
		{&lisp.LVal{Type: lisp.LError}, "durable json: cannot encode an error whose condition is empty or not UTF-8"},
		{&lisp.LVal{Type: lisp.LError, Str: "\xff"}, "durable json: cannot encode an error whose condition is empty or not UTF-8"},
		{&lisp.LVal{Type: lisp.LError, Str: "c", Cells: []*lisp.LVal{lisp.Native(new(int))}}, "durable json: no codec registered for native type *int"},
	} {
		_, err := libjson.DumpDurable(env, c.v, nil)
		require.EqualError(t, err, c.want)
	}
}

// An error counts against the limits like a tagged value: one value for
// the error, then its data.
func TestDurableErrorAtExactLimit(t *testing.T) {
	env := newTypedTestEnv(t)
	v := env.LoadString("test", `(error 'my-condition "boom" (list 1 2))`)
	requireExactLimit(t, env, v, "error")
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	for n := 1; n <= 6; n++ {
		_, derr := libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxValues(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(n))
		require.Equal(t, derr == nil, lerr == nil, "value limit %d: dump %v, load %v", n, derr, lerr)
		require.Equal(t, n >= 5, lerr == nil, "value limit %d", n)
	}
	for n := 1; n <= 3; n++ {
		_, derr := libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxDepth(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxDepth(n))
		require.Equal(t, derr == nil, lerr == nil, "depth limit %d: dump %v, load %v", n, derr, lerr)
	}
}

func TestLoadDurableRejectsErrors(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct{ doc, want string }{
		{`["~#durable",[1,["~#error",["internal-panic",[]]]]]`, "an error with condition internal-panic"},
		{`["~#durable",[1,["~#error",["",[]]]]]`, "an error with an empty or invalid condition"},
		{`["~#durable",[1,["~#error",[1,[]]]]]`, "expected '\"'"},
		{`["~#durable",[1,["~#error",["c"]]]]`, "expected ','"},
		{`["~#durable",[1,["~#error",["c",{}]]]]`, "expected '['"},
		{`["~#durable",[1,["~#error",["c",["boom"],2]]]]`, "expected ']'"},
		{`["~#durable",[1,["~#error","c"]]]`, "expected '['"},
		{`["~#durable",[1,["~#error",["c",[]]],1]]`, "expected ']'"},
		{`["~#durable",[1,["~#obj",[0,["~#error",["c",[]]]]]]]`, "object 0 is defined but never referenced"},
	} {
		t.Run(c.doc, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), nil)
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
}
