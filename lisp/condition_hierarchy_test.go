// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDefineCondition(t *testing.T) {
	rt := lisp.NewEnv(nil).Runtime
	require.NoError(t, rt.DefineCondition("storage-error", "error"))
	require.NoError(t, rt.DefineCondition("not-found", "storage-error"))
	// Defining the parent a condition already has does nothing.
	require.NoError(t, rt.DefineCondition("not-found", "storage-error"))

	assert.Equal(t, "storage-error", rt.ConditionParent("not-found"))
	assert.Equal(t, "error", rt.ConditionParent(lisp.CondArgumentError))
	assert.Equal(t, "", rt.ConditionParent("error"))
	assert.True(t, rt.ConditionIsA("not-found", "not-found"))
	assert.True(t, rt.ConditionIsA("not-found", "storage-error"))
	assert.True(t, rt.ConditionIsA("not-found", "error"))
	assert.False(t, rt.ConditionIsA("storage-error", "not-found"))
	assert.False(t, rt.ConditionIsA("not-found", lisp.CondArgumentError))
	assert.False(t, rt.ConditionIsA("not-found", "condition"))
	// The package function sees only the built-in parents.
	assert.False(t, lisp.ConditionIsA("not-found", "error"))
	assert.True(t, lisp.ConditionIsA(lisp.CondArgumentError, "error"))

	for _, tc := range []struct{ child, parent, msg string }{
		{"", "error", "empty"},
		{"x", "", "empty"},
		{"x", "x", "its own parent"},
		{"condition", "error", "root of every condition"},
		{"x", "condition", "root of every condition"},
		{lisp.CondInternalPanic, "error", "internal-panic"},
		{"x", lisp.CondInternalPanic, "internal-panic"},
		{"not-found", "error", "already has parent storage-error"},
		{lisp.CondArgumentError, "storage-error", "already has parent error"},
		{"error", "not-found", "cycle"},
		{"storage-error", "not-found", "already has parent"},
	} {
		err := rt.DefineCondition(tc.child, tc.parent)
		if assert.Error(t, err, "%q -> %q", tc.child, tc.parent) {
			assert.Contains(t, err.Error(), tc.msg)
		}
	}
	// A refused definition changes nothing.
	assert.Equal(t, "", rt.ConditionParent("error"))
	assert.Equal(t, "", rt.ConditionParent("x"))
}

func TestDefineConditionDepth(t *testing.T) {
	rt := lisp.NewEnv(nil).Runtime
	name := func(i int) string { return fmt.Sprintf("c%d", i) }
	// c0 is the root; c1..c64 each have one more ancestor than the last.
	for i := 1; i <= lisp.MaxConditionDepth; i++ {
		require.NoError(t, rt.DefineCondition(name(i), name(i-1)))
	}
	err := rt.DefineCondition(name(lisp.MaxConditionDepth+1), name(lisp.MaxConditionDepth))
	require.Error(t, err)
	assert.Contains(t, err.Error(), "more than 64 ancestors")
	assert.True(t, rt.ConditionIsA(name(lisp.MaxConditionDepth), name(0)))
}

func TestHandlerBindMostSpecific(t *testing.T) {
	env := newCallSemanticsEnv(t)
	load := func(src string) string {
		t.Helper()
		v := env.LoadString("hierarchy.lisp", src)
		require.NotEqual(t, lisp.LError, v.Type, "%v", v)
		return v.String()
	}
	load(`(define-condition 'storage-error 'error)
	      (define-condition 'not-found 'storage-error)`)
	handlers := `((condition (lambda (c &rest _) (list 'any c)))
	              (error (lambda (c &rest _) (list 'err c)))
	              (storage-error (lambda (c &rest _) (list 'storage c))))`
	for _, tc := range []struct{ raise, want string }{
		{`(error 'not-found 1)`, `'('storage 'not-found)`},
		{`(error 'storage-error 1)`, `'('storage 'storage-error)`},
		{`(error 'error 1)`, `'('err 'error)`},
		{`(+ 1 "x")`, `'('err 'error)`},
		{`(error 'other 1)`, `'('any 'other)`},
		{`(error 'argument-error 1)`, `'('err 'argument-error)`},
	} {
		got := load(fmt.Sprintf(`(handler-bind %s %s)`, handlers, tc.raise))
		assert.Equal(t, tc.want, got, tc.raise)
	}
	// Only the selected handler is evaluated.
	assert.Equal(t, `'chosen`, load(`(handler-bind ((error (error 'never-evaluated))
	                                                 (not-found (lambda (&rest _) 'chosen)))
	                                   (error 'not-found))`))
	// A handler for a child does not catch its parent; an outer one does.
	assert.Equal(t, `'outer`, load(`(handler-bind ((error (lambda (&rest _) 'outer)))
	                                  (handler-bind ((not-found (lambda (&rest _) 'inner)))
	                                    (error 'storage-error)))`))
	// condition-is? walks the same chain.
	assert.Equal(t, `'(true true false true false)`, load(`(list
	    (condition-is? 'not-found 'error)
	    (condition-is? "not-found" "storage-error")
	    (condition-is? 'error 'not-found)
	    (condition-is? 'anything 'condition)
	    (condition-is? 'not-found 'argument-error))`))
	assert.Equal(t, `not-found`, load(`(define-condition 'not-found 'storage-error)`))
}

func TestDefineConditionErrors(t *testing.T) {
	env := newCallSemanticsEnv(t)
	v := env.LoadString("hierarchy.lisp", `(define-condition 'a 'a)`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, "error", v.Str)
	assert.Contains(t, lisp.GoError(v).Error(), "a cannot be its own parent")
	v = env.LoadString("hierarchy.lisp", `(define-condition 'a 1)`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, lisp.CondArgumentError, v.Str)
	v = env.LoadString("hierarchy.lisp", `(condition-is? 'a)`)
	require.Equal(t, lisp.LError, v.Type)
}

// A definition does not change how an error renders.
func TestDefineConditionRendering(t *testing.T) {
	env := newCallSemanticsEnv(t)
	before := lisp.GoError(env.LoadString("r.lisp", `(error 'not-found "missing")`)).Error()
	require.NotEqual(t, lisp.LError, env.LoadString("r.lisp", `(define-condition 'not-found 'error)`).Type)
	after := lisp.GoError(env.LoadString("r.lisp", `(error 'not-found "missing")`)).Error()
	assert.Equal(t, before, after)
	assert.True(t, strings.Contains(after, "not-found"), after)
}

func TestConditionHierarchyTemplate(t *testing.T) {
	src := newCallSemanticsEnv(t)
	require.NoError(t, src.Runtime.DefineCondition("storage-error", "error"))
	tmpl, err := lisp.NewTemplate(src, templateCorePolicy())
	require.NoError(t, err)
	a, err := tmpl.NewVM()
	require.NoError(t, err)
	b, err := tmpl.NewVM()
	require.NoError(t, err)
	assert.True(t, a.Runtime.ConditionIsA("storage-error", "error"))
	require.NoError(t, a.Runtime.DefineCondition("not-found", "storage-error"))
	assert.True(t, a.Runtime.ConditionIsA("not-found", "error"))
	// A definition in one VM is seen by no other VM, nor by the source.
	assert.False(t, b.Runtime.ConditionIsA("not-found", "error"))
	assert.False(t, src.Runtime.ConditionIsA("not-found", "error"))
	c, err := tmpl.NewVM()
	require.NoError(t, err)
	assert.False(t, c.Runtime.ConditionIsA("not-found", "error"))
	// A later definition in the source does not reach the template.
	require.NoError(t, src.Runtime.DefineCondition("other", "error"))
	assert.False(t, c.Runtime.ConditionIsA("other", "error"))
}
