// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLazyScopeBindings(t *testing.T) {
	parent := initSafetyTestEnv(t)
	x := Symbol("x")
	require.NotEqual(t, LError, parent.Put(x, Int(1)).Type)
	require.NotEqual(t, LError, parent.PutGlobal(Symbol("global"), Int(9)).Type)
	child := newEnvN(parent, 32)
	require.False(t, child.scope.allocated())
	assert.Equal(t, 32, child.scopeHint)
	assert.Zero(t, child.NumBindings())
	for range child.Bindings() {
		t.Fatal("unwritten scope has a binding")
	}
	assert.Equal(t, 1, child.Get(x).Int)
	assert.Equal(t, 9, child.Get(Symbol("global")).Int)
	require.NotEqual(t, LError, child.Update(x, Int(2)).Type)
	require.NotEqual(t, LError, child.Update(Symbol("global"), Int(10)).Type)
	assert.Equal(t, 2, parent.Get(x).Int)
	assert.Equal(t, 10, parent.GetGlobal(Symbol("global")).Int)
	assert.False(t, child.scope.allocated(), "reading or updating an ancestor must not allocate")

	// An eagerly allocated empty map is the oracle for validation and errors.
	eager := NewEnv(parent)
	eager.scope = newScopeTable(0)
	for _, key := range []*LVal{Int(1), Symbol(":key"), Symbol(TrueSymbol), Symbol(FalseSymbol)} {
		assert.Equal(t, eager.Put(key, Int(0)).String(), child.Put(key, Int(0)).String())
		assert.False(t, child.scope.allocated(), "failed Put must not allocate")
	}
	missing := Symbol("missing")
	assert.Equal(t, eager.Get(missing).String(), child.Get(missing).String())
	assert.Equal(t, eager.Update(missing, Int(0)).String(), child.Update(missing, Int(0)).String())
	assert.False(t, child.scope.allocated())

	// Capture before the map exists: later Put and Update must stay visible,
	// including through Copy, which shares the captured environment.
	closure := child.Lambda(Formals(), []*LVal{x})
	copied := closure.Copy()
	require.Equal(t, LFun, copied.Type)
	require.NotEqual(t, LError, child.Put(x, Int(3)).Type)
	require.True(t, child.scope.allocated())
	assert.Equal(t, 1, child.NumBindings())
	assert.Equal(t, 2, parent.Get(x).Int)
	grandchild := NewEnv(child)
	require.NotEqual(t, LError, grandchild.Update(x, Int(4)).Type)
	assert.False(t, grandchild.scope.allocated())
	for _, fun := range []*LVal{closure, copied} {
		got := parent.FunCall(fun, Nil())
		require.Equal(t, LInt, got.Type, "%v", got)
		assert.Equal(t, 4, got.Int)
	}
	require.NotEqual(t, LError, child.Put(x, Int(5)).Type)
	assert.Equal(t, 5, child.Get(x).Int)
	assert.Equal(t, 2, parent.Get(x).Int)
}

func TestLazyCallScope(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.scopeHint = 32
	require.NotEqual(t, LError, env.Put(Symbol("captured"), Int(7)).Type)
	for _, tc := range []struct {
		name    string
		formals *LVal
		args    *LVal
	}{
		{"empty", Formals(), Nil()},
		{"required", Formals("x"), QExpr([]*LVal{Int(1)})},
		{"optional", Formals(OptArgSymbol, "x"), Nil()},
		{"rest", Formals(VarArgSymbol, "x"), Nil()},
		{"keyword", Formals(KeyArgSymbol, "x"), Nil()},
	} {
		t.Run(tc.name, func(t *testing.T) {
			fun := env.Lambda(tc.formals, []*LVal{Symbol("captured")})
			call, list := env.bind(fun, tc.args)
			require.Nil(t, list, "a lambda's body is read from fun, not returned")
			require.NotNil(t, call)
			assert.Same(t, env, call.parent)
			assert.Equal(t, tc.formals.Len(), call.scopeHint)
			if tc.name == "empty" {
				assert.False(t, call.scope.allocated())
			} else {
				assert.Equal(t, 1, call.NumBindings())
			}
			require.NotEqual(t, LError, call.Put(Symbol("captured"), Int(8)).Type)
			assert.Equal(t, 8, call.Get(Symbol("captured")).Int)
			assert.Equal(t, 7, env.Get(Symbol("captured")).Int)
		})
	}
}

func TestLazyTemplateScopes(t *testing.T) {
	for _, populated := range []bool{false, true} {
		source := templateOwnershipEnv()
		child := newEnvN(source, 32)
		x := Symbol("x")
		if populated {
			require.NotEqual(t, LError, child.Put(x, Int(1)).Type)
		}
		fun := child.Lambda(Formals(), []*LVal{x})
		require.NotEqual(t, LError, source.PutGlobal(Symbol("closure"), fun).Type)
		tmpl, err := NewTemplate(source)
		require.NoError(t, err)
		for range 2 {
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			assert.False(t, vm.scope.allocated())
			cloned := vm.GetGlobal(Symbol("closure"))
			require.Equal(t, LFun, cloned.Type)
			captured := cloned.funEnv()
			key := Symbol("x")
			require.NotSame(t, child, captured)
			assert.Equal(t, 32, captured.scopeHint)
			if populated {
				assert.Equal(t, 1, captured.Get(key).Int)
			} else {
				assert.False(t, captured.scope.allocated())
			}
			require.NotEqual(t, LError, captured.Put(key, Int(2)).Type)
			got := vm.FunCall(cloned, Nil())
			require.Equal(t, LInt, got.Type, "%v", got)
			assert.Equal(t, 2, got.Int)
		}
		if populated {
			assert.Equal(t, 1, child.Get(x).Int)
		} else {
			assert.False(t, child.scope.allocated())
		}
	}
	assert.False(t, NewEnvRuntime(nil).scope.allocated())
}
