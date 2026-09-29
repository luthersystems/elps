// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func constBuiltin(name string, v int) lisp.LBuiltinDef {
	return elpsutil.FunctionDoc(name, lisp.Formals("x"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		return lisp.Int(v)
	}, "doc for "+name)
}

func TestBindBuiltinsShadowsImport(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "shadowpkg")))
	// The package was created by ExtendPackage, so it uses lisp, like
	// in-package: lisp:get is visible unqualified.
	pkg := env.Runtime.Package
	require.Equal(t, "shadowpkg", pkg.Name)
	v := pkg.Get(lisp.Symbol("get"))
	require.Equal(t, lisp.LFun, v.Type)

	// Without Shadow the imported name is refused -- as an error, where
	// AddBuiltins panics -- and nothing is bound.
	lerr := env.BindBuiltins(lisp.BindOpts{Export: true}, constBuiltin("fresh", 1), constBuiltin("get", 2))
	require.Equal(t, lisp.LError, lerr.Type)
	assert.Contains(t, lisp.GoError(lerr).Error(), "symbol already defined: get")
	assert.Equal(t, lisp.LError, pkg.Get(lisp.Symbol("fresh")).Type, "validation happens before any binding")

	// With Shadow it binds like a defun would.
	lerr = env.BindBuiltins(lisp.BindOpts{Export: true, Shadow: true}, constBuiltin("fresh", 1), constBuiltin("get", 2))
	require.True(t, lerr.IsNil(), "%v", lerr)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol(lisp.DefaultUserPackage))))
	res := env.LoadString("test", `(list (shadowpkg:get 0) (shadowpkg:fresh 0) (get (sorted-map "a" 3) "a"))`)
	require.Equal(t, "'(2 1 3)", res.String())

	// Docstrings travel, as with AddBuiltins.
	fn := env.Runtime.Registry.Package("shadowpkg").Get(lisp.Symbol("get"))
	assert.Equal(t, "doc for get", fn.Docstring())
	assert.Contains(t, env.Runtime.Registry.Package("shadowpkg").Externals(), "get")
}

func TestBindBuiltinsUnexported(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "hidden")))
	require.True(t, env.BindBuiltins(lisp.BindOpts{}, constBuiltin("secret", 7)).IsNil())
	assert.NotContains(t, env.Runtime.Package.Externals(), "secret")
	res := env.LoadString("test", `(secret 0)`)
	assert.Equal(t, 7, res.Int)
}

func TestBindBuiltinsRejects(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "rej")))
	bad := elpsutil.Function("bad", lisp.Formals(":k"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	for _, tc := range []struct {
		name string
		defs []lisp.LBuiltinDef
		opts lisp.BindOpts
		want string
	}{
		{"constant", []lisp.LBuiltinDef{constBuiltin("true", 1)}, lisp.BindOpts{Shadow: true}, "cannot rebind constant: true"},
		{"formals", []lisp.LBuiltinDef{bad}, lisp.BindOpts{Shadow: true}, "builtin bad cannot be registered"},
		{"duplicate", []lisp.LBuiltinDef{constBuiltin("d", 1), constBuiltin("d", 2)}, lisp.BindOpts{Shadow: true}, "defined twice: d"},
		{"nil", []lisp.LBuiltinDef{nil}, lisp.BindOpts{}, "definition 1 is nil"},
	} {
		lerr := env.BindBuiltins(tc.opts, tc.defs...)
		require.Equal(t, lisp.LError, lerr.Type, tc.name)
		assert.Contains(t, lisp.GoError(lerr).Error(), tc.want, tc.name)
	}
}

func TestExtendPackageExisting(t *testing.T) {
	env := newLimitTestEnv(t)
	// An existing package is entered as is: ExtendPackage does not make a
	// package that does not use lisp start using it.
	require.NoError(t, lisp.GoError(env.DefinePackage(lisp.Symbol("bare"))))
	require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, "bare")))
	assert.Equal(t, "bare", env.Runtime.Package.Name)
	assert.Equal(t, lisp.LError, env.Runtime.Package.Get(lisp.Symbol("get")).Type)
	lerr := elpsutil.ExtendPackage(env, "a:b")
	require.Equal(t, lisp.LError, lerr.Type)
	assert.Contains(t, lisp.GoError(lerr).Error(), `invalid package name "a:b"`)
}
