// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// escapedEnv registers builtins whose package and name both need a JSON
// escape ("<" is written <): p<:n< returns 7 and n<:n< returns 8, so
// mixing up the package with the name restores the wrong callback.  It
// also defines the Lisp function p<:f< and a closure in package p<.
func escapedEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := freshBuiltinEnv(t)
	for _, c := range []struct {
		pkg string
		n   int
	}{{"p<", 7}, {"n<", 8}} {
		require.NoError(t, lisp.GoError(elpsutil.ExtendPackage(env, c.pkg)))
		require.NoError(t, lisp.GoError(env.BindBuiltins(lisp.BindOpts{Export: true}, constFn("n<", c.n))))
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String("p<"))))
	evalString(t, env, `(defun f< () 70) (export 'f<) (set 'c< (let ((x< "v<")) (lambda () x<))) (export 'c<)`)
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

// Every name the durable decoder reads (a builtin's package and name, a
// ~#fn name, a closure's package and frame names, a tagged value's type, an
// error's condition, map keys) survives an escaped spelling followed by
// another escaped string.
func TestDurableEscapedNames(t *testing.T) {
	env := escapedEnv(t)
	reg := env.Runtime.Registry
	pn := reg.RegisteredBuiltin("p<", "n<")
	nn := reg.RegisteredBuiltin("n<", "n<")
	require.NotNil(t, pn)
	require.NotNil(t, nn)

	b, err := libjson.DumpDurable(env, pn, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#builtin",["p\u003c","n\u003c"]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Equal(t, pn.Native, back.Native, "p<:n< restored as another builtin")

	evalString(t, env, `(deftype tag< (x) x)`)
	v := env.LoadString("test", `(list p<:n< n<:n< p<:f< p<:c<
  (new tag< "data<") () (sorted-map "k<" "v<" "k<<" "w<"))`)
	require.NoError(t, lisp.GoError(v))
	v.Cells[5] = &lisp.LVal{Type: lisp.LError, Str: "cond<", Cells: []*lisp.LVal{lisp.String("msg<"), lisp.String("x<")}}
	b, err = libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	back, err = libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	assert.Equal(t, `'(7 8 70 "v<")`, evalString(t, env,
		`(list (funcall (nth r 0) 0) (funcall (nth r 1) 0) (funcall (nth r 2)) (funcall (nth r 3)))`))
	assert.Equal(t, pn.Native, back.Cells[0].Native)
	assert.Equal(t, nn.Native, back.Cells[1].Native)
	assert.Equal(t, v.Cells[4].Str, back.Cells[4].Str)
	assert.Equal(t, v.Cells[5].Str, back.Cells[5].Str)
	assert.Equal(t, lisp.LError, back.Cells[5].Type)
	assert.Equal(t, v.Cells[6].String(), back.Cells[6].String())
}
