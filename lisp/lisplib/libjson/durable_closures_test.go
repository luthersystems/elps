// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// closureOver returns a closure that captures v as the variable captured.
func closureOver(t *testing.T, env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
	t.Helper()
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("closure-over-tmp"), v)))
	f := env.LoadString("test", `(let ((captured closure-over-tmp)) (lambda () captured))`)
	require.NoError(t, lisp.GoError(f))
	return f
}

// restore round-trips v and binds the restored value to the global name.
func restore(t *testing.T, env *lisp.LEnv, name string, v *lisp.LVal) string {
	t.Helper()
	doc, back := roundTrip(t, env, v, nil)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol(name), back)))
	return doc
}

// Two closures over one binding share it after a restore: a set! through
// one is seen by the other.
func TestDurableClosureCounterPair(t *testing.T) {
	env := newTypedTestEnv(t)
	pair := env.LoadString("test", `(let ((n 0)) (list (lambda () (set! n (+ n 1))) (lambda () n)))`)
	require.NoError(t, lisp.GoError(pair))
	doc := restore(t, env, "pair", pair)
	assert.Equal(t, `["~#durable",[1,["~#list",[`+
		`["~#closure",["user",["~#obj",[0,["~#env",[null,["n",0]]]]],["~#code",[true,null,["~#list",["~$set!","~$n",["~#list",["~$+","~$n",1]]]]]]]],`+
		`["~#closure",["user",["~#ref",0],["~#code",[true,null,"~$n"]]]]]]]]`, doc)
	assert.Equal(t, `2`, evalString(t, env, `(funcall (first pair)) (funcall (first pair)) (funcall (second pair))`))
	// The original pair is untouched.
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("orig"), pair)))
	assert.Equal(t, `0`, evalString(t, env, `(funcall (second orig))`))
}

// A recursive closure (labels) restores: the closure sits in its own
// frame.
func TestDurableClosureRecursive(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(labels ((fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))) fact)`)
	require.NoError(t, lisp.GoError(f))
	doc := restore(t, env, "fact", f)
	assert.Contains(t, doc, `["~#obj",[0,["~#closure",`)
	assert.Equal(t, `120`, evalString(t, env, `(funcall fact 5)`))
}

// A closure over a vector and a view of it keeps the sharing.
func TestDurableClosureOverView(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(let* ((v (vector 3 2 1)) (_ (append! v 0)) (s (slice 'vector v 0 2)))
		(lambda (x) (append! v x) (stable-sort < s) (list v s)))`)
	require.NoError(t, lisp.GoError(f))
	restore(t, env, "g", f)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("orig"), f)))
	assert.Equal(t, evalString(t, env, `(funcall orig 9)`), evalString(t, env, `(funcall g 9)`))
}

// Every kind of formal restores and binds as before.
func TestDurableClosureFormals(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct{ src, call string }{
		{`(let ((k 10)) (lambda (a b) (list a b k)))`, `(funcall f 1 2)`},
		{`(let ((k 10)) (lambda (a &optional b) (list a b k)))`, `(list (funcall f 1) (funcall f 1 2))`},
		{`(let ((k 10)) (lambda (a &rest more) (list a more k)))`, `(funcall f 1 2 3)`},
		{`(let ((k 10)) (lambda (&key x y) (list x y k)))`, `(funcall f :y 2 :x 1)`},
		{`(let ((k 10)) (lambda () (list 'k "s" 1.5 ''x '(1 (2)) k)))`, `(funcall f)`},
		{`(lambda (x) (+ x 1))`, `(funcall f 41)`},
	} {
		t.Run(c.src, func(t *testing.T) {
			orig := env.LoadString("test", c.src)
			require.NoError(t, lisp.GoError(orig))
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("f"), orig)))
			want := evalString(t, env, c.call)
			restore(t, env, "f", orig)
			assert.Equal(t, want, evalString(t, env, c.call))
		})
	}
}

// A named function is restored by name, so an upgrade changes it; a
// closure keeps the code it was saved with.
func TestDurableClosureUpgrade(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defun helper () "old helper")`)
	evalString(t, env, `(defun make () (let ((n 1)) (lambda () (list "old closure" n (helper)))))`)
	v := env.LoadString("test", `(list (make) helper)`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	evalString(t, env, `(defun helper () "new helper")`)
	evalString(t, env, `(defun make () (lambda () "new closure"))`)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	assert.Equal(t, `'("old closure" 1 "new helper")`, evalString(t, env, `(funcall (first r))`))
	assert.Equal(t, `"new helper"`, evalString(t, env, `(funcall (second r))`))
}

// A closure whose frame holds a refused value is refused with the path to
// it.
func TestDurableClosureRefusalPath(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(struct{}{}))))
	outer := env.LoadString("test", `(let ((held (let ((captured opaque)) (lambda () captured)))) (lambda () held))`)
	_, err := libjson.DumpDurable(env, outer, nil)
	require.EqualError(t, err, `durable json: captured variable "held": captured variable "captured": no codec registered for native type struct {}`)
	_, err = libjson.DumpDurable(env, env.LoadString("test", `(macrolet ((m () 1)) (lambda () (m)))`), nil)
	require.EqualError(t, err, `durable json: captured variable "m": cannot encode a macro or special operator`)
}

// Closures made by one lambda form share their code; the code and frames
// are written once.
func TestDurableClosureSharedCode(t *testing.T) {
	env := newTypedTestEnv(t)
	fs := env.LoadString("test", `(map 'list (lambda (i) (lambda () i)) '(1 2))`)
	require.NoError(t, lisp.GoError(fs))
	doc := restore(t, env, "fs", fs)
	assert.Equal(t, `["~#durable",[1,["~#list",[`+
		`["~#closure",["user",["~#env",[null,["i",1]]],["~#obj",[0,["~#code",[true,null,"~$i"]]]]]],`+
		`["~#closure",["user",["~#env",[null,["i",2]]],["~#ref",0]]]]]]]`, doc)
	assert.Equal(t, `'(1 2)`, evalString(t, env, `(map 'list #^(funcall %) fs)`))
}

// The value and depth limits agree between dump and load.
func TestDurableClosureLimits(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(let ((n (list 1 2))) (lambda (x) (if x '(a (b)) n)))`)
	requireExactLimit(t, env, f, "closure")
	b, err := libjson.DumpDurable(env, f, nil)
	require.NoError(t, err)
	for n := 1; n <= 30; n++ {
		_, derr := libjson.DumpDurable(env, f, nil, libjson.WithTypedMaxValues(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(n))
		require.Equal(t, derr == nil, lerr == nil, "values %d: dump %v, load %v", n, derr, lerr)
	}
	for n := 1; n <= 10; n++ {
		_, derr := libjson.DumpDurable(env, f, nil, libjson.WithTypedMaxDepth(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxDepth(n))
		require.Equal(t, derr == nil, lerr == nil, "depth %d: dump %v, load %v", n, derr, lerr)
	}
}

func TestLoadDurableRejectsClosures(t *testing.T) {
	env := newTypedTestEnv(t)
	const pre, post = `["~#durable",[1,`, `]]`
	code := `["~#code",[true,null,1]]`
	for _, c := range []struct{ doc, want string }{
		{pre + `["~#closure",["nope",null,` + code + `]]` + post, `closure of unknown package "nope"`},
		{pre + `["~#closure",["user",1,` + code + `]]` + post, "a closure's frame must be null, a frame or a shared one"},
		{pre + `["~#closure",["user",null,1]]` + post, "a closure's code must be code or shared code"},
		{pre + `["~#closure",["user",["~#env",[null,[]]],` + code + `]]` + post, "a frame with no bindings"},
		{pre + `["~#closure",["user",["~#env",[null,["b",1,"a",2]]],` + code + `]]` + post, "captured variables out of order or duplicated"},
		{pre + `["~#closure",["user",["~#env",[null,["a",1,"a",2]]],` + code + `]]` + post, "captured variables out of order or duplicated"},
		{pre + `["~#closure",["user",["~#env",[null,["",1]]],` + code + `]]` + post, "a captured variable's name is empty"},
		{pre + `["~#closure",["user",["~#env",[null,["true",1]]],` + code + `]]` + post, "cannot rebind constant"},
		{pre + `["~#env",[null,["a",1]]]` + post, "~#env outside a closure"},
		{pre + code + post, "~#code outside a closure"},
		{pre + `["~#closure",["user",null,["~#code",[maybe,null]]]]` + post, "code must start with true or false"},
		{pre + `["~#closure",["user",null,["~#code",[true,"~$x"]]]]` + post, "code formals must be a list"},
		{pre + `["~#closure",["user",null,["~#code",[true,["~#quote",null]]]]]` + post, "code formals must be a list"},
		{pre + `["~#closure",["user",null,["~#code",[true,["~#list",[1]]]]]]` + post, "first argument contains a non-symbol"},
		{pre + `["~#closure",["user",null,["~#code",[true,null,[1]]]]]` + post, "code may hold only scalars, lists and quotes"},
		{pre + `["~#closure",["user",null,["~#code",[true,null,{}]]]]` + post, "code may hold only scalars, lists and quotes"},
		{pre + `["~#closure",["user",null,["~#code",[true,null,"~bAA=="]]]]` + post, "code may hold only scalars, lists and quotes"},
		{pre + `["~#list",[["~#obj",[0,["~#env",[null,["a",1]]]]],["~#ref",0]]]` + post, "~#env outside a closure"},
		{pre + `["~#list",[["~#closure",["user",["~#obj",[0,["~#env",[null,["a",1]]]]],` + code + `]],["~#ref",0]]]` + post, "reference to a frame object 0 in the position of a value"},
		{pre + `["~#list",[["~#obj",[0,{}]],["~#closure",["user",["~#ref",0],` + code + `]]]]` + post, "reference to a value object 0 in the position of a frame"},
		{pre + `["~#obj",[0,["~#closure",["user",["~#env",[["~#ref",0],["a",1]]],` + code + `]]]]` + post, "reference to a value object 0 in the position of a frame"},
	} {
		t.Run(c.doc, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), nil)
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
}

// A frame saves only the names its closures' code reads: a value a closure
// never names, such as a flow's context, is left out, and is unbound after
// a restore.
func TestDurableClosureSavesReferencedNames(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(struct{}{}))))
	f := env.LoadString("test", `(let ((ctx opaque) (big "unused") (n 1)) (lambda (x) (+ n x)))`)
	require.NoError(t, lisp.GoError(f))
	doc := restore(t, env, "f", f)
	assert.Equal(t, `["~#durable",[1,["~#closure",["user",["~#env",[null,["n",1]]],["~#code",[true,["~#list",["~$x"]],["~#list",["~$+","~$n","~$x"]]]]]]]]`, doc)
	assert.Equal(t, `42`, evalString(t, env, `(funcall f 41)`))
	var names []string
	for name := range env.LoadString("test", `f`).LambdaEnv().Bindings() {
		names = append(names, name)
	}
	assert.Equal(t, []string{"n"}, names, "unreferenced captured names are not restored")

	// Two closures over one frame that read different names save the
	// union, and still share it.
	pair := env.LoadString("test", `(let ((ctx opaque) (a 1) (b 2)) (list (lambda () (set! a (+ a b)) a) (lambda () b)))`)
	require.NoError(t, lisp.GoError(pair))
	doc = restore(t, env, "pair", pair)
	assert.Contains(t, doc, `["~#env",[null,["a",1,"b",2]]]`)
	assert.Equal(t, `5`, evalString(t, env, `(funcall (first pair)) (funcall (first pair))`))
	requireExactLimit(t, env, pair, "pair")
	b, err := libjson.DumpDurable(env, pair, nil)
	require.NoError(t, err)
	for n := 1; n <= 40; n++ {
		_, derr := libjson.DumpDurable(env, pair, nil, libjson.WithTypedMaxValues(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(n))
		require.Equal(t, derr == nil, lerr == nil, "values %d: dump %v, load %v", n, derr, lerr)
	}
	for n := 1; n <= 10; n++ {
		_, derr := libjson.DumpDurable(env, pair, nil, libjson.WithTypedMaxDepth(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxDepth(n))
		require.Equal(t, derr == nil, lerr == nil, "depth %d: dump %v, load %v", n, derr, lerr)
	}

	// A name only a nested lambda reads is saved, and an outer frame's
	// name shadowed by an inner one is read from the inner frame.
	nested := env.LoadString("test", `(let ((ctx opaque) (k 3)) (let ((k 4) (j 5)) (lambda () (lambda () (list k j)))))`)
	require.NoError(t, lisp.GoError(nested))
	doc = restore(t, env, "nested", nested)
	assert.Contains(t, doc, `["~#env",[null,["j",5,"k",4]]]`)
	assert.Equal(t, `'(4 5)`, evalString(t, env, `(funcall (funcall nested))`))
}

// Code that names eval may read any captured name, so its frames are saved
// whole, and a refused value in them is refused.
func TestDurableClosureEvalKeepsWholeFrames(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(let ((a 1) (b 2)) (lambda () (eval 'a)))`)
	require.NoError(t, lisp.GoError(f))
	doc := restore(t, env, "f", f)
	assert.Contains(t, doc, `["~#env",[null,["a",1,"b",2]]]`)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(struct{}{}))))
	g := env.LoadString("test", `(let ((ctx opaque)) (lambda () (eval 'x)))`)
	_, err := libjson.DumpDurable(env, g, nil)
	require.EqualError(t, err, `durable json: captured variable "ctx": no codec registered for native type struct {}`)
}
