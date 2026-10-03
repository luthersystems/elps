// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"runtime"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
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
		`["~#closure",["user",["~#obj",[0,["~#env",[null,["n",0]]]]],["~#code",[null,["~#lit",["~#list",["~$set!","~$n",["~#list",["~$+","~$n",1]]]]]]]]],`+
		`["~#closure",["user",["~#ref",0],["~#code",[null,"~$n"]]]]]]]]`, doc)
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

// Every kind of formal restores and binds as before, quoted formal lists
// included.
func TestDurableClosureFormals(t *testing.T) {
	env := newTypedTestEnv(t)
	goLambda := env.Lambda(lisp.Formals("x", "y"), []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), lisp.Symbol("y"), lisp.Symbol("x")})})
	for _, c := range []struct {
		src  string
		v    *lisp.LVal
		call string
	}{
		{src: `(let ((k 10)) (lambda (a b) (list a b k)))`, call: `(funcall f 1 2)`},
		{src: `(let ((k 10)) (lambda (a &optional b) (list a b k)))`, call: `(list (funcall f 1) (funcall f 1 2))`},
		{src: `(let ((k 10)) (lambda (a &rest more) (list a more k)))`, call: `(funcall f 1 2 3)`},
		{src: `(let ((k 10)) (lambda (&key x y) (list x y k)))`, call: `(funcall f :y 2 :x 1)`},
		{src: `(let ((k 10)) (lambda () (list 'k "s" 1.5 ''x '(1 (2)) k)))`, call: `(funcall f)`},
		{src: `(lambda (x) (+ x 1))`, call: `(funcall f 41)`},
		{src: `(lambda '(x) (+ x 1))`, call: `(funcall f 41)`},
		{src: "lisp.Formals", v: goLambda, call: `(funcall f 1 2)`},
	} {
		t.Run(c.src, func(t *testing.T) {
			orig := c.v
			if orig == nil {
				orig = env.LoadString("test", c.src)
			}
			require.NoError(t, lisp.GoError(orig))
			doc := restore(t, env, "f", orig)
			assert.Contains(t, doc, `["~#closure",`)
			got := evalString(t, env, c.call)
			// Bound to a global only now, after the dump wrote it as a
			// closure.
			require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("f"), orig)))
			assert.Equal(t, evalString(t, env, c.call), got)
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

// Each closure made by a lambda form holds its own copy of the form's
// cells, so each writes its code; closures restored from one code object
// share it, and re-encode sharing it.
func TestDurableClosureSharedCode(t *testing.T) {
	env := newTypedTestEnv(t)
	fs := env.LoadString("test", `(map 'list (lambda (i) (lambda () i)) '(1 2))`)
	require.NoError(t, lisp.GoError(fs))
	doc := restore(t, env, "fs", fs)
	assert.Equal(t, `["~#durable",[1,["~#list",[`+
		`["~#closure",["user",["~#env",[null,["i",1]]],["~#code",[null,"~$i"]]]],`+
		`["~#closure",["user",["~#env",[null,["i",2]]],["~#code",[null,"~$i"]]]]]]]]`, doc)
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
	code := `["~#code",[null,1]]`
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
		{pre + `["~#closure",["user",null,["~#code",[maybe,null]]]]` + post, "invalid value"},
		{pre + `["~#closure",["user",null,["~#code",["~$x"]]]]` + post, "code formals must be a list"},
		{pre + `["~#closure",["user",null,["~#code",[["~#list",[1]]]]]]` + post, "first argument contains a non-symbol"},
		{pre + `["~#closure",["user",null,["~#code",[null,[1]]]]]` + post, "code may hold only scalars, lists, quotes and literals"},
		{pre + `["~#closure",["user",null,["~#code",[null,{}]]]]` + post, "code may hold only scalars, lists, quotes and literals"},
		{pre + `["~#closure",["user",null,["~#code",[null,"~bAA=="]]]]` + post, "code may hold only scalars, lists, quotes and literals"},
		{pre + `["~#closure",["user",null,["~#code",[null,["~#lit",1]]]]]` + post, "a literal in code must be a nonempty list or a quote"},
		{pre + `["~#closure",["user",null,["~#code",[null,["~#lit",null]]]]]` + post, "a literal in code must be a nonempty list or a quote"},
		{pre + `["~#closure",["user",null,["~#code",[null,["~#lit",["~#list",[["~#lit",["~#list",[1]]]]]]]]]]` + post, "a literal inside a literal in code"},
		{pre + `["~#closure",["user",null,["~#code",[null,["~#quote",["~#lit",["~#list",[1]]]]]]]]` + post, "a literal inside a quote must be a quote"},
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

// Closures restored from one code object share one copy of it, and the
// dump keys shared code by its cells, in constant size: 20,000 closures
// over a 20,000-form body load and dump again in bounded work and memory.
func TestDurableClosuresShareRestoredCode(t *testing.T) {
	env := newTypedTestEnv(t)
	const n = 20000
	var b strings.Builder
	b.WriteString(`["~#durable",[1,["~#list",[["~#closure",["user",null,["~#obj",[0,["~#code",[null`)
	for range n {
		b.WriteString(`,1`)
	}
	b.WriteString(`]]]]]]`)
	for range n - 1 {
		b.WriteString(`,["~#closure",["user",null,["~#ref",0]]]`)
	}
	b.WriteString(`]]]]`)
	var m0, m1, m2 runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&m0)
	back, err := libjson.LoadDurable(env, []byte(b.String()), nil)
	require.NoError(t, err)
	runtime.ReadMemStats(&m1)
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	runtime.ReadMemStats(&m2)
	assert.Equal(t, b.String(), string(again))
	require.Len(t, back.Cells, n)
	for _, f := range back.Cells[1:] {
		require.Same(t, &back.Cells[0].Cells[0], &f.Cells[0], "a restored closure copied its code")
	}
	// One body is 20,000 cells; a copy per closure, or a body-sized key
	// per closure, is gigabytes.
	assert.Less(t, m1.TotalAlloc-m0.TotalAlloc, uint64(128<<20), "load")
	assert.Less(t, m2.TotalAlloc-m1.TotalAlloc, uint64(128<<20), "dump")
}

// A literal a macro or quasiquote put inside new code stays protected:
// the code records each sealed node, not one bit for the whole code.
func TestDurableClosureCodeLiterals(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defmacro step (&rest body) (quasiquote (lambda () (progn (unquote-splicing body)))))`)
	sortCondition := func(f string) string {
		return evalString(t, env, `(handler-bind ((condition (lambda (c &rest _) (to-string c)))) (stable-sort < (funcall `+f+`)) "sorted")`)
	}
	for _, src := range []string{
		`(step '(3 1 2))`,
		`(eval (quasiquote (lambda () (+ 0 0) (unquote '(3 2 1)))))`,
	} {
		orig := env.LoadString("test", src)
		require.NoError(t, lisp.GoError(orig), src)
		doc := restore(t, env, "back", orig)
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("orig"), orig)))
		require.Contains(t, sortCondition("orig"), "modify-literal-error", src)
		assert.Contains(t, doc, `["~#lit",`, src)
		assert.Contains(t, sortCondition("back"), "modify-literal-error", "%s: %s", src, doc)
	}
}

// A mutable code list that shares cells with a value cannot keep that
// sharing after a load, so it is refused.
func TestDurableClosureCodeSharingRefused(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(let ((x (list 3 2 1))) (eval (quasiquote (lambda () (stable-sort < x) (unquote x)))))`)
	require.NoError(t, lisp.GoError(f))
	_, err := libjson.DumpDurable(env, f, nil)
	require.EqualError(t, err, "durable json: a closure's code shares a mutable list with another value; code restores as its own copy, so the sharing cannot be kept")
}

// A frame is saved whole, so a value the closure never names is saved
// too, and refused with its path when it cannot be.
func TestDurableClosureSavesWholeFrames(t *testing.T) {
	env := newTypedTestEnv(t)
	f := env.LoadString("test", `(let ((big "unused") (n 1)) (lambda (x) (+ n x)))`)
	require.NoError(t, lisp.GoError(f))
	doc := restore(t, env, "f", f)
	assert.Contains(t, doc, `["~#env",[null,["big","unused","n",1]]]`)
	assert.Equal(t, `42`, evalString(t, env, `(funcall f 41)`))

	// eval and macros may reach names the code does not spell; they work
	// because nothing is dropped.
	evalString(t, env, `(defmacro getx () (quote x))`)
	for _, c := range []struct{ src, want string }{
		{`(let ((x 7) (y 8)) (lambda () (getx)))`, `7`},
		{`(let ((run eval) (name 'secret) (secret 42)) (lambda () (run name)))`, `42`},
		{`(let ((m (sorted-map "e" eval)) (name 'secret) (secret 42)) (lambda () (apply (get m "e") (list name))))`, `42`},
	} {
		g := env.LoadString("test", c.src)
		require.NoError(t, lisp.GoError(g))
		restore(t, env, "g", g)
		assert.Equal(t, c.want, evalString(t, env, `(funcall g)`), c.src)
	}

	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(struct{}{}))))
	h := env.LoadString("test", `(let ((ctx opaque) (n 1)) (lambda () n))`)
	_, err := libjson.DumpDurable(env, h, nil)
	require.EqualError(t, err, `durable json: captured variable "ctx": no codec registered for native type struct {}`)
}

// Whole frames are reserved against the value limit, summed over every
// frame, before each is copied: 100 closures over 100 frames of 100,000
// bindings each, chained through their first bindings, stop at the first
// frame past the limit, not after copying them all.
func TestDurableClosureFramesReservedCumulatively(t *testing.T) {
	env := newTypedTestEnv(t)
	root := env
	for root.Parent() != nil {
		root = root.Parent()
	}
	const frames, size = 100, 100000
	next := lisp.Int(0)
	for range frames {
		frame := lisp.NewEnv(root)
		require.NoError(t, lisp.GoError(frame.Put(lisp.Symbol("a"), next)))
		for i := 1; i < size; i++ {
			require.NoError(t, lisp.GoError(frame.Put(lisp.Symbol(fmt.Sprintf("v%d", i)), lisp.Int(i))))
		}
		next = frame.Lambda(lisp.SExpr(nil), []*lisp.LVal{lisp.Symbol("a")})
	}
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	_, err := libjson.DumpDurable(env, next, nil, libjson.WithTypedMaxValues(size+10))
	runtime.ReadMemStats(&after)
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Less(t, after.TotalAlloc-before.TotalAlloc, uint64(64<<20), "the dump copied frames past the limit")
}

// Closures dump to the same bytes and charges in a cold environment and in
// eager, lazy and prewarmed template VMs.
func TestDurableClosureParity(t *testing.T) {
	const program = `
(defun helper () 1)
(set 'pair (let ((n 0) (m (list 1 2))) (list (lambda () (set! n (+ n 1))) (lambda () (list n m (helper))))))
(set 'rec (labels ((f (k) (if (< k 1) '(done) (f (- k 1))))) f))`
	dump := func(env *lisp.LEnv) (string, []int) {
		t.Helper()
		var charges []int
		v := env.LoadString("test", `(list pair rec)`)
		require.NoError(t, lisp.GoError(v))
		b, err := libjson.DumpDurable(env, v, nil, libjson.WithTypedCharge(func(n int) error { charges = append(charges, n); return nil }))
		require.NoError(t, err)
		return string(b), charges
	}
	source := lisp.NewEnv(nil)
	source.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(source)))
	require.NoError(t, lisp.GoError(lisplib.LoadRuntimeLibrary(source)))
	require.NoError(t, lisp.GoError(source.InPackage(lisp.String(lisp.DefaultUserPackage))))
	require.NoError(t, lisp.GoError(source.LoadString("program", program)))
	want, wantCharges := dump(source)
	assert.Contains(t, want, `["~#closure",`)
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	for _, c := range []struct {
		name string
		tmpl *lisp.Template
		opts []lisp.VMOption
	}{
		{"eager", eager, nil},
		{"lazy", lazy, nil},
		{"lazy prewarmed", lazy, []lisp.VMOption{lisp.VMWithPrewarm()}},
	} {
		vm, err := c.tmpl.NewVM(c.opts...)
		require.NoError(t, err)
		got, charges := dump(vm)
		assert.Equal(t, want, got, c.name)
		assert.Equal(t, wantCharges, charges, c.name)
	}
}
