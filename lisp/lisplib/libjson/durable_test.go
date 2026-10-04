// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// counter is a pointer native; its identity is its pointer.
type counter struct{ n int }

// point is a value native; its identity is its header.
type point struct{ x, y int }

// nativeOf returns v's native payload as a T, or T's zero value.
func nativeOf[T any](v *lisp.LVal) T {
	x, _ := v.Native.(T)
	return x
}

// durableTestRegistry registers test:counter (payload: the count) and
// test:point (payload: a list of x and y, read at versions 1 and 2).
func durableTestRegistry(t testing.TB, extra ...func(*libjson.DurableRegistry)) *libjson.DurableRegistry {
	t.Helper()
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](reg, "test:counter", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			return lisp.Int(nativeOf[*counter](v).n), nil
		},
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
			if p.Type != lisp.LInt {
				return nil, errors.New("counter payload is not an int")
			}
			return lisp.Native(&counter{n: p.Int}), nil
		},
	}))
	require.NoError(t, libjson.RegisterNative[point](reg, "test:point", 2, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			p := nativeOf[point](v)
			return lisp.QExpr([]*lisp.LVal{lisp.Int(p.x), lisp.Int(p.y)}), nil
		},
		Load: func(_ *lisp.LEnv, version int, p *lisp.LVal) (*lisp.LVal, error) {
			if version == 1 { // version 1 saved only x
				return lisp.Native(point{x: p.Int}), nil
			}
			if p.Type != lisp.LSExpr || len(p.Cells) != 2 {
				return nil, errors.New("point payload is not a pair")
			}
			return lisp.Native(point{x: p.Cells[0].Int, y: p.Cells[1].Int}), nil
		},
	}))
	for _, f := range extra {
		f(reg)
	}
	reg.Freeze()
	return reg
}

// durableRoundTrip dumps the value of src, loads it back, binds it to r and
// returns the document.
func durableRoundTrip(t *testing.T, env *lisp.LEnv, reg *libjson.DurableRegistry, src string) string {
	t.Helper()
	v := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(v), src)
	b, err := libjson.DumpDurable(env, v, reg)
	require.NoError(t, err, src)
	back, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err, string(b))
	again, err := libjson.DumpDurable(env, back, reg)
	require.NoError(t, err)
	require.Equal(t, string(b), string(again), "re-encoding a loaded value changes its bytes")
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	return string(b)
}

func evalString(t *testing.T, env *lisp.LEnv, src string) string {
	t.Helper()
	v := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(v), src)
	return v.String()
}

func TestDurableAliasing(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct {
		name, src, doc string
		after          [][2]string
	}{
		{
			name: "two locals hold one map",
			src:  `(let ((m (sorted-map "a" 1))) (list m m))`,
			doc:  `["~#durable",[1,["~#list",[["~#obj",[0,{"a":1}]],["~#ref",0]]]]]`,
			after: [][2]string{
				{`(assoc! (first r) "b" 2)`, `(sorted-map "a" 1 "b" 2)`},
				{`(get (second r) "b")`, `2`},
			},
		},
		{
			name: "a map inside two lists",
			src:  `(let ((m (sorted-map))) (vector (list m 1) (list m 2)))`,
			doc:  `["~#durable",[1,[["~#list",[["~#obj",[0,{}]],1]],["~#list",[["~#ref",0],2]]]]]`,
			after: [][2]string{
				{`(assoc! (first (aref r 0)) :k "v")`, `(sorted-map ':k "v")`},
				{`(get (first (aref r 1)) :k)`, `"v"`},
			},
		},
		{
			name: "a map that holds itself",
			src:  `(let ((m (sorted-map))) (assoc! m "self" m) m)`,
			doc:  `["~#durable",[1,["~#obj",[0,{"self":["~#ref",0]}]]]]`,
			after: [][2]string{
				{`(assoc! r "x" 1)`, `(sorted-map "self" #<cycle> "x" 1)`},
				{`(get (get (get r "self") "self") "x")`, `1`},
			},
		},
		{
			name: "a vector that holds itself",
			src:  `(let ((v (vector 1))) (append! v v) v)`,
			doc:  `["~#durable",[1,["~#obj",[0,["~#array",[[2],["~#view",[["~#cells",4],0,2,4,[1,["~#ref",0],null,null]]]]]]]]]`,
			after: [][2]string{
				{`(append! r 3)`, `(vector 1 #<cycle> 3)`},
				{`(length (aref r 1))`, `3`},
			},
		},
		{
			name: "two names for one bytes value",
			src:  `(let ((b (to-bytes "hi"))) (sorted-map "x" b "y" b))`,
			doc:  `["~#durable",[1,{"x":["~#obj",[0,"~baGk="]],"y":["~#ref",0]}]]`,
			after: [][2]string{
				{`(append-bytes! (get r "x") "!")`, `#<bytes 104 105 33>`},
				{`(to-string (get r "y"))`, `"hi!"`},
			},
		},
		{
			name: "shared list and tagged value",
			src:  `(let* ((l (list 1 2)) (tv (new point l))) (list l l tv tv))`,
			doc:  `["~#durable",[1,["~#list",[["~#obj",[0,["~#list",[1,2]]]],["~#ref",0],["~#obj",[1,["~#tagged",["user:point",["~#ref",0]]]]],["~#ref",1]]]]]`,
		},
		{
			name: "a tree is typed JSON in a header",
			src:  `(sorted-map 'a (list 1 2.0 "s") "b" (vector :k ()))`,
			doc:  `["~#durable",[1,{"b":["~:k",null],"~$a":["~#list",[1,"~d2","s"]]}]]`,
		},
	} {
		t.Run(c.name, func(t *testing.T) {
			if c.name == "shared list and tagged value" {
				evalString(t, env, `(deftype point (x) x)`)
			}
			doc := durableRoundTrip(t, env, nil, c.src)
			assert.Equal(t, c.doc, doc)
			for _, step := range c.after {
				assert.Equal(t, step[1], evalString(t, env, step[0]), step[0])
			}
		})
	}
}

func TestDurableSharedArray(t *testing.T) {
	env := newTypedTestEnv(t)
	a := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(2)}), nil)
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{a, a}), nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#array",[[2,2],[null,null,null,null]]]]],["~#ref",0]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Same(t, back.Cells[0], back.Cells[1])
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))
}

func TestDurableDeterministic(t *testing.T) {
	env := newTypedTestEnv(t)
	a := env.LoadString("test", `(let ((m (sorted-map))) (assoc! m "z" m) (assoc! m "a" (list m)) m)`)
	b := env.LoadString("test", `(let ((m (sorted-map))) (assoc! m "a" (list m)) (assoc! m "z" m) m)`)
	da, err := libjson.DumpDurable(env, a, nil)
	require.NoError(t, err)
	db, err := libjson.DumpDurable(env, b, nil)
	require.NoError(t, err)
	assert.Equal(t, string(da), string(db))
	assert.Equal(t, `["~#durable",[1,["~#obj",[0,{"a":["~#list",[["~#ref",0]]],"z":["~#ref",0]}]]]]`, string(da))
}

func TestDurableNatives(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := durableTestRegistry(t)
	c := lisp.Native(&counter{n: 7})
	p := lisp.Native(point{x: 1, y: 2})
	v := lisp.QExpr([]*lisp.LVal{c, c, p, p})
	b, err := libjson.DumpDurable(env, v, reg)
	require.NoError(t, err)
	// The pointer native is one object; the value native has two headers
	// here, but they are one header, so it is shared too.
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#native",["test:counter",1,7]]]],["~#ref",0],`+
		`["~#obj",[1,["~#native",["test:point",2,["~#list",[1,2]]]]]],["~#ref",1]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	require.Len(t, back.Cells, 4)
	assert.Same(t, back.Cells[0].Native, back.Cells[1].Native)
	assert.Equal(t, 7, nativeOf[*counter](back.Cells[0]).n)
	assert.Same(t, back.Cells[2], back.Cells[3])
	assert.Equal(t, point{1, 2}, back.Cells[2].Native)

	// A codec reads an older payload version.
	old, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#native",["test:point",1,5]]]]`), reg)
	require.NoError(t, err)
	assert.Equal(t, point{x: 5}, old.Native)

	// A native inside another native's payload.
	nested := lisp.Native(point{x: 3})
	reg2 := durableTestRegistry(t, func(reg *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[*lisp.LVal](reg, "test:boxed", 1, libjson.NativeFuncs{
			Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return nativeOf[*lisp.LVal](v), nil },
			Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(p), nil },
		}, libjson.WithSharedPayload()))
	})
	box := lisp.Native(nested)
	b, err = libjson.DumpDurable(env, box, reg2)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#native",["test:boxed",1,["~#native",["test:point",2,["~#list",[3,0]]]]]]]]`, string(b))
	_, err = libjson.LoadDurable(env, b, reg2)
	require.NoError(t, err)
}

func TestDurableFunctions(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defun my-cmp (a b) (< a b))`)
	doc := durableRoundTrip(t, env, nil, `(list my-cmp string< 'my-cmp)`)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#fn","user:my-cmp"],["~#fn","lisp:string\u003c"],"~$my-cmp"]]]]`, doc)
	assert.Equal(t, `true`, evalString(t, env, `(funcall (first r) 1 2)`))
	assert.Equal(t, `true`, evalString(t, env, `(funcall (second r) "a" "b")`))

	// A restored function is the current definition of its global.
	evalString(t, env, `(defun my-cmp (a b) (> a b))`)
	back, err := libjson.LoadDurable(env, []byte(doc), nil)
	require.NoError(t, err)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
	assert.Equal(t, `false`, evalString(t, env, `(funcall (first r) 1 2)`))
}

func TestDurableRefusals(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := durableTestRegistry(t)
	self := lisp.SortedMap()
	reg2 := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*lisp.LVal](reg2, "test:boxed", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return nativeOf[*lisp.LVal](v), nil },
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(p), nil },
	}, libjson.WithSharedPayload()))
	reg2.Freeze()
	self.MapSetLVal(lisp.String("box"), lisp.Native(self))
	ownBox := lisp.Native(nil)
	ownBox.Native = ownBox
	failing := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](failing, "test:fail", 1, libjson.NativeFuncs{
		Save: func(*lisp.LEnv, *lisp.LVal) (*lisp.LVal, error) { return nil, errors.New("flush first") },
		Load: func(*lisp.LEnv, int, *lisp.LVal) (*lisp.LVal, error) { return nil, errors.New("unused") },
	}))
	failing.Freeze()
	for _, c := range []struct {
		name string
		v    *lisp.LVal
		reg  *libjson.DurableRegistry
		want string
	}{
		{"builtin no global binds", lisp.Fun("anon", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() }), reg, "durable json: cannot encode an anonymous function"},
		{"closure over a native", closureOver(t, env, lisp.Native(struct{}{})), reg, `durable json: captured variable "captured": no codec registered for native type struct {}`},
		{"macro", env.LoadString("test", `defun`), reg, "durable json: cannot encode a macro or special operator"},
		{"special operator", env.LoadString("test", `if`), reg, "durable json: cannot encode a macro or special operator"},
		{"internal panic", &lisp.LVal{Type: lisp.LError, Str: lisp.CondInternalPanic}, reg, "durable json: cannot encode an internal panic"},
		{"unregistered native", lisp.Native(struct{}{}), reg, "durable json: no codec registered for native type struct {}"},
		{"native with no registry", lisp.Native(&counter{}), nil, "durable json: no codec registered for native type *libjson_test.counter"},
		{"native that holds its enclosing map", self, reg2, `durable json: native "test:boxed" payload refers to a value that encloses the native`},
		{"native that holds itself", ownBox, reg2, `durable json: native "test:boxed" payload refers to a value that encloses the native`},
		{"codec error", lisp.Native(&counter{}), failing, `durable json: native "test:fail": flush first`},
		{"nested quote", &lisp.LVal{Type: lisp.LQuote, Cells: []*lisp.LVal{lisp.Int(1)}}, reg, "typed json: cannot encode a nested quote"},
		{"invalid utf-8", lisp.String("\xff"), reg, "typed json: cannot encode a string that is not valid UTF-8"},
	} {
		t.Run(c.name, func(t *testing.T) {
			_, err := libjson.DumpDurable(env, c.v, c.reg)
			require.Error(t, err)
			assert.Equal(t, c.want, err.Error())
		})
	}
}

func TestLoadDurableRejects(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := durableTestRegistry(t)
	evalString(t, env, `(defun f () 1)`)
	evalString(t, env, `(set 'g f)`)
	evalString(t, env, `(set 'not-fn 1)`)
	for _, c := range []struct{ doc, want string }{
		{`1`, "not a durable document"},
		{`["~#durable",[2,1]]`, "unsupported format version"},
		{`["~#durable",[1,1]]x`, "trailing bytes"},
		{`["~#durable",[1,1] ]`, "expected ']'"},
		{`["~#durable",[1,["~#obj",[1,{}]]]]`, "object id 1 out of sequence"},
		{`["~#durable",[1,["~#obj",[0,{}]]]]`, "object 0 is defined but never referenced"},
		{`["~#durable",[1,["~#ref",0]]]`, "reference to undefined object 0"},
		{`["~#durable",[1,["~#list",[["~#ref",0],["~#obj",[0,{}]]]]]]`, "reference to undefined object 0"},
		{`["~#durable",[1,["~#obj",[0,1]]]]`, "an object must be"},
		{`["~#durable",[1,["~#obj",[0,"s"]]]]`, "an object must be"},
		{`["~#durable",[1,["~#obj",[0,null]]]]`, "an object must be"},
		{`["~#durable",[1,["~#obj",[0,["~#fn","user:g"]]]]]`, "an object must be"},
		{`["~#durable",[1,["~#obj",[0,["~#obj",[1,{}]]]]]]`, "object definition inside another definition"},
		{`["~#durable",[1,["~#obj",[0,["~#ref",0]]]]]`, "an object must be a list, vector, array, map, tagged value, bytes or native, not a reference"},
		{`["~#durable",[1,["~#obj",[0,["~#list",[]]]]]]`, "object 0 is defined but never referenced"},
		{`["~#durable",[1,["~#obj",[-1,{}]]]]`, "expected a nonnegative integer"},
		{`["~#durable",[1,["~#obj",[0.5,{}]]]]`, "expected a nonnegative integer"},
		{`["~#durable",[1,["~#ref","0"]]]`, "expected an integer"},
		{`["~#durable",[1,["~#native",["test:none",1,1]]]]`, `no codec registered for native "test:none"`},
		{`["~#durable",[1,["~#native",["test:counter",2,1]]]]`, `native "test:counter": unsupported version 2`},
		{`["~#durable",[1,["~#native",["test:counter",0,1]]]]`, `native "test:counter": unsupported version 0`},
		{`["~#durable",[1,["~#native",["test:counter",1,"x"]]]]`, `native "test:counter": counter payload is not an int`},
		{`["~#durable",[1,["~#obj",[0,["~#native",["test:point",2,["~#list",[["~#ref",0],1]]]]]]]]`, "does not keep sharing holds a reference"},
		{`["~#durable",[1,["~#obj",[0,{"n":["~#native",["test:point",2,["~#list",[["~#ref",0],1]]]]}]]]]`, "does not keep sharing holds a reference"},
		{`["~#durable",[1,["~#list",[["~#native",["test:point",2,["~#obj",[0,["~#list",[1,2]]]]]],["~#ref",0]]]]]`, "does not keep sharing holds a shared object"},
		{`["~#durable",[1,["~#array",[[["~#native",["test:counter",1,1]],1],[1]]]]]`, "invalid array dimension"},
		{`["~#durable",[1,["~#fn","lisp:defun"]]]`, "the global is not a regular function"},
		{`["~#durable",[1,["~#fn","user:nope"]]]`, "the global is not a regular function"},
		{`["~#durable",[1,["~#fn","user:not-fn"]]]`, "the global is not a regular function"},
		{`["~#durable",[1,["~#fn","nopkg:f"]]]`, "unknown package"},
		{`["~#durable",[1,["~#fn","f"]]]`, "invalid function name"},
		{`["~#durable",[1,["~#fn","user:car"]]]`, "the global holds a function of package lisp"},
		{`["~#durable",[1,["~#durable",[1,1]]]]`, "unknown tag"},
		{`["~#durable",[1, 1]]`, "invalid value"},
		{`["~#durable",[1,1.0]]`, "whole float requires ~d"},
	} {
		t.Run(c.doc, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), reg)
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
	// f and g bind one function; DumpDurable writes the first name.
	doc := durableRoundTrip(t, env, nil, `g`)
	assert.Equal(t, `["~#durable",[1,["~#fn","user:f"]]]`, doc)
}

func TestLoadDurableNativeResultType(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](reg, "test:wrong", 1, libjson.NativeFuncs{
		Save: func(*lisp.LEnv, *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(1), nil },
		Load: func(*lisp.LEnv, int, *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(1), nil },
	}))
	reg.Freeze()
	_, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#native",["test:wrong",1,1]]]]`), reg)
	require.EqualError(t, err, `durable json: native "test:wrong": LoadNative did not return a native *libjson_test.counter`)
}

func TestDurableRegistryRegister(t *testing.T) {
	reg := libjson.NewDurableRegistry()
	codec := libjson.NativeFuncs{}
	require.NoError(t, libjson.RegisterNative[*counter](reg, "a", 1, codec))
	for _, c := range []struct {
		err  error
		want string
	}{
		{libjson.RegisterNative[*counter](reg, "b", 1, codec), `durable json: type *libjson_test.counter is already registered as "a"`},
		{libjson.RegisterNative[point](reg, "a", 1, codec), `durable json: name "a" is already registered`},
		{libjson.RegisterNative[point](reg, "", 1, codec), `durable json: a native name must be a nonempty UTF-8 string`},
		{libjson.RegisterNative[point](reg, "p", 0, codec), `durable json: native "p": version 0 is below 1`},
		{libjson.RegisterNative[point](reg, "p", 1, nil), `durable json: cannot register a nil codec for "p"`},
		{reg.Register(nil, "p", 1, codec), `durable json: cannot register a nil type`},
		{(&libjson.DurableRegistry{}).Register(nil, "p", 1, codec), `durable json: registry is not initialized; use NewDurableRegistry`},
	} {
		assert.EqualError(t, c.err, c.want)
	}
}

// TestDurableLimitsAgree checks that the encoder and the decoder count
// values and depth the same way: the smallest limit that dumps a value is
// the smallest that loads its document.
func TestDurableLimitsAgree(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := durableTestRegistry(t)
	evalString(t, env, `(defun h () 1)`)
	for _, src := range []string{
		`1`, `"s"`, `()`, `(vector)`, `(sorted-map)`,
		`(let ((m (sorted-map "a" (list 1 2)))) (assoc! m "m" m) (list m m (vector m)))`,
		`(let ((v (vector 1))) (append! v v) (list v (vector v)))`,
		`array`,
		`(list h (to-bytes "x") (sorted-map 'k (vector (vector (vector)))))`,
	} {
		v := lisp.Nil()
		if src != `array` {
			v = env.LoadString("test", src)
			require.NoError(t, lisp.GoError(v))
		}
		if src == `array` {
			v = lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(1)}), []*lisp.LVal{lisp.Int(1), lisp.Vector(nil)})
		}
		if src == `1` {
			v = lisp.QExpr([]*lisp.LVal{v, lisp.Native(&counter{n: 1}), lisp.Native(point{1, 2})})
		}
		b, err := libjson.DumpDurable(env, v, reg)
		require.NoError(t, err, src)
		for _, lim := range []struct {
			name string
			opt  func(int) libjson.TypedOption
		}{
			{"values", libjson.WithTypedMaxValues},
			{"depth", libjson.WithTypedMaxDepth},
		} {
			minDump, minLoad := -1, -1
			for n := 0; n < 64 && (minDump < 0 || minLoad < 0); n++ {
				if _, derr := libjson.DumpDurable(env, v, reg, lim.opt(n)); derr == nil && minDump < 0 {
					minDump = n
				}
				if _, lerr := libjson.LoadDurable(env, b, reg, lim.opt(n)); lerr == nil && minLoad < 0 {
					minLoad = n
				}
			}
			assert.Equal(t, minDump, minLoad, "%s limit for %s", lim.name, src)
			assert.GreaterOrEqual(t, minDump, 0)
		}
		_, err = libjson.DumpDurable(env, v, reg, libjson.WithTypedMaxBytes(len(b)-1))
		require.ErrorIs(t, err, libjson.ErrTypedLimit)
		_, err = libjson.LoadDurable(env, b, reg, libjson.WithTypedMaxBytes(len(b)-1))
		require.ErrorIs(t, err, libjson.ErrTypedLimit)
	}
}

func TestDurableCharge(t *testing.T) {
	env := newTypedTestEnv(t)
	v := lisp.String(strings.Repeat("x", 3000))
	total := 0
	b, err := libjson.DumpDurable(env, v, nil, libjson.WithTypedCharge(func(kib int) error {
		total += kib
		return nil
	}))
	require.NoError(t, err)
	assert.Equal(t, (len(b)+1023)/1024, total)
	stop := errors.New("budget")
	_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedCharge(func(int) error { return stop }))
	assert.ErrorIs(t, err, stop)
}

// TestDurableLeavesTypedUnchanged checks that a value DumpTyped refuses
// for sharing is still refused there after durable mode exists, and that
// DumpTyped writes shared structure in full.
func TestDurableLeavesTypedUnchanged(t *testing.T) {
	env := newTypedTestEnv(t)
	cyclic := env.LoadString("test", `(let ((m (sorted-map))) (assoc! m "self" m) m)`)
	_, err := libjson.DumpTyped(cyclic)
	require.EqualError(t, err, "typed json: cannot encode a value that contains itself")
	shared := env.LoadString("test", `(let ((m (sorted-map "a" 1))) (list m m))`)
	b, err := libjson.DumpTyped(shared)
	require.NoError(t, err)
	assert.Equal(t, `["~#list",[{"a":1},{"a":1}]]`, string(b))
	assert.Equal(t, `"[\"~#list\",[{\"a\":1},{\"a\":1}]]"`,
		evalString(t, env, `(let ((m (sorted-map "a" 1))) (json:dump-string (list m m) :typed true))`))
	_, err = libjson.LoadTyped([]byte(`["~#obj",[0,{}]]`))
	require.ErrorContains(t, err, "unknown tag")
}

func ExampleDumpDurable() {
	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env)
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String("self"), m)
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{m, m}), nil)
	fmt.Println(string(b), err)
	back, _ := libjson.LoadDurable(env, b, nil)
	fmt.Println(back.Cells[0] == back.Cells[1])
	// Output:
	// ["~#durable",[1,["~#list",[["~#obj",[0,{"self":["~#ref",0]}]],["~#ref",0]]]]] <nil>
	// true
}

func TestDurableRegistryFrozen(t *testing.T) {
	env := newTypedTestEnv(t)
	codec := libjson.NativeFuncs{
		Save: func(*lisp.LEnv, *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(1), nil },
		Load: func(*lisp.LEnv, int, *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(&counter{}), nil },
	}
	open := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](open, "test:counter", 1, codec))
	_, err := libjson.DumpDurable(env, lisp.Int(1), open)
	require.EqualError(t, err, "durable json: the registry is not frozen; call Freeze after registration")
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,1]]`), open)
	require.EqualError(t, err, "durable json: the registry is not frozen; call Freeze after registration")
	open.Freeze()
	assert.True(t, open.Frozen())
	require.EqualError(t, libjson.RegisterNative[point](open, "test:point", 1, codec),
		`durable json: cannot register "test:point": the registry is frozen`)

	// Registration order changes neither the fingerprint nor the bytes.
	a, b := libjson.NewDurableRegistry(), libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](a, "test:counter", 1, codec))
	require.NoError(t, libjson.RegisterNative[point](a, "test:point", 3, codec))
	require.NoError(t, libjson.RegisterNative[point](b, "test:point", 3, codec))
	require.NoError(t, libjson.RegisterNative[*counter](b, "test:counter", 1, codec))
	a.Freeze()
	b.Freeze()
	// The fingerprint is compared byte for byte, not as JSON.
	pkg := `\"github.com/luthersystems/elps/lisp/lisplib/libjson_test\"`
	wantFingerprint := `[{"name":"test:counter","type":"*github.com/luthersystems/elps/lisp/lisplib/libjson_test.counter",` +
		`"shape":"ptr(github.com/luthersystems/elps/lisp/lisplib/libjson_test.counter=struct(\"n\" ` + pkg + ` false \"\" int))","version":1,"charge":0,"shared":false},` +
		`{"name":"test:point","type":"github.com/luthersystems/elps/lisp/lisplib/libjson_test.point",` +
		`"shape":"struct(\"x\" ` + pkg + ` false \"\" int,\"y\" ` + pkg + ` false \"\" int)","version":3,"charge":0,"shared":false}]`
	assert.Equal(t, wantFingerprint, a.Fingerprint())
	assert.Equal(t, a.Fingerprint(), b.Fingerprint())
	v := lisp.QExpr([]*lisp.LVal{lisp.Native(point{}), lisp.Native(&counter{})})
	da, err := libjson.DumpDurable(env, v, a)
	require.NoError(t, err)
	db, err := libjson.DumpDurable(env, v, b)
	require.NoError(t, err)
	assert.Equal(t, string(da), string(db))
}

func TestDurableNativeCharge(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*counter](reg, "test:counter", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(nativeOf[*counter](v).n), nil },
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
			return lisp.Native(&counter{n: p.Int}), nil
		},
	}, libjson.WithNativeCharge(5)))
	reg.Freeze()
	c := lisp.Native(&counter{n: 1})
	// Two natives, one shared: three codec calls in all would be wrong.
	v := lisp.QExpr([]*lisp.LVal{c, c, lisp.Native(&counter{n: 2})})
	var charges []int
	record := libjson.WithTypedCharge(func(n int) error {
		charges = append(charges, n)
		return nil
	})
	b, err := libjson.DumpDurable(env, v, reg, record)
	require.NoError(t, err)
	assert.Equal(t, []int{5, 5, 1}, charges, "two saves, then one KiB of output")
	charges = nil
	_, err = libjson.LoadDurable(env, b, reg, record)
	require.NoError(t, err)
	assert.Equal(t, []int{1, 5, 5}, charges, "one KiB of input, then two loads")

	stop := errors.New("step budget exceeded")
	refuse := libjson.WithTypedCharge(func(n int) error {
		if n == 5 {
			return stop
		}
		return nil
	})
	_, err = libjson.DumpDurable(env, v, reg, refuse)
	require.ErrorIs(t, err, stop)
	_, err = libjson.LoadDurable(env, b, reg, refuse)
	require.ErrorIs(t, err, stop)
	_, err = libjson.LoadDurable(env, b, reg, libjson.WithTypedCharge(func(int) error { return stop }))
	require.ErrorIs(t, err, stop)
}

func TestDurableAllocationCap(t *testing.T) {
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(64))
	v := lisp.String(strings.Repeat("x", 100))
	_, err := libjson.DumpDurable(env, v, nil)
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	// A caller option cannot raise the cap.
	_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxBytes(1<<20))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	doc := []byte(`["~#durable",[1,"` + strings.Repeat("x", 100) + `"]]`)
	_, err = libjson.LoadDurable(env, doc, nil, libjson.WithTypedMaxBytes(1<<20))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}

func TestDurableRoots(t *testing.T) {
	env := newTypedTestEnv(t)
	m := lisp.SortedMap()
	roots := []libjson.DurableRoot{
		{Name: "order", Value: m},
		{Name: "alias", Value: m},
		{Name: "n", Value: lisp.Int(3)},
	}
	b, err := libjson.DumpDurableRoots(env, roots, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",["order",["~#obj",[0,{}]],"alias",["~#ref",0],"n",3]]]]`, string(b))
	back, err := libjson.LoadDurableRoots(env, b, nil)
	require.NoError(t, err)
	require.Len(t, back, 3)
	assert.Equal(t, []string{"order", "alias", "n"}, []string{back[0].Name, back[1].Name, back[2].Name})
	assert.Same(t, back[0].Value, back[1].Value)

	empty, err := libjson.DumpDurableRoots(env, nil, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,null]]`, string(empty))
	none, err := libjson.LoadDurableRoots(env, empty, nil)
	require.NoError(t, err)
	assert.Empty(t, none)

	_, err = libjson.DumpDurableRoots(env, []libjson.DurableRoot{{Name: "a", Value: m}, {Name: "a", Value: m}}, nil)
	require.EqualError(t, err, `durable json: root "a" appears twice`)
	_, err = libjson.DumpDurableRoots(env, []libjson.DurableRoot{{Name: "", Value: m}}, nil)
	require.EqualError(t, err, "durable json: a root name must be a nonempty UTF-8 string")
	for doc, want := range map[string]string{
		`["~#durable",[1,["~#obj",[0,["~#list",["a",["~#ref",0]]]]]]]`: "durable json: the root list is a shared object",
		`["~#durable",[1,["~#list",["a"]]]]`:                           "durable json: not a list of named roots",
		`["~#durable",[1,{}]]`:                                         "durable json: not a list of named roots",
		`["~#durable",[1,["~#list",["~$a",1]]]]`:                       "durable json: a root name is not a nonempty string",
		`["~#durable",[1,["~#list",["a",1,"a",2]]]]`:                   `durable json: root "a" appears twice`,
	} {
		_, err := libjson.LoadDurableRoots(env, []byte(doc), nil)
		require.EqualError(t, err, want, doc)
	}
}
