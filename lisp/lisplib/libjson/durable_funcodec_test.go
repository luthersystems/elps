// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

// newStoreObj is a constructor of Go objects over a state map, the shape a
// substrate storage object has: (obj 'put k v), (obj 'get k), (obj 'store).
func newStoreObj(state *lisp.LVal) *lisp.LVal {
	return lisp.NewCapturedFun(lisp.CapturedFun{
		Package: lisp.DefaultUserPackage, FID: "test-store", Name: "test:store",
		Formals:  lisp.Formals("op", lisp.VarArgSymbol, "args"),
		Captures: state,
		Eval:     storeObjEval,
	})
}

func storeObjEval(env *lisp.LEnv, args, state *lisp.LVal) *lisp.LVal {
	switch args.Cells[0].Str {
	case "put":
		if _, err := env.MapPut(state, args.Cells[1], args.Cells[2]); err != nil {
			return env.Errorf("%v", err)
		}
		return lisp.Nil()
	case "get":
		v, err := env.MapLookup(state, args.Cells[1])
		if err != nil {
			return lisp.Nil()
		}
		return v
	}
	return env.Errorf("unknown op %v", args.Cells[0])
}

var storeCodec = libjson.DurableFunCodec{
	Package: lisp.DefaultUserPackage, FID: "test-store", Name: "test:store", Version: 1,
	Load: func(_ *lisp.LEnv, _ int, captures *lisp.LVal) (*lisp.LVal, error) {
		if captures.Type != lisp.LSortMap {
			return nil, fmt.Errorf("test store: captures are a %v, want a sorted map", captures.Type)
		}
		return newStoreObj(captures), nil
	},
}

func storeRegistry(t *testing.T, codecs ...libjson.DurableEntry) *libjson.DurableRegistry {
	t.Helper()
	reg, err := libjson.NewFrozenDurableRegistry(append([]libjson.DurableEntry{storeCodec}, codecs...)...)
	require.NoError(t, err)
	return reg
}

func call(t *testing.T, env *lisp.LEnv, fun *lisp.LVal, args ...*lisp.LVal) *lisp.LVal {
	t.Helper()
	v := env.FunCall(fun, lisp.QExpr(args))
	require.NoError(t, lisp.GoError(v))
	return v
}

// TestDurableFunCodecRoundTrip saves a value that holds one Go object twice
// and its state once.  The load gives one object over one state map, the
// object works, and a second save writes the same bytes.
func TestDurableFunCodecRoundTrip(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	reg := storeRegistry(t)
	state := lisp.SortedMap()
	obj := newStoreObj(state)
	call(t, env, obj, lisp.Symbol("put"), lisp.String("k"), lisp.Int(7))

	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{obj, obj, state}), reg)
	require.NoError(t, err)
	require.Contains(t, string(b), `["~#native",["test:store",1,`)
	got, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	o1, o2, st := got.Cells[0], got.Cells[1], got.Cells[2]
	require.Same(t, o1, o2, "the two references load as two objects")
	require.Equal(t, "7", call(t, env, o1, lisp.Symbol("get"), lisp.String("k")).String())
	// The object's state and the saved map are one map after the load.
	call(t, env, o1, lisp.Symbol("put"), lisp.String("k"), lisp.Int(8))
	v, ok := st.Map().Get(lisp.String("k"))
	require.True(t, ok)
	require.Equal(t, "8", v.String())
	require.Equal(t, "#<test:store>", o1.String())

	call(t, env, o1, lisp.Symbol("put"), lisp.String("k"), lisp.Int(7))
	b2, err := libjson.DumpDurable(env, got, reg)
	require.NoError(t, err)
	require.Equal(t, string(b), string(b2))
}

// TestDurableFunCodecNested saves an object whose state holds another
// object, and an object a closure captures.
func TestDurableFunCodecNested(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	reg := storeRegistry(t)
	inner := newStoreObj(lisp.SortedMap())
	call(t, env, inner, lisp.Symbol("put"), lisp.String("n"), lisp.Int(1))
	outer := newStoreObj(lisp.SortedMap())
	call(t, env, outer, lisp.Symbol("put"), lisp.String("inner"), inner)
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("o"), outer)))
	closure := env.LoadString("test", `(let ([x o]) (lambda () ((x 'get "inner") 'get "n")))`)
	require.NoError(t, lisp.GoError(closure))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("o"), lisp.Nil())))

	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{closure, outer}), reg)
	require.NoError(t, err)
	got, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	require.Equal(t, "1", call(t, env, got.Cells[0]).String())
	loadedInner := call(t, env, got.Cells[1], lisp.Symbol("get"), lisp.String("inner"))
	call(t, env, loadedInner, lisp.Symbol("put"), lisp.String("n"), lisp.Int(2))
	require.Equal(t, "2", call(t, env, got.Cells[0]).String(), "the closure and the outer object load two inner objects")
}

// TestDurableFunCodecRefuses checks that a Go object with no codec is
// refused as before, and that a codec whose Load does not rebuild the
// object over its captures fails the load.
func TestDurableFunCodecRefuses(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	obj := newStoreObj(lisp.SortedMap())
	_, err := libjson.DumpDurable(env, obj, nil)
	require.EqualError(t, err, "durable json: cannot encode an anonymous function")
	other, err := libjson.NewFrozenDurableRegistry(libjson.DurableFunCodec{
		Package: lisp.DefaultUserPackage, FID: "other", Name: "test:other", Version: 1, Load: storeCodec.Load,
	})
	require.NoError(t, err)
	_, err = libjson.DumpDurable(env, obj, other)
	require.EqualError(t, err, "durable json: cannot encode an anonymous function")

	b, err := libjson.DumpDurable(env, obj, storeRegistry(t))
	require.NoError(t, err)
	for name, load := range map[string]func(*lisp.LEnv, int, *lisp.LVal) (*lisp.LVal, error){
		"copied captures": func(_ *lisp.LEnv, _ int, c *lisp.LVal) (*lisp.LVal, error) {
			return newStoreObj(lisp.SortedMap()), nil
		},
		"another function": func(_ *lisp.LEnv, _ int, c *lisp.LVal) (*lisp.LVal, error) {
			return lisp.NewCapturedFun(lisp.CapturedFun{Package: lisp.DefaultUserPackage, FID: "other", Captures: c, Eval: storeObjEval}), nil
		},
		"not a function": func(_ *lisp.LEnv, _ int, c *lisp.LVal) (*lisp.LVal, error) { return c, nil },
		"error":          func(_ *lisp.LEnv, _ int, _ *lisp.LVal) (*lisp.LVal, error) { return nil, errors.New("bad") },
	} {
		bad := storeCodec
		bad.Load = load
		reg, err := libjson.NewFrozenDurableRegistry(bad)
		require.NoError(t, err)
		_, err = libjson.LoadDurable(env, b, reg)
		require.Error(t, err, name)
		require.Contains(t, err.Error(), `"test:store"`, name)
	}
}

func TestDurableFunCodecRegistration(t *testing.T) {
	t.Parallel()
	for name, tc := range map[string]struct {
		codecs []libjson.DurableEntry
		err    string
	}{
		"no load":       {[]libjson.DurableEntry{libjson.DurableFunCodec{Package: "user", FID: "f", Name: "n", Version: 1}}, "needs a Load function"},
		"no fid":        {[]libjson.DurableEntry{libjson.DurableFunCodec{Package: "user", Name: "n", Version: 1, Load: storeCodec.Load}}, "needs a package and an FID"},
		"no name":       {[]libjson.DurableEntry{libjson.DurableFunCodec{Package: "user", FID: "f", Version: 1, Load: storeCodec.Load}}, "nonempty UTF-8"},
		"version":       {[]libjson.DurableEntry{libjson.DurableFunCodec{Package: "user", FID: "f", Name: "n", Load: storeCodec.Load}}, "below 1"},
		"charge":        {[]libjson.DurableEntry{libjson.DurableFunCodec{Package: "user", FID: "f", Name: "n", Version: 1, Charge: -1, Load: storeCodec.Load}}, "negative charge"},
		"same function": {[]libjson.DurableEntry{storeCodec, libjson.DurableFunCodec{Package: "user", FID: "test-store", Name: "n", Version: 1, Load: storeCodec.Load}}, "already registered"},
		"same name":     {[]libjson.DurableEntry{storeCodec, libjson.DurableCodec[*codecHandle]{Name: "test:store", Version: 1, Save: codecSave, Load: codecLoad}}, "already registered"},
	} {
		_, err := libjson.NewFrozenDurableRegistry(tc.codecs...)
		require.ErrorContains(t, err, tc.err, name)
	}
}

// TestDurableFunCodecFingerprint pins that a function codec changes the
// Fingerprint, which a flow manifest holds, and that its version does too.
func TestDurableFunCodecFingerprint(t *testing.T) {
	t.Parallel()
	base, err := libjson.NewFrozenDurableRegistry(handleCodec)
	require.NoError(t, err)
	with := storeRegistry(t, handleCodec)
	v2 := storeCodec
	v2.Version = 2
	with2, err := libjson.NewFrozenDurableRegistry(v2, handleCodec)
	require.NoError(t, err)
	require.NotEqual(t, base.Fingerprint(), with.Fingerprint())
	require.NotEqual(t, with.Fingerprint(), with2.Fingerprint())
	require.Equal(t, with.Fingerprint(), storeRegistry(t, handleCodec).Fingerprint())
}

// TestDurableFunCodecRoots saves Go objects as named roots, as a defflow
// run saves its locals, and checks what DurableSharing reports for them: a
// Go object is shared like a native and is never a kept closure, since it
// has no Lisp code to check.
func TestDurableFunCodecRoots(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	reg := storeRegistry(t)
	obj := newStoreObj(lisp.SortedMap())
	call(t, env, obj, lisp.Symbol("put"), lisp.String("k"), lisp.Int(3))
	roots := []libjson.DurableRoot{{Name: "a", Value: obj}, {Name: "b", Value: obj}}
	b, err := libjson.DumpDurableRoots(env, roots, reg)
	require.NoError(t, err)
	got, err := libjson.LoadDurableRoots(env, b, reg)
	require.NoError(t, err)
	require.Len(t, got, 2)
	require.Same(t, got[0].Value, got[1].Value)
	require.Equal(t, "3", call(t, env, got[1].Value, lisp.Symbol("get"), lisp.String("k")).String())

	fresh := newStoreObj(lisp.SortedMap())
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{
		Old: []libjson.DurableRoot{{Name: "old", Value: obj}},
		New: []libjson.DurableRoot{{Name: "same", Value: obj}, {Name: "fresh", Value: fresh}},
	}, reg)
	require.NoError(t, err)
	require.Equal(t, []libjson.RootSharing{
		{Name: "same", Shared: true},
		{Name: "fresh"},
	}, s.Roots)
	require.Empty(t, s.Closures)
}

// TestDurableFunCodecKeyIsPackageAndFID checks that a codec matches the
// package and FID as a pair: package "a" with FID "b:c" is not package
// "a:b" with FID "c".
func TestDurableFunCodecKeyIsPackageAndFID(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	reg, err := libjson.NewFrozenDurableRegistry(libjson.DurableFunCodec{
		Package: "a", FID: "b:c", Name: "test:a", Version: 1, Load: storeCodec.Load,
	})
	require.NoError(t, err)
	obj := lisp.NewCapturedFun(lisp.CapturedFun{Package: "a:b", FID: "c", Captures: lisp.SortedMap(), Eval: storeObjEval})
	_, err = libjson.DumpDurable(env, obj, reg)
	require.EqualError(t, err, "durable json: cannot encode an anonymous function")
}

// TestDurableFunCodecNoCaptures checks that a value of a codec's package
// and FID with no captures is refused by name.
func TestDurableFunCodecNoCaptures(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	obj := lisp.NewCapturedFun(lisp.CapturedFun{Package: lisp.DefaultUserPackage, FID: "test-store", Eval: storeObjEval})
	_, err := libjson.DumpDurable(env, obj, storeRegistry(t))
	require.EqualError(t, err, `durable json: function codec "test:store": user:test-store has no captures to save`)
}

// TestDurableFunCodecGlobalWithoutCodec pins the documented hazard: with no
// codec, an object a global binds saves as a reference to the global, and
// loads as the global's value at load time.
func TestDurableFunCodecGlobalWithoutCodec(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	obj := newStoreObj(lisp.SortedMap())
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("the-store"), obj)))
	b, err := libjson.DumpDurable(env, obj, nil)
	require.NoError(t, err)
	require.Equal(t, `["~#durable",[1,["~#fn","user:the-store"]]]`, string(b))
	// With the codec, the same value saves its state.
	b, err = libjson.DumpDurable(env, obj, storeRegistry(t))
	require.NoError(t, err)
	require.Contains(t, string(b), `"test:store"`)
}
