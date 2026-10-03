// Copyright © 2026 The ELPS authors

package libjson_test

// Regression tests for the reviews of luthersystems/elps#796.

import (
	"errors"
	"runtime"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// allocatedBy returns the bytes f allocates.
func allocatedBy(f func()) uint64 {
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	f()
	runtime.ReadMemStats(&after)
	return after.TotalAlloc - before.TotalAlloc
}

// Two builtins of one short name in different packages share an FID.
func TestDurableFunctionsOfOneNameInTwoPackages(t *testing.T) {
	env := newTypedTestEnv(t)
	doc := durableRoundTrip(t, env, nil, `(list lisp:not s:not)`)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#fn","lisp:not"],["~#fn","s:not"]]]]]`, doc)
	assert.Equal(t, `true`, evalString(t, env, `(funcall (first r) false)`))
	r := env.LoadString("test", `r`)
	assert.Equal(t, "lisp", r.Cells[0].Package())
	assert.Equal(t, "s", r.Cells[1].Package())
}

// The saved name does not depend on which alias was bound last.
func TestDurableFunctionNameIgnoresAliasHistory(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defun foo () 1)`)
	evalString(t, env, `(set 'zed foo)`)
	evalString(t, env, `(set 'zed 5)`)
	// zed was bound last, then rebound to a number; foo still binds it.
	doc := durableRoundTrip(t, env, nil, `foo`)
	assert.Equal(t, `["~#durable",[1,["~#fn","user:foo"]]]`, doc)
	// A smaller alias becomes the written name, and the old document
	// still loads.
	evalString(t, env, `(set 'bar foo)`)
	assert.Equal(t, `["~#durable",[1,["~#fn","user:bar"]]]`, durableRoundTrip(t, env, nil, `foo`))
	back, err := libjson.LoadDurable(env, []byte(doc), nil)
	require.NoError(t, err)
	assert.Equal(t, `1`, func() string {
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), back)))
		return evalString(t, env, `(funcall r)`)
	}())
}

func TestDurableRefusesOverlappingStorage(t *testing.T) {
	env := newTypedTestEnv(t)
	overlap := "durable json: two values share storage (a list and its tail, or a slice of a vector); copy one of them before saving"
	for _, src := range []string{
		`(let* ((xs (list 0 3 2 1)) (tail (rest xs))) (list xs tail))`,
		`(let* ((xs (list 0 3 2 1)) (tail (cdr xs))) (vector tail xs))`,
		`(let* ((v (vector 1 2 3)) (w (slice 'vector v 1 3))) (list v w))`,
		`(let* ((xs (list 0 3 2 1))) (list xs (slice 'list xs 0 2)))`,
	} {
		v := env.LoadString("test", src)
		require.NoError(t, lisp.GoError(v), src)
		_, err := libjson.DumpDurable(env, v, nil)
		require.EqualError(t, err, overlap, src)
	}
	// A vector's data list held as a list.
	vec := lisp.Vector([]*lisp.LVal{lisp.Int(1)})
	_, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{vec, vec.Cells[1]}), nil)
	require.EqualError(t, err, overlap)
	// Two headers over the same cells are one list, so they stay shared.
	l := lisp.QExpr([]*lisp.LVal{lisp.Int(3), lisp.Int(1)})
	alias := lisp.SExpr(l.Cells)
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{l, alias}), nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#list",[3,1]]]],["~#ref",0]]]]]`, string(b))
	// Disjoint parts of one array are separate values.
	cells := []*lisp.LVal{lisp.Int(1), lisp.Int(2)}
	_, err = libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{lisp.QExpr(cells[:1]), lisp.QExpr(cells[1:])}), nil)
	require.NoError(t, err)
}

func TestDurableMapLimitBeforeCopy(t *testing.T) {
	env := newTypedTestEnv(t)
	m := lisp.SortedMap()
	for i := range 100000 {
		m.MapSetLVal(lisp.String(strconv.Itoa(i)), lisp.Int(i))
	}
	var err error
	n := allocatedBy(func() { _, err = libjson.DumpDurable(env, m, nil, libjson.WithTypedMaxValues(10)) })
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Less(t, n, uint64(64<<10), "bytes allocated before the value limit")
	big := lisp.SortedMap()
	big.MapSetLVal(lisp.String(strings.Repeat("k", 1<<20)), lisp.Int(1))
	n = allocatedBy(func() { _, err = libjson.DumpDurable(env, big, nil, libjson.WithTypedMaxBytes(1024)) })
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Less(t, n, uint64(64<<10), "bytes allocated before the byte limit")
}

func TestDurableEscapedOutputLimitBeforeWrite(t *testing.T) {
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(64<<10))
	for _, v := range []*lisp.LVal{
		lisp.String(strings.Repeat("<", 30000)),
		lisp.Symbol(strings.Repeat("<", 30000)),
		&lisp.LVal{Type: lisp.LTaggedVal, Str: strings.Repeat("<", 30000), Cells: []*lisp.LVal{lisp.Int(1)}},
	} {
		var err error
		n := allocatedBy(func() { _, err = libjson.DumpDurable(env, v, nil) })
		require.ErrorIs(t, err, libjson.ErrTypedLimit)
		assert.Less(t, n, uint64(64<<10), "bytes allocated past the cap")
		n = allocatedBy(func() { _, err = libjson.DumpTyped(v, libjson.WithTypedMaxBytes(64<<10)) })
		require.ErrorIs(t, err, libjson.ErrTypedLimit)
		assert.Less(t, n, uint64(64<<10), "typed: bytes allocated past the cap")
	}
	// The exact size is accepted at the limit.
	s := lisp.String(strings.Repeat("<", 100))
	typed, err := libjson.DumpTyped(s)
	require.NoError(t, err)
	_, err = libjson.DumpTyped(s, libjson.WithTypedMaxBytes(len(typed)))
	require.NoError(t, err)
	_, err = libjson.DumpTyped(s, libjson.WithTypedMaxBytes(len(typed)-1))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	kw := lisp.Symbol(":" + strings.Repeat("<", 10))
	typed, err = libjson.DumpTyped(kw)
	require.NoError(t, err)
	_, err = libjson.DumpTyped(kw, libjson.WithTypedMaxBytes(len(typed)))
	require.NoError(t, err)
}

func TestDurableRootsCheckLimitsBeforeAllocating(t *testing.T) {
	env := newTypedTestEnv(t)
	roots := make([]libjson.DurableRoot, 1<<20)
	for i := range roots {
		roots[i] = libjson.DurableRoot{Name: "r", Value: lisp.Int(1)}
	}
	var err error
	n := allocatedBy(func() { _, err = libjson.DumpDurableRoots(env, roots, nil, libjson.WithTypedMaxValues(10)) })
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Less(t, n, uint64(64<<10))
	_, err = libjson.DumpDurableRoots(nil, roots, nil)
	require.EqualError(t, err, "durable json: DumpDurableRoots needs an environment")
}

func TestDurableFingerprintIsUnambiguous(t *testing.T) {
	codec := libjson.NativeFuncs{}
	build := func(f func(*libjson.DurableRegistry)) string {
		r := libjson.NewDurableRegistry()
		f(r)
		r.Freeze()
		return r.Fingerprint()
	}
	cheap := build(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[*counter](r, "c", 1, codec))
	})
	dear := build(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[*counter](r, "c", 1, codec, libjson.WithNativeCharge(9)))
	})
	assert.NotEqual(t, cheap, dear)
	two := build(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[int](r, "a", 1, codec))
		require.NoError(t, libjson.RegisterNative[string](r, "b", 1, codec))
	})
	one := build(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[string](r, "a@1=int;b", 1, codec))
	})
	assert.NotEqual(t, two, one)
	// The fingerprint is compared byte for byte, not as JSON.
	wantFingerprint := `[{"name":"a","type":"int","version":1,"charge":0,"shared":false},{"name":"b","type":"string","version":1,"charge":0,"shared":false}]`
	assert.Equal(t, wantFingerprint, two)
}

type refMap map[string]int

func TestDurableReferenceKindNatives(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[refMap](reg, "test:map", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(len(nativeOf[refMap](v))), nil },
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(refMap{}), nil },
	}))
	codec := libjson.NativeFuncs{}
	require.EqualError(t, libjson.RegisterNative[func()](reg, "test:func", 1, codec),
		`durable json: native "test:func": a func payload has no identity; register a pointer type`)
	require.EqualError(t, libjson.RegisterNative[[]int](reg, "test:slice", 1, codec),
		`durable json: native "test:slice": a slice payload has no identity; register a pointer type`)
	reg.Freeze()
	m := refMap{}
	// Two headers over one Go map are one native.
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{lisp.Native(m), lisp.Native(m)}), reg)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#native",["test:map",1,0]]]],["~#ref",0]]]]]`, string(b))
}

func TestDurableRegisterRejectsVersionAbove2To53(t *testing.T) {
	if strconv.IntSize < 64 {
		t.Skip("int is 32 bits")
	}
	reg := libjson.NewDurableRegistry()
	v := 1<<53 + 1
	require.EqualError(t, libjson.RegisterNative[*counter](reg, "c", v, libjson.NativeFuncs{}),
		`durable json: native "c": version `+strconv.Itoa(v)+` is above 2^53`)
	require.NoError(t, libjson.RegisterNative[*counter](reg, "c", 1<<53, libjson.NativeFuncs{}))
}

// countingRegistry counts codec calls.
func countingRegistry(t *testing.T, shared bool) (*libjson.DurableRegistry, *int) {
	t.Helper()
	calls := new(int)
	opts := []libjson.NativeOption{}
	if shared {
		opts = append(opts, libjson.WithSharedPayload())
	}
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*boxed](reg, "test:box", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { *calls++; return nativeOf[*boxed](v).v, nil },
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
			*calls++
			if p.Type == lisp.LArray && len(p.Cells) == 2 && len(p.Cells[1].Cells) == 0 {
				return nil, errors.New("payload is unfinished")
			}
			return lisp.Native(&boxed{p}), nil
		},
	}, opts...))
	reg.Freeze()
	return reg, calls
}

// A native's payload that reaches an enclosing object only through a
// finished object is refused too.
func TestDurableIndirectCycleThroughNative(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, calls := countingRegistry(t, true)
	doc := `["~#durable",[1,["~#obj",[0,[["~#obj",[1,["~#list",[["~#ref",0]]]]],["~#native",["test:box",1,["~#ref",1]]]]]]]]`
	_, err := libjson.LoadDurable(env, []byte(doc), reg)
	require.ErrorContains(t, err, `native "test:box" payload refers to object 0, which encloses the native`)
	assert.Zero(t, *calls, "LoadNative ran on an unfinished payload")

	v := lisp.Vector([]*lisp.LVal{lisp.Nil(), lisp.Nil()})
	l := lisp.QExpr([]*lisp.LVal{v})
	v.Cells[1].Cells[0] = l
	v.Cells[1].Cells[1] = lisp.Native(&boxed{l})
	_, err = libjson.DumpDurable(env, v, reg)
	require.EqualError(t, err, `durable json: native "test:box" payload refers to a value that encloses the native`)
	// A container-only cycle is fine.
	v.Cells[1].Cells[1] = lisp.Int(1)
	_, err = libjson.DumpDurable(env, v, reg)
	require.NoError(t, err)
}

// A codec that rebuilds its payload cannot keep sharing, so sharing in or
// out of its payload is refused.
func TestDurableTreePayloadRefusesSharing(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, _ := countingRegistry(t, false)
	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	for _, v := range []*lisp.LVal{
		lisp.QExpr([]*lisp.LVal{lisp.Native(&boxed{shared}), shared}),
		lisp.QExpr([]*lisp.LVal{shared, lisp.Native(&boxed{shared})}),
		lisp.Native(&boxed{lisp.QExpr([]*lisp.LVal{shared, shared})}),
	} {
		_, err := libjson.DumpDurable(env, v, reg)
		require.EqualError(t, err, `durable json: native "test:box" payload shares a value, and its codec does not keep sharing`)
	}
	_, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#list",[["~#native",["test:box",1,["~#obj",[0,["~#list",[1,2]]]]]],["~#ref",0]]]]]`), reg)
	require.ErrorContains(t, err, "does not keep sharing holds a shared object")
	b, err := libjson.DumpDurable(env, lisp.Native(&boxed{lisp.QExpr([]*lisp.LVal{shared, lisp.Int(2)})}), reg)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#native",["test:box",1,["~#list",[["~#list",[1]],2]]]]]]`, string(b))
}

// Array dimensions are read on the scalar path: no codec runs there.
func TestLoadDurableDimsRunNoCodec(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, calls := countingRegistry(t, true)
	_, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#array",[[["~#native",["test:box",1,1]],1],[1]]]]]`), reg)
	require.ErrorContains(t, err, "invalid array dimension")
	assert.Zero(t, *calls)
}

// Restored values are fresh and mutable, even when the saved value was a
// sealed program literal.
func TestDurableRestoresLiteralsMutable(t *testing.T) {
	env := newTypedTestEnv(t)
	lit := env.LoadString("test", `(defun lit () '(3 2 1)) (lit)`)
	require.True(t, lit.IsSealed())
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("before"), lit)))
	assert.Contains(t, evalString(t, env, `(handler-bind ((condition (lambda (c &rest _) (to-string c)))) (stable-sort < before))`), "modify")
	b, err := libjson.DumpDurable(env, lit, nil)
	require.NoError(t, err)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.False(t, back.IsSealed())
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("after"), back)))
	assert.Equal(t, `'(1 2 3)`, evalString(t, env, `(stable-sort < after)`))
}

// Independent expected counts: a limit one below the count fails, and no
// codec runs before a limit error.
func TestDurableLimitCountsPinned(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, c := range []struct {
		src    string
		values int
		depth  int
	}{
		{`1`, 1, 0},
		{`(sorted-map "a" 1 "b" 2)`, 5, 1},               // map, 2 keys, 2 values
		{`(let ((m (sorted-map))) (list m m))`, 4, 2},    // list, obj wrapper, map, ref
		{`(vector (vector (vector)))`, 3, 3},             // three vectors
		{`(list 'a :b "c")`, 4, 1},                       // list and three leaves
		{`(let ((v (vector 1))) (append! v v) v)`, 4, 1}, // obj, vector, 1, ref
	} {
		v := env.LoadString("test", c.src)
		require.NoError(t, lisp.GoError(v))
		b, err := libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxValues(c.values), libjson.WithTypedMaxDepth(c.depth))
		require.NoError(t, err, c.src)
		_, err = libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(c.values), libjson.WithTypedMaxDepth(c.depth))
		require.NoError(t, err, c.src)
		_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxValues(c.values-1))
		require.ErrorIs(t, err, libjson.ErrTypedLimit, c.src)
		_, err = libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(c.values-1))
		require.ErrorIs(t, err, libjson.ErrTypedLimit, c.src)
		if c.depth > 0 {
			_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxDepth(c.depth-1))
			require.ErrorIs(t, err, libjson.ErrTypedLimit, c.src)
			_, err = libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxDepth(c.depth-1))
			require.ErrorIs(t, err, libjson.ErrTypedLimit, c.src)
		}
	}
	reg, calls := countingRegistry(t, true)
	natives := lisp.QExpr([]*lisp.LVal{lisp.Native(&boxed{lisp.Int(1)}), lisp.Native(&boxed{lisp.Int(2)})})
	_, err := libjson.DumpDurable(env, natives, reg, libjson.WithTypedMaxValues(1))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Zero(t, *calls, "SaveNative ran before the value limit")
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#list",[["~#native",["test:box",1,1]]]]]]`), reg, libjson.WithTypedMaxValues(2))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Zero(t, *calls, "LoadNative ran before the value limit")
}

// A package that binds another package's function of the same FID does not
// change the name of its own function.
func TestDurableFunctionNameNeedsDefiningPackage(t *testing.T) {
	env := newTypedTestEnv(t)
	v := env.LoadString("test", `(in-package 's) (lisp:set 'aaa lisp:not) (lisp:in-package 'user) s:not`)
	require.NoError(t, lisp.GoError(v))
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#fn","s:not"]]]`, string(b))
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#fn","s:aaa"]]]`), nil)
	require.ErrorContains(t, err, "function s:aaa: the global holds a function of package lisp")
}
