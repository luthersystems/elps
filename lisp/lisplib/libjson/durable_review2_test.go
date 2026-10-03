// Copyright © 2026 The ELPS authors

package libjson_test

// Regression tests for the reviews of luthersystems/elps#797.

import (
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// A native payload that reaches an unfinished object through a chain of
// finished objects is refused.
func TestDurableChainedLowLinks(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, calls := countingRegistry(t, true)
	doc := `["~#durable",[1,["~#obj",[0,[["~#obj",[1,["~#list",[["~#obj",[2,["~#list",[["~#ref",1]]]]],["~#ref",0]]]]],["~#native",["test:box",1,["~#ref",2]]]]]]]]`
	_, err := libjson.LoadDurable(env, []byte(doc), reg)
	require.ErrorContains(t, err, `native "test:box" payload refers to object 0, which encloses the native`)
	assert.Zero(t, *calls, "LoadNative ran on an unfinished payload")

	// The same graph: v = [l1, box(l2)], l1 = (l2 v), l2 = (l1).
	v := lisp.Vector([]*lisp.LVal{lisp.Nil(), lisp.Nil()})
	l1 := lisp.QExpr([]*lisp.LVal{lisp.Nil(), v})
	l2 := lisp.QExpr([]*lisp.LVal{l1})
	l1.Cells[0] = l2
	v.Cells[1].Cells[0] = l1
	v.Cells[1].Cells[1] = lisp.Native(&boxed{l2})
	_, err = libjson.DumpDurable(env, v, reg)
	require.EqualError(t, err, `durable json: native "test:box" payload refers to a value that encloses the native`)
}

// A dimension above 2^53 is a "~n" string, and it loads.
func TestDurableArrayDimsAbove2To53(t *testing.T) {
	if strconv.IntSize < 64 {
		t.Skip("int is 32 bits")
	}
	env := newTypedTestEnv(t)
	n, err := strconv.ParseInt("9007199254740993", 10, 64)
	require.NoError(t, err)
	a := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(0), lisp.Int(int(n))}), nil)
	b, err := libjson.DumpDurable(env, a, nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#array",[[0,"~n9007199254740993"],[]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#array",[[0,"~bAA=="],[]]]]]`), nil)
	require.ErrorContains(t, err, "invalid array dimension")
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#array",[[0,{}],[]]]]]`), nil)
	require.ErrorContains(t, err, "invalid array dimension")
}

// The keys of many maps are summed during the first pass.
func TestDurableCumulativeKeyBytes(t *testing.T) {
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(65536))
	m := lisp.SortedMap()
	for i := range 1000 {
		m.MapSetLVal(lisp.String(strconv.Itoa(i)+strings.Repeat("k", 60000)), lisp.Int(i))
	}
	var err error
	n := allocatedBy(func() { _, err = libjson.DumpDurable(env, m, nil) })
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	assert.Less(t, n, uint64(1<<20), "bytes allocated before the byte limit")
	// Keys spread over many small maps are summed too.
	maps := make([]*lisp.LVal, 100)
	for i := range maps {
		maps[i] = lisp.SortedMap()
		maps[i].MapSetLVal(lisp.String(strings.Repeat("k", 1000)), lisp.Int(i))
	}
	_, err = libjson.DumpDurable(env, lisp.QExpr(maps), nil)
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}

type (
	namedChan     chan int
	namedRecvChan <-chan int
)

// Only named types (and pointers to them) register, so the fingerprint's
// qualified type name identifies the type, channel direction included.
func TestDurableRegisterNeedsNamedType(t *testing.T) {
	codec := libjson.NativeFuncs{}
	reg := libjson.NewDurableRegistry()
	require.EqualError(t, libjson.RegisterNative[chan int](reg, "c", 1, codec),
		`durable json: native "c": type chan int is not named; declare a named type for it`)
	require.EqualError(t, libjson.RegisterNative[<-chan int](reg, "c", 1, codec),
		`durable json: native "c": type <-chan int is not named; declare a named type for it`)
	require.EqualError(t, libjson.RegisterNative[*struct{ x int }](reg, "c", 1, codec),
		`durable json: native "c": type *struct { x int } is not named; declare a named type for it`)
	fp := func(f func(*libjson.DurableRegistry)) string {
		r := libjson.NewDurableRegistry()
		f(r)
		r.Freeze()
		return r.Fingerprint()
	}
	a := fp(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[namedChan](r, "c", 1, codec))
	})
	b := fp(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[namedRecvChan](r, "c", 1, codec))
	})
	assert.NotEqual(t, a, b)
	assert.Contains(t, b, `"type":"github.com/luthersystems/elps/lisp/lisplib/libjson_test.namedRecvChan"`)
	c := fp(func(r *libjson.DurableRegistry) {
		require.NoError(t, libjson.RegisterNative[**counter](r, "c", 1, codec))
	})
	assert.Contains(t, c, `"type":"**github.com/luthersystems/elps/lisp/lisplib/libjson_test.counter"`)
}

// Distinct nil Go maps are distinct natives.
func TestDurableNilReferenceNatives(t *testing.T) {
	env := newTypedTestEnv(t)
	reg := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[refMap](reg, "test:map", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(len(nativeOf[refMap](v))), nil },
		Load: func(*lisp.LEnv, int, *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(refMap(nil)), nil },
	}))
	reg.Freeze()
	var m1, m2 refMap
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{lisp.Native(m1), lisp.Native(m2)}), reg)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#native",["test:map",1,0]],["~#native",["test:map",1,0]]]]]]`, string(b))
}

// Function names come from one index per package per dump, which reads no
// lazy binding, and the read is charged.
func TestDurableFunctionNamesLazyAndCharged(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defun my-fn () 1)`)
	var charges []int
	record := libjson.WithTypedCharge(func(n int) error {
		charges = append(charges, n)
		return nil
	})
	b, err := libjson.DumpDurable(env, env.LoadString("test", `(list my-fn my-fn lisp:not lisp:car)`), nil, record)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#fn","user:my-fn"],["~#fn","user:my-fn"],["~#fn","lisp:not"],["~#fn","lisp:car"]]]]]`, string(b))
	lispBindings := len(env.Runtime.Registry.Package("lisp").SymbolNames())
	userBindings := len(env.Runtime.Registry.Package("user").SymbolNames())
	kib := func(n int) int { return (n + 1023) / 1024 }
	// One read per package (user, then lisp), then the output.
	assert.Equal(t, []int{kib(userBindings), kib(lispBindings), 1}, charges)
}

// The closing bracket of a function is reserved with it.
func TestDurableFunctionReserveIsExact(t *testing.T) {
	env := newTypedTestEnv(t)
	evalString(t, env, `(defun my-fn () 1)`)
	f := env.LoadString("test", `my-fn`)
	b, err := libjson.DumpDurable(env, f, nil)
	require.NoError(t, err)
	_, err = libjson.DumpDurable(env, f, nil, libjson.WithTypedMaxBytes(len(b)))
	require.NoError(t, err)
	_, err = libjson.DumpDurable(env, f, nil, libjson.WithTypedMaxBytes(len(b)-1))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}
