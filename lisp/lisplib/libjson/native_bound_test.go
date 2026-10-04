// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"context"
	"encoding/json"
	"math/big"
	"runtime"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// The tests in this file pin luthersystems/elps#802: json:dump-* bounds,
// meters and marshals once the native values it encodes.

// deepNode is the pointer chain from the svc reviews.
type deepNode struct {
	N    int
	Next *deepNode
}

// countingMarshaler counts its MarshalJSON calls.
type countingMarshaler struct{ calls *int }

func (c countingMarshaler) MarshalJSON() ([]byte, error) {
	*c.calls++
	return []byte(`"marshalled"`), nil
}

// dumpBytes runs json:dump-bytes on v in env.
func dumpBytes(t *testing.T, env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
	t.Helper()
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), v)))
	return env.LoadStringContext(context.Background(), "test", `(json:dump-bytes v)`)
}

// A native nested 3,000,000 deep used to kill the process with a Go stack
// overflow inside encoding/json.  It is now refused with the error
// checkLoadable gives a native nested past the decoder's limit.
func TestDumpRefusesDeepNativeWithoutCrashing(t *testing.T) {
	const depth = 3_000_000
	var chain *deepNode
	for i := range depth {
		chain = &deepNode{N: i, Next: chain}
	}
	var nested any = 1
	for range depth {
		nested = []any{nested}
	}
	for _, tc := range []struct {
		name    string
		native  any
		bracket string
	}{
		{"pointer-chain", chain, "{"},
		{"nested-slices", nested, "["},
	} {
		t.Run(tc.name, func(t *testing.T) {
			want := "unable to encode native value: invalid character '" + tc.bracket + "' exceeded max depth"
			_, err := libjson.Dump(lisp.Native(tc.native), false)
			require.EqualError(t, err, want)

			got := dumpBytes(t, newTypedTestEnv(t), lisp.Native(tc.native))
			require.Equal(t, lisp.LError, got.Type)
			assert.Contains(t, got.String(), want)
		})
	}
}

// A native too deep to load reports the same error at every depth: through
// encoding/json and checkLoadable below the walk's bound, and from the walk
// above it.
func TestDumpDeepNativeErrorIsTheSameAtEveryDepth(t *testing.T) {
	const want = "unable to encode native value: invalid character '[' exceeded max depth"
	for _, depth := range []int{10_001, 20_000, 30_000, 60_000} {
		var v any = 1
		for range depth {
			v = []any{v}
		}
		_, err := libjson.Dump(lisp.Native(v), false)
		assert.EqualError(t, err, want, "depth %d", depth)
	}
}

// Pointers and interfaces nest on encoding/json's stack without nesting the
// JSON.  Past the walk's bound such a native is refused too.
func TestDumpRefusesDeepPointerChain(t *testing.T) {
	var v any = 1
	for range 100_000 {
		x := v
		v = &x
	}
	_, err := libjson.Dump(lisp.Native(v), false)
	require.EqualError(t, err, "unable to encode native value: value nests more than 50000 levels deep")

	v = 1
	for range 1000 {
		x := v
		v = &x
	}
	b, err := libjson.Dump(lisp.Native(v), false)
	require.NoError(t, err)
	assert.Equal(t, "1", string(b))
}

// A native whose JSON is over the cap is refused before encoding/json runs:
// nothing is marshalled and its output is never built.
func TestDumpRefusesOversizedNativeBeforeMarshalling(t *testing.T) {
	const maxAlloc = 1 << 20
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(maxAlloc))
	calls := 0
	native := lisp.Native([]any{strings.Repeat("x", 4*maxAlloc), countingMarshaler{&calls}})
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), native)))

	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	got := env.LoadStringContext(context.Background(), "test", `(json:dump-bytes v)`)
	runtime.ReadMemStats(&after)

	require.Equal(t, lisp.LError, got.Type)
	assert.Contains(t, got.String(), "allocation size exceeds maximum (1048576)")
	assert.Zero(t, calls, "MarshalJSON ran for a native already over the cap")
	assert.Less(t, after.TotalAlloc-before.TotalAlloc, uint64(maxAlloc),
		"the native's 4 MiB of output was built before it was refused")
}

// math/big converts to decimal inside MarshalJSON and MarshalText.  A number
// whose conversion would pass the cap is refused before it runs.
func TestDumpRefusesBigNumbersBeforeConverting(t *testing.T) {
	env := newTypedTestEnv(t)
	tiny := new(big.Float).SetMantExp(big.NewFloat(0.5), -50_000_000)
	huge := new(big.Int).Lsh(big.NewInt(1), 100_000_000)
	for name, native := range map[string]any{"float": tiny, "int": huge} {
		got := dumpBytes(t, env, lisp.Native(native))
		require.Equal(t, lisp.LError, got.Type, name)
		assert.Contains(t, got.String(), "allocation size exceeds maximum", name)
	}

	got := dumpBytes(t, env, lisp.Native([]any{big.NewFloat(1.5), big.NewInt(-12), big.NewRat(1, 3)}))
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	assert.Equal(t, `["1.5",-12,"1/3"]`, string(got.Bytes()))
}

// A native is marshalled at most once per document, including inside a value
// that contains itself, where the encoder used to marshal it again on each of
// the 62 levels it descended before it found the cycle.
func TestDumpMarshalsEachNativeOnce(t *testing.T) {
	calls := 0
	n := lisp.Native(countingMarshaler{&calls})
	b, err := libjson.Dump(lisp.QExpr([]*lisp.LVal{n, n, lisp.QExpr([]*lisp.LVal{n})}), false)
	require.NoError(t, err)
	assert.Equal(t, `["marshalled","marshalled",["marshalled"]]`, string(b))
	assert.Equal(t, 1, calls)

	calls = 0
	self := lisp.QExpr([]*lisp.LVal{n})
	self.Cells = append(self.Cells, self)
	_, err = libjson.Dump(self, false)
	require.EqualError(t, err, "cannot serialize a value that contains itself")
	assert.Equal(t, 1, calls)
}

// A step budget smaller than the document stops the encode partway, with the
// budget's own condition: the native at the end is never reached.
func TestDumpStopsAtStepBudgetPartway(t *testing.T) {
	env := newTypedTestEnv(t)
	calls := 0
	cells := make([]*lisp.LVal, 0, 201)
	for range 200 {
		cells = append(cells, lisp.String(strings.Repeat("x", 1000)))
	}
	cells = append(cells, lisp.Native(countingMarshaler{&calls}))
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), lisp.QExpr(cells))))
	env.Runtime.SetStepBudget(50)
	got := env.LoadStringContext(context.Background(), "test", `(json:dump-bytes v)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Contains(t, got.String(), lisp.CondStepBudgetExceeded)
	assert.Zero(t, calls, "the encode ran to the end of the document")
}

// Charging as the output is written charges what charging after it did: one
// step per whole KiB of the document, natives included.
func TestDumpChargesOneStepPerKiB(t *testing.T) {
	env := newTypedTestEnv(t)
	raw := json.RawMessage(`"` + strings.Repeat("y", 3000) + `"`)
	v := lisp.QExpr([]*lisp.LVal{
		lisp.String(strings.Repeat("x", 5000)),
		lisp.Native(&raw),
		lisp.Int(7),
	})
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), v)))
	b, err := libjson.Dump(v, false)
	require.NoError(t, err)
	base := typedSteps(t, env, `(identity v)`)
	for _, form := range []string{`(json:dump-bytes v)`, `(json:dump-string v)`, `(json:dump-message v)`} {
		assert.Equal(t, int64(len(b)/1024), typedSteps(t, env, form)-base, form)
	}
}

// ringNode is a pointer cycle of any length.
type ringNode struct {
	Next *ringNode
	N    int
}

// A native that contains itself reports encoding/json's own cycle error,
// whether encoding/json reports it or, when its trips around the cycle would
// pass the cap, the walk does.  A cycle so long that encoding/json would only
// see it past nativeNestLimit is refused as too deep, like any native that
// nests that far.
func TestDumpNativeCycleErrorMatchesEncodingJSON(t *testing.T) {
	ring := func(n int) *ringNode {
		nodes := make([]ringNode, n)
		for i := range nodes {
			nodes[i] = ringNode{Next: &nodes[(i+1)%n], N: i}
		}
		return &nodes[0]
	}
	for _, n := range []int{1, 7, 100, 20_000} {
		_, want := json.Marshal(ring(n))
		require.Error(t, want, "ring of %d", n)
		_, err := libjson.Dump(lisp.Native(ring(n)), false)
		require.EqualError(t, err, want.Error(), "ring of %d", n)

		// encoding/json writes about 1000 nodes before it reports even the
		// shortest of these cycles, which passes a 4 KiB cap.  The walk sees
		// the cycle first and reports it without running encoding/json.
		if n <= 100 {
			got := dumpBytes(t, newTypedTestEnv(t, lisp.WithMaxAlloc(4096)), lisp.Native(ring(n)))
			require.Equal(t, lisp.LError, got.Type, "ring of %d", n)
			assert.Contains(t, got.String(), want.Error(), "ring of %d under a 4 KiB cap", n)
		}
	}

	_, err := libjson.Dump(lisp.Native(ring(40_000)), false)
	assert.EqualError(t, err, "unable to encode native value: invalid character '{' exceeded max depth")
}
