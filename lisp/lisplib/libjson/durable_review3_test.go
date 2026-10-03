// Copyright © 2026 The ELPS authors

package libjson_test

// Regression tests for the round-4 reviews of luthersystems/elps#797.

import (
	"math"
	"math/rand/v2"
	"reflect"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// requireExactLimit checks that v dumps at exactly its document length and
// is refused one byte below it.
func requireExactLimit(t *testing.T, env *lisp.LEnv, v *lisp.LVal, msg string) {
	t.Helper()
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err, msg)
	_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxBytes(len(b)))
	require.NoError(t, err, "%s: refused at its exact length %d: %s", msg, len(b), b)
	_, err = libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxBytes(len(b)-1))
	require.ErrorIs(t, err, libjson.ErrTypedLimit, "%s: accepted one byte short", msg)
}

// A map key is checked at its exact key text, so a document of exactly the
// byte limit is written.
func TestDurableMapKeysAtExactLimit(t *testing.T) {
	env := newTypedTestEnv(t)
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String(""), lisp.Int(0))
	b, err := libjson.DumpDurable(env, m, nil)
	require.NoError(t, err)
	require.Equal(t, `["~#durable",[1,{"":0}]]`, string(b))
	requireExactLimit(t, env, m, `{"":0}`)

	keys := []*lisp.LVal{
		lisp.Int(0), lisp.Int(-1), lisp.Int(math.MinInt32), lisp.Int(math.MaxInt32),
		lisp.String(""), lisp.String("~"), lisp.String("~~x"), lisp.String("<>&\n\"\\"), lisp.String("é😀"),
		lisp.Symbol("true"), lisp.Symbol("false"), lisp.Symbol("x"), lisp.Symbol(":k"), lisp.Symbol("a<b"),
	}
	if strconv.IntSize == 64 {
		var lo, hi, p53, n53 int64 = math.MinInt64, math.MaxInt64, 1 << 53, -(1 << 53)
		for _, n := range []int64{lo, hi, p53, n53, p53 + 1, n53 - 1} {
			keys = append(keys, lisp.Int(int(n)))
		}
	}
	rng := rand.New(rand.NewPCG(797, 4)) //nolint:gosec // a fixed seed makes the test repeatable; no secret is involved
	letters := []rune("ab~<>&\"\\\n\té😀:")
	randText := func() string {
		r := make([]rune, rng.IntN(6))
		for i := range r {
			r[i] = letters[rng.IntN(len(letters))]
		}
		return string(r)
	}
	for range 3000 {
		m := lisp.SortedMap()
		for range 1 + rng.IntN(4) {
			var k *lisp.LVal
			switch rng.IntN(5) {
			case 0:
				k = keys[rng.IntN(len(keys))]
			case 1:
				k = lisp.Int(int(rng.Int32()) - rng.IntN(3))
			case 2:
				k = lisp.String(randText())
			case 3:
				k = lisp.Symbol("s" + randText())
			default:
				k = lisp.Symbol(":" + randText())
			}
			m.MapSetLVal(k, lisp.Int(rng.IntN(100)))
		}
		if _, err := libjson.DumpDurable(env, m, nil); err != nil {
			continue // a key typed JSON refuses (two keys of one encoding)
		}
		requireExactLimit(t, env, m, m.String())
	}
}

// intFuncChan and stringFuncChan declare one type name in two functions,
// with different function signatures in their element types.
func intFuncChan() reflect.Type {
	type C chan func(int)
	return reflect.TypeFor[C]()
}

func stringFuncChan() reflect.Type {
	type C chan func(string)
	return reflect.TypeFor[C]()
}

// Embedded is embedded by one function-scope C and named by the other.
type Embedded struct{ N int }

func anonymousField() reflect.Type {
	type C struct{ Embedded }
	return reflect.TypeFor[C]()
}

func namedField() reflect.Type {
	type C struct{ Embedded Embedded }
	return reflect.TypeFor[C]()
}

func readerIface() reflect.Type {
	type C chan interface{ Read(p []byte) (int, error) }
	return reflect.TypeFor[C]()
}

func writerIface() reflect.Type {
	type C chan interface{ Write(p []byte) (int, error) }
	return reflect.TypeFor[C]()
}

func variadicFunc() reflect.Type {
	type C chan func(...int)
	return reflect.TypeFor[C]()
}

func sliceFunc() reflect.Type {
	type C chan func([]int)
	return reflect.TypeFor[C]()
}

func fingerprintOf(t *testing.T, typ reflect.Type) string {
	t.Helper()
	r := libjson.NewDurableRegistry()
	require.NoError(t, r.Register(typ, "c", 1, libjson.NativeFuncs{}))
	r.Freeze()
	return r.Fingerprint()
}

// Function-scope types of one name differ in the fingerprint whenever their
// complete structure differs.
func TestDurableFingerprintCompleteShape(t *testing.T) {
	for _, c := range []struct {
		name string
		a, b reflect.Type
	}{
		{"func parameters", intFuncChan(), stringFuncChan()},
		{"embedded field", anonymousField(), namedField()},
		{"interface methods", readerIface(), writerIface()},
		{"variadic", variadicFunc(), sliceFunc()},
	} {
		require.Equal(t, c.a.Name(), c.b.Name(), c.name)
		require.Equal(t, c.a.PkgPath(), c.b.PkgPath(), c.name)
		assert.NotEqual(t, fingerprintOf(t, c.a), fingerprintOf(t, c.b), c.name)
	}
	assert.Contains(t, fingerprintOf(t, intFuncChan()), `func(in(int),out(),variadic=false)`)
}

// A long pointer chain keeps the shape of the type at its end.
func TestDurableFingerprintPointerChain(t *testing.T) {
	send, recv := sendChanType(), recvChanType()
	for range 9 {
		send, recv = reflect.PointerTo(send), reflect.PointerTo(recv)
	}
	assert.NotEqual(t, fingerprintOf(t, send), fingerprintOf(t, recv))
	assert.NotContains(t, fingerprintOf(t, send), "...")
}
