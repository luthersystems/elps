// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"encoding/binary"
	"math"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

// FuzzCanonizeRoundTripInvariant checks exact types, idempotence, the plain
// image, identical typed/plain bytes, and unchanged adoption bytes over ALL
// value shapes. It also exercises arbitrary float bits, mixed map keys, and
// the common ASCII string-keyed case. The value generator and walkers have
// bounded depth/work; there is no Lisp evaluation or watchdog here.
func FuzzCanonizeRoundTripInvariant(f *testing.F) {
	for _, seed := range fuzzval.Seeds() {
		f.Add(seed)
	}
	for _, seed := range fuzzseed.Adversarial() {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		env := newTypedTestEnv(t)
		snapshot := lisp.TakeSingletonSnapshot()
		v := fuzzval.New(data, env).Value()
		checkCanonizeInvariant(t, v)
		result := libjson.CanonizeBuiltin(env, lisp.SExpr([]*lisp.LVal{v}))
		require.False(t, lisp.IsInternalPanic(result))
		// An existing error argument propagates unchanged. All new data
		// rejections carry the stable condition and handler data instead.
		if result.Type == lisp.LError && v.Type != lisp.LError {
			require.Equal(t, "json:canonize-error", result.Str)
			require.Len(t, result.Cells, 3)
			require.Equal(t, lisp.LString, result.Cells[0].Type)
			require.Equal(t, lisp.LSymbol, result.Cells[1].Type)
			require.Contains(t, []string{":leading-tilde", ":invalid-utf8", ":int-range", ":float-range",
				":negative-zero", ":non-finite", ":key-type", ":key-collision", ":key-order",
				":depth", ":cycle", ":unsupported", ":limit"}, result.Cells[1].Str)
			require.Equal(t, lisp.LString, result.Cells[2].Type)
		}
		var bits [8]byte
		copy(bits[:], data)
		x := math.Float64frombits(binary.LittleEndian.Uint64(bits[:]))
		checkCanonizeInvariant(t, lisp.Float(x))
		m := canonMap(t, lisp.String("text"), lisp.String("hello <>& é😀"),
			lisp.String("int"), lisp.Int(int(bits[0])), lisp.String("nested"),
			lisp.Vector([]*lisp.LVal{canonMap(t, lisp.String("float"), lisp.Float(float64(bits[1])/8))}))
		require.True(t, checkCanonizeInvariant(t, m))
		// Keys are built through the same mutable map operation as assoc!.
		mixed := lisp.SortedMap()
		for i, k := range []*lisp.LVal{lisp.Symbol(":k"), lisp.Symbol("true"), lisp.String("z"), lisp.Symbol("a"), lisp.Int(9), lisp.Int(10)} {
			if bits[0]&(1<<i) != 0 {
				mixed.MapSetLVal(k, lisp.Int(i))
			}
		}
		checkCanonizeInvariant(t, mixed)
		require.Empty(t, snapshot.Verify())
	})
}
