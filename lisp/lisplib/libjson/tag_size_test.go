// Copyright © 2026 The ELPS authors

package libjson

import (
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// tagSizeSamples are values whose strings, keys and numbers exercise every
// branch of the size count: escapes, a string longer than any stack buffer,
// ints of every width and floats in both notations.
func tagSizeSamples() []*lisp.LVal {
	long := strings.Repeat("long \"quoted\" <text> é\n", 40)
	// A value past 2^53 where int has 64 bits.  The shift count is a
	// variable so the file also compiles for 386, where the value is 0.
	shift := 53
	wide := 1 << shift
	m := lisp.SortedMap()
	m.MapSetString("plain", lisp.String("value"))
	m.MapSetString("~tilde", lisp.String("~tilde"))
	m.MapSetString(long, lisp.String(long))
	m.MapSetLVal(lisp.Symbol("sym"), lisp.Symbol("sym"))
	m.MapSetLVal(lisp.Symbol(":kw"), lisp.Symbol(":kw"))
	m.MapSetLVal(lisp.Int(-12345), lisp.Int(math.MinInt))
	m.MapSetString("ints", lisp.Vector([]*lisp.LVal{
		lisp.Int(0), lisp.Int(7), lisp.Int(99), lisp.Int(100), lisp.Int(-1), lisp.Int(-100),
		lisp.Int(wide), lisp.Int(wide + 1), lisp.Int(math.MaxInt),
	}))
	m.MapSetString("floats", lisp.QExpr([]*lisp.LVal{
		lisp.Float(0.125), lisp.Float(1), lisp.Float(-2.5e-9), lisp.Float(1e21), lisp.Float(1e300),
		lisp.Float(math.Inf(1)), lisp.Float(math.NaN()), lisp.Float(math.Copysign(0, -1)),
	}))
	m.MapSetString("bytes", lisp.Bytes([]byte("bytes")))
	m.MapSetString("tagged", &lisp.LVal{Type: lisp.LTaggedVal, Str: "user:\"type\"", Cells: []*lisp.LVal{lisp.Int(1234)}})
	m.MapSetString("array", lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(1)}), []*lisp.LVal{lisp.Int(1000), lisp.Nil()}))
	return []*lisp.LVal{
		m, typedBenchFrame(), typedBenchRecords(3), jsonWalkerBenchRecords(3),
		lisp.Int(123456), lisp.Float(0.1), lisp.String(long), lisp.Nil(),
	}
}

// TestTagAndUntagCountTheEncodedBytes pins the byte count of the Tag and
// Untag walkers.  Each walker counts the bytes of the JSON it stands for
// without writing them.  The count enforces the byte limit and sets the step
// charge of json:tag and json:untag, so it is consensus-visible.  Tag counts
// the bytes DumpTyped writes for the value.  Untag counts the bytes Dump
// writes for its input.
func TestTagAndUntagCountTheEncodedBytes(t *testing.T) {
	for _, v := range tagSizeSamples() {
		typed, err := DumpTyped(v)
		require.NoError(t, err)

		tw := tagWalker{cfg: newTypedConfig(nil)}
		tagged, err := tw.value(v, 0)
		require.NoError(t, err)
		assert.Equal(t, len(typed), tw.size, "tag size of %v", v)

		plain, err := Dump(tagged, false)
		require.NoError(t, err)
		assert.Equal(t, string(typed), string(plain), "Tag then Dump is DumpTyped")

		uw := untagWalker{cfg: newTypedConfig(nil)}
		back, err := uw.value(tagged, 0)
		require.NoError(t, err)
		assert.Equal(t, len(plain), uw.size, "untag size of %v", tagged)

		again, err := DumpTyped(back)
		require.NoError(t, err)
		assert.Equal(t, string(typed), string(again), "Untag restores the value")
	}
}

// TestTagAndUntagChargeIsStable pins the charge of both walkers for one
// document: one unit for each started KiB of counted bytes.
func TestTagAndUntagChargeIsStable(t *testing.T) {
	v := typedBenchRecords(400)
	typed, err := DumpTyped(v)
	require.NoError(t, err)
	want := startedKiB(len(typed))

	charged := 0
	tagged, err := Tag(v, WithTypedCharge(func(kib int) error { charged += kib; return nil }))
	require.NoError(t, err)
	assert.Equal(t, want, charged, "tag charge")

	charged = 0
	_, err = Untag(tagged, WithTypedCharge(func(kib int) error { charged += kib; return nil }))
	require.NoError(t, err)
	assert.Equal(t, want, charged, "untag charge")
}
