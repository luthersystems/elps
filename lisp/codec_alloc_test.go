// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"encoding/binary"
	"runtime"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// amplifyInput nests levels lists, each declaring as many elements as the
// input has bytes left, then pads.  Every declared count passes a
// "count <= remaining input" check, so a decoder that preallocates from
// declared counts allocates about 8*size bytes per level.
func amplifyInput(size, levels int) []byte {
	b := []byte{lisp.CanonicalVersion}
	for range levels {
		b = append(b, 0x08)
		b = binary.AppendUvarint(b, uint64(size-len(b)-10)) //nolint:gosec // G115: size exceeds len(b)+10 by construction
	}
	for len(b) < size {
		b = append(b, 0x01, 0x00)
	}
	return b
}

// TestCanonicalDecodeNoAllocAmplification: decoding hostile input must
// allocate within a small multiple of the input size, whatever counts it
// declares.
func TestCanonicalDecodeNoAllocAmplification(t *testing.T) {
	for _, tc := range []struct {
		name string
		in   []byte
	}{
		{"nested lists", amplifyInput(1<<20, 200)},
		{"nested maps", func() []byte {
			b := amplifyInput(1<<20, 200)
			for i := 1; i < 400 && i < len(b); i++ {
				if b[i] == 0x08 {
					b[i] = 0x0a
				}
			}
			return b
		}()},
		{"array rank", func() []byte {
			// One array claiming a rank of nearly the whole input, each
			// dimension a one-byte uvarint 1.
			b := []byte{lisp.CanonicalVersion, 0x09}
			b = binary.AppendUvarint(b, uint64((1<<20)-16))
			for len(b) < 1<<20 {
				b = append(b, 0x01)
			}
			return b
		}()},
		{"array dims", func() []byte {
			b := []byte{lisp.CanonicalVersion}
			for range 200 {
				b = append(b, 0x09, 0x01)
				b = binary.AppendUvarint(b, uint64((1<<20)-len(b)-10)) //nolint:gosec // G115: positive by construction
			}
			for len(b) < 1<<20 {
				b = append(b, 0x01, 0x00)
			}
			return b
		}()},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var before, after runtime.MemStats
			runtime.GC()
			runtime.ReadMemStats(&before)
			v, err := lisp.DecodeCanonical(tc.in)
			runtime.ReadMemStats(&after)
			require.Error(t, err)
			assert.Nil(t, v)
			alloc := after.TotalAlloc - before.TotalAlloc
			assert.Less(t, alloc, uint64(32*len(tc.in)), "decode of %d bytes allocated %d", len(tc.in), alloc)
		})
	}
}

// wellFormed builds a valid encoding of one container of n copies of elem.
func wellFormed(head byte, n int, elem []byte) []byte {
	b := []byte{lisp.CanonicalVersion, head}
	b = binary.AppendUvarint(b, uint64(n)) //nolint:gosec // G115: n is a positive test size
	for range n {
		b = append(b, elem...)
	}
	return b
}

// codecAllocCeiling is the documented worst case of decode allocation per
// input byte (docs/internals/canonical-codec.md): each decoded value costs
// at most a few hundred bytes of Go heap, an LVal header plus its share of
// the containing slice, and the densest encodings spend one or two input
// bytes per value.
const codecAllocCeiling = 200

// TestCanonicalDecodeAllocDenseValid measures valid, maximally dense input,
// where the ratio of heap to input is highest.
func TestCanonicalDecodeAllocDenseValid(t *testing.T) {
	intKeyMap := []byte{lisp.CanonicalVersion, 0x0a}
	n := 100000
	intKeyMap = binary.AppendUvarint(intKeyMap, uint64(n))
	for i := range n {
		intKeyMap = append(intKeyMap, 0x01)
		intKeyMap = binary.AppendUvarint(intKeyMap, uint64(2*i))
		intKeyMap = append(intKeyMap, 0x08, 0x00)
	}
	for _, tc := range []struct {
		name string
		in   []byte
	}{
		{"list of empty lists", wellFormed(0x08, 1<<18, []byte{0x08, 0x00})},
		{"list of ints", wellFormed(0x08, 1<<18, []byte{0x01, 0x00})},
		{"list of small arrays", wellFormed(0x08, 1<<17, []byte{0x09, 0x01, 0x00})},
		{"list of tagged", wellFormed(0x08, 1<<17, []byte{0x0b, 0x01, 0x74, 0x08, 0x00})},
		{"int-key map", intKeyMap},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var before, after runtime.MemStats
			runtime.GC()
			runtime.ReadMemStats(&before)
			v, err := lisp.DecodeCanonical(tc.in)
			runtime.ReadMemStats(&after)
			require.NoError(t, err)
			require.NotNil(t, v)
			alloc := after.TotalAlloc - before.TotalAlloc
			ratio := float64(alloc) / float64(len(tc.in))
			t.Logf("%s: %d bytes in, %d allocated, %.0fx", tc.name, len(tc.in), alloc, ratio)
			assert.Less(t, ratio, float64(codecAllocCeiling))
		})
	}
}
