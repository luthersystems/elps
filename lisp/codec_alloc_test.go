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
			b = binary.AppendUvarint(b, uint64((1<<20)-16)) //nolint:gosec // G115: positive constant
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
