// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"testing"
)

// BenchmarkGoValueBytes pins the cost of GoValue's LBytes arm across sizes.
//
// The arm copies (see lisp/embed.go for why), so its cost is O(len) and
// unbounded by anything the kernel controls -- the only such arm in the
// switch.  This benchmark exists so that is a measured number in CI rather
// than folklore, and so a future change that makes the cost worse, or that
// removes the copy to make it better, has to move a row here.
//
// Read it against BenchmarkGoValueLeafArms below: an LNative carrying the
// identical megabyte returns in single-digit nanoseconds with no allocation,
// because its payload is the embedder's own and is shared rather than
// copied.  The gap between those two rows is the price of the LBytes arm's
// safety property, stated rather than implied.
func BenchmarkGoValueBytes(b *testing.B) {
	for _, n := range []int{16, 1024, 64 * 1024, 1024 * 1024} {
		v := Bytes(make([]byte, n))
		b.Run(fmt.Sprintf("size=%d", n), func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				goValueSink = GoValue(v)
			}
		})
	}
}

// BenchmarkGoValueLeafArms is the comparison set: the other leaf arms that
// return without walking anything, so the LBytes numbers above can be read
// against arms that do no copying at all.
func BenchmarkGoValueLeafArms(b *testing.B) {
	arms := []struct {
		name string
		v    *LVal
	}{
		{"native-1MB", Native(make([]byte, 1024*1024))},
		{"string", String("here I stand")},
		// Deliberately NOT a small int: Go boxes 0..255 from a static table,
		// so Int(42) would measure that cache rather than the arm and read
		// as 0 allocs where a real int costs one.
		{"int", Int(1 << 20)},
	}
	for _, arm := range arms {
		b.Run(arm.name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				goValueSink = GoValue(arm.v)
			}
		})
	}
}

// goValueSink keeps the benchmarked conversions from being optimized away.
var goValueSink interface{}
