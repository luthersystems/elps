// Copyright © 2026 The ELPS authors

package libjson

import (
	"math"
	"math/rand"
	"strconv"
	"testing"
)

// TestDecimalLenMatchesItoa pins that decimalLen counts the bytes
// strconv.Itoa writes.  The size walkers charge steps and enforce byte
// limits from this count, so a wrong count changes a consensus-visible
// result.
func TestDecimalLenMatchesItoa(t *testing.T) {
	check := func(n int) {
		t.Helper()
		if got, want := decimalLen(n), len(strconv.Itoa(n)); got != want {
			t.Fatalf("decimalLen(%d) = %d, want %d", n, got, want)
		}
	}
	for _, n := range []int{0, 1, -1, 9, 10, -9, -10, 99, 100, math.MaxInt, math.MinInt, math.MaxInt - 1, math.MinInt + 1} {
		check(n)
	}
	// Every power of ten and its neighbors, in both signs.
	for p := 1; p > 0 && p < math.MaxInt/10; p *= 10 {
		for _, n := range []int{p - 1, p, p + 1} {
			check(n)
			check(-n)
		}
	}
	r := rand.New(rand.NewSource(1)) //nolint:gosec // G404: a fixed seed makes the test repeatable
	for range 100000 {
		// A random width, so that short numbers are as common as long ones.
		check(int(r.Uint64() >> (r.Intn(64))))  //nolint:gosec // G115: any int is a fair input
		check(-int(r.Uint64() >> (r.Intn(64)))) //nolint:gosec // G115: any int is a fair input
	}
}
