// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"testing"
	"time"
)

// TestPowIntTerminates is the regression test for the non-terminating
// exponent-doubling loop powInt used to run (see powInt's doc comment).
//
// IT MUST FAIL IN MILLISECONDS, NOT BY TIMING OUT.  Reverting the fix does
// not make powInt return a wrong answer; it makes powInt never return at all.
// A plain `got := powInt(...)` regression test would therefore hang until the
// package's 10-minute `go test` timeout, take the whole test binary down with
// it, and report the failure against whatever test happened to be running.
// Running the call on its own goroutine with a 500ms deadline turns the hang
// into a normal, fast, attributable failure -- measured: the mutated build
// fails this test in 500ms, versus a 10-minute package timeout without it.
// 500ms is ~5 x 10^5 times the real cost of the worst case (63 iterations).
//
// The goroutine is leaked when the deadline fires. That is the point: the
// loop under test cannot be interrupted, which is exactly the defect. It only
// happens on a regression.
func TestPowIntTerminates(t *testing.T) {
	cases := []struct{ a, b int }{
		// The originally reported input.
		{-128, math.MaxInt},
		// The input FuzzApplyStdlib minimised to.
		{math.MinInt, math.MaxInt - 1},
		// 2^62 is the largest b the old loop survived; 2^62+1 is the smallest
		// it did not. Both must return.
		{3, 1 << 62},
		{3, (1 << 62) + 1},
		{0, math.MaxInt},
		{1, math.MaxInt},
		{-1, math.MaxInt},
		{2, math.MaxInt},
	}
	for _, c := range cases {
		done := make(chan *LVal, 1)
		go func() { done <- powInt(c.a, c.b) }()
		select {
		case v := <-done:
			if v.Type != LInt {
				t.Errorf("powInt(%d, %d) returned %v, want an int", c.a, c.b, v.Type)
			}
		case <-time.After(500 * time.Millisecond):
			t.Fatalf("powInt(%d, %d) did not terminate within 500ms", c.a, c.b)
		}
	}
}

// powIntNaive is the definition of integer exponentiation in Go's wrapping
// int arithmetic: b factors of a, multiplied left to right. It is the ground
// truth TestPowIntMatchesDefinition compares against -- not the previous
// implementation, so the sweep proves powInt is CORRECT rather than merely
// bug-compatible with what it replaced.
func powIntNaive(a, b int) int {
	acc := 1
	for range b {
		acc *= a
	}
	return acc
}

// powIntLegacy is the pre-fix implementation, verbatim. Used only to confirm
// the fix changed no answer the old code was able to produce.
//
// CALLERS MUST BOUND b. This function does not terminate for b > 2^62, and
// costs O(b) even when it does.
func powIntLegacy(a, b int) int {
	if b == 0 {
		return 1
	}
	n := 1
	atob := a
	for 2*n < b {
		atob *= atob
		n *= 2
	}
	for n < b {
		atob *= a
		n++
	}
	return atob
}

// sweepBases are the a-values swept. Sign, magnitude and the overflow
// boundaries all change how the wrapping multiplications compose.
func sweepBases() []int {
	bases := []int{
		0, 1, -1, 2, -2, 3, -3, 7, -7, 10, -10, 16, -16, 127, -128,
		255, -255, 256, -256, 1000, -1000, 65535, 65536,
		math.MaxInt32, math.MinInt32, math.MaxInt32 + 1,
		1 << 31, 1 << 32, 1 << 53, -(1 << 53), 1 << 62,
		math.MaxInt, math.MinInt, math.MaxInt - 1, math.MinInt + 1,
		3037000499, // floor(sqrt(MaxInt)): a^2 is the last product that fits
		-3037000499,
	}
	// Fill out to a wide sweep with arbitrary but fixed values, so the result
	// is not an artifact of round numbers. Deterministic: no rand.
	for i := 1; i <= 40; i++ {
		v := i*2654435761 + 12345
		bases = append(bases, v, -v)
	}
	return bases
}

// TestPowIntMatchesDefinition sweeps powInt against b factors of a for every
// base above and every exponent in [0, sweepMaxExp).
//
// The naive reference is computed INCREMENTALLY (one multiply per exponent,
// carried across the inner loop) rather than by calling powIntNaive per pair,
// which is what makes a multi-million-pair sweep affordable: the reference
// costs O(1) per pair instead of O(b).
func TestPowIntMatchesDefinition(t *testing.T) {
	const sweepMaxExp = 33000
	bases := sweepBases()
	pairs := 0
	for _, a := range bases {
		want := 1
		for b := range sweepMaxExp {
			got := powInt(a, b)
			if got.Type != LInt {
				t.Fatalf("powInt(%d, %d) returned %v, want an int", a, b, got.Type)
			}
			if got.Int != want {
				t.Fatalf("powInt(%d, %d) = %d, want %d", a, b, got.Int, want)
			}
			pairs++
			want *= a
		}
	}
	if pairs < 1_000_000 {
		// A sweep that silently shrinks stops being evidence.
		t.Fatalf("swept only %d (a,b) pairs; the sweep is meant to be millions", pairs)
	}
	t.Logf("swept %d (a,b) pairs against the naive definition", pairs)
}

// TestPowIntMatchesLegacy confirms the fix is not a behaviour change: for
// every (a,b) the OLD implementation could actually compute, the new one
// returns the same bits, wrapped answers included.
//
// b is bounded at 2048 because powIntLegacy is O(b).
func TestPowIntMatchesLegacy(t *testing.T) {
	const maxExp = 2048
	for _, a := range sweepBases() {
		for b := range maxExp {
			got := powInt(a, b)
			if want := powIntLegacy(a, b); got.Int != want {
				t.Fatalf("powInt(%d, %d) = %d, legacy = %d", a, b, got.Int, want)
			}
		}
	}
}

// TestPowIntWrapsLikeMultiplication pins the specific wrapped answers a caller
// could be relying on. (pow 2 64) has always returned 0 -- 2^64 mod 2^64 --
// and must keep doing so: switching association order is only safe because
// mod-2^64 multiplication is commutative and associative, and this is the
// test that would catch a "fix" that started saturating or erroring instead.
func TestPowIntWrapsLikeMultiplication(t *testing.T) {
	cases := []struct {
		a, b, want int
	}{
		{2, 0, 1},
		{2, 10, 1024},
		{2, 62, 1 << 62},
		{2, 63, math.MinInt}, // 2^63 wraps to the sign bit
		{2, 64, 0},           // and 2^64 to exactly zero
		{2, 65, 0},
		{2, math.MaxInt, 0},
		{-2, 63, math.MinInt},
		{-2, 64, 0},
		{0, 5, 0},
		{1, math.MaxInt, 1},
		{-1, math.MaxInt, -1},
		{-1, math.MaxInt - 1, 1},
		{3, 40, powIntNaive(3, 40)},
		{10, 19, powIntNaive(10, 19)},
		{10, 20, powIntNaive(10, 20)}, // 10^20 > MaxInt: wrapped
	}
	for _, c := range cases {
		if got := powInt(c.a, c.b); got.Int != c.want {
			t.Errorf("powInt(%d, %d) = %d, want %d", c.a, c.b, got.Int, c.want)
		}
	}
}

// TestPowIntNegativeExponent pins that a negative exponent still falls back to
// float math rather than entering the integer loop at all.
func TestPowIntNegativeExponent(t *testing.T) {
	v := powInt(2, -2)
	if v.Type != LFloat {
		t.Fatalf("powInt(2, -2) returned %v, want a float", v.Type)
	}
	if v.Float != 0.25 {
		t.Errorf("powInt(2, -2) = %v, want 0.25", v.Float)
	}
	if v := powInt(2, math.MinInt); v.Type != LFloat {
		t.Errorf("powInt(2, MinInt) returned %v, want a float", v.Type)
	}
}
