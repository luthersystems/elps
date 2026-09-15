// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"math/big"
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
			wantType := LError
			if c.a >= -1 && c.a <= 1 {
				wantType = LInt
			}
			if v.Type != wantType {
				t.Errorf("powInt(%d, %d) returned %v, want %v", c.a, c.b, v.Type, wantType)
			}
		case <-time.After(500 * time.Millisecond):
			t.Fatalf("powInt(%d, %d) did not terminate within 500ms", c.a, c.b)
		}
	}
}

// powIntExact computes the exact mathematical result independently of the
// checked machine-width implementation. Callers must bound b.
func powIntExact(a, b int) *big.Int {
	return new(big.Int).Exp(big.NewInt(int64(a)), big.NewInt(int64(b)), nil)
}

// checkPowInt checks both representable results and the overflow condition.
func checkPowInt(t *testing.T, a, b int, got *LVal, want *big.Int) {
	t.Helper()
	if !want.IsInt64() {
		if got.Type != LError || got.Str != "error" || got.Cells[0].Str != "integer overflow: power overflows int" {
			t.Fatalf("powInt(%d, %d) = %v, want integer overflow error", a, b, got)
		}
		return
	}
	if got.Type != LInt || int64(got.Int) != want.Int64() {
		t.Fatalf("powInt(%d, %d) = %v, want int %v", a, b, got, want)
	}
}

// powIntLegacy is the pre-fix implementation, verbatim. Used only to confirm
// representable results remain compatible with the old implementation.
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
// boundaries exercise both valid results and overflow.
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
// carried across the inner loop) rather than by calling powIntExact per pair,
// which is what makes a multi-million-pair sweep affordable: the reference
// costs O(1) per pair instead of O(b).
func TestPowIntMatchesDefinition(t *testing.T) {
	const sweepMaxExp = 33000
	bases := sweepBases()
	pairs := 0
	for _, a := range bases {
		want := big.NewInt(1)
		factor := big.NewInt(int64(a))
		for b := range sweepMaxExp {
			checkPowInt(t, a, b, powInt(a, b), want)
			pairs++
			// Once an integer power overflows, all higher powers do too.
			// Stop growing the oracle to keep the million-pair sweep cheap.
			if want.IsInt64() {
				want.Mul(want, factor)
			}
		}
	}
	if pairs < 1_000_000 {
		// A sweep that silently shrinks stops being evidence.
		t.Fatalf("swept only %d (a,b) pairs; the sweep is meant to be millions", pairs)
	}
	t.Logf("swept %d (a,b) pairs against the naive definition", pairs)
}

// TestPowIntMatchesLegacy checks every bounded legacy input: representable
// powers retain their values, and powers outside int now report overflow.
// b is bounded at 2048 because powIntLegacy is O(b).
func TestPowIntMatchesLegacy(t *testing.T) {
	const maxExp = 2048
	for _, a := range sweepBases() {
		exact := big.NewInt(1)
		factor := big.NewInt(int64(a))
		for b := range maxExp {
			got := powInt(a, b)
			checkPowInt(t, a, b, got, exact)
			if exact.IsInt64() {
				if want := powIntLegacy(a, b); got.Int != want {
					t.Fatalf("powInt(%d, %d) = %d, legacy = %d", a, b, got.Int, want)
				}
				exact.Mul(exact, factor)
			}
		}
	}
}

// TestPowIntOverflowBoundaries replaces the old wraparound expectations with
// the checked-power contract, retaining every original boundary input.
func TestPowIntOverflowBoundaries(t *testing.T) {
	cases := []struct{ a, b int }{
		{2, 0}, {2, 10}, {2, 62}, {2, 63}, {2, 64}, {2, 65},
		{-2, 63}, {-2, 64}, {0, 5}, {0, 0},
		{3, 40}, {10, 18}, {10, 19}, {10, 20},
		{math.MinInt, 1}, {math.MinInt, 2}, {math.MaxInt, 1},
		{3037000499, 2}, {3037000500, 2},
	}
	for _, c := range cases {
		checkPowInt(t, c.a, c.b, powInt(c.a, c.b), powIntExact(c.a, c.b))
	}
	// Huge exponents need bounded oracle values, not a huge big.Int.
	checkPowInt(t, 2, math.MaxInt, powInt(2, math.MaxInt), powIntExact(2, 64))
	for _, c := range []struct{ a, b, want int }{
		{1, math.MaxInt, 1}, {-1, math.MaxInt, -1}, {-1, math.MaxInt - 1, 1},
	} {
		checkPowInt(t, c.a, c.b, powInt(c.a, c.b), big.NewInt(int64(c.want)))
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
