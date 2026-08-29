// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// iterSteps builds n '* steps, the shape that drove issue #565.
func iterSteps(n int) []*lisp.LVal {
	steps := make([]*lisp.LVal, n)
	for i := range steps {
		steps[i] = lisp.Symbol("*")
	}
	return steps
}

// TestNormalizePathsIsIdempotent pins the invariant the #565 fix RESTS ON.
//
// The fix stopped normalizePaths' iterator branch from calling Iter, which
// re-entered Chain and re-normalized the tail. Skipping that is only sound
// if normalizePaths is idempotent -- if a second pass over an
// already-normalized chain returns the same chain. It is: expandPaths
// flattens a normalized chain back to exactly the sequence the loop
// consumed to build it, and the loop then rebuilds it.
//
// If that ever stops holding, the fix silently changes what paths MEAN
// rather than merely what they cost, and no cost test would notice. This is
// the test that would.
func TestNormalizePathsIsIdempotent(t *testing.T) {
	t.Parallel()
	cases := [][]Path{
		{Dot("a"), Dot("b")},
		{Iter()},
		{Iter(), Dot("a")},
		{Dot("a"), Iter(), Dot("b")},
		{Iter(), Iter()},
		{Iter(), Iter(), Iter(), Dot("x")},
		{Dot("a"), Iter(), Index(0), Iter(), Range(1, 3, false)},
		{Dot("a"), Iter(), Range(1, 0, true)},
		{Chain(Dot("a"), Iter()), Dot("b")},
		{Iter(Dot("a"), Iter(Dot("b")))},
	}
	for i, in := range cases {
		once := normalizePaths(in...)
		twice := normalizePaths(once...)
		got, want := (&chainPath{paths: twice}).String(), (&chainPath{paths: once}).String()
		if got != want {
			t.Errorf("case %d: not idempotent: once=%q twice=%q", i, want, got)
		}
		if len(once) != len(twice) {
			t.Errorf("case %d: length changed: once=%d twice=%d", i, len(once), len(twice))
		}
	}
}

// TestNormalizePathsIsNotExponential is the cost regression for issue #565.
//
// normalizePaths' iterator branch used to call Iter, which re-entered Chain
// and re-normalized the whole tail; one re-entry per iterator over a
// structure the previous one had just rebuilt is 2^n. Measured before the
// fix, on ArgsToPath alone with no document in sight: 12 steps 1.2ms, 16
// steps 21ms, 20 steps 292ms, 24 steps 4.7s. Reachable from the shipped ?
// builtin, and from a 45-byte selector string.
//
// It asserts ALLOCATIONS rather than wall time deliberately: a timing bound
// is a flake on a loaded CI runner, and allocation count is exactly as
// exponential as the work was. Linear construction is ~8 allocs per step
// (201 at n=24); the old code was in the millions, so the bound below has
// several orders of magnitude of headroom and still cannot be passed by a
// regression.
func TestNormalizePathsIsNotExponential(t *testing.T) {
	// NOT t.Parallel(): testing.AllocsPerRun measures process-wide heap
	// counters, so a concurrently running test's allocations land in this
	// one's total. With t.Parallel() this test failed against the FIXED
	// code, counting the rest of the package's parallel suite.
	const (
		steps = 24
		limit = 2000
	)
	args := iterSteps(steps)
	allocs := testing.AllocsPerRun(3, func() {
		if _, err := ArgsToPath(args); err != nil {
			t.Fatalf("ArgsToPath: %v", err)
		}
	})
	if allocs > limit {
		t.Errorf("ArgsToPath with %d iterator steps made %.0f allocations, want <= %d "+
			"-- normalization is re-entering Chain again (issue #565)", steps, allocs, limit)
	}
	// The same path reached through the selector parser, since that is the
	// other surface a short input arrives on.
	sel := "."
	for range steps {
		sel += "[]"
	}
	if _, err := ParseSelector(sel); err != nil {
		t.Fatalf("ParseSelector(%q): %v", sel, err)
	}
}

// TestNormalizePathsAgreesWithIterConstruction proves the direct
// construction the fix uses builds the same path the Iter route did, over
// the nesting shapes where the two could differ.
func TestNormalizePathsAgreesWithIterConstruction(t *testing.T) {
	t.Parallel()
	for _, n := range []int{1, 2, 3, 5, 8} {
		viaArgs, err := ArgsToPath(iterSteps(n))
		if err != nil {
			t.Fatalf("n=%d: %v", n, err)
		}
		// The same shape assembled by hand through the exported Iter/Chain
		// constructors, which still normalize on entry.
		nested := Iter()
		for range n - 1 {
			nested = Iter(nested)
		}
		viaIter := Root(Chain(nested))
		if got, want := viaArgs.String(), viaIter.String(); got != want {
			t.Errorf("n=%d: ArgsToPath=%q Iter-nested=%q", n, got, want)
		}
	}
}
