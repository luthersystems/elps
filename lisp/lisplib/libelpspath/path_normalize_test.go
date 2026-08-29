// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"runtime"
	"strings"
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
	sel := "." + strings.Repeat("[]", steps)
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

// TestNormalizePathsIsNotQuadratic is the second cost regression, and it
// measures allocated BYTES where TestNormalizePathsIsNotExponential measures
// allocation COUNT. The distinction is the whole point: the chain used to be
// assembled by prepending, append([]Path{path}, curChain...), which allocates
// one slice per step either way -- the same O(n) count -- while COPYING every
// element already placed, so the bytes were O(n^2).
//
// Measured at 3200 dot steps before the fix: 38.16ms, against 0.26ms after,
// and the growth was unmistakably superlinear (800 -> 1600 cost 3.4x, 1600 ->
// 3200 cost 4.8x). It is far milder than the exponential -- a ~6KB selector
// to reach 38ms, where the exponential needed 45 bytes -- but it is the same
// shape: cost superlinear in the length of an input a caller may not control.
//
// The bound below sits between the two by orders of magnitude. Quadratic
// copying at n=2000 moves roughly 2000*2000/2 pointers, some 16MB; linear
// assembly moves a few tens of KB.
func TestNormalizePathsIsNotQuadratic(t *testing.T) {
	// Not t.Parallel(): ReadMemStats reports process-wide totals, so a
	// concurrent test's allocations would land in this one's delta.
	const (
		steps = 2000
		limit = 4 << 20 // 4MiB, vs ~16MiB quadratic and ~tens of KB linear
	)
	args := make([]*lisp.LVal, steps)
	for i := range args {
		args[i] = lisp.String("k")
	}
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	if _, err := ArgsToPath(args); err != nil {
		t.Fatalf("ArgsToPath: %v", err)
	}
	runtime.ReadMemStats(&after)
	used := after.TotalAlloc - before.TotalAlloc
	if used > limit {
		t.Errorf("ArgsToPath with %d steps allocated %d bytes, want <= %d "+
			"-- the chain is being assembled by prepending again (issue #565)",
			steps, used, limit)
	}
	t.Logf("%d steps allocated %d bytes", steps, used)
}

// TestStringIsLinearInNesting is the cost regression for path RENDERING.
//
// Path.String() returns a string, so a composite path rendered by asking each
// child for its own string and copying that into the parent's: one
// full-length allocation and copy per level of nesting, O(depth^2) in bytes.
// Measured on nested iterators before the fix, 200 -> 400 cost 5.1x and 400
// -> 800 cost 4.2x, reaching 706us; it is 13.6us after, and doubling cleanly.
//
// The three composites -- root, chain and iter -- now write into one builder
// through the unexported stringAppender interface. The leaves keep their
// existing String() and satisfy the interface by delegating to it, so they
// cannot drift from it.
//
// Bytes again rather than allocation count: the quadratic was in how much was
// copied, not in how many times.
func TestStringIsLinearInNesting(t *testing.T) {
	// Not t.Parallel(): ReadMemStats reports process-wide totals.
	const (
		depth = 600
		limit = 1 << 20 // 1MiB, vs ~350KB quadratic and ~10s of KB linear
	)
	steps := make([]*lisp.LVal, depth)
	for i := range steps {
		steps[i] = lisp.Symbol("*")
	}
	p, err := ArgsToPath(steps)
	if err != nil {
		t.Fatalf("ArgsToPath: %v", err)
	}
	var before, after runtime.MemStats
	runtime.GC()
	runtime.ReadMemStats(&before)
	got := p.String()
	runtime.ReadMemStats(&after)
	used := after.TotalAlloc - before.TotalAlloc

	// The rendering itself must still be right: "." then depth * "[]".
	if want := "." + strings.Repeat("[]", depth); got != want {
		t.Fatalf("String() = %q (len %d), want %q (len %d)",
			truncate(got), len(got), truncate(want), len(want))
	}
	if used > limit {
		t.Errorf("String() over %d nested iterators allocated %d bytes, want <= %d "+
			"-- composition is materialising each child's string again", depth, used, limit)
	}
	t.Logf("depth %d rendered %d bytes using %d bytes of allocation", depth, len(got), used)
}

// TestStringAppenderAgreesWithString guards the one drift the split allows:
// a type whose appendString says something its String() does not.
func TestStringAppenderAgreesWithString(t *testing.T) {
	t.Parallel()
	paths := []Path{
		Dot("a"), Dot(`a"b`), Dot(""),
		Index(0), Index(-1),
		Range(1, 3, false), Range(1, 0, true),
		Iter(), Iter(Dot("a")),
		Chain(), Chain(Dot("a"), Index(0)),
		Root(Chain(Dot("a"), Iter(), Dot("b"))),
		Root(Chain(Iter(Chain(Iter(), Dot("x"))))),
	}
	for _, p := range paths {
		a, ok := p.(stringAppender)
		if !ok {
			t.Errorf("%T does not implement stringAppender", p)
			continue
		}
		var sb strings.Builder
		a.appendString(&sb)
		if got, want := sb.String(), p.String(); got != want {
			t.Errorf("%T: appendString wrote %q, String() returned %q", p, got, want)
		}
	}
}

func truncate(s string) string {
	if len(s) <= 60 {
		return s
	}
	return s[:60] + "..."
}
