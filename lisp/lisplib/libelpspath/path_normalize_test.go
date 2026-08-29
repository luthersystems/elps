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

// TestNormalizePathsIsIdempotent pins the invariant normalizePaths RESTS ON.
//
// Its iterator branch builds directly instead of calling Iter, which would
// re-enter Chain and re-normalize the tail. Skipping that re-entry is sound
// only if normalizePaths is idempotent -- if a second pass over an
// already-normalized chain returns the same chain. It is: expandPaths
// flattens a normalized chain back to exactly the sequence the loop
// consumed to build it, and the loop rebuilds it.
//
// If that stops holding, paths change what they MEAN rather than merely
// what they cost, and no cost test would notice. This is the test that
// would.
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

// TestNormalizePathsIsNotExponential guards normalizePaths against its
// iterator branch re-entering Chain (issue #565): one re-entry per iterator,
// each over a structure the previous rebuilt, is 2^n in Path CONSTRUCTION
// alone, with no document in sight. It is reachable from the shipped ?
// builtin and from a 45-byte selector string, so both surfaces are bounded
// below.
//
// It asserts ALLOCATIONS rather than wall time deliberately: a timing bound
// flakes on a loaded CI runner, while allocation count is exactly as
// exponential as the work is. Linear construction is ~8 allocs per step
// (201 at n=24) and the exponential is in the millions, so the bound has
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
	// The same path reached through the selector parser, since a 45-byte
	// selector string is the other surface this cost arrives on -- and it
	// is BOUNDED here, not merely executed. Calling ParseSelector and
	// checking only that err is nil, which is what this did at first, would
	// take 4.7 seconds against the exponential and still pass.
	sel := "." + strings.Repeat("[]", steps)
	selAllocs := testing.AllocsPerRun(3, func() {
		if _, err := ParseSelector(sel); err != nil {
			t.Fatalf("ParseSelector(%q): %v", sel, err)
		}
	})
	if selAllocs > limit {
		t.Errorf("ParseSelector over %d iterators made %.0f allocations, want <= %d "+
			"-- the selector surface reaches the same normalization (issue #565)",
			steps, selAllocs, limit)
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

// TestNormalizePathsIsNotQuadratic measures allocated BYTES where
// TestNormalizePathsIsNotExponential measures allocation COUNT, and the
// distinction is the whole point: assembling the chain by prepending
// allocates one slice per step either way -- the same O(n) count, invisible
// to a count-based bound -- while COPYING every element already placed, so
// the bytes are O(n^2).
//
// The bound sits between the two by orders of magnitude: quadratic copying
// at n=2000 moves roughly 2000*2000/2 pointers, some 16MB, where linear
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
// Path.String() returns a string, so a composite that renders by asking each
// child for its own string and copying that into the parent's costs one
// full-length allocation and copy per level of nesting -- O(depth^2) in
// bytes. The three composites write into one builder through the unexported
// stringAppender interface instead; the leaves keep their existing String()
// and satisfy the interface by delegating to it, so the two cannot drift.
//
// Bytes rather than allocation count: the quadratic is in how much gets
// copied, not how many times.
func TestStringIsLinearInNesting(t *testing.T) {
	// Not t.Parallel(): ReadMemStats reports process-wide totals.
	const (
		depth = 600
		// 64KiB: linear rendering allocates ~3.4KB at this depth and
		// quadratic ~782KB, so the bound sits ~19x above what must pass and
		// ~12x below what must fail. Do NOT raise it without re-measuring
		// both arms -- a bound above the quadratic cost makes this test
		// unable to fail, which is a state it has been in before.
		limit = 64 << 10
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
