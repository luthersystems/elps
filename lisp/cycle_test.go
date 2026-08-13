// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"runtime/debug"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// cyclicMap returns a sorted-map holding itself under every key in keys.  It
// is what (set 'm (sorted-map)) (assoc! m "k" m) builds, which is the repro in
// issue #390.
func cyclicMap(keys ...string) *LVal {
	m := SortedMap()
	for _, k := range keys {
		m.MapSet(k, m)
	}
	return m
}

// cyclicVector returns a vector holding itself n times, which is what
// (set 'v (vector 1)) (append! v v) builds.  It mirrors builtinAppendMutate:
// the value goes into the backing storage and the dimension is bumped.
func cyclicVector(n int) *LVal {
	v := Vector([]*LVal{Int(1)})
	for range n {
		v.Cells[1].Cells = append(v.Cells[1].Cells, v)
		v.Cells[0].Cells[0].Int++
	}
	return v
}

// nestList returns a list nested depth levels deep, with inner at the bottom.
func nestList(depth int, inner *LVal) *LVal {
	v := inner
	for range depth {
		v = SExpr([]*LVal{v})
	}
	return v
}

func TestStringOfCyclicValue(t *testing.T) {
	tagged := &LVal{Type: LTaggedVal, Str: "wrapper", Cells: []*LVal{Nil()}}
	taggedMap := SortedMap()
	taggedMap.MapSet("t", tagged)
	tagged.Cells[0] = taggedMap

	tests := []struct {
		name  string
		value *LVal
		want  string
	}{
		{"self-referential map", cyclicMap("k"), `(sorted-map "k" #<cycle>)`},
		{"map with two self keys", cyclicMap("a", "b"), `(sorted-map "a" #<cycle> "b" #<cycle>)`},
		{"self-referential vector", cyclicVector(1), `(vector 1 #<cycle>)`},
		{"vector holding itself twice", cyclicVector(2), `(vector 1 #<cycle> #<cycle>)`},
		{"cycle through a tagged value", tagged, `#{wrapper (sorted-map "t" #<cycle>)}`},
		{"cycle below an acyclic root", SExpr([]*LVal{Int(1), cyclicMap("k")}), `(1 (sorted-map "k" #<cycle>))`},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			assert.Equal(t, test.want, test.value.String())
		})
	}
}

func TestEqualOfCyclicValue(t *testing.T) {
	a, b := cyclicMap("k"), cyclicMap("k")
	different := SortedMap()
	different.MapSet("k", cyclicMap("k"))
	different.MapSet("other", Int(1))

	// Two operands whose infinite unfoldings agree compare equal, which is the
	// co-inductive answer Equal documents.  Two that differ compare unequal on
	// the strength of a difference found at a finite depth, so termination is
	// not bought by assuming equality.
	assert.True(t, True(a.Equal(a)), "a value must equal itself")
	assert.True(t, True(a.Equal(b)), "distinct values with the same unfolding must compare equal")
	assert.False(t, True(a.Equal(different)), "a real difference must still be reported")
	assert.False(t, True(different.Equal(a)), "the comparison must be symmetric")

	v := cyclicVector(1)
	assert.True(t, True(v.Equal(v)), "a self-referential vector must equal itself")
	assert.True(t, True(cyclicVector(2).Equal(cyclicVector(2))))
	assert.False(t, True(cyclicVector(1).Equal(cyclicVector(2))))
}

// TestCopyOfCyclicValueTerminates pins the reasoning that keeps (*LVal).Copy
// out of the guarded set.  Copy recurses through Cells, but a cycle a program
// can build has to pass through a sorted-map or an array, and Copy descends
// into neither: a map's data is rebuilt entry by entry, sharing the value
// pointers, and an array keeps its backing storage by reference.  So the deep
// copy stops one level into the cycle on its own.  If that ever stops being
// true -- if Copy starts copying either container's contents deeply -- this
// test is where it will be noticed, and Copy will need the guard too.
func TestCopyOfCyclicValueTerminates(t *testing.T) {
	for _, v := range []*LVal{
		cyclicMap("k"),
		cyclicVector(1),
		SExpr([]*LVal{Int(1), cyclicMap("k")}),
		SExpr([]*LVal{Int(1), cyclicVector(1)}),
	} {
		cp := v.Copy()
		require.NotNil(t, cp)
		// The copy renders one level deeper than the original: its root is a
		// fresh node whose child is the original cycle, so the walk reaches a
		// repeat one step later.  What is being pinned here is that Copy
		// returned at all.
		assert.Contains(t, cp.String(), cycleMark)
	}
}

func TestGoValueOfCyclicValue(t *testing.T) {
	m := cyclicMap("k")
	// A cyclic value has no Go representation, so it comes back as itself.
	assert.Equal(t, m, GoValue(m))

	_, ok := GoMap(m)
	assert.False(t, ok, "GoMap must report a cyclic map as not representable")

	l := SExpr([]*LVal{Int(1)})
	l.Cells = append(l.Cells, l)
	_, ok = GoSlice(l)
	assert.False(t, ok, "GoSlice must report a cyclic list as not representable")
}

func TestStampMacroExpansionOfCyclicValue(t *testing.T) {
	env := initSafetyTestEnv(t)
	v := cyclicVector(1)
	// Mirror a macro that built its expansion with append!: every node carries
	// the synthetic source location stampMacroExpansion is there to replace.
	loc, _ := Symbol("x").Source()
	callSite := &loc
	stampMacroExpansion(v, callSite, nil, env.Runtime)
	got, ok := v.Source()
	assert.True(t, ok, "the root must be stamped")
	assert.Equal(t, *callSite, got, "the root must be stamped")
	got, ok = v.Cells[1].Source()
	assert.True(t, ok, "the storage list must be stamped")
	assert.Equal(t, *callSite, got, "the storage list must be stamped")
}

// TestAcyclicRenderingIsUnchangedBelowAndAboveTheGuard pins the property the
// guard exists to preserve: it is invisible to any value that is not cyclic,
// at any depth.  Both cases nest past cycleGuardDepth, so the path set is
// live; both must still render in full.
func TestAcyclicRenderingIsUnchangedBelowAndAboveTheGuard(t *testing.T) {
	for _, depth := range []int{1, cycleGuardDepth - 1, cycleGuardDepth, cycleGuardDepth + 1, 4 * cycleGuardDepth} {
		t.Run(fmt.Sprintf("depth=%d", depth), func(t *testing.T) {
			want := strings.Repeat("(", depth) + "()" + strings.Repeat(")", depth)
			assert.Equal(t, want, nestList(depth, SExpr(nil)).String())
		})
	}

	// Shared substructure is a DAG, not a cycle: both occurrences must render
	// in full even though the second one reaches a node the walk has already
	// seen.  This is what makes the path set path-scoped rather than
	// walk-scoped.
	shared := SExpr([]*LVal{Int(7), String("s")})
	dag := nestList(2*cycleGuardDepth, SExpr([]*LVal{shared, shared}))
	want := strings.Repeat("(", 2*cycleGuardDepth) + `((7 "s") (7 "s"))` + strings.Repeat(")", 2*cycleGuardDepth)
	assert.Equal(t, want, dag.String())
}

// TestAcyclicEqualityIsUnchangedBelowAndAboveTheGuard is the equality half of
// the property above.  The negative cases matter most: a guard that reported
// equality whenever it ran out of patience would pass the positive ones.
func TestAcyclicEqualityIsUnchangedBelowAndAboveTheGuard(t *testing.T) {
	for _, depth := range []int{1, cycleGuardDepth - 1, cycleGuardDepth, cycleGuardDepth + 1, 4 * cycleGuardDepth} {
		t.Run(fmt.Sprintf("depth=%d", depth), func(t *testing.T) {
			assert.True(t, True(nestList(depth, Int(1)).Equal(nestList(depth, Int(1)))))
			assert.False(t, True(nestList(depth, Int(1)).Equal(nestList(depth, Int(2)))),
				"a difference at the bottom of a deep value must still be found")
			assert.False(t, True(nestList(depth, Int(1)).Equal(nestList(depth+1, Int(1)))))
		})
	}
}

// TestGuardedWalksDoNotAllocate is the cost half of the design: the common
// path carries a depth counter and the address of a stack-allocated
// cycleState, and neither may reach the heap.  Equality is the walk that can
// state this without noise, because it builds no strings.
func TestGuardedWalksDoNotAllocate(t *testing.T) {
	shallow := SExpr([]*LVal{Int(1), String("x"), SExpr([]*LVal{Int(2), Float(3)})})
	assert.Zero(t, testing.AllocsPerRun(100, func() {
		shallow.Equal(shallow)
	}), "comparing a shallow value must not allocate")

	deep := nestList(cycleGuardDepth-2, Int(1))
	assert.Zero(t, testing.AllocsPerRun(100, func() {
		deep.Equal(deep)
	}), "a value that stays below cycleGuardDepth must not allocate a path set")
}

// wideAtDepth returns a value nested depth levels deep whose innermost list
// holds width single-element lists.  Those lists are the only wide layer of
// containers in it, and they sit at depth+2 counting the outermost list as 1,
// which is what lets a caller place them exactly at cycleGuardDepth.
func wideAtDepth(depth, width int) *LVal {
	kids := make([]*LVal, width)
	for i := range kids {
		kids[i] = SExpr([]*LVal{Int(i)})
	}
	return nestList(depth, SExpr(kids))
}

// TestGuardIsFlatAcrossTheGuardDepth is the half of the cost claim a deep but
// narrow value cannot make: a value that is merely *wide* at exactly
// cycleGuardDepth must not pay for the path set per node.
//
// The two shapes compared are the same width and differ by one level of
// nesting, which places the wide layer either just below the guard depth or
// exactly on it.  Rendering and comparing them therefore does the same work
// bar a constant, and the whole difference is the guard.
//
// The path set is allocated once per walk because it lives on the shared
// state.  Built in the by-value guard instead, each of the width nodes at the
// guard depth would inherit a nil path from its parent and make its own --
// turning a fixed cost into a linear one, for an acyclic value, which is
// exactly what the guard promises not to do.
//
// Red-proof: moving path back into cycleGuard/pairGuard fails this at every
// width, with the difference tracking width instead of staying constant.
func TestGuardIsFlatAcrossTheGuardDepth(t *testing.T) {
	// One less level of nesting puts the wide layer below the guard depth,
	// where nothing is tracked at all.
	const onGuard = cycleGuardDepth - 2
	const below = onGuard - 1

	// Whatever the guard costs at the boundary, it is a fixed number of
	// allocations for the walk and not a number per node.
	const constantCost = 32

	for _, width := range []int{2, 64, 1024} {
		t.Run(fmt.Sprintf("String/width=%d", width), func(t *testing.T) {
			lo, hi := wideAtDepth(below, width), wideAtDepth(onGuard, width)
			base := testing.AllocsPerRun(20, func() { _ = lo.String() })
			got := testing.AllocsPerRun(20, func() { _ = hi.String() })
			assert.Less(t, got-base, float64(constantCost),
				"rendering a value wide at the guard depth allocated per node: %v vs %v", got, base)
		})
		t.Run(fmt.Sprintf("Equal/width=%d", width), func(t *testing.T) {
			lo, hi := wideAtDepth(below, width), wideAtDepth(onGuard, width)
			loOther, hiOther := wideAtDepth(below, width), wideAtDepth(onGuard, width)
			base := testing.AllocsPerRun(20, func() { _ = lo.Equal(loOther) })
			got := testing.AllocsPerRun(20, func() { _ = hi.Equal(hiOther) })
			assert.Zero(t, base, "comparing below the guard depth must not allocate")
			assert.Less(t, got-base, float64(constantCost),
				"comparing values wide at the guard depth allocated per node: %v vs %v", got, base)
		})
	}
}

// cyclicWalkEnv names the walk a helper subprocess should run.  It is read,
// never written, by anything that ships.
const cyclicWalkEnv = "ELPS_TEST_CYCLIC_WALK"

// TestCyclicValueDoesNotKillTheProcess is the assertion issue #390 actually
// needs, and it cannot be made in-process.  An unbounded walk over a cyclic
// value does not panic -- it grows the goroutine stack past the runtime's
// limit, and a stack overflow is a fatal runtime error that recover() cannot
// catch and no assertion inside this process would survive to report.  So each
// walk runs in a helper subprocess and the assertion is on that process's exit
// status and output.
//
// Red-proof: with any one guard reverted, the corresponding subtest fails with
// the helper's "fatal error: stack overflow" in the captured output rather
// than passing.
func TestCyclicValueDoesNotKillTheProcess(t *testing.T) {
	for name, want := range cyclicWalks {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), time.Minute)
			defer cancel()
			//nolint:gosec // os.Args[0] is this test binary
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=TestCyclicWalkHelper", "-test.v")
			cmd.Env = append(os.Environ(), cyclicWalkEnv+"="+name)
			out, err := cmd.CombinedOutput()
			require.NoError(t, err, "the %s walk must not kill the process:\n%s", name, out)
			assert.Contains(t, string(out), "RESULT: "+want,
				"the %s walk must produce a bounded result:\n%s", name, out)
		})
	}
}

// cyclicWalks are the walks TestCyclicValueDoesNotKillTheProcess runs in a
// subprocess, and the result each must produce.
var cyclicWalks = map[string]string{
	"render":       `(sorted-map "k" #<cycle>)`,
	"render-wide":  `(sorted-map "a" #<cycle> "b" #<cycle>)`,
	"render-error": `<native code>: test-error: (sorted-map "k" <native code>: test-error: #<cycle>)`,
	"equal":        "true",
	"govalue":      "unconverted",
	"stamp":        "stamped",
}

// TestCyclicWalkHelper is the subprocess half of
// TestCyclicValueDoesNotKillTheProcess.  It is inert unless armed.
func TestCyclicWalkHelper(t *testing.T) {
	walk := os.Getenv(cyclicWalkEnv)
	if walk == "" {
		t.Skip("set " + cyclicWalkEnv + " to run one cyclic walk in this process")
	}
	// An unguarded walk dies at the default 1GB stack limit, which costs a
	// second and a gigabyte before the run fails.  Failing at 16MB proves the
	// same thing far more cheaply, and no guarded walk comes near it.
	debug.SetMaxStack(16 << 20)

	var result string
	switch walk {
	case "render":
		result = cyclicMap("k").String()
	case "render-wide":
		result = cyclicMap("a", "b").String()
	case "render-error":
		// An error renders its cells, so a cycle through an error's data has
		// to be bounded by the same walk rather than restarting it.
		lerr := ErrorConditionf("test-error", "placeholder")
		m := SortedMap()
		m.MapSet("k", lerr)
		lerr.Cells = []*LVal{m}
		result = GoError(lerr).Error()
	case "equal":
		result = cyclicMap("k").Equal(cyclicMap("k")).String()
	case "govalue":
		m := cyclicMap("k")
		if GoValue(m) == m {
			result = "unconverted"
		}
	case "stamp":
		env := initSafetyTestEnv(t)
		v := cyclicVector(1)
		loc, _ := Symbol("x").Source()
		stampMacroExpansion(v, &loc, nil, env.Runtime)
		result = "stamped"
	default:
		t.Fatalf("unknown walk: %s", walk)
	}
	fmt.Printf("RESULT: %s\n", result)
}
