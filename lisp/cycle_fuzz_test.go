// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
)

// cycleWatchdog bounds a whole round of walks over one generated value.  The
// walks build strings and compare pointers and nothing else -- no evaluation,
// no I/O -- so a round that has not finished in this much SCHEDULED time is
// not slow, it is not finishing.  See internal/fuzzwatch for why scheduled
// rather than wall-clock time.
const cycleWatchdog = 15 * time.Second

// maxKnots bounds how many self-references one input ties.
const maxKnots = 8

// FuzzCyclicValueWalks fuzzes the recursive walks over *LVal against values
// that contain themselves.
//
// The existing value-shaped targets (FuzzApplyStdlib, FuzzDumpJSON,
// FuzzSchemaValidate) all build their subject with fuzzval, which constructs
// trees -- so no target in the repository had ever handed a walk a value with
// a cycle in it, and issue #390 sat on main until the container-family targets
// reached assoc! and append! from source.  This target closes that gap
// directly: it builds a value, ties a fuzzer-chosen set of knots in it, and
// runs every walk the guard covers.
//
// The asserted properties, and only these:
//
//  1. Every walk terminates (the watchdog).  Before the guard, a cyclic value
//     did not merely hang these walks: it grew the goroutine stack until the
//     Go runtime killed the process, which recover() cannot catch and no
//     in-process assertion survives.  Under `go test -fuzz` that is a crash,
//     which is exactly how it should be reported.
//  2. No walk panics.
//  3. Rendering is deterministic: two renders of one value agree.  A guard
//     that depended on Go's randomised map iteration order would produce a
//     value that prints differently each time, which for a host that hashes
//     or logs rendered state is its own defect.
//  4. json:dump either serializes the value or refuses it with an error.  A
//     cycle has no JSON representation, so refusing is the right answer; what
//     is not allowed is a truncated document or a dead process.
//  5. The shared singletons are unmutated (issue #274).
//
// NOT asserted: what a cyclic value renders as, or whether two of them
// compare equal.  Those are pinned by the deterministic tests in
// lisp/cycle_test.go; asserting them here would only restate them against
// inputs whose shape nobody can read.
func FuzzCyclicValueWalks(f *testing.F) {
	for i, seed := range fuzzval.Seeds() {
		f.Add(seed, uint8(i%maxKnots)) //nolint:gosec // G115: bounded by maxKnots
	}
	f.Add([]byte{0x00}, uint8(1))
	f.Add([]byte{0xff, 0xff, 0xff, 0xff}, uint8(maxKnots))

	f.Fuzz(func(t *testing.T, data []byte, knots uint8) {
		before := lisp.TakeSingletonSnapshot()
		env := newCycleEnv(t)
		gen := fuzzval.New(data, env)

		v := gen.Value()
		other := gen.Value()
		knot(v, gen, int(knots)%maxKnots+1)
		knot(other, gen, int(knots)%maxKnots+1)

		done := make(chan string, 1)
		panicked := make(chan any, 1)
		go func() {
			defer func() {
				if r := recover(); r != nil {
					panicked <- r
				}
			}()
			done <- walkAll(v, other)
		}()

		budget := fuzzwatch.New(cycleWatchdog)
		wait := budget.Total()
		var rendered string
	wait:
		for {
			select {
			case rendered = <-done:
				break wait
			case r := <-panicked:
				panic(r)
			case <-time.After(wait):
				verdict, more, report := budget.Check()
				switch verdict {
				case fuzzwatch.Continue:
					wait = more
				case fuzzwatch.Inconclusive:
					t.Skipf("no verdict: the process was starved throughout (%s)", report)
					return
				default:
					// The walk goroutine is unstoppable by construction.
					// Leaking it is the price of reporting the failure; the
					// process is about to fail the test regardless.
					t.Fatalf("walking a value with %d knots did not terminate within %s of SCHEDULED time (%s)"+
						"\n--- input (%d bytes) ---\n%x",
						int(knots)%maxKnots+1, budget.Total(), report, len(data), data)
					return
				}
			}
		}

		if again := v.String(); again != rendered {
			t.Fatalf("rendering is not deterministic\n--- first ---\n%s\n--- second ---\n%s", rendered, again)
		}
		if drift := before.Verify(); drift != "" {
			t.Fatalf("walking mutated the shared singleton %s\n--- value ---\n%s", drift, rendered)
		}
	})
}

// walkAll runs every guarded walk over v and returns its rendering.
func walkAll(v, other *lisp.LVal) string {
	s := v.String()
	_ = v.Equal(v)
	_ = v.Equal(other)
	_ = other.Equal(v)
	_ = lisp.GoValue(v)
	// A cycle has no JSON representation: an error here is the correct
	// answer, and the bytes are discarded either way.  What is being asserted
	// is that Dump returns at all.
	_, _ = libjson.Dump(v, false)
	return s
}

// knot ties up to n self-references into v, turning the tree fuzzval built
// into a graph with cycles in it.
//
// Only sorted-maps, arrays and non-empty lists are written to, and only ones
// containers() collected: the shared Nil and Bool singletons are an empty
// list and two symbols, and writing through one of those corrupts every other
// holder of it (issue #274).  Every target value is a node from v itself, so
// a knot that lands on an ancestor is a real cycle and one that lands
// elsewhere is shared structure -- both are shapes the walks have to survive.
func knot(v *lisp.LVal, gen *fuzzval.Gen, n int) {
	nodes := containers(v)
	if len(nodes) == 0 {
		return
	}
	for i := range n {
		// The root is always a candidate target so that a knot is a cycle
		// even when the generated value has a single container.
		targets := append(nodes, v) //nolint:gocritic // deliberately a fresh slice
		into := nodes[gen.Intn(len(nodes))]
		target := targets[gen.Intn(len(targets))]
		switch into.Type {
		case lisp.LSortMap:
			into.MapSet(string(rune('a'+i%26)), target)
		case lisp.LArray:
			into.Cells[1].Cells = append(into.Cells[1].Cells, target)
			if into.Cells[0].Len() == 1 {
				into.Cells[0].Cells[0].Int++
			}
		default:
			into.Cells = append(into.Cells, target)
		}
	}
}

// containers collects the nodes of v that THIS INPUT BUILT and that can be
// written to, bounded so that a value already carrying a cycle cannot make
// this walk the unbounded one.
//
// "This input built" is the load-bearing half, and it was missing.  A
// generated value may CONTAIN a function -- fuzzval's fun() kinds 2 and 3
// return the environment's own globals, straight from the seed corpus via
// []byte{kindFun, 2} -- and an LFun's cells are its formals and its body.
// For a builtin, Cells[0] is the formals list from the package-level builtin
// table: constructed once when the lisp package is initialized, stored into
// every LFun by pointer, and therefore shared by every LEnv the process will
// ever create.  Collecting it handed knot() the standard library to write
// through, which is what issue #398 was: one seed rewrote lisp:car's
// signature, every assertion here still passed, and environments built
// afterwards -- in later tests, in later targets -- got a car that rejects
// its own argument.
//
// So nothing below an LFun is collected or descended into.  The subtree is
// not ours: for a builtin it is shared process-wide, and for a lambda it is a
// body the interpreter may hold references into.  On the sealed-AST branches
// the same rule is spelled `if v.IsSealed() { return }`; main has no seal
// bit, and on main the sealed subtrees a generated value can reach are
// exactly the ones under a function.
//
// The cost is two seeds' worth of container, and only for values that are
// nothing BUT a function -- see TestCycleSeedsStillTieCycles, which pins how
// much of the corpus still ties a real cycle.
func containers(v *lisp.LVal) []*lisp.LVal {
	const maxNodes = 256
	var out []*lisp.LVal
	seen := make(map[*lisp.LVal]struct{}, maxNodes)
	var walk func(*lisp.LVal)
	walk = func(v *lisp.LVal) {
		if v == nil || len(out) >= maxNodes {
			return
		}
		if _, ok := seen[v]; ok {
			return
		}
		seen[v] = struct{}{}
		// SEALED nodes are not candidates, and their subtrees are not walked.
		//
		// knot() writes through Cells on whatever this returns. A generated
		// value can reach the environment's own function objects (fuzzval's
		// fun() hands back globals), and a function's cells are its formals
		// and BODY -- shared, sealed parse nodes belonging to the stdlib that
		// InitializeUserEnv loaded. Knotting into one does not build a cyclic
		// test value; it corrupts the standard library for every later test in
		// the process, which is the substrate#378 class this branch exists to
		// close. The sealed-AST teardown oracle caught exactly that: three
		// mutated parse trees, reported by VerifySealedASTs and by nothing
		// else.
		//
		// Nothing is lost. Everything the target means to knot -- the lists,
		// vectors and sorted-maps fuzzval constructs -- is runtime storage and
		// unsealed. A sealed node's descendants are all sealed, so returning
		// here skips the whole subtree.
		if v.IsSealed() {
			return
		}
		switch v.Type {
		case lisp.LFun:
			// Not ours to write, and not ours to walk into.  See the doc
			// comment above and issue #398.
			return
		case lisp.LSortMap:
			out = append(out, v)
			for _, pair := range v.MapEntries().Cells {
				walk(pair.Cells[1])
			}
			return
		case lisp.LArray:
			if v.Cells[0].Len() == 1 {
				out = append(out, v)
			}
		case lisp.LSExpr:
			// An empty list may be the shared Nil singleton.
			if len(v.Cells) > 0 {
				out = append(out, v)
			}
		default:
			// Not a container: nothing can be knotted into it, but its cells
			// are still walked below (a tagged value holds its user data
			// there).
		}
		for _, c := range v.Cells {
			walk(c)
		}
	}
	walk(v)
	return out
}

// newCycleEnv exists only so the generator can build tagged values, which need
// an LEnv to stamp a source location.
func newCycleEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}
