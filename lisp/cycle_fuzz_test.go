// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/internal/walkraw"
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
// runs every walk the cycle guard covers (String, Equal, GoValue, json:dump)
// plus the two value-REBUILDING walks that carry a header memo of their own
// instead of the guard: (*LVal).Copy (lisp/copier.go, memoised in #604; it
// used to recurse without bound on a self-containing value) and detach
// (lisp/detach.go, reached through internal/walkraw).  Until this target
// each of those was driven on a cycle by one fixed shape --
// TestCopyTerminatesOnACycle and TestCopyMemoSpillsPastTheInlineArray for
// Copy, lisp/detach_test.go's cycle cases for detach -- and by nothing the
// fuzzer built.
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
//  6. Copy returns a value that is the source's own and indistinguishable
//     from it: it renders as the source renders (up to the heap addresses
//     one arm of String prints -- see withoutAddresses); Equal gives
//     the same answer for it as for the source -- against the source, and
//     against the other generated value -- because what Equal promises a
//     cyclic value is the greatest fixed point over its unfolding and the
//     copy's unfolding IS the source's, so the agreement is asserted and the
//     answer itself is not; and, walked in lockstep with the source, it has
//     one fresh header per source header, the same copied header wherever
//     one source header is reached twice (so a cycle closes onto the copy),
//     and no *MapData or *[]byte payload reachable from both sides.  Those
//     are Copy's documented memos (lisp/copier.go); TestCopyTerminatesOnACycle
//     and TestCopyMeetsTheAliasGuard each pin one shape of them, this pins
//     every shape the generator ties.
//  7. Detach either returns a value or refuses with an error -- an LFun, or
//     an LNative whose payload has no CloneNative, both of which fuzzval
//     builds, are refusals by contract -- and never hangs or panics.  A value
//     it does return meets property 6 word for word.  Nothing is asserted
//     about the error's text.
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
	f.Add(cycleSpillSeed, uint8(maxKnots-1))

	f.Fuzz(func(t *testing.T, data []byte, knots uint8) {
		before := lisp.TakeSingletonSnapshot()
		env := newCycleEnv(t)
		gen := fuzzval.New(data, env)

		v := gen.Value()
		other := gen.Value()
		knot(v, gen, int(knots)%maxKnots+1)
		knot(other, gen, int(knots)%maxKnots+1)

		done := make(chan walkResult, 1)
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
		var res walkResult
	wait:
		for {
			select {
			case res = <-done:
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

		rendered := res.rendered
		if len(res.failures) > 0 {
			t.Fatalf("%d property violation(s) with %d knots:\n%s\n--- value ---\n%s\n--- input (%d bytes) ---\n%x",
				len(res.failures), int(knots)%maxKnots+1, strings.Join(res.failures, "\n"), rendered, len(data), data)
		}
		if again := v.String(); again != rendered {
			t.Fatalf("rendering is not deterministic\n--- first ---\n%s\n--- second ---\n%s", rendered, again)
		}
		if drift := before.Verify(); drift != "" {
			t.Fatalf("walking mutated the shared singleton %s\n--- value ---\n%s", drift, rendered)
		}
	})
}

// cycleSpillSeed builds a list of seven lists of seven ints: 57 headers, past
// the copier's inline header memo (lisp.CopierSmallMemo), so that with the
// knots the corpus ties into it the memo lookup that finds a header
// remembered BEFORE the spill runs on a fuzzer-built value.  fuzzval caps a
// sequence at eight cells, so TestCopyMemoSpillsPastTheInlineArray's flat
// 62-element shape is unreachable by generation; the spill is reachable by
// nesting, and this seed reaches it in the first generation instead of
// leaving it to mutation.  TestCycleSpillSeedOutgrowsTheInlineMemo pins that
// it still does.
//
// Bytes: kind tag 12 is QExpr, 7 is the cell count, kind tag 3 is Int and
// its two zero bytes pick interestingInts[0] (see fuzzval.construct).
var cycleSpillSeed = func() []byte {
	seed := []byte{12, 7}
	for range 7 {
		seed = append(seed, 12, 7)
		for range 7 {
			seed = append(seed, 3, 0, 0)
		}
	}
	return seed
}()

// TestCycleSpillSeedOutgrowsTheInlineMemo pins what cycleSpillSeed is in the
// corpus for: the value it builds has more headers than the copier's inline
// memo holds, and knotting it as the target does ties a real cycle, so the
// header-memo spill is exercised by a seed and not only by whatever the
// mutator happens to find.
func TestCycleSpillSeedOutgrowsTheInlineMemo(t *testing.T) {
	gen := fuzzval.New(cycleSpillSeed, newCycleEnv(t))
	v := gen.Value()
	if n := reachableHeaders(v); n <= lisp.CopierSmallMemo {
		t.Fatalf("cycleSpillSeed builds %d headers, want more than the inline memo's %d:\n%s", n, lisp.CopierSmallMemo, v)
	}
	knot(v, gen, maxKnots)
	if s := v.String(); !strings.Contains(s, "#<cycle>") {
		t.Fatalf("knotting cycleSpillSeed's value did not tie a cycle:\n%s", s)
	}
	if fails := checkCopy(v, lisp.Nil(), v.String()); len(fails) > 0 {
		t.Fatalf("Copy over the knotted seed:\n%s", strings.Join(fails, "\n"))
	}
}

// reachableHeaders counts the distinct *LVal headers reachable from v through
// cells and sorted-map values.
func reachableHeaders(v *lisp.LVal) int {
	seen := make(map[*lisp.LVal]bool)
	var walk func(*lisp.LVal)
	walk = func(v *lisp.LVal) {
		if v == nil || seen[v] {
			return
		}
		seen[v] = true
		for _, c := range v.Cells {
			walk(c)
		}
		if v.Type == lisp.LSortMap && v.Map() != nil {
			for _, k := range v.Map().Keys().Cells {
				val, _ := v.Map().Get(k)
				walk(val)
			}
		}
	}
	walk(v)
	return len(seen)
}

// walkResult is what one round of walks over a value reports back to the
// test goroutine: the rendering, and every property the rebuilding walks
// violated.  The walks run on a goroutine the watchdog can abandon, and
// t.Fatalf may only be called from the test goroutine, so the verdicts
// travel as strings.
type walkResult struct {
	rendered string
	failures []string
}

// walkAll runs every guarded walk over v, then Copy and detach, and returns
// the rendering with whatever the rebuilding walks got wrong.
func walkAll(v, other *lisp.LVal) walkResult {
	s := v.String()
	_ = v.Equal(v)
	_ = v.Equal(other)
	_ = other.Equal(v)
	_ = lisp.GoValue(v)
	// A cycle has no JSON representation: an error here is the correct
	// answer, and the bytes are discarded either way.  What is being asserted
	// is that Dump returns at all.
	_, _ = libjson.Dump(v, false)
	res := walkResult{rendered: s}
	res.failures = append(res.failures, checkCopy(v, other, s)...)
	res.failures = append(res.failures, checkDetach(v, other, s)...)
	return res
}

// checkCopy runs (*LVal).Copy over v, whose rendering is s, and returns the
// properties the copy violates (property 6 of the target).
func checkCopy(v, other *lisp.LVal, s string) []string {
	cp := v.Copy()
	if cp == nil || cp == v {
		return []string{"Copy: returned the source (or nil) instead of a copy"}
	}
	return checkRebuilt("Copy", v, cp, other, s)
}

// checkDetach runs detach over v and returns the properties a returned value
// violates (property 7).  An error is a correct outcome -- detach refuses an
// LFun and an LNative without a cloner by contract -- so nothing is asserted
// about it beyond its being an error and not also a value.
func checkDetach(v, other *lisp.LVal, s string) []string {
	dt, err := walkraw.Detach(v)
	if err != nil {
		if dt != nil {
			return []string{"Detach: returned both a value and an error"}
		}
		return nil
	}
	if dt == nil || dt == v {
		return []string{"Detach: returned the source (or nil) with no error"}
	}
	return checkRebuilt("Detach", v, dt, other, s)
}

// checkRebuilt is the property both rebuilding walks share: cp, built from
// v by the walk named what, renders as v renders, compares as v compares,
// and is v's own.
func checkRebuilt(what string, v, cp, other *lisp.LVal, s string) []string {
	var fail []string
	if got := cp.String(); withoutAddresses(got) != withoutAddresses(s) {
		fail = append(fail, what+": the copy renders differently from the source\n--- source ---\n"+s+"\n--- copy ---\n"+got)
	}
	// Equal on a cyclic value is the greatest fixed point over its unfolding
	// (lisp/lisp.go, Equal), and the copy's unfolding is the source's.  That
	// makes the agreement below a consequence of the contract without
	// asserting the answer, which for a value carrying an LFun, an LNative or
	// a NaN is false against itself, and is the source's business.
	self := lisp.True(v.Equal(v))
	if got := lisp.True(cp.Equal(v)); got != self {
		fail = append(fail, fmt.Sprintf("%s: copy.Equal(source) is %t, source.Equal(source) is %t", what, got, self))
	}
	if got := lisp.True(v.Equal(cp)); got != self {
		fail = append(fail, fmt.Sprintf("%s: source.Equal(copy) is %t, source.Equal(source) is %t", what, got, self))
	}
	if want, got := lisp.True(v.Equal(other)), lisp.True(cp.Equal(other)); got != want {
		fail = append(fail, fmt.Sprintf("%s: copy.Equal(other) is %t, source.Equal(other) is %t", what, got, want))
	}
	return append(fail, lockstep(what, v, cp)...)
}

// goAddress matches a heap address inside a rendering.
var goAddress = regexp.MustCompile(`0x[0-9a-f]+`)

// withoutAddresses blanks the heap addresses in a rendering, so that a copy
// can be compared against its source through the one arm of String that
// prints them: strNested's default renders a type it has no case for as
// `#<type %#v>`, the Go struct literal of the header, and an LQSymbol
// reaches that arm.  For a located one (fuzzval's selector top bit; seeds
// 8c00000000000000 and 0cfffefdfcfbfaf9) the literal carries the address of
// its *token.Location, which every rebuilding walk copies -- so the source
// and its copy render the same value at two addresses.  That is a defect of
// the rendering, not of the copy (detach, the reference hermetic walk, fails
// the raw comparison on exactly the same seeds), and it is not this
// target's: the addresses are blanked and everything else about the
// rendering is still compared.
func withoutAddresses(s string) string {
	return goAddress.ReplaceAllLiteralString(s, "0x?")
}

// lockstep walks v and cp together and reports every way cp is not v's own:
// a header cp shares with v, a source header reached twice whose copies
// differ (the header memo), a *MapData or *[]byte payload shared between the
// two sides (the payload memos), or a shape difference, which would be a
// rendering difference caught above but is named here by path.
//
// Sealed nodes and function values are not descended: they are not this
// input's subtree (see containers), detach refuses an LFun outright, and
// what Copy does below one is pinned elsewhere (lisp/copier_test.go).
func lockstep(what string, v, cp *lisp.LVal) []string {
	var fail []string
	report := func(format string, args ...any) {
		fail = append(fail, what+": "+fmt.Sprintf(format, args...))
	}
	pairs := make(map[*lisp.LVal]*lisp.LVal)
	srcMaps, cpMaps := make(map[*lisp.MapData]string), make(map[*lisp.MapData]string)
	srcBytes, cpBytes := make(map[*[]byte]string), make(map[*[]byte]string)
	var walk func(path string, s, c *lisp.LVal)
	walk = func(path string, s, c *lisp.LVal) {
		if s == nil || c == nil {
			if s != c {
				report("%s: source %p, copy %p", path, s, c)
			}
			return
		}
		if prev, ok := pairs[s]; ok {
			if prev != c {
				report("%s: a source header reached twice was copied twice", path)
			}
			return
		}
		pairs[s] = c
		if s == c {
			report("%s: the copy holds the source's header", path)
			return
		}
		if s.Type != c.Type {
			report("%s: source is %v, copy is %v", path, s.Type, c.Type)
			return
		}
		if s.IsSealed() || s.Type == lisp.LFun {
			return
		}
		switch s.Type {
		case lisp.LSortMap:
			sm, cm := s.Map(), c.Map()
			if sm == nil || cm == nil {
				if sm != cm {
					report("%s: source map data %p, copy map data %p", path, sm, cm)
				}
				return
			}
			if sm == cm {
				report("%s: the copy shares the source's *MapData", path)
				return
			}
			srcMaps[sm], cpMaps[cm] = path, path
			keys, ckeys := sm.Keys().Cells, cm.Keys().Cells
			if len(keys) != len(ckeys) {
				report("%s: source has %d keys, copy has %d", path, len(keys), len(ckeys))
				return
			}
			for _, k := range keys {
				sv, _ := sm.Get(k)
				cv, _ := cm.Get(k)
				walk(path+"["+k.String()+"]", sv, cv)
			}
			return
		case lisp.LBytes:
			sb, _ := s.Native.(*[]byte)
			cb, _ := c.Native.(*[]byte)
			if sb != nil && cb != nil {
				if sb == cb {
					report("%s: the copy shares the source's *[]byte", path)
				}
				srcBytes[sb], cpBytes[cb] = path, path
			}
		default:
			// Everything else keeps its children in Cells, walked below.
		}
		if len(s.Cells) != len(c.Cells) {
			report("%s: source has %d cells, copy has %d", path, len(s.Cells), len(c.Cells))
			return
		}
		for i := range s.Cells {
			walk(fmt.Sprintf("%s.Cells[%d]", path, i), s.Cells[i], c.Cells[i])
		}
	}
	walk("v", v, cp)
	for md, path := range cpMaps {
		if src, ok := srcMaps[md]; ok {
			report("the *MapData at %s of the copy is the source's at %s", path, src)
		}
	}
	for b, path := range cpBytes {
		if src, ok := srcBytes[b]; ok {
			report("the *[]byte at %s of the copy is the source's at %s", path, src)
		}
	}
	return fail
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
