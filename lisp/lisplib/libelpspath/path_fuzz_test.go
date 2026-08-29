// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// FuzzPathEngine is the structure-aware fuzz target for the elpspath engine
// (issue #379).
//
// WHY A DEDICATED TARGET.  Coverage of the ?-family was indirect: the
// builtins are enumerated by lisplib's FuzzApplyStdlib (elpspath is
// default-loaded), which reaches them with GENERIC value seeds — argument
// lists built to exercise 240 unrelated callables.  The path engine's
// defects do not live in the argument types; they live in the interaction
// between a nested STRUCTURE and a sequence of STEPS that walks partway
// into it.  Both path_bounds defects (a negative index whose magnitude
// exceeds the sequence length, an out-of-range range bound) needed a step
// that resolves against a specific container length, which a generic value
// generator produces only by accident.  This target generates the two axes
// together and crosses them with all seven operations.
//
// THE TWO AXES
//
//   - Structures: nested sorted-maps, vectors, lists and scalars, built to
//     the depth and breadth the engine actually recurses through.
//   - Steps: string keys, integer indices (including negatives and the
//     values that break index arithmetic), the '* iterator and both range
//     spellings -- (range a b) and the open-ended (range a), whose end is
//     resolved against the document rather than carried in the step.
//     Steps are drawn PREFERENTIALLY from the keys and indices the
//     generated structure really has — a fuzzer that mostly misses spends
//     its budget on the "key absent" branch.  Missing keys and out-of-range
//     indices still appear; they are the other half of the boundary.
//
// Every input is crossed with one of the seven operations: ?, ?set!, ?set,
// ?del!, ?del, ?nil!, ?nil.
//
// INVARIANTS
//
//  1. No panic escapes.  The builtins are applied through env.FunCall, NOT
//     env.Eval: eval's recover() would convert a panic into an
//     `internal-panic` condition and the target would report a pass on a
//     crash.  FunCall has no recover, so a panicking input surfaces with the
//     Go stack of the offending frame.
//  2. Catchable errors only.  Every failure must arrive as an LError value —
//     the thing handler-bind can catch — and never as a nil *LVal or a Go
//     panic.  Both path_bounds defects violated exactly this: they panicked
//     where lisp code expected a condition.
//  3. The result renders.  LVal.String() terminates and does not panic; every
//     error path in the interpreter formats its operands.
//  4. Every LIST node's own cell layout is unchanged afterwards, for ALL
//     SEVEN operations including the mutating ones.  A list is the shape a
//     parsed program literal supplies, and the parse cache aliases one into
//     every warm environment, so an in-place write to a list corrupts a
//     value the caller never handed over.  A mutating operation is free to
//     rework a vector or a sorted-map in place — that is the documented
//     meaning of the "!" names — and must refuse to touch a list:
//     errMutateList is the guard, and a drift here is that guard failing,
//     which is the substrate#378 shared-AST corruption class.  See
//     fpListSpines for why the digest stops at the list's own cells.
//  5. Copy-vs-mutate.  The non-mutating operations (?, ?set, ?del, ?nil) must
//     leave the input document structurally unchanged — the documented
//     contract of the names without "!".  The mutating operations carry no
//     such obligation, and are held only to invariant 4.
//  6. The splice keeps what it did not replace.  When the whole path is a
//     single (range a b) over a sequence document, the six operations that
//     rewrite the sequence must return a result that BEGINS with the
//     source's elements before `a` and ENDS with the source's elements from
//     `b` on, by value and in order.  Only the middle is theirs to change.
//     See "THE RESULT AXIS" below for why this had to be added.
//  7. A view is written only where the path points.  The same operation is
//     run a second time against a VIEW — an array LVal whose cells are a
//     window onto a longer backing array, which is what the kernel's
//     (slice 'vector ...) and this package's rangePath.Get hand out — and
//     the backing array may come back changed only at the positions the step
//     names.  See viewWitness for why the harness's own documents cannot
//     express this and why issue #471 therefore survived every campaign.
//
// THE OWNERSHIP AXIS (issue #471).  Invariants 1-6 watch documents the
// harness BUILT, whose cells belong to nobody else.  A write that runs off
// the end of what the path named lands in the harness's own array and no
// invariant can see it.  #471 is exactly that write: the two deleteMutate
// paths compacted with `append(cells[:i], cells[i+1:]...)`, shifting the tail
// left through the caller's array — harmless on a document the operation
// owns, and on a view it scrambles the longer sequence the view aliases.  The
// answer stayed correct (a left shift copies before it overwrites), so
// invariants 1-3 passed; no list was touched, so 4 passed; the mutating ops
// are exempt from 5; and the ends survived, so 6 passed.  Seven invariants
// are not more than six unless the seventh sees a shape the others cannot.
//
// THE RESULT AXIS (issue found in review of #402).  Invariants 1-5 all watch
// the INPUT: they ask whether the document was left alone, never whether the
// answer was right.  That blind spot hid a real defect for the whole life of
// this target.  rangePath.setMutate built the splice with
// `append(cells[:from], setCells...)`, writing the replacement through the
// document's own backing array, and then read the tail `cells[to:]` back out
// of that overwritten array — so a replacement longer than the range it
// replaced returned the replacement's own cells where the source's tail
// belonged.  The input was untouched (the copying path splices a private
// copy), no error was raised, no list was written, and nothing panicked:
// every one of invariants 1-5 passed on a wrong answer.
//
// Two changes were needed, and either alone is insufficient:
//
//   - The VALUE axis.  The trailing value argument was always the string
//     "REPLACEMENT", which toCells rejects before the splice, so no input
//     this target could generate ever reached the splice with a usable
//     replacement — let alone one longer than the range.  g.value now
//     produces sequences of varying length as well.
//   - Invariant 6.  With sequences flowing in, the corruption still lands
//     only in the result, which nothing was reading.
//
// Invariant 6 is deliberately a SUB-property, not a reimplementation of the
// splice: it says nothing about the middle, only that the untouched ends
// survive.  That is the part the defect broke, and it holds identically for
// ?set (middle becomes the replacement), ?del (middle vanishes) and ?nil
// (middle becomes nils), so one check covers all six.  It borrows toCells
// and validateRange to agree with the engine on WHICH range is meant —
// neither is implicated in the defect, and an oracle that disagreed about
// bounds would report false positives instead of finding splice bugs.
//
// UPSTREAM NOTE on invariant 4: on the branch this target came from, it was
// expressed with lisp.LVal.SealAST and lisp.SealedASTFingerprint — the
// document was sealed before the call, which marked exactly the
// parser-producible shapes (lists, symbols, strings, numbers) and left the
// runtime-only shapes (vectors, sorted-maps) mutable.  Neither exists here,
// so the invariant is narrowed to the list nodes: the shape errMutateList is
// actually about, and the only parser-producible shape a path operation can
// write through.  Symbols, strings and numbers are leaf LVals that no path
// operation writes to, so the narrowing costs no reachable coverage there.
// What it does cost: the seal drew the mutable/immutable line for the whole
// document at once, so a sealed scalar deep inside a runtime container was
// covered too.  The shared-leaf write that TestCopyOpOnListDoesNotWrite-
// ThroughASharedLeaf pins is exactly such a write, and here it is caught by
// invariant 5 (for the copying ops, which is where it is reachable) and by
// that test rather than by this invariant.
//
// NOT ASSERTED
//
//   - Any particular error message, or WHICH inputs fail.  A step that
//     misses is a legitimate answer, not a defect; query_test.go pins the
//     specific outcomes.
//   - Deep-copy semantics of the copying operations.  All three copy helpers
//     are deep and agree on how deep, which is a property about pointer
//     identity that a value-shaped fuzzer cannot see: a shallow copy and a
//     deep one are equal by value and differ only in what a later write
//     reaches.  TestCopyHelpersAgreeOnNestingDepth asserts it directly.
//     Invariant 5 asserts what this target can see: the ORIGINAL is
//     unchanged.  (Until issue #395 copyMap was shallow where its siblings
//     were deep, and this target was silent throughout.)
//
// SEEDS.  Synthesized here — every seed is a byte string written in this
// file, chosen to reach each of the seven operations and each generator
// branch at least once.  No seed is derived from any downstream corpus.
func FuzzPathEngine(f *testing.F) {
	// One seed per operation, crossed with a spread of generator drivers
	// that reach the shape branches (map / vector / list / scalar) and the
	// step branches (key / index / iterator / range).
	drivers := [][]byte{
		{},
		{0x00},
		{0x01, 0x02, 0x03},
		{0x10, 0x21, 0x32, 0x43, 0x54},
		{0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa},
		{0x05, 0x00, 0x07, 0x01, 0x02, 0x00, 0x03, 0x09},
		{0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99},
		{0x07, 0x03, 0x01, 0x04, 0x01, 0x05, 0x09, 0x02, 0x06},
	}
	for op := range numPathOps {
		for _, d := range drivers {
			seed := append([]byte{byte(op)}, d...) //nolint:gocritic // deliberate new slice per seed
			f.Add(seed)
		}
	}
	// Invariant 7's seeds.  Added deliberately, and they are the whole
	// difference between the property running and not running: the drivers
	// above reach newViewWitness's shape ZERO times, because it needs a
	// vector document AND a step sequence of length exactly one AND a step
	// that names a strict, non-empty subset of that vector's positions, and
	// nothing above lines all three up.  A property the corpus never reaches
	// is a property that cannot fail, which is the failure mode #462 and #470
	// both hit.  TestViewWitnessSeedsAreReached pins that these do reach it.
	//
	// One per operation per step form, found by search over short inputs.
	for _, seed := range viewWitnessSeeds {
		f.Add(seed)
	}

	f.Fuzz(func(t *testing.T, data []byte) {
		env := fuzzPathEnv(t)

		g := &pathGen{b: data, budget: pathGenBudget}
		opIdx := int(g.next()) % numPathOps
		op := pathOps[opIdx]

		doc := g.doc(0)
		steps := g.steps(doc)

		// The list nodes are the parser-producible, cache-shared shapes;
		// vectors and sorted-maps are runtime values the "!" ops may rework.
		listRoots := collectLists(doc)
		fpListsBefore := fpListSpines(listRoots)
		fpDocBefore := fpAST([]*lisp.LVal{doc})

		// Invariant 6 setup, taken BEFORE the call: the "!" operations
		// rework the document in place, so the source's ends have to be
		// rendered now or there is nothing left to compare against.
		splice, spliceOK := newSpliceOracle(doc, steps)

		// Invariant 7 setup, taken BEFORE the call for the same reason and a
		// sharper one: the witness copies the document's cells, and a "!"
		// operation run first would have already shrunk the document, so the
		// window built afterwards is a different — and typically too small —
		// shape than the one the property is about.  Built after the call,
		// this property silently passes on a tree that has #471 in it.
		witness, witnessOK := newViewWitness(doc, steps)

		// The new value is drawn once and shared by both arms, so the view
		// arm below exercises the same operation the main arm did rather than
		// a second, unrelated draw from the generator.
		var newVal *lisp.LVal
		if op.needsValue {
			newVal = g.value()
		}

		args := make([]*lisp.LVal, 0, len(steps)+2)
		args = append(args, doc)
		args = append(args, steps...)
		if op.needsValue {
			args = append(args, newVal)
		}

		fun := env.GetGlobal(lisp.Symbol("elpspath:" + op.name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s is not a function, got %v", op.name, fun.Type)
		}

		// Invariant 1: FunCall, not Eval — see the doc comment.
		result := env.FunCall(fun, lisp.SExpr(args))

		// Invariant 2.
		if result == nil {
			t.Fatalf("%s returned a nil *LVal\n--- doc ---\n%s\n--- steps ---\n%s",
				op.name, doc, renderSteps(steps))
		}
		// Invariant 3.
		_ = result.String()

		// Invariant 4.
		if fpAfter := fpListSpines(listRoots); fpAfter != fpListsBefore {
			t.Fatalf("%s mutated a list node in place (fingerprint %s -> %s):"+
				" a program literal handed to this operation would be corrupted for"+
				" every environment sharing the parse (the substrate#378 class)"+
				"\n--- doc ---\n%s\n--- steps ---\n%s",
				op.name, fpListsBefore, fpAfter, doc, renderSteps(steps))
		}

		// Invariant 5.
		if !op.mutates {
			if fpAfter := fpAST([]*lisp.LVal{doc}); fpAfter != fpDocBefore {
				t.Fatalf("%s is a non-mutating operation but changed its input"+
					"\n--- doc after ---\n%s\n--- steps ---\n%s",
					op.name, doc, renderSteps(steps))
			}
		}

		// Invariant 6.
		if spliceOK && op.splices && result.Type != lisp.LError {
			if why := splice.check(result); why != "" {
				t.Fatalf("%s spliced a range and lost what it did not replace: %s"+
					"\n--- steps ---\n%s\n--- source ends ---\n%v ... %v"+
					"\n--- result ---\n%s",
					op.name, why, renderSteps(steps),
					splice.prefix, splice.suffix, result)
			}
		}

		// Invariant 7.  Run the same operation again on a VIEW of the same
		// shape, and require it to write only where its path points.
		if witnessOK {
			viewArgs := make([]*lisp.LVal, 0, len(steps)+2)
			viewArgs = append(viewArgs, witness.view)
			viewArgs = append(viewArgs, steps...)
			if op.needsValue {
				viewArgs = append(viewArgs, newVal)
			}
			_ = env.FunCall(fun, lisp.SExpr(viewArgs))
			if why := witness.check(); why != "" {
				t.Fatalf("%s through a view wrote into the backing array it does"+
					" not own: %s\n--- steps ---\n%s\n--- view ---\n%s",
					op.name, why, renderSteps(steps), witness.view)
			}
		}
	})
}

// viewWitness is invariant 7's apparatus (issue #471).
//
// WHY THIS AXIS EXISTS.  Every document invariants 1-6 watch is one the
// harness built and OWNS: its cells belong to no one else, so a write that
// runs off the end of what the path named lands in the harness's own array
// and is invisible.  A real caller has a second shape the harness never
// produced -- a VIEW: an ordinary array LVal whose cells are a window onto a
// LONGER sequence, handed out by the kernel's (slice 'vector ...) or by this
// package's own rangePath.Get.  Mutating builtins accept one, because there
// is nothing in an LVal that says "view".
//
// The alias battery has the same hole from the other side, and says so: it
// writes through COPIES, and a view is not a copy.  So the one shape in which
// an in-place compaction is visible was outside both.  That is why #471 --
// `append(cells[:index], cells[index+1:]...)` shifting a view's tail left
// through the aliased source's array -- survived every campaign and every
// battery run, and was found by hand instead.
//
// THE PROPERTY.  A path operation applied to a view may change the backing
// array only at the positions its path NAMES: {i} for an integer step,
// [from,to) for a range step, nothing for a step that resolves to no
// position.  Everything else in that array belongs to a sequence the caller
// never passed.
//
// It is stated as "named positions" rather than "no change at all" because
// element assignment through a view is legitimate and documented: (?set! w 0
// 97) sets the source's element 0, exactly as writing through any alias
// does.  #471 is not that.  It shifted every element after the deleted one,
// so positions the path never named moved -- which this catches while
// leaving the legitimate write alone.
//
// It is deliberately a SUB-property and applies only to the isolated
// single-step-on-a-vector shape, the same restriction newSpliceOracle takes:
// a longer path reaches its view through intermediate Gets whose own aliasing
// is a separate question, and an oracle that guessed at those would report
// false positives rather than find defects.
type viewWitness struct {
	view     *lisp.LVal
	backing  []*lisp.LVal
	snapshot []*lisp.LVal
	named    map[int]bool
	window   int
}

// newViewWitness builds a view over a padded copy of doc's cells, plus the
// before-snapshot and the set of positions the step is allowed to touch.
//
// The view is built over a COPY of the document rather than the document
// itself so that the extra call cannot disturb the assertions invariants 1-6
// have already made about doc.
//
// The window is clamped three-index, so the view carries no spare capacity:
// this is #471 and not #369/#373, and the clamp keeps the two apart.  A
// capacity-retention bug would need an append past len to show; every write
// this catches is WITHIN len, which is why no clamp anywhere prevents it.
func newViewWitness(doc *lisp.LVal, steps []*lisp.LVal) (*viewWitness, bool) {
	if len(steps) != 1 || doc.Type != lisp.LArray || len(doc.Cells) != 2 {
		return nil, false
	}
	if doc.Cells[0].Len() > 1 {
		return nil, false
	}
	named, ok := namedPositions(len(doc.Cells[1].Cells), steps[0])
	if !ok {
		return nil, false
	}
	cp, err := copyLVal(doc)
	if err != nil || cp.Type != lisp.LArray || len(cp.Cells) != 2 {
		return nil, false
	}
	cells := cp.Cells[1].Cells
	n := len(cells)
	if n == 0 {
		// An empty window has no interior to shift and no elements to lose.
		return nil, false
	}
	// The tail is what makes this a view rather than a whole sequence.  Its
	// markers are disjoint from everything the generators produce, so a
	// marker that moves names the defect instead of describing a coincidence.
	const viewPad = 3
	backing := make([]*lisp.LVal, n+viewPad)
	copy(backing, cells)
	for i := n; i < len(backing); i++ {
		backing[i] = lisp.String(fmt.Sprintf("V%d", i-n))
	}
	snapshot := make([]*lisp.LVal, len(backing))
	copy(snapshot, backing)
	return &viewWitness{
		view:     lisp.Array(nil, backing[0:n:n]),
		backing:  backing,
		snapshot: snapshot,
		named:    named,
		window:   n,
	}, true
}

// namedPositions is the set of backing positions a single step points at.
// It borrows resolveIndex and validateRange so the oracle agrees with the
// engine about WHICH positions are meant; neither is implicated in #471, and
// an oracle that disagreed about bounds would report false positives.
func namedPositions(n int, step *lisp.LVal) (map[int]bool, bool) {
	out := map[int]bool{}
	switch step.Type {
	case lisp.LInt:
		if i, ok := resolveIndex(n, step.Int); ok {
			out[i] = true
		}
		return out, true
	case lisp.LSExpr:
		if len(step.Cells) != 2 && len(step.Cells) != 3 {
			return nil, false
		}
		if head := step.Cells[0]; head.Type != lisp.LSymbol || head.Str != "range" {
			return nil, false
		}
		lo := step.Cells[1]
		if lo.Type != lisp.LInt {
			return nil, false
		}
		// The open-ended form names [from, n).  Modelling it here rather
		// than returning false is the point: a witness that bails leaves
		// the step unwatched, and the splice bounds are exactly what
		// issue #471 got wrong on the two-argument arm.
		hiInt, implicitTo := 0, true
		if len(step.Cells) == 3 {
			hi := step.Cells[2]
			if hi.Type != lisp.LInt {
				return nil, false
			}
			hiInt, implicitTo = hi.Int, false
		}
		from, to, err := validateRange(n, lo.Int, hiInt, implicitTo)
		if err != nil {
			// A range the engine refuses names nothing and must write nothing.
			return out, true
		}
		for i := from; i < to; i++ {
			out[i] = true
		}
		return out, true
	default:
		return nil, false
	}
}

// check reports the first backing position that moved without being named.
func (w *viewWitness) check() string {
	for i := range w.snapshot {
		if w.backing[i] == w.snapshot[i] || w.named[i] {
			continue
		}
		where := fmt.Sprintf("position %d, inside the view's window", i)
		if i >= w.window {
			where = fmt.Sprintf("position %d, PAST the view's window of %d", i, w.window)
		}
		// Identity as well as rendering: two different elements can print the
		// same (a vector of nils is the common case), and then a report that
		// quoted only the text would read "changed from () to ()".
		return fmt.Sprintf("%s changed from %s (%p) to %s (%p), and the path does not"+
			" name it (named: %v) — the operation compacted through the source's"+
			" array instead of one of its own",
			where, w.snapshot[i], w.snapshot[i], w.backing[i], w.backing[i],
			sortedNamed(w.named))
	}
	return ""
}

func sortedNamed(named map[int]bool) []int {
	out := make([]int, 0, len(named))
	for i := range named {
		out = append(out, i)
	}
	sort.Ints(out)
	return out
}

// spliceOracle holds the two ends of a sequence document that a single
// top-level range step is not allowed to disturb: everything before `from`
// and everything from `to` on, rendered by value before the operation runs.
type spliceOracle struct {
	prefix []string
	suffix []string
}

// newSpliceOracle recognises the isolated-splice shape — the whole path is
// one range step, in either spelling, and the document is a sequence —
// and snapshots its ends.
// Anything else returns false and the invariant simply does not apply.
func newSpliceOracle(doc *lisp.LVal, steps []*lisp.LVal) (spliceOracle, bool) {
	if len(steps) != 1 {
		return spliceOracle{}, false
	}
	step := steps[0]
	// BOTH range spellings, for the reason namedPositions gives for
	// modelling them: an oracle that bails leaves the step unwatched, and
	// the splice bounds are exactly what issue #471 got wrong. The
	// open-ended form resolves its end against the document rather than
	// carrying one, so it exercises a different arm of validateRange and
	// therefore a different set of bounds for the splice to get wrong.
	if step.Type != lisp.LSExpr || (len(step.Cells) != 2 && len(step.Cells) != 3) {
		return spliceOracle{}, false
	}
	if head := step.Cells[0]; head.Type != lisp.LSymbol || head.Str != "range" {
		return spliceOracle{}, false
	}
	lo := step.Cells[1]
	if lo.Type != lisp.LInt {
		return spliceOracle{}, false
	}
	hiInt, implicitTo := 0, true
	if len(step.Cells) == 3 {
		hi := step.Cells[2]
		if hi.Type != lisp.LInt {
			return spliceOracle{}, false
		}
		hiInt, implicitTo = hi.Int, false
	}
	cells, err := toCells(doc)
	if err != nil {
		return spliceOracle{}, false
	}
	from, to, err := validateRange(len(cells), lo.Int, hiInt, implicitTo)
	if err != nil {
		// A range the engine refuses is an error answer, not a splice.
		return spliceOracle{}, false
	}
	return spliceOracle{
		prefix: fpValues(cells[:from]),
		suffix: fpValues(cells[to:]),
	}, true
}

// check reports why result violates the invariant, or "" if it does not.
func (o spliceOracle) check(result *lisp.LVal) string {
	cells, err := toCells(result)
	if err != nil {
		// ?nil on an empty range can answer with a nil-ish value; that is
		// not a lost-tail report to make from here.
		return ""
	}
	got := fpValues(cells)
	if len(got) < len(o.prefix)+len(o.suffix) {
		return fmt.Sprintf("result holds %d cells, fewer than the %d untouched ones"+
			" it had to keep", len(got), len(o.prefix)+len(o.suffix))
	}
	for i, want := range o.prefix {
		if got[i] != want {
			return fmt.Sprintf("element %d is before the range and should still be %s, got %s",
				i, want, got[i])
		}
	}
	off := len(got) - len(o.suffix)
	for i, want := range o.suffix {
		if got[off+i] != want {
			return fmt.Sprintf("the element at result position %d is the source's"+
				" position %d, from after the range, and should still be %s, got %s"+
				" — the splice overwrote the tail before reading it",
				off+i, off+i-len(got)+len(o.suffix), want, got[off+i])
		}
	}
	return ""
}

// fpValues renders a cell slice for value comparison.  By value, not by
// pointer: a correct splice is free to hand back the source's own element
// LVals or copies of them, and invariant 6 is about content.
func fpValues(cells []*lisp.LVal) []string {
	out := make([]string, len(cells))
	for i, c := range cells {
		out[i] = c.String()
	}
	return out
}

// pathOp names one of the seven operations and the three facts the harness
// needs about it: whether a trailing new-value argument is required, whether
// it is allowed to change its input, and whether a range step makes it
// rewrite the whole sequence (invariant 6) rather than just read part of it.
type pathOp struct {
	name       string
	needsValue bool
	mutates    bool
	splices    bool
}

// Every operation but "?" splices: a range step has ?set/?del/?nil rebuild
// the sequence around the range, so the elements outside it must survive.
// "?" is excluded because it returns the range and nothing else, so it has
// no ends to keep.
var pathOps = []pathOp{
	{name: "?"},
	{name: "?set!", needsValue: true, mutates: true, splices: true},
	{name: "?set", needsValue: true, splices: true},
	{name: "?del!", mutates: true, splices: true},
	{name: "?del", splices: true},
	{name: "?nil!", mutates: true, splices: true},
	{name: "?nil", splices: true},
}

const numPathOps = 7

func init() {
	if len(pathOps) != numPathOps {
		panic("pathOps must list all seven elpspath operations")
	}
}

// fuzzPathEnv builds an environment with the elpspath package loaded.  Built
// per iteration for isolation, as the stdlib target does: the operations
// under test take their input as arguments, so a leaked global from one
// iteration could only confuse the next one's failure report.
func fuzzPathEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := LoadPackage(env); lerr.Type == lisp.LError {
		t.Fatalf("LoadPackage: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		t.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// collectLists returns every list node reachable from v.
//
// Every list, not the outermost: a list nested inside another list, a vector
// or a map is just as much a program literal, and each is checked on its own
// spine below.
//
// IMPORTANT: an LArray is [dims, data] and BOTH of those wrapper nodes are
// LSExpr — the array's element storage is a list by Type.  Reworking a
// vector in place is the documented meaning of the "!" names, so collecting
// those two would report every legitimate vector mutation as list
// corruption.  The walk therefore steps over an array's wrapper straight to
// its elements.
func collectLists(v *lisp.LVal) []*lisp.LVal {
	var roots []*lisp.LVal
	seen := map[*lisp.LVal]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > 64 || seen[v] {
			return
		}
		seen[v] = true
		// Only the container shapes reach other values.
		switch v.Type {
		case lisp.LSExpr:
			roots = append(roots, v)
			for _, c := range v.Cells {
				walk(c, depth+1)
			}
		case lisp.LArray:
			if len(v.Cells) == 2 {
				for _, c := range v.Cells[1].Cells {
					walk(c, depth+1)
				}
			}
		case lisp.LSortMap:
			m := v.Map()
			if m == nil {
				return
			}
			entries := make([]*lisp.LVal, m.Len())
			if lerr := m.Entries(entries); lerr.Type == lisp.LError {
				return
			}
			for _, e := range entries {
				if len(e.Cells) == 2 {
					walk(e.Cells[1], depth+1)
				}
			}
		default:
			for _, c := range v.Cells {
				walk(c, depth+1)
			}
		}
	}
	walk(v, 0)
	return roots
}

// fpListSpines fingerprints each list node's OWN cell layout: its quote flag
// and the identity of the cells it holds, one level, no recursion.
//
// That is exactly what an in-place write to a list changes — toCells hands
// back the live backing and the operations shift it or store a new slice
// over it — and it is deliberately blind to what happens further down.  A
// mutating operation is allowed to rework a vector or a sorted-map that
// happens to hang off a list, and a recursive digest would report those as
// list corruption.  Every list below is collected in its own right, so
// nesting costs no coverage.
//
// Cell identity is the pointer.  Go's collector does not move heap objects,
// so a pointer is stable across the operation under test.
func fpListSpines(lists []*lisp.LVal) string {
	var sb strings.Builder
	for _, l := range lists {
		fmt.Fprintf(&sb, "%p q=%v n=%d[", l, l.IsQuoted(), len(l.Cells))
		for _, c := range l.Cells {
			fmt.Fprintf(&sb, "%p,", c)
		}
		sb.WriteString("];")
	}
	return sb.String()
}

func renderSteps(steps []*lisp.LVal) string {
	var sb strings.Builder
	for i, s := range steps {
		if i > 0 {
			sb.WriteByte(' ')
		}
		sb.WriteString(s.String())
	}
	return sb.String()
}

// --- the structure-aware generator ---

// pathGenBudget caps how many LVals one input may build.  The generator is
// recursive (a map holds vectors that hold lists), so without a global cap a
// few bytes can ask for an exponentially large structure and the target
// spends its budget in the allocator rather than in the engine.
const pathGenBudget = 96

// pathGenMaxDepth bounds nesting independently of the budget: a 60-deep
// spine would test LVal.String()'s recursion rather than the path engine.
const pathGenMaxDepth = 4

// pathGenMaxLen bounds any one container.  Small on purpose — index and
// range arithmetic breaks at the ENDS of a sequence, and a short sequence
// puts the fuzzer's mutations near both ends.
const pathGenMaxLen = 5

// pathGenMaxSteps bounds a step sequence.  Longer than the deepest structure
// the generator builds, so paths that run off the end of the document are
// reachable.
const pathGenMaxSteps = 6

// pathKeys is the key pool.  A small closed pool is what makes hits and
// misses BOTH common: keys are drawn from it for structures and for steps,
// so a step lands on a real key often enough to reach the code past the
// lookup, and misses often enough to keep exercising the absent branch.
var pathKeys = []string{"a", "b", "c", "k", "", "x y", "0"}

// pathIndices are the integer steps.  The small values around the ends of a
// sequence, plus the magnitudes that made resolveIndex and validateRange
// fold a negative index into another negative one.
var pathIndices = []int{0, 1, 2, -1, -2, 5, -5, 1 << 31, -(1 << 31)}

type pathGen struct {
	b      []byte
	i      int
	budget int
}

// next consumes one byte, or 0 once the input is exhausted.  Reads past the
// end never fail, so a one-byte seed produces a small, boring structure
// rather than an error — which is what lets the fuzzer grow it.
func (g *pathGen) next() byte {
	if g.i >= len(g.b) {
		return 0
	}
	c := g.b[g.i]
	g.i++
	return c
}

func (g *pathGen) intn(n int) int {
	if n <= 0 {
		return 0
	}
	return int(g.next()) % n
}

// Structure kinds, ordered so the low tags are the cheap scalars: a mutator
// flipping a byte down shrinks the structure, which keeps crashers small.
const (
	shapeNil = iota
	shapeBool
	shapeInt
	shapeFloat
	shapeString
	shapeList
	shapeVector
	shapeMap
	numShapes
)

func (g *pathGen) doc(depth int) *lisp.LVal {
	if g.budget <= 0 {
		return lisp.Nil()
	}
	g.budget--

	shape := g.intn(numShapes)
	if depth >= pathGenMaxDepth && shape >= shapeList {
		// Collapse compound shapes onto a scalar at the depth limit rather
		// than truncating mid-structure: a half-built container would be a
		// shape no caller can construct.
		shape = g.intn(shapeList)
	}

	switch shape {
	case shapeBool:
		return lisp.Bool(g.next()%2 == 0)
	case shapeInt:
		return lisp.Int(pathIndices[g.intn(len(pathIndices))])
	case shapeFloat:
		return lisp.Float(float64(g.next()) / 4)
	case shapeString:
		return lisp.String(pathKeys[g.intn(len(pathKeys))])
	case shapeList:
		return lisp.QExpr(g.cells(depth))
	case shapeVector:
		return lisp.Array(nil, g.cells(depth))
	case shapeMap:
		sm := lisp.SortedMap()
		m := sm.Map()
		n := g.intn(pathGenMaxLen + 1)
		for range n {
			if g.budget <= 0 {
				break
			}
			key := pathKeys[g.intn(len(pathKeys))]
			if lerr := m.Set(lisp.String(key), g.doc(depth+1)); lerr.Type == lisp.LError {
				break
			}
		}
		return sm
	default:
		return lisp.Nil()
	}
}

// Value kinds for the trailing new-value argument of ?set / ?set!.
//
// This axis used to be the constant string "REPLACEMENT".  A string is not a
// sequence, so toCells rejected it and the range splice — the one code path
// in the engine that consumes the value's CELLS rather than storing the
// value whole — was structurally unreachable from this target.  It could not
// generate a replacement longer than the range it replaced, which is exactly
// the input that corrupts, and the defect described in the target's doc
// comment survived every campaign.
//
// The string stays in the pool (it is the value shape a caller most often
// passes, and the one every existing corpus entry decodes to), joined by
// vectors and lists of VARYING length so the splice sees replacements
// shorter than, equal to and longer than the range.
const (
	valueString = iota
	valueScalar
	valueVector
	valueList
	valueNested
	numValueShapes
)

// pathValueMaxLen bounds a generated sequence value.  One longer than
// pathGenMaxLen, so a replacement can exceed the longest sequence the
// document generator builds — that is the corner where the splice has to
// grow past the source's own capacity instead of writing inside it.
const pathValueMaxLen = 6

// value builds the trailing new-value argument.
func (g *pathGen) value() *lisp.LVal {
	switch g.intn(numValueShapes) {
	case valueScalar:
		return lisp.Int(pathIndices[g.intn(len(pathIndices))])
	case valueVector:
		return lisp.Vector(g.markerCells())
	case valueList:
		return lisp.QExpr(g.markerCells())
	case valueNested:
		// A container value, so a replacement can itself hold structure
		// the copy helpers have to walk.
		return g.doc(pathGenMaxDepth - 1)
	default:
		return lisp.String("REPLACEMENT")
	}
}

// markerCells builds a replacement body of 0..pathValueMaxLen cells.
//
// The markers are "R0", "R1", ... — disjoint from pathKeys, pathIndices and
// every other value the document generator produces.  That disjointness is
// what makes invariant 6's report unambiguous: a marker sitting where a
// source element belongs can only have come from the replacement, so the
// failure names the defect instead of describing a coincidence.
func (g *pathGen) markerCells() []*lisp.LVal {
	n := g.intn(pathValueMaxLen + 1)
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.String(fmt.Sprintf("R%d", i))
	}
	return cells
}

func (g *pathGen) cells(depth int) []*lisp.LVal {
	n := g.intn(pathGenMaxLen + 1)
	cells := make([]*lisp.LVal, 0, n)
	for range n {
		if g.budget <= 0 {
			break
		}
		cells = append(cells, g.doc(depth+1))
	}
	return cells
}

// steps builds the step sequence.  Keys and indices are drawn
// PREFERENTIALLY from what doc actually contains, so a step lands inside the
// structure often; the remaining draws come from the fixed pools and are the
// misses and the out-of-range boundaries.
func (g *pathGen) steps(doc *lisp.LVal) []*lisp.LVal {
	liveKeys, maxLen := surveyDoc(doc)
	n := g.intn(pathGenMaxSteps + 1)
	steps := make([]*lisp.LVal, 0, n)
	for range n {
		switch g.intn(4) {
		case 0: // map key
			if len(liveKeys) > 0 && g.next()%4 != 0 {
				steps = append(steps, lisp.String(liveKeys[g.intn(len(liveKeys))]))
			} else {
				steps = append(steps, lisp.String(pathKeys[g.intn(len(pathKeys))]))
			}
		case 1: // array index
			if maxLen > 0 && g.next()%4 != 0 {
				steps = append(steps, lisp.Int(g.intn(maxLen)))
			} else {
				steps = append(steps, lisp.Int(pathIndices[g.intn(len(pathIndices))]))
			}
		case 2: // iterator
			steps = append(steps, lisp.Symbol("*"))
		default: // range
			from := pathIndices[g.intn(len(pathIndices))]
			to := pathIndices[g.intn(len(pathIndices))]
			if maxLen > 0 && g.next()%2 == 0 {
				from, to = g.intn(maxLen+1), g.intn(maxLen+1)
			}
			if g.next()%4 == 0 {
				// The open-ended form (issue #563).  It resolves its
				// end against the document at evaluation time rather
				// than carrying one, which is a different arm of
				// validateRange and so a different set of bounds for
				// the splice to get wrong.  Kept a minority draw so
				// the two-argument form stays the common case.
				steps = append(steps, lisp.QExpr([]*lisp.LVal{
					lisp.Symbol("range"), lisp.Int(from),
				}))
				break
			}
			steps = append(steps, lisp.QExpr([]*lisp.LVal{
				lisp.Symbol("range"), lisp.Int(from), lisp.Int(to),
			}))
		}
	}
	return steps
}

// surveyDoc collects the map keys present anywhere in doc and the longest
// sequence it contains — the two facts that make a generated step land
// inside the structure rather than beside it.
func surveyDoc(doc *lisp.LVal) (keys []string, maxLen int) {
	seen := map[string]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > pathGenMaxDepth+2 {
			return
		}
		switch v.Type { //nolint:exhaustive // only the three container shapes carry keys or a length; scalars contribute nothing to the survey
		case lisp.LSortMap:
			m := v.Map()
			if m == nil {
				return
			}
			entries := make([]*lisp.LVal, m.Len())
			if lerr := m.Entries(entries); lerr.Type == lisp.LError {
				return
			}
			maxLen = max(maxLen, len(entries))
			for _, e := range entries {
				if len(e.Cells) == 2 {
					if k := e.Cells[0]; k.Type == lisp.LString && !seen[k.Str] {
						seen[k.Str] = true
						keys = append(keys, k.Str)
					}
					walk(e.Cells[1], depth+1)
				}
			}
		case lisp.LArray:
			if len(v.Cells) == 2 {
				maxLen = max(maxLen, len(v.Cells[1].Cells))
				for _, c := range v.Cells[1].Cells {
					walk(c, depth+1)
				}
			}
		case lisp.LSExpr:
			maxLen = max(maxLen, len(v.Cells))
			for _, c := range v.Cells {
				walk(c, depth+1)
			}
		}
	}
	walk(doc, 0)
	return keys, maxLen
}

// TestPathGenCoverage is the harness's own gate: a fuzz target whose
// generator silently produces one shape forever cannot fail, and nothing
// about that is visible from a green fuzz run.  It asserts the generator
// reaches every structure shape and every step form from short inputs.
func TestPathGenCoverage(t *testing.T) {
	shapes := map[lisp.LType]int{}
	stepForms := map[string]int{}
	for i := range 4096 {
		g := &pathGen{b: []byte{byte(i), byte(i >> 8), byte(i >> 4), byte(i * 7), byte(i * 13)}, budget: pathGenBudget}
		doc := g.doc(0)
		shapes[doc.Type]++
		for _, s := range g.steps(doc) {
			switch s.Type {
			case lisp.LString:
				stepForms["key"]++
			case lisp.LInt:
				stepForms["index"]++
			case lisp.LSymbol:
				stepForms["iter"]++
			default:
				stepForms["range"]++
			}
		}
	}
	for _, want := range []lisp.LType{lisp.LSortMap, lisp.LArray, lisp.LSExpr, lisp.LString, lisp.LInt} {
		if shapes[want] == 0 {
			t.Errorf("generator never produced a %v document: %v", want, shapes)
		}
	}
	for _, form := range []string{"key", "index", "iter", "range"} {
		if stepForms[form] == 0 {
			t.Errorf("generator never produced a %s step: %v", form, stepForms)
		}
	}
}

// viewWitnessSeeds are the inputs that reach invariant 7's shape: a vector
// document, a single step, and a step that names some but not all of the
// vector's positions -- the configuration in which a compaction through the
// source's array is observable.  One per operation per step form.
//
// SEEDS DRIFT WHEN THE GENERATOR CHANGES, and the comments below say what
// each one decodes to TODAY, not what it was found for.  Adding the
// open-ended range draw to pathGen.steps shifted byte consumption, so four
// of the original fourteen silently moved from the explicit range arm to the
// open one -- taking the explicit-range witness for ?set!, ?del! and ?nil!
// with them, which is exactly where issue #471's in-place splice bug lived.
// Nothing caught it, because the coverage test classified both arms as
// "range".  It now distinguishes them, and the seven seeds at the end of the
// list restore what the drift removed.
//
// The same shift means testdata/fuzz/FuzzPathEngine/ae76e6821af9dfd8, a saved
// crasher, now decodes to (range 0) where it was minimised as (range 0 0).
// It is still a valid regression input and is kept; the explicit arm it used
// to cover is covered by the added seeds rather than by it.
//
// They are byte strings written in this file, found by searching short random
// inputs for the shape.  No seed is derived from any downstream corpus.
var viewWitnessSeeds = [][]byte{
	{0x62, 0xce, 0xe1, 0x20, 0xb4, 0xdb, 0xe0, 0x47, 0x15},                                     // ? index
	{0xbd, 0x46, 0x4b, 0x50, 0x58, 0x2b, 0x10, 0xb7, 0x1b, 0x94, 0x4a, 0xbb, 0x16},             // ? range-explicit
	{0xd3, 0xa6, 0xe1, 0x10, 0x94, 0x27, 0x20, 0x78, 0x11},                                     // ?set! index
	{0x08, 0xbe, 0x6f, 0x83, 0x1a, 0xd0, 0x54, 0x5e, 0xf6, 0xcb, 0x07, 0xc4, 0x84, 0x99, 0xee}, // ?set! range-open
	{0x3a, 0xa6, 0x4b, 0x40, 0x9a, 0x13, 0x90, 0xe1, 0xdd},                                     // ?set index
	{0x10, 0xee, 0x15, 0x76, 0x60, 0xca, 0x38, 0x88, 0x5c, 0x9b, 0xdc, 0xae, 0xa3, 0x6b, 0x7a}, // ?set range-explicit
	{0x0a, 0x3e, 0x81, 0x7c, 0xfb, 0x50, 0xb0, 0x5c, 0xe5, 0x81},                               // ?del! index
	{0x9d, 0x8e, 0xab, 0x2c, 0xba, 0xe8, 0x20, 0xc5, 0xb7, 0xbd, 0x88, 0x6d, 0xf8, 0xed, 0xb4}, // ?del! range-open
	{0x2e, 0x9e, 0x15, 0xd8, 0xc8, 0x1a, 0x46, 0xa9, 0x99},                                     // ?del index
	{0xd6, 0xde, 0xc3, 0x8a, 0xf9, 0x13, 0x02, 0x20, 0x0f, 0x7b, 0xeb, 0xad, 0x43, 0x47},       // ?del range-explicit
	{0x67, 0x36, 0x39, 0x25, 0x54, 0x09, 0x77, 0xe8, 0x16, 0xcd},                               // ?nil! index
	{0x67, 0xce, 0x99, 0x92, 0xd6, 0xd0, 0xdb, 0xc4, 0xb7, 0xbb, 0x51, 0x93, 0x65},             // ?nil! range-open
	{0x61, 0xb6, 0x75, 0xa0, 0xb8, 0xf0, 0xb0, 0x99, 0x1f},                                     // ?nil index
	{0xfb, 0x66, 0xff, 0xa0, 0x94, 0x5c, 0x52, 0x28, 0x01, 0x2b, 0x0d, 0x6e, 0x4f},             // ?nil range-open

	// Restoring what the open-range generator draw shifted away (see above).
	{0x5b, 0x2e, 0x51, 0x30, 0x6b, 0x5a, 0xcb, 0x61, 0x01, 0x2b, 0x4c},                               // ? range-open
	{0xb7, 0x56, 0x3d, 0xd1, 0xbe, 0x1d, 0x57, 0xda, 0xee, 0xd5, 0x87, 0xd0, 0xcb},                   // ?set! range-explicit
	{0xc6, 0xf6, 0xf9, 0x81, 0xa6, 0x78, 0x00, 0x1d, 0x07, 0x3b, 0x76, 0xd5},                         // ?set range-open
	{0xdc, 0x86, 0x85, 0x82, 0x49, 0xfd, 0x6f, 0xb1, 0xdb, 0xab, 0x12, 0x52, 0x13, 0xbb},             // ?del! range-explicit
	{0xf2, 0xde, 0x2c, 0x5b, 0xff, 0x17, 0xc6, 0x47, 0x13, 0x30, 0xc3, 0x12, 0xa5, 0x32, 0xbc, 0x32}, // ?del range-open
	{0xde, 0x5e, 0x67, 0xb3, 0xc6, 0x32, 0xb7, 0x67, 0x49, 0x59, 0x1a},                               // ?nil! range-explicit
	{0xa7, 0x76, 0x3d, 0x51, 0x06, 0xef, 0x87, 0x37, 0xf0, 0x21, 0x3e, 0xf4, 0x43},                   // ?nil range-explicit
}

// TestViewWitnessSeedsAreReached is invariant 7's own gate, in the spirit of
// TestPathGenCoverage and TestEnumeratePathsCoversEveryStepForm.
//
// A fuzz invariant that no input reaches is not an invariant, it is a comment
// that compiles -- and nothing about that is visible from a green fuzz run.
// This asserts two separate things, because they fail separately:
//
//  1. The general drivers reach the shape ZERO times.  That is not a defect
//     to fix, it is the measurement that justifies viewWitnessSeeds existing;
//     if it ever stops being zero this test says so rather than leaving the
//     seeds looking like superstition.
//  2. Every viewWitnessSeed does reach it, and between them they cover all
//     seven operations and both sequence step forms.
func TestViewWitnessSeedsAreReached(t *testing.T) {
	t.Parallel()

	reach := func(data []byte) (int, string, bool) {
		g := &pathGen{b: data, budget: pathGenBudget}
		opIdx := int(g.next()) % numPathOps
		doc := g.doc(0)
		steps := g.steps(doc)
		if _, ok := newViewWitness(doc, steps); !ok {
			return 0, "", false
		}
		form := "other"
		if len(steps) == 1 {
			switch steps[0].Type { //nolint:exhaustive // only the two sequence step forms reach the witness
			case lisp.LInt:
				form = "index"
			case lisp.LSExpr:
				// The two range spellings are separate forms, not one.
				// Classifying both as "range" is what let the generator
				// change silently move four seeds from the explicit arm
				// to the open one while this test stayed green -- and
				// with them went the explicit-range witness for every
				// mutating operation, which is where issue #471 lived.
				if len(steps[0].Cells) == 2 {
					form = "range-open"
				} else {
					form = "range-explicit"
				}
			}
		}
		return opIdx, form, true
	}

	drivers := [][]byte{
		{}, {0x00}, {0x01, 0x02, 0x03},
		{0x10, 0x21, 0x32, 0x43, 0x54},
		{0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa},
		{0x05, 0x00, 0x07, 0x01, 0x02, 0x00, 0x03, 0x09},
		{0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99},
		{0x07, 0x03, 0x01, 0x04, 0x01, 0x05, 0x09, 0x02, 0x06},
	}
	generic := 0
	for op := range numPathOps {
		for _, d := range drivers {
			if _, _, ok := reach(append([]byte{byte(op)}, d...)); ok { //nolint:gocritic // deliberate new slice per seed
				generic++
			}
		}
	}
	if generic != 0 {
		t.Logf("NOTE: %d of the general drivers now reach invariant 7; the dedicated"+
			" seeds are no longer the only route (not a failure)", generic)
	}

	ops := map[string]bool{}
	forms := map[string]bool{}
	// Keyed by operation AND form. Per-op and per-form coverage checked
	// separately would both stay green while a whole cell emptied: that is
	// how ?set!, ?del! and ?nil! lost their explicit-range witness without
	// this test noticing.
	cells := map[string]bool{}
	for i, seed := range viewWitnessSeeds {
		opIdx, form, ok := reach(seed)
		if !ok {
			t.Errorf("viewWitnessSeeds[%d] no longer reaches invariant 7: the property"+
				" it guards is not being exercised", i)
			continue
		}
		ops[pathOps[opIdx].name] = true
		forms[form] = true
		cells[pathOps[opIdx].name+"/"+form] = true
	}
	for _, op := range pathOps {
		for _, form := range []string{"index", "range-explicit", "range-open"} {
			if !cells[op.name+"/"+form] {
				t.Errorf("no seed reaches invariant 7 for %s with a %s step -- if a"+
					" generator change moved a seed, add one back rather than"+
					" letting the cell empty", op.name, form)
			}
		}
	}
	for _, op := range pathOps {
		if !ops[op.name] {
			t.Errorf("no seed reaches invariant 7 for %s", op.name)
		}
	}
	for _, form := range []string{"index", "range-explicit", "range-open"} {
		if !forms[form] {
			t.Errorf("no seed reaches invariant 7 with a %s step", form)
		}
	}
	t.Logf("invariant 7 reached by %d/%d dedicated seeds across %d operations and %d step forms;"+
		" general drivers reach it %d times",
		len(viewWitnessSeeds), len(viewWitnessSeeds), len(ops), len(forms), generic)
}
