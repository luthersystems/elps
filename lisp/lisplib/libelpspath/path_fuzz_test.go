// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
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
//     values that break index arithmetic), the '* iterator and (range a b)
//     slices.  Steps are drawn PREFERENTIALLY from the keys and indices the
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

		args := make([]*lisp.LVal, 0, len(steps)+2)
		args = append(args, doc)
		args = append(args, steps...)
		if op.needsValue {
			args = append(args, g.value())
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
	})
}

// spliceOracle holds the two ends of a sequence document that a single
// top-level range step is not allowed to disturb: everything before `from`
// and everything from `to` on, rendered by value before the operation runs.
type spliceOracle struct {
	prefix []string
	suffix []string
}

// newSpliceOracle recognises the isolated-splice shape — the whole path is
// one (range a b) and the document is a sequence — and snapshots its ends.
// Anything else returns false and the invariant simply does not apply.
func newSpliceOracle(doc *lisp.LVal, steps []*lisp.LVal) (spliceOracle, bool) {
	if len(steps) != 1 {
		return spliceOracle{}, false
	}
	step := steps[0]
	if step.Type != lisp.LSExpr || len(step.Cells) != 3 {
		return spliceOracle{}, false
	}
	if head := step.Cells[0]; head.Type != lisp.LSymbol || head.Str != "range" {
		return spliceOracle{}, false
	}
	lo, hi := step.Cells[1], step.Cells[2]
	if lo.Type != lisp.LInt || hi.Type != lisp.LInt {
		return spliceOracle{}, false
	}
	cells, err := toCells(doc)
	if err != nil {
		return spliceOracle{}, false
	}
	from, to, err := validateRange(len(cells), lo.Int, hi.Int, false)
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
		fmt.Fprintf(&sb, "%p q=%v n=%d[", l, l.Quoted, len(l.Cells))
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
