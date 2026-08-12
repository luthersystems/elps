// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// Shared machinery for the STRUCTURE-AWARE container-family fuzz targets
// (FuzzSortedMapOps, FuzzSequenceOps, FuzzHigherOrderOps).
//
// # Why these targets exist alongside FuzzApplyStdlib
//
// FuzzApplyStdlib already applies every stdlib callable — the container
// builtins included — to generated arguments.  It reaches them with GENERIC
// values, though: one argument generator serving 240 unrelated callables.
// The defects in the mutable-container families do not live in the argument
// TYPES.  They live in the relationship between two values:
//
//	(set 'lit '(1 2 3))
//	(append 'vector (slice 'list lit 0 1) 99)
//	lit  ; => (1 99 3)     -- elps#369 mechanism 2
//
// Nothing is wrong with either call in isolation.  The corruption is that
// `slice` handed out a window onto `lit`'s backing array INCLUDING its
// retained capacity, and `append` then wrote through it.  A generator that
// produces one independent argument list per call cannot construct that
// relationship except by accident, which is why the structure-aware
// elpspath target found in six seconds what the generic sweep had not found
// at all.
//
// So these targets fuzz an OPERATION SEQUENCE over a value POOL.  A
// generated value goes into the pool; each step picks operands from the
// pool, applies a builtin, and pushes the result back.  Aliasing between
// step i's output and step j's input is then the normal case rather than a
// coincidence, and the #369/#373 shape — build a slice, append to the slice,
// append to the source — is a three-step path the mutator finds readily.
//
// # The two oracles, and why both
//
//   - SEALED stability, via lisp.SealedASTFingerprint over every sealed root
//     in the pool, re-checked after EVERY step.  Values are sealed exactly
//     as a quoted program literal arrives sealed: SealAST marks lists,
//     symbols, strings and numbers and leaves vectors, sorted-maps and bytes
//     mutable.  Drift means shared program storage was written, which is the
//     substrate#378 / elps#369 class and a defect for EVERY op, mutating
//     ones included.
//   - VALUE stability, via valueFingerprint (lisp/valuefp_test.go), over the
//     DIRECT ARGUMENTS of any op the language documents as non-mutating.
//     The sealed oracle is blind here by design — a sorted-map is never
//     sealed, so it fingerprints as one opaque hole byte, and every
//     map-mutating defect would look identical before and after.
//
// # What is deliberately NOT asserted, and why that is not a gap
//
// Whole-pool value stability is NOT asserted.  Appending to a slice result
// and writing into the source's retained capacity is DOCUMENTED behaviour
// (issue #373, maintainer decision): the language mirrors Go slices
// deliberately, `concat` is the documented copy idiom, and copy-on-append
// would turn amortized O(1) vector building into O(n^2).  Asserting
// pool-wide stability would report that decision as a bug on every other
// input and drown the real signal.
//
// The line the assertions draw is exactly the line #373 draws: within-
// environment value aliasing is language semantics, and writing SEALED
// storage is the corruption.  A sealed source therefore gets the full
// guarantee — that is what makes the shape above safe for a phylum — while
// an unsealed one gets the documented Go-slice contract, and each op is
// still held to its own docstring for its own arguments.

const (
	// containerBudget caps how many LVals one input may construct.  The
	// generator is recursive, so without a global cap a few bytes can ask
	// for an exponentially large structure and the target spends its time in
	// the allocator rather than in the builtins.
	containerBudget = 96

	// containerMaxDepth bounds nesting independently of the budget: a deep
	// spine would test LVal.String()'s recursion rather than the containers.
	containerMaxDepth = 4

	// containerMaxLen bounds any one container.  Small on purpose — index,
	// range and capacity arithmetic breaks at the ENDS of a sequence, and a
	// short sequence puts the mutator's byte flips near both ends.
	containerMaxLen = 5

	// containerMaxSteps bounds an operation sequence.  Three steps is the
	// minimum that reaches the #369 shape (slice, append, read); six leaves
	// room for the chains that build up retained capacity.
	containerMaxSteps = 6

	// containerMaxAlloc is the per-operation allocation cap, deliberately
	// tiny.  At the default (10M elements) a single oversized append costs
	// hundreds of milliseconds and the fuzzer spends its budget in the
	// allocator; at 4096 the guard itself is the boundary being explored.
	containerMaxAlloc = 4096

	// containerMaxSteps' evaluation counterpart: the callbacks in the
	// higher-order family run arbitrary lisp, so the step budget bounds it.
	containerMaxEvalSteps = 20000

	// containerDeadline bounds one whole operation sequence.
	containerDeadline = 5 * time.Second

	// containerWatchdogGrace is the SCHEDULED time past the deadline after
	// which a call is declared unbounded rather than slow.  See
	// internal/fuzzwatch: wall clock this process was not given the CPU for
	// is not charged to the builtin.
	containerWatchdogGrace = 10 * time.Second
)

// containerKeys is the key/string pool.  A small closed pool is what makes
// hits and misses BOTH common: keys are drawn from it when building maps and
// again when building lookups, so a lookup lands on a real key often enough
// to reach the code past it, and misses often enough to keep the absent
// branch alive.
var containerKeys = []string{"a", "b", "c", "k", "", "x y", "0"}

// containerInts are the integer operands: the small values around the ends
// of a sequence, plus the magnitudes that break index and range arithmetic.
var containerInts = []int{0, 1, 2, 3, -1, -2, 5, -5, 1 << 31, -(1 << 31)}

// containerGen is a deterministic structure generator driven by a byte
// string.  Determinism is the load-bearing property: a saved crasher is a
// byte string, and it only reproduces if those bytes always build the same
// structure.  Nothing here reads a clock, a map iteration order or a random
// source.
type containerGen struct {
	b      []byte
	i      int
	budget int
}

func newContainerGen(data []byte) *containerGen {
	return &containerGen{b: data, budget: containerBudget}
}

// next consumes one byte, or 0 once the input is exhausted.  Reads past the
// end never fail, so a one-byte seed produces a small, boring structure
// rather than an error — which is what lets the fuzzer grow it.
func (g *containerGen) next() byte {
	if g.i >= len(g.b) {
		return 0
	}
	c := g.b[g.i]
	g.i++
	return c
}

func (g *containerGen) intn(n int) int {
	if n <= 0 {
		return 0
	}
	return int(g.next()) % n
}

func (g *containerGen) key() string  { return containerKeys[g.intn(len(containerKeys))] }
func (g *containerGen) anInt() int   { return containerInts[g.intn(len(containerInts))] }
func (g *containerGen) coin() bool   { return g.next()%2 == 0 }
func (g *containerGen) spent() bool  { return g.budget <= 0 }
func (g *containerGen) charge() bool { g.budget--; return g.budget > 0 }

// Value shapes, ordered so the low tags are the cheap scalars: a mutator
// flipping a byte downward shrinks the structure, which keeps crashers small.
const (
	shapeNil = iota
	shapeBool
	shapeInt
	shapeFloat
	shapeString
	shapeSymbol
	shapeList
	shapeVector
	shapeBytes
	shapeMap
	numContainerShapes
)

// value builds an arbitrary value of any shape.
func (g *containerGen) value(depth int) *lisp.LVal {
	if g.spent() {
		return lisp.Nil()
	}
	g.charge()

	shape := g.intn(numContainerShapes)
	if depth >= containerMaxDepth && shape >= shapeList {
		// Collapse a compound shape onto a scalar at the depth limit rather
		// than truncating mid-structure: a half-built container is a shape
		// no caller can construct, so a crasher on one would not reproduce
		// from lisp.
		shape = g.intn(shapeList)
	}
	return g.shape(shape, depth)
}

// seq builds a SEQUENCE — the shapes the sequence family operates on.
func (g *containerGen) seq(depth int) *lisp.LVal {
	if g.spent() {
		return lisp.QExpr(nil)
	}
	g.charge()
	switch g.intn(3) {
	case 0:
		return g.shape(shapeList, depth)
	case 1:
		return g.shape(shapeVector, depth)
	default:
		return g.shape(shapeBytes, depth)
	}
}

func (g *containerGen) shape(shape, depth int) *lisp.LVal {
	switch shape {
	case shapeBool:
		return lisp.Bool(g.coin())
	case shapeInt:
		return lisp.Int(g.anInt())
	case shapeFloat:
		return lisp.Float(float64(g.next()) / 4)
	case shapeString:
		return lisp.String(g.key())
	case shapeSymbol:
		return lisp.Symbol(g.key())
	case shapeList:
		return lisp.QExpr(g.cells(depth))
	case shapeVector:
		return lisp.Array(nil, g.cells(depth))
	case shapeBytes:
		n := g.intn(containerMaxLen + 1)
		b := make([]byte, n)
		for i := range b {
			b[i] = g.next()
		}
		return lisp.Bytes(b)
	case shapeMap:
		return g.sortedMap(depth)
	default:
		return lisp.Nil()
	}
}

func (g *containerGen) cells(depth int) []*lisp.LVal {
	n := g.intn(containerMaxLen + 1)
	cells := make([]*lisp.LVal, 0, n)
	for range n {
		if g.spent() {
			break
		}
		cells = append(cells, g.value(depth+1))
	}
	return cells
}

// sortedMap builds a nested sorted-map.  Keys come from the shared pool so
// that a later lookup hits; values are arbitrary, which is what makes the
// map NESTED (a value may itself be a map).
func (g *containerGen) sortedMap(depth int) *lisp.LVal {
	sm := lisp.SortedMap()
	m := sm.Map()
	n := g.intn(containerMaxLen + 1)
	for range n {
		if g.spent() {
			break
		}
		var key *lisp.LVal
		if g.coin() {
			key = lisp.String(g.key())
		} else {
			key = lisp.Symbol(g.key())
		}
		if lerr := m.Set(key, g.value(depth+1)); lerr.Type == lisp.LError {
			break
		}
	}
	return sm
}

// mapKey builds a lookup key, drawn PREFERENTIALLY from the keys the given
// value actually contains so lookups land inside the structure.  The
// remaining draws are misses and wrong-typed keys, which are the other half
// of the boundary (sorted-maps accept only string and symbol keys).
func (g *containerGen) mapKey(v *lisp.LVal) *lisp.LVal {
	live := surveyMapKeys(v)
	switch {
	case len(live) > 0 && g.next()%4 != 0:
		return live[g.intn(len(live))]
	case g.coin():
		return lisp.String(g.key())
	case g.coin():
		return lisp.Symbol(g.key())
	default:
		// A key type sorted-maps reject.  The error path is as much of the
		// contract as the success path.
		return lisp.Int(g.anInt())
	}
}

// index builds a sequence index, drawn PREFERENTIALLY from the valid range
// of the given sequence and otherwise from the pool of arithmetic-breaking
// magnitudes.
func (g *containerGen) index(v *lisp.LVal) int {
	n := seqLen(v)
	if n > 0 && g.next()%4 != 0 {
		return g.intn(n + 1)
	}
	return g.anInt()
}

// surveyMapKeys collects the keys present anywhere in v, so a generated
// lookup lands inside the structure rather than beside it.  Keys are
// returned as fresh LVals: handing back the map's own key nodes would let a
// builtin that writes through a key argument corrupt the map behind the
// oracle's back and have it look like the map was never touched.
func surveyMapKeys(v *lisp.LVal) []*lisp.LVal {
	var keys []*lisp.LVal
	seen := map[string]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > containerMaxDepth+2 {
			return
		}
		switch v.Type { //nolint:exhaustive // only maps carry keys; other containers are descended for the maps inside them
		case lisp.LSortMap:
			m := v.Map()
			if m == nil {
				return
			}
			ks := m.Keys()
			if ks == nil || ks.Type == lisp.LError {
				return
			}
			for _, k := range ks.Cells {
				tag := fmt.Sprintf("%d:%s", k.Type, k.Str)
				if !seen[tag] {
					seen[tag] = true
					if k.Type == lisp.LSymbol {
						keys = append(keys, lisp.Symbol(k.Str))
					} else {
						keys = append(keys, lisp.String(k.Str))
					}
				}
				if val, ok := m.Get(k); ok {
					walk(val, depth+1)
				}
			}
		case lisp.LSExpr, lisp.LArray:
			for _, c := range v.Cells {
				walk(c, depth+1)
			}
		}
	}
	walk(v, 0)
	return keys
}

// seqLen reports the element count of a sequence-shaped value, or 0.
func seqLen(v *lisp.LVal) int {
	if v == nil {
		return 0
	}
	switch v.Type { //nolint:exhaustive // only these shapes have a sequence length; everything else contributes 0
	case lisp.LSExpr, lisp.LArray, lisp.LBytes, lisp.LString:
		return v.Len()
	default:
		return 0
	}
}

// ---------------------------------------------------------------------------
// the shared assertion harness
// ---------------------------------------------------------------------------

// containerOp is one builtin under test plus the two facts the harness needs
// about it: its qualified name, and which argument (if any) its docstring
// licenses it to modify in place.
type containerOp struct {
	name string
	// mutatesArg is the index of the argument this op is DOCUMENTED to
	// modify in place, or -1 when the op promises to modify nothing.  It is
	// an index rather than a bool because the mutated operand is not always
	// the first: `stable-sort` takes the predicate first and sorts its
	// SECOND argument.  Every other argument is held to the no-modification
	// contract even for a mutating op — `(assoc! m k v)` may rewrite m and
	// must not touch k or v.
	mutatesArg int
}

// containerCall applies one op and makes every assertion the family shares.
// It returns the result so the caller can push it back into the pool.
//
// The op is applied through env.FunCall, NOT env.Eval: eval installs a
// recover() that converts any Go panic into an ordinary-looking
// `internal-panic` LVal, so a target that went through Eval would report a
// pass on a crash.  FunCall has no recover, and a panicking input therefore
// surfaces with the Go stack of the offending frame.
func containerCall(t *testing.T, env *lisp.LEnv, op containerOp, args []*lisp.LVal, sealedRoots []*lisp.LVal, fpSealed uint64, step int) *lisp.LVal {
	t.Helper()

	// Every failure message below renders the arguments, so the arguments
	// must be renderable before the call is made.  The generator builds
	// trees, but it draws operands from a pool that earlier steps have
	// aliased, so a doubling chain can make the rendered form exponential
	// even with no cycle in sight.
	if !containerRenderable(args) {
		return nil
	}

	singletons := lisp.TakeSingletonSnapshot()

	// The value oracle covers every argument the op is not licensed to
	// modify.  Fingerprinted individually so the failure message can name
	// WHICH argument moved.
	//
	// An argument that SHARES STORAGE with the mutated one is exempt, and
	// has to be: the generator draws operands from a pool, so `(append-bytes!
	// b b)` — the same value in both positions — is an ordinary draw, and
	// asserting that argument 1 is unchanged there is asserting that a
	// documented in-place mutation did not happen.  The exemption is
	// computed from the actual object graph rather than from pointer
	// equality alone, because containment is the same problem one level
	// down: if argument 1 CONTAINS the vector argument 0 rewrites, its
	// digest legitimately moves too.
	//
	// Nothing is lost for the copying operations, which are the ones the
	// contract is really about: mutatesArg is -1 for them, so no argument is
	// ever exempt and the assertion runs at full strength.
	fpArgs := make([]string, len(args))
	skip := make([]bool, len(args))
	for i, a := range args {
		if i == op.mutatesArg {
			skip[i] = true
			continue
		}
		if op.mutatesArg >= 0 && op.mutatesArg < len(args) && sharesStorage(a, args[op.mutatesArg]) {
			skip[i] = true
			continue
		}
		fpArgs[i] = valueFingerprint([]*lisp.LVal{a})
	}

	fun := env.GetGlobal(lisp.Symbol(op.name))
	if fun.Type != lisp.LFun {
		t.Fatalf("%s is not bound to a function in the fuzz environment (got %v)", op.name, fun.Type)
	}

	result := containerFunCall(t, env, op.name, fun, lisp.SExpr(args))
	if result == nil {
		// Either the watchdog skipped the input, or the builtin returned a
		// nil *LVal, which containerFunCall has already reported.
		return nil
	}

	// STOP HERE if the step just built a value the interpreter cannot
	// render.  See containerRenderable: this is a KNOWN, separately-reported
	// defect (LVal.String() has no cycle or depth bound, so a
	// self-referential runtime container kills the process with a stack
	// overflow recover() cannot intercept).  Every line below either renders
	// the result or renders the arguments in a failure message, so
	// continuing would mean this target reports that one defect over and
	// over instead of finding the ones it was built for.
	if !containerRenderable(append([]*lisp.LVal{result}, args...)) {
		return nil
	}

	// Rendering runs the same walk every error path in the interpreter runs
	// when it formats an operand; an unprintable result is a latent crash in
	// the error path.
	_ = result.String()

	if drift := singletons.Verify(); drift != "" {
		t.Fatalf("step %d: %s mutated the shared singleton %s\n%s",
			step, op.name, drift, renderContainerArgs(args))
	}

	if fpAfter := lisp.SealedASTFingerprint(sealedRoots); fpAfter != fpSealed {
		t.Fatalf("step %d: %s mutated SEALED storage in place (fingerprint %016x -> %016x):"+
			" a program literal reaching this operation would be corrupted for every"+
			" environment sharing the parse (the substrate#378 / elps#369 class)\n%s",
			step, op.name, fpSealed, fpAfter, renderContainerArgs(args))
	}

	for i, a := range args {
		if skip[i] {
			continue
		}
		if after := valueFingerprint([]*lisp.LVal{a}); after != fpArgs[i] {
			t.Fatalf("step %d: %s modified argument %d, which its docstring says it does not modify"+
				" (value fingerprint %s -> %s)\n%s",
				step, op.name, i, fpArgs[i], after, renderContainerArgs(args))
		}
	}
	return result
}

// containerFunCall applies fun on a separate goroutine and fails the test if
// the call outlives the deadline by containerWatchdogGrace of SCHEDULED
// time.
//
// A watchdog is necessary because neither of the interpreter's own bounds
// sees every loop: checkLimits is consulted per EVALUATION, and a builtin
// that loops inside Go without evaluating anything consults nothing.  `go
// test -fuzz` has no per-input timeout, so without this the failure mode is
// the whole test binary being killed by -timeout with no crasher written.
//
// The goroutine is deliberately leaked on timeout: it cannot be interrupted
// — that is the defect being reported — and a fuzz worker that reports a
// crasher exits immediately afterwards.
func containerFunCall(t *testing.T, env *lisp.LEnv, name string, fun, args *lisp.LVal) *lisp.LVal {
	t.Helper()
	done := make(chan *lisp.LVal, 1)
	panicked := make(chan any, 1)
	go func() {
		defer func() {
			if r := recover(); r != nil {
				panicked <- r
			}
		}()
		done <- env.FunCall(fun, args)
	}()

	budget := fuzzwatch.New(containerDeadline + containerWatchdogGrace)
	wait := budget.Total()
	for {
		select {
		case v := <-done:
			if v == nil {
				t.Fatalf("%s returned a nil *LVal\n%s", name, renderContainerArgs(args.Cells))
				return nil
			}
			return v
		case r := <-panicked:
			// Re-panic on the test goroutine so the fuzzing engine records
			// the crasher and prints the stack.
			panic(r)
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				// The machine never gave us the CPU.  Whether this call
				// terminates is unknown, and guessing in either direction is
				// worse than declining to answer for one input.
				t.Skipf("%s: no verdict, the process was starved throughout (%s)", name, report)
				return nil
			default:
				t.Fatalf("%s did not terminate within %v of SCHEDULED time despite a %v deadline"+
					" and a %d-step limit (%s)\n%s",
					name, budget.Total(), containerDeadline, containerMaxEvalSteps, report,
					renderContainerArgs(args.Cells))
				return nil
			}
		}
	}
}

// collectContainerSealed returns the maximal sealed subtrees reachable from
// vs.  A sealed node is collected whole (the fingerprint walk covers its
// descendants); an unsealed container is descended, because sealed values
// legitimately sit inside runtime storage — a vector holding a quoted
// literal, say.  Roots are held by pointer so the oracle compares the same
// storage before and after: an op legitimately DROPPING a sealed value from
// an unsealed container must not (and does not) affect the comparison.
func collectContainerSealed(vs []*lisp.LVal) []*lisp.LVal {
	var roots []*lisp.LVal
	seen := map[*lisp.LVal]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > 64 || seen[v] {
			return
		}
		seen[v] = true
		if v.IsSealed() {
			roots = append(roots, v)
			return
		}
		if v.Type == lisp.LSortMap {
			m := v.Map()
			if m == nil {
				return
			}
			ks := m.Keys()
			if ks == nil || ks.Type == lisp.LError {
				return
			}
			for _, k := range ks.Cells {
				if val, ok := m.Get(k); ok {
					walk(val, depth+1)
				}
			}
			return
		}
		for _, c := range v.Cells {
			walk(c, depth+1)
		}
	}
	for _, v := range vs {
		walk(v, 0)
	}
	return roots
}

// sharesStorage reports whether a and b reach any LVal in common — the same
// node, or one contained in the other, or a third node both hold.  It is the
// exemption test for the value oracle: an argument that shares storage with
// the one an operation is licensed to rewrite cannot be held to the
// no-modification contract, because a write the docstring permits can show
// up in either of them.
//
// Reachability is followed through Cells and through a sorted-map's Native
// entries, and is bounded by a visited set (so aliasing and cycles
// terminate) and a depth cap.
func sharesStorage(a, b *lisp.LVal) bool {
	if a == nil || b == nil {
		return false
	}
	nodes := map[*lisp.LVal]bool{}
	collectNodes(a, nodes, 0)
	found := false
	var probe func(v *lisp.LVal, depth int)
	seen := map[*lisp.LVal]bool{}
	probe = func(v *lisp.LVal, depth int) {
		if v == nil || found || depth > 64 || seen[v] {
			return
		}
		seen[v] = true
		if nodes[v] {
			found = true
			return
		}
		forEachChild(v, func(c *lisp.LVal) { probe(c, depth+1) })
	}
	probe(b, 0)
	return found
}

func collectNodes(v *lisp.LVal, into map[*lisp.LVal]bool, depth int) {
	if v == nil || depth > 64 || into[v] {
		return
	}
	into[v] = true
	forEachChild(v, func(c *lisp.LVal) { collectNodes(c, into, depth+1) })
}

// forEachChild visits the LVals a value directly holds.  A sorted-map's
// entries live in Native, not Cells, so a plain Cells walk would treat every
// map as a leaf — which is exactly how a sharing relationship through a map
// would go unnoticed.
func forEachChild(v *lisp.LVal, fn func(*lisp.LVal)) {
	if v.Type == lisp.LSortMap {
		m := v.Map()
		if m == nil {
			return
		}
		ks := m.Keys()
		if ks == nil || ks.Type == lisp.LError {
			return
		}
		for _, k := range ks.Cells {
			if val, ok := m.Get(k); ok {
				fn(val)
			}
		}
		return
	}
	for _, c := range v.Cells {
		fn(c)
	}
}

// containerRenderMaxNodes bounds the expansion these targets are willing to
// hand to LVal.String().  It counts nodes the way the RENDERER visits them —
// a shared child is counted once per reference — because that is the
// quantity that blows up, not the number of distinct allocations.
const containerRenderMaxNodes = 1 << 16

// containerRenderable reports whether every value in vs can be rendered by
// LVal.String() within a bounded amount of work.
//
// # Why a fuzz harness has to ask this question at all
//
// LVal.String() and its helpers (exprString, sortedMapString, bodyStr)
// recurse with NO cycle detection and NO depth bound.  A self-referential
// runtime container is ordinary, legal lisp —
//
//	(set 'm (sorted-map))
//	(assoc! m "k" m)
//	m                      ; renders forever
//
//	(set 'v (vector))
//	(append! v v)
//	v                      ; likewise
//
// — and rendering one exhausts the goroutine stack.  A Go stack overflow is
// a FATAL runtime error, not a panic: env.eval's recover() cannot intercept
// it and the process dies.  The same shape kills `equal?` and
// `json:dump-string`, so the defect is a class spanning several unguarded
// recursive walks rather than one site, and choosing what a cyclic value
// should print as is a language decision.  It is reported rather than fixed
// here; the checked-mode code already refuses to call LVal.String() on
// "possibly cyclic" values for exactly this reason (see
// sealViolationMessage and ownershipViolationMessage).
//
// This target therefore declines to be the millionth reporter of that one
// defect.  A step that produces an unrenderable value abandons the sequence,
// and everything up to that step has already been asserted.
//
// The bound is on WORK, not just on cycles: a chain of doubling appends
// builds an acyclic value whose rendered form is exponential in the number
// of steps, and a harness that only checked for cycles would hang on it.
func containerRenderable(vs []*lisp.LVal) bool {
	budget := containerRenderMaxNodes
	onPath := map[*lisp.LVal]bool{}
	var walk func(v *lisp.LVal) bool
	walk = func(v *lisp.LVal) bool {
		if v == nil {
			return true
		}
		if onPath[v] {
			return false // a cycle: the renderer would not terminate
		}
		budget--
		if budget <= 0 {
			return false
		}
		onPath[v] = true
		defer delete(onPath, v)

		if v.Type == lisp.LSortMap {
			m := v.Map()
			if m == nil {
				return true
			}
			ks := m.Keys()
			if ks == nil || ks.Type == lisp.LError {
				return true
			}
			for _, k := range ks.Cells {
				val, ok := m.Get(k)
				if ok && !walk(val) {
					return false
				}
			}
			return true
		}
		for _, c := range v.Cells {
			if !walk(c) {
				return false
			}
		}
		return true
	}
	for _, v := range vs {
		if !walk(v) {
			return false
		}
	}
	return true
}

func renderContainerArgs(args []*lisp.LVal) string {
	var sb strings.Builder
	sb.WriteString("--- args ---")
	for i, a := range args {
		fmt.Fprintf(&sb, "\n  [%d] %v: %s", i, a.Type, a)
	}
	return sb.String()
}

// newContainerEnv builds a fresh, fully-loaded environment per input.
//
// Fresh, not cached: these builtins are reached through an environment whose
// globals a previous input's callback could have rewritten, so a cached env
// would make iteration N's result depend on iterations 0..N-1 and a saved
// crasher would not reproduce from a clean start — the one property a
// regression corpus must have.
//
// Runtime.Library is left nil so `load-file` returns an error instead of
// reading the filesystem, and Stderr is discarded so debug-print does not
// bury the fuzzer's own output.
func newContainerEnv(t *testing.T, ctx context.Context) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env,
		lisp.WithContext(ctx),
		lisp.WithMaxSteps(containerMaxEvalSteps),
		lisp.WithMaxAlloc(containerMaxAlloc),
	)
	if rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		t.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	env.Runtime.Library = nil
	return env
}

// TestContainerGenCoverage is the generator's own gate.
//
// A fuzz target whose generator silently produces one shape forever cannot
// fail, and nothing about that is visible from a green fuzz run.  This
// asserts every shape is reachable from short inputs, and that the
// preferential draws really do land inside generated structures — a key
// generator that always missed would leave every target exercising nothing
// but the "absent" branch.
func TestContainerGenCoverage(t *testing.T) {
	t.Parallel()
	shapes := map[lisp.LType]int{}
	hits, draws := 0, 0
	for i := range 8192 {
		g := newContainerGen([]byte{byte(i), byte(i >> 8), byte(i >> 4), byte(i * 7), byte(i * 13), byte(i * 31)})
		v := g.value(0)
		shapes[v.Type]++

		live := surveyMapKeys(v)
		if len(live) == 0 {
			continue
		}
		draws++
		k := g.mapKey(v)
		for _, l := range live {
			if l.Type == k.Type && l.Str == k.Str {
				hits++
				break
			}
		}
	}
	for _, want := range []lisp.LType{
		lisp.LSExpr, lisp.LArray, lisp.LBytes, lisp.LSortMap,
		lisp.LString, lisp.LSymbol, lisp.LInt, lisp.LFloat,
	} {
		if shapes[want] == 0 {
			t.Errorf("the generator never produced a %v value: %v", want, shapes)
		}
	}
	if draws == 0 {
		t.Fatal("no generated value ever contained a sorted-map key; the key survey is dead")
	}
	if hits*2 < draws {
		t.Errorf("generated lookup keys hit a live key only %d/%d of the time;"+
			" the targets would spend their budget on the key-absent branch", hits, draws)
	}
}

// TestContainerRenderableGuard pins both directions of the renderability
// guard.  A guard stuck at "always renderable" would let a cyclic value
// reach LVal.String() and kill the process with an unattributable stack
// overflow; a guard stuck at "never renderable" would abandon every
// sequence at step one and the three targets would assert nothing while
// still reporting a healthy exec rate.
func TestContainerRenderableGuard(t *testing.T) {
	t.Parallel()

	plain := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("x")})
	if !containerRenderable([]*lisp.LVal{plain}) {
		t.Error("an ordinary list was rejected as unrenderable")
	}

	// Sharing without a cycle must stay renderable: aliasing is the normal
	// case in these targets, and rejecting it would gut them.
	shared := lisp.QExpr([]*lisp.LVal{plain, plain, plain})
	if !containerRenderable([]*lisp.LVal{shared}) {
		t.Error("an acyclic value with shared children was rejected as unrenderable")
	}

	// (set 'v (vector)) (append! v v) — the exact shape that kills the
	// process, built here without going through the builtin.
	cyc := lisp.Array(nil, nil)
	cyc.Cells[1].Cells = append(cyc.Cells[1].Cells, cyc)
	cyc.Cells[0].Cells[0].Int = 1
	if containerRenderable([]*lisp.LVal{cyc}) {
		t.Error("a self-referential vector was accepted as renderable;" +
			" LVal.String() on it exhausts the goroutine stack, which recover() cannot intercept")
	}

	// And through a sorted-map's Native storage, the route the fuzzer
	// actually found first: (assoc! m "k" m).
	m := lisp.SortedMap()
	if lerr := m.Map().Set(lisp.String("k"), m); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	if containerRenderable([]*lisp.LVal{m}) {
		t.Error("a self-referential sorted-map was accepted as renderable")
	}
}

// TestContainerSealedRootsFound pins the other half of the harness: the
// sealed oracle is only as good as the roots it is given.  A collector that
// returned nothing would make every sealed assertion in all three targets
// trivially true.
func TestContainerSealedRootsFound(t *testing.T) {
	t.Parallel()
	// A vector (never sealed) holding a list literal (sealed by SealAST) —
	// the shape a phylum produces when it pushes a quoted literal into a
	// runtime container.
	lit := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	lit.SealAST()
	if !lit.IsSealed() {
		t.Fatal("SealAST did not seal a list literal")
	}
	vec := lisp.Array(nil, []*lisp.LVal{lit})
	roots := collectContainerSealed([]*lisp.LVal{vec})
	if len(roots) != 1 || roots[0] != lit {
		t.Fatalf("the sealed literal inside an unsealed vector was not collected: got %d roots", len(roots))
	}

	// And through a sorted-map's Native storage, which has no Cells for the
	// generic walk to descend.
	sm := lisp.SortedMap()
	if lerr := sm.Map().Set(lisp.String("k"), lit); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	roots = collectContainerSealed([]*lisp.LVal{sm})
	if len(roots) != 1 || roots[0] != lit {
		t.Fatalf("the sealed literal inside a sorted-map was not collected: got %d roots", len(roots))
	}
}
