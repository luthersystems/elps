// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// FuzzHigherOrderOps is the structure-aware target for the traversal family:
// map, foldl, foldr, zip, select and reject, applied over SHARED structures
// with callbacks that try to mutate what they are handed.
//
// # Why this family is its own corruption surface
//
// The other two container targets fuzz builtins that read and write storage
// directly.  This family's builtins mostly do not touch element storage at
// all — and that is exactly the risk.  They HAND ELEMENTS OUT:
//
//	for j, list := range lists {
//	    elemCells[j] = seqCells(list)[i]   // builtinZip
//	}
//
// `zip` puts the input's own element nodes into its output; `select` and
// `reject` do the same for the elements that pass; `map` and the folds pass
// each element to a caller-supplied function.  Nothing is copied.  So the
// corruption question for this family is not "does the builtin write the
// input" but "can what the builtin hands out be used to write the input" —
// and under a shared parse cache the answer decides whether a phylum can
// scribble on a program literal by folding over it.
//
// The callbacks are therefore ADVERSARIAL BY CONSTRUCTION: each one attempts
// an in-place mutation of the element it receives, wrapped in
// `ignore-errors` so a refusal is an ordinary outcome rather than an
// abandoned traversal.  On an unsealed element the mutation is expected to
// succeed — that is the language.  On a SEALED element the mutating builtins
// must refuse (the modify-literal-error condition, issue #378), and the
// sealed oracle is what says the refusal left no write behind.
//
// # Shared structures, deliberately
//
// The input sequences are built with REPEATED ELEMENTS: the same LVal
// appears at several indices, and the several sequences handed to `zip`
// share elements with each other.  A callback that mutates element 0
// therefore changes what the traversal sees at index 3, and a defect that
// needs the same node reached twice is reachable rather than a coincidence.
//
// # Assertions
//
// Everything containerCall asserts — no panic escapes (env.FunCall, not
// env.Eval), never a nil *LVal, the result renders, the shared singletons
// are untouched, the pool's sealed roots are bit-identical after EVERY step,
// and each non-mutated argument is bit-identical by value.
//
// For this family the FUNCTION argument is what makes the last assertion
// sharp: none of these builtins is documented as mutating anything, so
// mutatesArg is -1 throughout and every argument — the sequences included —
// is held to the no-modification contract... with one deliberate exception,
// recorded in the table below: a callback that mutates its argument is the
// caller's own doing, not the builtin's, so the sequence argument is exempt
// whenever an adversarial callback was selected.  Without that exemption the
// target would report the language's own semantics on nearly every input.
// The SEALED oracle keeps its teeth in both cases, and that is the clause
// that matters: a callback may rewrite a mutable value it is given and must
// never rewrite a program literal.
//
// NOT ASSERTED: which inputs fail.  A wrong-typed sequence, a non-function
// in function position and an arity mismatch are all legitimate errors.
func FuzzHigherOrderOps(f *testing.F) {
	for _, seed := range containerSeeds {
		f.Add(seed)
	}

	f.Fuzz(func(t *testing.T, data []byte) {
		ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
		defer cancel()
		env := newContainerEnv(t, ctx)
		defineHigherOrderCallbacks(t, env)
		runHigherOrderSequence(t, env, newContainerGen(data))
	})
}

// higherOrderOps is the family under test.  Every entry is documented as
// returning a NEW sequence, so mutatesArg is -1 for all of them: the
// builtins themselves promise to modify nothing.
var higherOrderOps = []containerOp{
	{name: "map", mutatesArg: -1},
	{name: "foldl", mutatesArg: -1},
	{name: "foldr", mutatesArg: -1},
	{name: "zip", mutatesArg: -1},
	{name: "select", mutatesArg: -1},
	{name: "reject", mutatesArg: -1},
}

// higherOrderCallbacks is the callback pool, as ELPS source.
//
// Each mutating entry wraps its attempt in `ignore-errors`, so a refusal
// (the right answer for a sealed or wrongly-typed element) leaves the
// traversal running instead of collapsing it into a single error — a
// callback that always errors would leave the builtin's own loop body
// untested after the first element.
//
// The `mutates` flag is what containerCall's exemption keys off: a callback
// that writes its argument makes the sequence argument's contents move for
// reasons that are the CALLER's, not the builtin's.
var higherOrderCallbacks = []struct {
	name    string
	src     string
	arity   int
	mutates bool
}{
	{name: "hof-id", arity: 1, src: `(defun hof-id (x) x)`},
	{name: "hof-car", arity: 1, src: `(defun hof-car (x) (ignore-errors (car x)))`},
	{name: "hof-wrap", arity: 1, src: `(defun hof-wrap (x) (list x x))`},
	{name: "hof-truthy", arity: 1, src: `(defun hof-truthy (x) (not (nil? x)))`},
	// The adversarial unary callbacks: each tries to write the element it
	// was handed, which on a program literal must be refused.
	{name: "hof-append", arity: 1, mutates: true, src: `(defun hof-append (x) (ignore-errors (append! x 'z)) x)`},
	{name: "hof-sort", arity: 1, mutates: true, src: `(defun hof-sort (x) (ignore-errors (stable-sort < x)) x)`},
	{name: "hof-assoc", arity: 1, mutates: true, src: `(defun hof-assoc (x) (ignore-errors (assoc! x "k" 1)) x)`},
	// Binary callbacks for the folds.
	{name: "hof-cons", arity: 2, src: `(defun hof-cons (a b) (cons a b))`},
	{name: "hof-pair", arity: 2, src: `(defun hof-pair (a b) (list a b))`},
	{name: "hof-fold-append", arity: 2, mutates: true, src: `(defun hof-fold-append (a b) (ignore-errors (append! a b)) a)`},
	{name: "hof-fold-sort", arity: 2, mutates: true, src: `(defun hof-fold-sort (a b) (ignore-errors (stable-sort < a)) a)`},
}

// defineHigherOrderCallbacks installs the callback pool into env.
func defineHigherOrderCallbacks(t *testing.T, env *lisp.LEnv) {
	t.Helper()
	for _, cb := range higherOrderCallbacks {
		if rc := env.LoadString("hof-callbacks", cb.src); rc.Type == lisp.LError {
			t.Fatalf("defining callback %s failed: %v", cb.name, rc)
		}
	}
}

// runHigherOrderSequence executes one generated traversal sequence.  Named,
// not inline, so the coverage gate drives the identical code path.
func runHigherOrderSequence(t *testing.T, env *lisp.LEnv, g *containerGen) (wanted, ran int) {
	t.Helper()

	// The pool is built from SHARED elements: sharedSeq repeats the same
	// LVal at several indices, and the two sequences share elements with
	// each other.  See the file comment for why that is the point.
	elems := g.cells(1)
	pool := []*lisp.LVal{
		g.sharedSeq(elems, false),
		g.sharedSeq(elems, true),
		g.value(0),
	}
	sealPool(g, pool)
	sealedRoots := collectContainerSealed(pool)
	fpSealed := lisp.SealedASTFingerprint(sealedRoots)

	wanted = 1 + g.intn(containerMaxSteps)
	for step := range wanted {
		op := higherOrderOps[g.intn(len(higherOrderOps))]
		cb := higherOrderCallbacks[g.intn(len(higherOrderCallbacks))]
		fn := env.GetGlobal(lisp.Symbol(cb.name))
		if fn == nil || fn.Type != lisp.LFun {
			t.Fatalf("callback %s is not bound in the fuzz environment", cb.name)
		}

		typespec := lisp.Symbol(sequenceTypeSpecs[g.intn(len(sequenceTypeSpecs))])
		seq := g.pick(pool)

		var args []*lisp.LVal
		switch op.name {
		case "map":
			args = []*lisp.LVal{typespec, fn, seq}
		case "foldl", "foldr":
			args = []*lisp.LVal{fn, g.pick(pool), seq}
		case "zip":
			args = []*lisp.LVal{typespec, seq}
			for range 1 + g.intn(2) {
				args = append(args, g.pick(pool))
			}
		default: // select, reject
			args = []*lisp.LVal{typespec, fn, seq}
		}

		// The exemption described in the file comment.  It applies only when
		// the operation actually RUNS the callback: `zip` takes no function,
		// so its arguments stay under the full value oracle no matter which
		// callback the driver bytes happened to select.
		//
		// The exemption is whole-call rather than per-argument because a
		// caller-supplied write does not have a fixed target: a fold's
		// callback writes the ACCUMULATOR while a map's writes an ELEMENT of
		// the sequence, and an earlier version that named the sequence
		// reported `foldl` for a write its own callback made to the
		// accumulator.
		effective := op
		if cb.mutates && op.name != "zip" {
			effective.skipValueOracle = true
		}

		result := containerCall(t, env, effective, args, sealedRoots, fpSealed, step)
		if result == nil {
			return wanted, ran // skipped, abandoned, or already failed
		}
		ran++
		pool = append(pool, result)
		if len(pool) > 2*containerMaxSteps {
			pool = pool[1:]
		}
		sealedRoots = collectContainerSealed(pool)
		fpSealed = lisp.SealedASTFingerprint(sealedRoots)
	}
	return wanted, ran
}

// sharedSeq builds a sequence whose cells are drawn WITH REPETITION from
// elems, so the same node appears at several indices and, across two calls,
// in two different sequences.  asVector selects the container shape.
//
// An empty element set yields an empty sequence rather than a synthesised
// one: an empty input is its own boundary (issue #334's empty-case bug lived
// there) and pretending otherwise would remove it from the corpus.
func (g *containerGen) sharedSeq(elems []*lisp.LVal, asVector bool) *lisp.LVal {
	n := g.intn(containerMaxLen + 1)
	cells := make([]*lisp.LVal, 0, n)
	for range n {
		if len(elems) == 0 {
			break
		}
		cells = append(cells, elems[g.intn(len(elems))])
	}
	if asVector {
		return lisp.Array(nil, cells)
	}
	return lisp.QExpr(cells)
}

// TestHigherOrderSharingIsReachable is the gate on the premise of the whole
// target: that the generated sequences really do share element storage, both
// within one sequence and between two.  A generator that produced distinct
// elements everywhere would leave this target fuzzing traversals over
// unrelated values, which is what FuzzApplyStdlib already does.
func TestHigherOrderSharingIsReachable(t *testing.T) {
	t.Parallel()
	withinShared, betweenShared, samples := 0, 0, 0
	for i := range 4000 {
		g := newContainerGen(containerProbeBytes(i))
		elems := g.cells(1)
		a := g.sharedSeq(elems, false)
		b := g.sharedSeq(elems, true)
		if a.Len() < 2 {
			continue
		}
		samples++
		seen := map[*lisp.LVal]bool{}
		for _, c := range a.Cells {
			if seen[c] {
				withinShared++
				break
			}
			seen[c] = true
		}
		for _, c := range b.Cells[1].Cells {
			if seen[c] {
				betweenShared++
				break
			}
		}
	}
	if samples == 0 {
		t.Fatal("no generated sequence had two or more elements; the sharing premise is untestable")
	}
	if withinShared*10 < samples {
		t.Errorf("only %d/%d sequences repeated an element within themselves;"+
			" a callback mutating element 0 would almost never be observed at another index",
			withinShared, samples)
	}
	if betweenShared*10 < samples {
		t.Errorf("only %d/%d sequence pairs shared an element between them;"+
			" zip's hand-out-the-input-node path is barely reached", betweenShared, samples)
	}
}

// TestHigherOrderCallbacksAreAdversarial pins that the mutating callbacks
// really do mutate.  If `append!` inside `ignore-errors` silently stopped
// working — or the callback pool were installed but never selected — this
// target would traverse structures with a set of pure functions and report
// a healthy exec rate while asserting nothing about the interesting half.
func TestHigherOrderCallbacksAreAdversarial(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)
	defineHigherOrderCallbacks(t, env)

	// A MUTABLE vector: the mutation must land.
	vec := lisp.Array(nil, []*lisp.LVal{lisp.Int(1)})
	before := valueFingerprint([]*lisp.LVal{vec})
	fn := env.GetGlobal(lisp.Symbol("hof-append"))
	if fn.Type != lisp.LFun {
		t.Fatal("hof-append is not bound")
	}
	if rc := env.FunCall(fn, lisp.SExpr([]*lisp.LVal{vec})); rc.Type == lisp.LError {
		t.Fatalf("hof-append: %v", rc)
	}
	if valueFingerprint([]*lisp.LVal{vec}) == before {
		t.Fatal("the adversarial callback did not modify a mutable vector;" +
			" this target is traversing with pure functions only")
	}

	// A SEALED list literal: the same callback must be refused, and the
	// literal must come through bit-identical.  This is the property the
	// fuzz target asserts on every step, pinned once at `go test` speed.
	lit := lisp.QExpr([]*lisp.LVal{lisp.Int(3), lisp.Int(1), lisp.Int(2)})
	lit.SealAST()
	roots := collectContainerSealed([]*lisp.LVal{lit})
	if len(roots) == 0 {
		t.Fatal("the sealed literal was not collected; the oracle would assert nothing")
	}
	fp := lisp.SealedASTFingerprint(roots)
	sortFn := env.GetGlobal(lisp.Symbol("hof-sort"))
	if rc := env.FunCall(sortFn, lisp.SExpr([]*lisp.LVal{lit})); rc.Type == lisp.LError {
		t.Fatalf("hof-sort: %v", rc)
	}
	if after := lisp.SealedASTFingerprint(roots); after != fp {
		t.Fatalf("a callback sorted a sealed program literal in place"+
			" (fingerprint %016x -> %016x): elps#369 mechanism 1 is live again;"+
			" the literal is now %v", fp, after, lit)
	}
}

// TestHigherOrderSequencesRunToCompletion is the renderability guard's gate
// for this target — see the note on TestSortedMapSequencesRunToCompletion.
func TestHigherOrderSequencesRunToCompletion(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)
	defineHigherOrderCallbacks(t, env)

	const samples = 2000
	completed, wantedTotal, ranTotal := 0, 0, 0
	for i := range samples {
		g := newContainerGen(containerProbeBytes(i))
		wanted, ran := runHigherOrderSequence(t, env, g)
		wantedTotal += wanted
		ranTotal += ran
		if ran == wanted {
			completed++
		}
	}
	t.Logf("sequences completed %d/%d; steps run %d/%d", completed, samples, ranTotal, wantedTotal)
	if completed*2 < samples {
		t.Errorf("only %d/%d generated sequences ran to completion;"+
			" the renderability guard is abandoning the target's own coverage",
			completed, samples)
	}
}
