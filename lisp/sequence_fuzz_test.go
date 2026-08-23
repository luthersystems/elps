// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// FuzzSequenceOps is the structure-aware target for the sequence family:
// append, append!, append-bytes, append-bytes!, slice, concat, nth, reverse,
// stable-sort, insert-index and insert-sorted, across LISTS, VECTORS and
// BYTES.
//
// # The shape this target exists to build
//
// elps#369 mechanism 2 is not a bug in any single call:
//
//	(set 'lit '(1 2 3))
//	(append 'vector (slice 'list lit 0 1) 99)
//	lit  ; => (1 99 3)
//
// `slice` hands out a window onto the source's backing array INCLUDING its
// retained capacity, and `append` writes through it.  Two correct-looking
// calls, one corrupted literal.  The same shape exists for bytes, where
// `slice 'bytes` returns `Bytes(src[i:j])` — a Go reslice, capacity and all.
//
// A generator producing one independent argument list per call reaches that
// only by accident, so this target fuzzes an operation SEQUENCE over a value
// POOL: every result is pushed back, and the next step's operands are drawn
// from the pool.  Aliasing between step i's output and step j's input is
// then the normal case.  The sequence is additionally given an explicit
// capacity-sharing PRELUDE — build a slice of the base sequence, put BOTH
// the slice and the source in the pool — so the fuzzer starts from the
// three-step path rather than having to discover it.
//
// # Where the assertions draw the line
//
// Issue #373 is the maintainer's decision on this exact behaviour: keep Go
// semantics, document the pitfall, do not add copy-on-append (it would turn
// amortized O(1) vector building into O(n^2), and `concat` is the documented
// copy idiom).  So writing through a shared backing array is NOT by itself a
// defect, and a target that asserted pool-wide value stability would report
// the language's own design on most inputs.
//
// What #373 also says is that the one dangerous instance of the class —
// program literals shared across environments — is closed mechanically by
// the seal.  That is the line the assertions draw:
//
//   - SEALED storage is bit-identical after EVERY step, for every operation
//     including the mutating ones.  `slice` propagates the seal to its
//     result and `append` copies on a sealed input; drift means one of those
//     guards failed and a phylum's literal is corrupted for every
//     environment sharing the parse.
//   - Each operation's OWN arguments are bit-identical unless its docstring
//     says otherwise.  `append` promises "Does not mutate the original", so
//     it is held to that for the sequence it was handed — writing into the
//     retained capacity of a value it was NOT handed is the documented
//     aliasing and is not asserted against.
//
// BYTES ARE OUTSIDE THE SEAL BY CONSTRUCTION, and that is worth stating
// rather than discovering: SealAST marks only parser-producible shapes, and
// no literal is bytes, so an LBytes value is never sealed and the sealed
// oracle can say nothing about one.  The value oracle is the whole guarantee
// for the bytes half of this family — which is why valueFingerprint digests
// the LBytes payload out of Native, where the sealed walk does not look.
//
// # Assertions
//
// Everything containerCall asserts: no panic escapes (env.FunCall, not
// env.Eval), never a nil *LVal, the result renders, the shared singletons
// are untouched, the pool's sealed roots are bit-identical, and each
// non-mutated argument is bit-identical by value.
//
// NOT ASSERTED: which inputs fail.  An out-of-range slice bound and a
// wrong-typed sequence are legitimate errors; the specific outcomes are
// pinned by the package's own tests.
func FuzzSequenceOps(f *testing.F) {
	for _, seed := range containerSeeds {
		f.Add(seed)
	}

	f.Fuzz(func(t *testing.T, data []byte) {
		ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
		defer cancel()
		runSequenceSequence(t, newContainerEnv(t, ctx), newContainerGen(data))
	})
}

// sequenceOps is the family under test.  mutatesArg names the argument each
// operation's DOCSTRING licenses it to modify in place; -1 means it promises
// to modify nothing.  Note that it is not always argument zero — stable-sort
// takes the predicate first and sorts its SECOND argument — which is why the
// field is an index.
var sequenceOps = []containerOp{
	{name: "append", mutatesArg: -1},        // "Does not mutate the original."
	{name: "append!", mutatesArg: 0},        // "mutating it in place"
	{name: "append-bytes", mutatesArg: -1},  // "Returns new bytes"
	{name: "append-bytes!", mutatesArg: 0},  // "mutating it in place"
	{name: "slice", mutatesArg: -1},         // "Returns a subsequence"
	{name: "concat", mutatesArg: -1},        // "Concatenates sequences into one"
	{name: "nth", mutatesArg: -1},           // a read
	{name: "reverse", mutatesArg: -1},       // "Returns a new sequence"
	{name: "stable-sort", mutatesArg: 1},    // "A mutable list is sorted in place"
	{name: "insert-index", mutatesArg: -1},  // "Returns a new sequence"
	{name: "insert-sorted", mutatesArg: -1}, // "Returns a new sequence"
}

// sequenceTypeSpecs are the type-specifier symbols the family accepts.
// 'string and 'bytes are in the pool even where an operation rejects them:
// the type-specifier validation is part of the contract, and `(append 'bytes
// 0)` reaching LVal.Bytes() with an int was a real crash (see
// builtinAppend_Bytes' note).
var sequenceTypeSpecs = []string{"list", "vector", "bytes", "string", "nonsense"}

// runSequenceSequence executes one generated operation sequence.  Named,
// not inline, so the coverage gate drives the identical code path.
func runSequenceSequence(t *testing.T, env *lisp.LEnv, g *containerGen) (wanted, ran int) {
	t.Helper()

	// The base sequence, plus a free-form value so an operation can be
	// handed something that is not a sequence at all.
	base := g.seq(0)
	pool := []*lisp.LVal{base, g.value(0)}
	sealPool(g, pool)

	// THE CAPACITY-SHARING PRELUDE.  Build a slice of the base and put both
	// in the pool, so the very first fuzzed step can append to either one —
	// the elps#369 mechanism-2 path, reached deliberately rather than by
	// luck.  Skipped on some inputs so the un-sliced shapes stay reachable.
	if n := seqLen(base); n > 0 && g.next()%4 != 0 {
		if sub := sliceOf(env, base, g.intn(n+1)); sub != nil {
			pool = append(pool, sub)
		}
	}

	sealedRoots := collectContainerSealed(pool)
	fpSealed := lisp.SealedASTFingerprint(sealedRoots)

	wanted = 1 + g.intn(containerMaxSteps)
	for step := range wanted {
		op := sequenceOps[g.intn(len(sequenceOps))]
		args := g.sequenceArgs(env, op, pool)

		result := containerCall(t, env, op, args, sealedRoots, fpSealed, step)
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

// sequenceArgs builds the argument list for one operation, sized against the
// arity the builtin actually has.
//
// Arity is respected on purpose: builtins index args.Cells directly because
// LEnv.bind guarantees the count before the builtin runs, so handing a
// four-argument builtin two arguments would report a harness bug as a
// builtin bug.  What is fuzzed is the VALUE of each argument and the
// variadic count.
func (g *containerGen) sequenceArgs(env *lisp.LEnv, op containerOp, pool []*lisp.LVal) []*lisp.LVal {
	typespec := lisp.Symbol(sequenceTypeSpecs[g.intn(len(sequenceTypeSpecs))])
	seq := g.pick(pool)

	switch op.name {
	case "append":
		args := []*lisp.LVal{typespec, seq}
		for range g.intn(3) {
			args = append(args, g.pick(pool))
		}
		return args
	case "append!":
		args := []*lisp.LVal{seq}
		for range g.intn(3) {
			args = append(args, g.pick(pool))
		}
		return args
	case "append-bytes", "append-bytes!":
		return []*lisp.LVal{seq, g.pick(pool)}
	case "slice":
		return []*lisp.LVal{typespec, seq, lisp.Int(g.index(seq)), lisp.Int(g.index(seq))}
	case "concat":
		args := []*lisp.LVal{typespec}
		for range 1 + g.intn(3) {
			args = append(args, g.pick(pool))
		}
		return args
	case "nth":
		return []*lisp.LVal{seq, lisp.Int(g.index(seq))}
	case "reverse":
		return []*lisp.LVal{typespec, seq}
	case "stable-sort":
		return []*lisp.LVal{g.predicate(env), seq}
	case "insert-index":
		return []*lisp.LVal{typespec, seq, lisp.Int(g.index(seq)), g.pick(pool)}
	case "insert-sorted":
		return []*lisp.LVal{typespec, seq, g.predicate(env), g.pick(pool)}
	default:
		return []*lisp.LVal{seq}
	}
}

// sequencePredicates are the comparison functions handed to the sorting
// operations.  They are the real builtins rather than generated lambdas: a
// generated predicate that errors on the first comparison would leave the
// sort's own code path untouched, and the sort paths are what this target is
// aimed at.  A non-function is included because "second argument is not a
// function" is a branch too.
var sequencePredicates = []string{"<", ">", "<=", ">=", "=", "not-a-function"}

func (g *containerGen) predicate(env *lisp.LEnv) *lisp.LVal {
	name := sequencePredicates[g.intn(len(sequencePredicates))]
	fun := env.GetGlobal(lisp.Symbol(name))
	if fun == nil || fun.Type != lisp.LFun {
		return lisp.Symbol(name)
	}
	return fun
}

// sliceOf builds `(slice 'list seq 0 n)` — or the vector/bytes equivalent —
// through the real builtin, so the result carries whatever backing and
// capacity the real implementation gives it.  Hand-rolling the slice would
// test the harness's idea of the semantics rather than the interpreter's.
// A nil return means the operation errored, which is fine: the prelude is an
// optimisation for the mutator, not a required step.
func sliceOf(env *lisp.LEnv, seq *lisp.LVal, n int) *lisp.LVal {
	spec := "list"
	switch seq.Type { //nolint:exhaustive // the generator's base is always one of these three
	case lisp.LArray:
		spec = "vector"
	case lisp.LBytes:
		spec = "bytes"
	}
	fun := env.GetGlobal(lisp.Symbol("slice"))
	if fun == nil || fun.Type != lisp.LFun {
		return nil
	}
	sub := env.FunCall(fun, lisp.SExpr([]*lisp.LVal{
		lisp.Symbol(spec), seq, lisp.Int(0), lisp.Int(n),
	}))
	if sub == nil || sub.Type == lisp.LError {
		return nil
	}
	return sub
}

// TestSequenceCapacitySharingIsReachable is this target's most important
// gate: it pins that the harness really does construct the elps#369
// mechanism-2 relationship, rather than merely intending to.
//
// The test asserts the RELATIONSHIP, not the outcome — that a slice of a
// mutable list shares backing with its source, so an append into the slice's
// retained capacity is observable in the source.  If `slice` ever stopped
// sharing (a capacity-clamped slice is work item 3 on issue #373), this
// test is the thing that says the target's prelude has become a no-op and
// the shape needs rebuilding, instead of the target silently covering
// nothing.
func TestSequenceCapacitySharingIsReachable(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	// An UNSEALED list: the sealed path is copy-on-write by design, so it is
	// the wrong probe for whether backing is shared at all.
	src := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	sub := sliceOf(env, src, 1)
	if sub == nil {
		t.Fatal("slice of a three-element list failed; the prelude cannot build the shape")
	}
	if sub.Len() != 1 {
		t.Fatalf("expected a one-element slice, got %v", sub)
	}

	before := valueFingerprint([]*lisp.LVal{src})
	appendFun := env.GetGlobal(lisp.Symbol("append"))
	if appendFun.Type != lisp.LFun {
		t.Fatal("append is not bound")
	}
	rc := env.FunCall(appendFun, lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("vector"), sub, lisp.Int(99),
	}))
	if rc.Type == lisp.LError {
		t.Fatalf("append into the slice failed: %v", rc)
	}
	if valueFingerprint([]*lisp.LVal{src}) == before {
		t.Skipf("appending into a slice's retained capacity no longer reaches the source"+
			" (src is still %v); slice may now clamp capacity (issue #373 work item 3)."+
			" FuzzSequenceOps' capacity-sharing prelude is a no-op and the shape needs rebuilding.", src)
	}
}

// TestSequenceSealedLiteralSurvivesTheSameShape is the other half: the exact
// #369 mechanism-2 path, but with a SEALED source, where the seal's guard
// must hold — the append is refused with the catchable modify-literal-error
// condition (issue #378; the site used to copy-on-write silently) and the
// literal's bytes stay put.  This is the property the fuzz target asserts on
// every step, pinned once at `go test` speed so a seal regression is caught
// in milliseconds rather than only by a sweep.
func TestSequenceSealedLiteralSurvivesTheSameShape(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	lit := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	lit.SealAST()
	roots := collectContainerSealed([]*lisp.LVal{lit})
	if len(roots) == 0 {
		t.Fatal("the sealed literal was not collected; the oracle would assert nothing")
	}
	fp := lisp.SealedASTFingerprint(roots)

	sub := sliceOf(env, lit, 1)
	if sub == nil {
		t.Fatal("slice of a sealed literal failed")
	}
	appendFun := env.GetGlobal(lisp.Symbol("append"))
	rc := env.FunCall(appendFun, lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("vector"), sub, lisp.Int(99),
	}))
	if rc.Type != lisp.LError {
		t.Fatalf("append into the sealed slice was not refused: %v", rc)
	}
	if rc.Str != lisp.CondModifyLiteral {
		t.Errorf("append into the sealed slice raised %q, want %q: %v", rc.Str, lisp.CondModifyLiteral, rc)
	}
	if after := lisp.SealedASTFingerprint(roots); after != fp {
		t.Fatalf("(append 'vector (slice 'list <literal> 0 1) 99) corrupted the literal"+
			" (fingerprint %016x -> %016x): the sealed-write guard has regressed"+
			" and elps#369 mechanism 2 is live again; literal is now %v", fp, after, lit)
	}
}

// TestSequenceOpsAreDocumented pins the mutatesArg table against actual
// behaviour, in both directions, for the two operations where getting it
// wrong would silently disable a whole assertion: append (promises not to
// mutate) and append! (promises to).
func TestSequenceOpsAreDocumented(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	cases := []struct {
		op          string
		spec        string
		wantChanged bool
	}{
		{op: "append", spec: "vector", wantChanged: false},
		{op: "append", spec: "list", wantChanged: false},
		{op: "append!", wantChanged: true},
	}
	for _, tc := range cases {
		t.Run(tc.op+"/"+tc.spec, func(t *testing.T) {
			vec := lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2)})
			before := valueFingerprint([]*lisp.LVal{vec})
			args := []*lisp.LVal{vec, lisp.Int(3)}
			if tc.spec != "" {
				args = append([]*lisp.LVal{lisp.Symbol(tc.spec)}, args...)
			}
			fun := env.GetGlobal(lisp.Symbol(tc.op))
			if fun.Type != lisp.LFun {
				t.Fatalf("%s is not a function", tc.op)
			}
			if rc := env.FunCall(fun, lisp.SExpr(args)); rc.Type == lisp.LError {
				t.Fatalf("%s: %v", tc.op, rc)
			}
			changed := valueFingerprint([]*lisp.LVal{vec}) != before
			if changed != tc.wantChanged {
				t.Fatalf("%s %s modified its sequence argument = %v, want %v;"+
					" the mutatesArg table is asserting the wrong contract",
					tc.op, tc.spec, changed, tc.wantChanged)
			}
		})
	}
}

// TestSequenceSequencesRunToCompletion is the renderability guard's gate for
// this target — see the note on TestSortedMapSequencesRunToCompletion.  It
// matters more here: `append!` on a pool member with itself as the value is
// exactly how a cyclic vector gets built, so this family creates the
// unrenderable shape far more readily than the map family does.
func TestSequenceSequencesRunToCompletion(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	const samples = 4000
	completed, wantedTotal, ranTotal := 0, 0, 0
	for i := range samples {
		g := newContainerGen(containerProbeBytes(i))
		wanted, ran := runSequenceSequence(t, env, g)
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
