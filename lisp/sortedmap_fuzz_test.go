// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// FuzzSortedMapOps is the structure-aware target for the sorted-map family:
// assoc, assoc!, dissoc, dissoc!, get, keys and key?.
//
// WHY THIS FAMILY NEEDS ITS OWN ORACLE.  A sorted-map stores its entries in
// LVal.Native as a *MapData, which no parser can produce, so SealAST never
// marks one and lisp.SealedASTFingerprint reports the whole map as a single
// opaque "hole" byte.  Under the sealed oracle alone, `(get m "k")`
// scribbling over `m` fingerprints identically before and after — every
// map-mutating defect in the language is invisible.  This target runs the
// sealed oracle (for the literals a phylum puts INSIDE a map) together with
// the runtime-value oracle (for the map itself); see the header on
// lisp/containergen_test.go for why neither subsumes the other.
//
// THE SHAPE THAT MATTERS.  Maps here are NESTED — a value may itself be a
// map, a vector or a quoted list literal — and the sequence of operations
// runs over a POOL, so the map an operation is handed may be one an earlier
// `assoc` derived from another.  `copyMapData` shares its VALUES with the
// original by design, so a copying op followed by a mutating op on the copy
// is the family's version of the slice/append aliasing that produced
// elps#369, and it is a two-step path rather than a coincidence.
//
// ASSERTIONS (all from containerCall, plus the pool-level sealed oracle)
//
//  1. No panic escapes.  Applied through env.FunCall, not env.Eval, so
//     eval's recover() cannot launder a crash into an ordinary-looking
//     value.
//  2. Never a nil *LVal; the result renders.
//  3. The three shared singletons are bit-identical afterwards.
//  4. Every sealed root in the WHOLE POOL is bit-identical after EVERY
//     step, mutating operations included.  A quoted literal stored as a map
//     value must survive `assoc!` on the map that holds it.
//  5. Every argument an operation does not document itself as modifying is
//     bit-identical afterwards, by value.  `assoc` and `dissoc` are held to
//     it for the map; `assoc!` and `dissoc!` are held to it for the KEY and
//     the VALUE, which they store but must not rewrite.
//
// NOT ASSERTED: which inputs fail.  A wrong-typed key is a legitimate error
// (sorted-maps accept strings and symbols), and map_test.go pins the
// specific outcomes.
func FuzzSortedMapOps(f *testing.F) {
	for _, seed := range containerSeeds {
		f.Add(seed)
	}

	f.Fuzz(func(t *testing.T, data []byte) {
		ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
		defer cancel()
		runSortedMapSequence(t, newContainerEnv(t, ctx), newContainerGen(data))
	})
}

// runSortedMapSequence executes one generated operation sequence.  It is a
// named function, not an inline closure, so the coverage gate below can
// drive the identical code path — a harness whose gate tests a paraphrase of
// the harness is testing the paraphrase.
//
// It returns how many steps were requested and how many actually ran; the
// two differ when a step produces a value the interpreter cannot render (see
// containerRenderable) and the sequence is abandoned.
func runSortedMapSequence(t *testing.T, env *lisp.LEnv, g *containerGen) (wanted, ran int) {
	t.Helper()

	// The pool starts with one nested map plus one free-form value, so an
	// operation can be handed something that is not a map at all — the
	// type-check branch is as much of the contract as the rest.
	pool := []*lisp.LVal{g.sortedMap(0), g.value(0)}
	sealPool(g, pool)
	sealedRoots := collectContainerSealed(pool)
	fpSealed := lisp.SealedASTFingerprint(sealedRoots)

	wanted = 1 + g.intn(containerMaxSteps)
	for step := range wanted {
		op := sortedMapOps[g.intn(len(sortedMapOps))]
		target := pool[g.intn(len(pool))]

		args := []*lisp.LVal{target}
		switch op.name {
		case "keys":
			// arity 1
		case "assoc", "assoc!":
			args = append(args, g.mapKey(target), g.pick(pool))
		default: // dissoc, dissoc!, get, key?
			args = append(args, g.mapKey(target))
		}

		result := containerCall(t, env, op, args, sealedRoots, fpSealed, step)
		if result == nil {
			return wanted, ran // skipped, abandoned, or already failed
		}
		ran++
		// Feeding the result back in is the point: the next step's operand
		// may share storage with this one's.
		pool = append(pool, result)
		if len(pool) > 2*containerMaxSteps {
			pool = pool[1:]
		}
		// A new value may carry sealed nodes the earlier roots did not cover
		// (a copying op returning a map that still holds the original's
		// sealed literals under a fresh header).
		sealedRoots = collectContainerSealed(pool)
		fpSealed = lisp.SealedASTFingerprint(sealedRoots)
	}
	return wanted, ran
}

// sortedMapOps is the family under test.  mutatesArg names the argument the
// docstring licenses each one to rewrite; -1 means it promises to rewrite
// nothing.  Getting this table wrong in the permissive direction is how a
// target goes quiet, so each entry mirrors the docstring in lisp/builtins.go
// exactly.
var sortedMapOps = []containerOp{
	{name: "assoc", mutatesArg: -1},  // "Returns a new sorted-map"
	{name: "assoc!", mutatesArg: 0},  // "mutating it in place"
	{name: "dissoc", mutatesArg: -1}, // "Returns a new sorted-map"
	{name: "dissoc!", mutatesArg: 0}, // "mutating it in place"
	{name: "get", mutatesArg: -1},
	{name: "keys", mutatesArg: -1},
	{name: "key?", mutatesArg: -1},
}

// pick returns an existing pool member — the alias source.  Handing an
// operation a value another pool member already holds is what makes shared
// storage the normal case here rather than an accident.
func (g *containerGen) pick(pool []*lisp.LVal) *lisp.LVal {
	if len(pool) == 0 || g.next()%4 == 0 {
		return g.value(1)
	}
	return pool[g.intn(len(pool))]
}

// sealPool seals a fuzzer-chosen subset of the pool, exactly as a quoted
// program literal arrives sealed.  SealAST marks only the parser-producible
// shapes — lists, symbols, strings and numbers — and leaves vectors,
// sorted-maps and bytes mutable, which is the split production sees when a
// phylum passes a literal into a container builtin.
//
// Sealing a SUBSET rather than everything is deliberate: the unsealed half
// is where the documented Go-slice aliasing of issue #373 lives, and a
// target that sealed everything would never exercise it.
func sealPool(g *containerGen, pool []*lisp.LVal) {
	for _, v := range pool {
		if g.coin() {
			v.SealAST()
		}
	}
}

// containerSeeds are the shared seed corpus for the three container targets:
// byte strings written here, chosen to reach each operation and each
// generator branch at least once.  None is derived from any downstream
// corpus.
var containerSeeds = [][]byte{
	{},
	{0x00},
	{0x01},
	{0x01, 0x02, 0x03},
	{0x09, 0x03, 0x01, 0x02, 0x05, 0x00, 0x04},
	{0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76},
	{0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa, 0xf9, 0xf8},
	{0x05, 0x00, 0x07, 0x01, 0x02, 0x00, 0x03, 0x09, 0x04, 0x06},
	{0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa},
	{0x07, 0x03, 0x01, 0x04, 0x01, 0x05, 0x09, 0x02, 0x06, 0x05, 0x03, 0x05},
	{0x06, 0x04, 0x02, 0x08, 0x03, 0x07, 0x01, 0x05, 0x00, 0x09, 0x02, 0x04, 0x06, 0x08},
	{0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03},
}

// TestSortedMapSequencesRunToCompletion is the guard's own gate.
//
// containerRenderable abandons a sequence when a step builds a value
// LVal.String() cannot render.  That is necessary — the alternative is
// reporting one known defect forever — but a guard that fired too eagerly
// would abandon every sequence at step one, and the target would keep
// reporting a healthy exec rate while asserting almost nothing.  Nothing
// about that is visible from a green fuzz run, so it is measured here: the
// overwhelming majority of generated sequences must run every step they
// asked for.
func TestSortedMapSequencesRunToCompletion(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	const samples = 4000
	completed, wantedTotal, ranTotal := 0, 0, 0
	for i := range samples {
		g := newContainerGen([]byte{
			byte(i), byte(i >> 8), byte(i >> 4), byte(i * 7),
			byte(i * 13), byte(i * 31), byte(i * 3), byte(i * 5),
		})
		wanted, ran := runSortedMapSequence(t, env, g)
		wantedTotal += wanted
		ranTotal += ran
		if ran == wanted {
			completed++
		}
	}
	t.Logf("sequences completed %d/%d; steps run %d/%d", completed, samples, ranTotal, wantedTotal)
	if completed*10 < samples*9 {
		t.Errorf("only %d/%d generated sequences ran to completion;"+
			" the renderability guard is abandoning the target's own coverage",
			completed, samples)
	}
}

// TestSortedMapOpsAreDocumented is the table's own gate.  A `mutatesArg`
// that drifted permissive would silently stop asserting the copy contract
// of a whole operation, and a green fuzz run would look exactly the same.
// This pins the two halves of the family against actual behaviour: the
// copying ops leave their input alone, and the mutating ones do not.
func TestSortedMapOpsAreDocumented(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithTimeout(context.Background(), containerDeadline)
	defer cancel()
	env := newContainerEnv(t, ctx)

	build := func() *lisp.LVal {
		m := lisp.SortedMap()
		if lerr := m.Map().Set(lisp.String("a"), lisp.Int(1)); lerr.Type == lisp.LError {
			t.Fatalf("map set: %v", lerr)
		}
		return m
	}

	for _, op := range sortedMapOps {
		t.Run(op.name, func(t *testing.T) {
			m := build()
			before := valueFingerprint([]*lisp.LVal{m})
			args := []*lisp.LVal{m}
			switch op.name {
			case "keys":
			case "assoc", "assoc!":
				args = append(args, lisp.String("z"), lisp.Int(9))
			default:
				args = append(args, lisp.String("a"))
			}
			fun := env.GetGlobal(lisp.Symbol(op.name))
			if fun.Type != lisp.LFun {
				t.Fatalf("%s is not a function: %v", op.name, fun.Type)
			}
			if rc := env.FunCall(fun, lisp.SExpr(args)); rc.Type == lisp.LError {
				t.Fatalf("%s: %v", op.name, rc)
			}
			changed := valueFingerprint([]*lisp.LVal{m}) != before
			wantChanged := op.mutatesArg == 0
			if changed != wantChanged {
				t.Fatalf("%s modified its map argument = %v, but the table says mutatesArg=%d;"+
					" the value oracle is asserting the wrong contract for this operation",
					op.name, changed, op.mutatesArg)
			}
		})
	}
}
