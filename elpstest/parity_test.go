// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
)

// The parity oracle's own controls.  Each one drives a fork that is wrong
// in a known way through elpstest.CheckParity and requires the witness the
// defect must produce, so a change that weakened the oracle -- dropped a
// comparison, compared the wrong arm -- turns one of these red rather than
// silently passing every input.  The over-sharing control lives in
// parity_sharing_test.go, behind `!elpscheck`, for the reason that file
// gives.

// parityAliasProgram is the shape of issue #576, pinned by
// TestForkCheck_SortedMapAliasAcrossHeaders: two names for one sorted map.
const parityAliasProgram = `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
`

// brokenForkDealiases forks correctly, then gives the fork's `b` its own
// copy of the map: the #576 defect reproduced from outside Fork.  Nothing
// is shared with the template, so the ownership checker has nothing to
// say, and only PARITY can see it -- the fork is perfectly isolated and
// simply wrong.
func brokenForkDealiases(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	if rc := f.LoadString("dealias.lisp", `(set 'b (copy b))`); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return f, nil
}

func parityWitnessKinds(ws []elpstest.Witness) (result, state bool) {
	for _, w := range ws {
		switch {
		case strings.Contains(w.Property, "returns what it returns"):
			result = true
		case strings.Contains(w.Property, "reachable state"):
			state = true
		}
	}
	return result, state
}

// TestForkParity_DetectsADealiasingFork: a write through `a` must be
// visible through `b` on a cold load; on the de-aliased fork it is not, so
// the transaction's RESULT diverges (7 against nil) and so does the
// post-run state of b.  Both witnesses are required: deleting either
// comparison in CheckParity turns this red.
func TestForkParity_DetectsADealiasingFork(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      [][]string{{`(assoc! a "y" 7) (get b "y")`}},
		Fork:    brokenForkDealiases,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	result, state := parityWitnessKinds(got)
	for _, w := range got {
		t.Logf("%s", w)
	}
	if !result || !state {
		t.Fatalf("a de-aliasing fork produced result witness=%t, state witness=%t; the parity oracle has been weakened", result, state)
	}
}

// TestForkParity_CorrectForkHoldsOverTheAliasShape is the other half of
// the control above: the same program and transaction through the real
// Fork produce no witness, so the de-aliasing witness is the defect and
// not the shape.
func TestForkParity_CorrectForkHoldsOverTheAliasShape(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      [][]string{{`(assoc! a "y" 7) (get b "y")`}, {`(dissoc! b "k") (get a "k")`}},
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// TestForkParity_ErrorsAreResults: a transaction that raises must raise
// the same way on a fork, and the oracle compares that rather than
// aborting -- a fork that raised where a cold load did not would otherwise
// be a harness error instead of a finding.
func TestForkParity_ErrorsAreResults(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      [][]string{{`(get a "k")`, `(nth a 0)`, `(get a "k")`}},
	})
	if err != nil {
		t.Fatalf("a raising transaction must be a result, not a harness error: %v", err)
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// TestForkParity_HandWrittenInterleavings runs the historical fork bugs'
// programs (forkcheck_test.go) as multi-environment sequences under both
// schedules and both hop depths, so the shapes RunForkCheck holds one
// transaction at a time are also held as sequences.
func TestForkParity_HandWrittenInterleavings(t *testing.T) {
	t.Parallel()
	program := `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(set 'both (list a b))
(set 'buf (to-bytes "abc"))
(set 'buf2 (quasiquote (unquote buf)))
(set 'counter (let ([n 0]) (list (lambda () n) (lambda () (set 'n (+ n 1))))))
`
	tx := [][]string{
		{`(assoc! a "y" 7) (get b "y")`, `((second counter)) ((first counter))`, `(dissoc! b "k") (get a "k")`},
		{`(append! buf 7) (length buf2)`, `((second counter))`, `(list (get (second both) "y") (get a "y"))`},
		{`((first counter))`, `(assoc! (first both) "z" 1) (get (second both) "z")`},
	}
	for _, interleave := range []bool{false, true} {
		for _, hops := range []int{1, 2} {
			got, err := elpstest.CheckParity(elpstest.ParityCheck{
				NewEnv:     newFuzzEnv,
				Program:    program,
				Tx:         tx,
				Interleave: interleave,
				Hops:       hops,
			})
			if err != nil {
				t.Fatalf("interleave=%t hops=%d: harness error: %v", interleave, hops, err)
			}
			for _, w := range got {
				t.Errorf("interleave=%t hops=%d: %s", interleave, hops, w)
			}
		}
	}
}

// TestForkParity_RefusesAVacuousCheck: no sequences means nothing is
// compared, and the oracle says so instead of returning no witnesses.
func TestForkParity_RefusesAVacuousCheck(t *testing.T) {
	t.Parallel()
	if _, err := elpstest.CheckParity(elpstest.ParityCheck{NewEnv: newFuzzEnv, Program: parityAliasProgram}); err == nil {
		t.Fatal("CheckParity with no transaction sequences returned no error; parity would hold vacuously")
	}
	if _, err := elpstest.CheckParity(elpstest.ParityCheck{NewEnv: newFuzzEnv, Program: parityAliasProgram, Tx: [][]string{{`a`}}, Hops: 3}); err == nil {
		t.Fatal("CheckParity with Hops=3 returned no error")
	}
}

// TestForkParity_HopsCountForkCalls pins ParityCheck.Hops: with n
// environments the walker is called n times at one hop and 2n at two, so
// the two-hop arm is actually a fork of a fork and not the same fork
// counted twice.
func TestForkParity_HopsCountForkCalls(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct{ hops, want int }{{0, 3}, {1, 3}, {2, 6}} {
		calls := 0
		_, err := elpstest.CheckParity(elpstest.ParityCheck{
			NewEnv:  newFuzzEnv,
			Program: parityAliasProgram,
			Tx:      [][]string{{`a`}, {`b`}, {`(assoc! a "x" 1)`}},
			Hops:    tc.hops,
			Fork: func(env *lisp.LEnv) (*lisp.LEnv, error) {
				calls++
				return env.Fork()
			},
		})
		if err != nil {
			t.Fatalf("hops=%d: harness error: %v", tc.hops, err)
		}
		if calls != tc.want {
			t.Errorf("hops=%d: the fork walker was called %d times, want %d", tc.hops, calls, tc.want)
		}
	}
}

// TestForkParity_DetectsAForkRefusal: the template loaded, a cold
// environment runs the program, and a fork of that template cannot be
// created.  Under the definition at the top of parity.go that is a parity
// violation, and it used to be a returned error the fuzz target turned
// into a skip.  The refusal is the PRODUCTION Fork's own: the walker here
// only stages a non-quiescent template (one frame on its call stack) for
// the second fork and lets checkQuiescent refuse it.  Environment 0 is
// still compared in full, which is the "continue past the failure" half.
func TestForkParity_DetectsAForkRefusal(t *testing.T) {
	t.Parallel()
	for _, interleave := range []bool{false, true} {
		calls := 0
		refusing := func(env *lisp.LEnv) (*lisp.LEnv, error) {
			calls++
			if calls != 2 {
				return env.Fork()
			}
			if err := env.Runtime.Stack.PushFID(nil, "_fun0", "user", "staged"); err != nil {
				return nil, err
			}
			defer env.Runtime.Stack.Pop()
			return env.Fork()
		}
		got, err := elpstest.CheckParity(elpstest.ParityCheck{
			NewEnv:     newFuzzEnv,
			Program:    parityAliasProgram,
			Tx:         [][]string{{`(assoc! a "y" 7) (get b "y")`}, {`(get a "k")`}},
			Interleave: interleave,
			Fork:       refusing,
		})
		if err != nil {
			t.Fatalf("interleave=%t: a fork refusal must be a witness, not a harness error: %v", interleave, err)
		}
		for _, w := range got {
			t.Logf("interleave=%t: %s", interleave, w)
		}
		if len(got) != 1 || !strings.Contains(got[0].Property, "a fork can be taken") || !strings.Contains(got[0].Detail, "not quiescent") {
			t.Fatalf("interleave=%t: want exactly one fork-refusal witness citing the quiescence check, got %d witness(es)", interleave, len(got))
		}
		if !strings.Contains(got[0].Detail, "fork 1,") {
			t.Errorf("interleave=%t: the witness names the wrong fork: %s", interleave, got[0].Detail)
		}
	}
}

// TestForkParity_DetectsAnAsymmetricLoad: the template loaded and cold
// environment 0 did not -- the same source loading differently in two
// fresh environments -- must be a witness carrying the failure, with
// environment 1 still compared.  A template that does not load stays a
// returned error: there is nothing to compare.
func TestForkParity_DetectsAnAsymmetricLoad(t *testing.T) {
	t.Parallel()
	failingOn := func(k int) func() (*lisp.LEnv, error) {
		calls := 0
		return func() (*lisp.LEnv, error) {
			calls++
			if calls == k {
				return nil, fmt.Errorf("staged failure on environment build %d", k)
			}
			return newFuzzEnv()
		}
	}
	got, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  failingOn(2),
		Program: parityAliasProgram,
		Tx:      [][]string{{`(assoc! a "y" 7) (get b "y")`}, {`(get a "k")`}},
	})
	if err != nil {
		t.Fatalf("a cold environment that fails to build must be a witness, not a harness error: %v", err)
	}
	for _, w := range got {
		t.Logf("%s", w)
	}
	if len(got) != 1 || !strings.Contains(got[0].Property, "loads on a cold environment exactly when") ||
		!strings.Contains(got[0].Detail, "cold environment 0 did not: new environment: staged failure on environment build 2") {
		t.Fatalf("want exactly one asymmetric-load witness naming cold environment 0 and carrying the error, got %d witness(es)", len(got))
	}
	if _, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  failingOn(1),
		Program: parityAliasProgram,
		Tx:      [][]string{{`a`}},
	}); err == nil || !strings.Contains(err.Error(), "template: new environment: staged failure") {
		t.Fatalf("a template that does not build must remain a harness error, got %v", err)
	}
}

// errStagedBuild is the failure the load-asymmetry controls stage.
var errStagedBuild = errors.New("staged failure on environment build")

// TestForkParity_DetectsARaiseAsymmetry: a fork on which `(get b "y")`
// raises where the cold load returns is reported under ParityPropertyRaises,
// not ParityPropertyReturns -- the split that lets the #579 revert be
// pinned by a property no other historical mutation emits.  Deleting the
// raise branch in CheckParity demotes the witness to the value property and
// turns this red.
func TestForkParity_DetectsARaiseAsymmetry(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckParity(elpstest.ParityCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      [][]string{{`(get b "k")`}},
		Fork:    brokenForkRevokes,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Logf("%s", w)
	}
	raises, returns := false, false
	for _, w := range got {
		switch w.Property {
		case elpstest.ParityPropertyRaises:
			raises = true
		case elpstest.ParityPropertyReturns:
			returns = true
		}
	}
	if !raises || returns {
		t.Fatalf("a fork that raises where the cold load returns: raise witness=%t, value witness=%t; want the raise property alone", raises, returns)
	}
}

// ---------------------------------------------------------------------------
// A transaction whose observation is a NATIVE.
//
// Parity is the backstop: it runs the program, so it sees divergences no
// structural channel can.  It has one blind spot of its own, and it is the
// shape an embedder meets first -- substrate reaches its own state through
// native handles.  A native is rendered by its Go TYPE in the result
// comparison (renderResult, forkcheck.go) and by type plus an identity
// ordinal in the state fingerprint, so a transaction returning a handle
// that holds 41 on the fork arm and 0 on the cold arm renders identically
// on both and NOTHING fires.
//
// ParityCheck.RenderNative is the opt-in that closes it.  Both halves are
// asserted below, because the first alone would pass on an oracle that had
// simply become permissive and the second alone would not say what the
// default costs.
// ---------------------------------------------------------------------------

// nativeLedger is an embedder payload two forks share, in the ordinary way:
// it declares nothing, so Fork hands every fork the same one (docs/fork.md).
type nativeLedger map[string]int

// nativeHandle is what a transaction OBSERVES: a by-value snapshot of the
// ledger, which is exactly the shape whose contents no channel compares.
type nativeHandle struct{ n int }

func nativeObservationEnv() (*lisp.LEnv, error) {
	env, err := elpstest.NewForkCheckEnv()
	if err != nil {
		return nil, err
	}
	ledger := nativeLedger{"n": 0}
	if rc := env.PutGlobal(lisp.Symbol("led"), lisp.Native(ledger)); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	env.AddBuiltins(true,
		elpsutil.Function("ledger-set", lisp.Formals("l", "v"),
			func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				if l, ok := args.Cells[0].Native.(nativeLedger); ok {
					l["n"] = args.Cells[1].Int
				}
				return lisp.Nil()
			}),
		elpsutil.Function("ledger-peek", lisp.Formals("l"),
			func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				l, _ := args.Cells[0].Native.(nativeLedger)
				return lisp.Native(nativeHandle{n: l["n"]})
			}))
	return env, nil
}

// nativeObservationCheck is the shared shape: environment 0 writes 41
// through the shared ledger and observes it, environment 1 only observes.
// On a cold load environment 1 reads 0; on a fork of a template whose
// ledger is shared it reads 41.
func nativeObservationCheck() elpstest.ParityCheck {
	return elpstest.ParityCheck{
		NewEnv:  nativeObservationEnv,
		Program: `(set 'probe (list led))`,
		Tx: [][]string{
			{`(ledger-set led 41)`, `(ledger-peek led)`},
			{`(ledger-peek led)`},
		},
		Repro: "an observation that is a native",
	}
}

func TestForkParity_ANativeObservationNeedsARenderer(t *testing.T) {
	t.Parallel()
	// Ground truth, so neither half of this control can pass vacuously:
	// the two arms really do observe different numbers.
	c := nativeObservationCheck()
	cold, err := c.NewEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := cold.LoadString("p.lisp", c.Program); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	fork, err := cold.Fork()
	if err != nil {
		t.Fatal(err)
	}
	if rc := cold.LoadString("w.lisp", `(ledger-set led 41)`); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	seen := fork.LoadString("o.lisp", `(ledger-peek led)`)
	if h, ok := seen.Native.(nativeHandle); !ok || h.n != 41 {
		t.Fatalf("premise: the fork does not observe the template's write through the shared "+
			"ledger (%v); this control is not exercising the shape it describes", seen)
	}

	// Half one: with no renderer the divergence is invisible.  This is a
	// STATEMENT OF THE DEFAULT, not an aspiration -- if it ever starts
	// firing, delete this half and say so, do not weaken the other one.
	got, err := elpstest.CheckParity(nativeObservationCheck())
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if len(got) != 0 {
		t.Errorf("parity reported %d witness(es) for a native-only divergence with no "+
			"RenderNative set. The default has changed: either a canonical rendering was given to "+
			"every payload (which only its owner can write) or the comparison has become noisy.\n%v",
			len(got), got)
	}

	// Half two: the hook catches it, in the result channel.
	withRenderer := nativeObservationCheck()
	withRenderer.RenderNative = func(payload any) string {
		if h, ok := payload.(nativeHandle); ok {
			return fmt.Sprintf("handle(n=%d)", h.n)
		}
		return fmt.Sprintf("%T", payload)
	}
	got, err = elpstest.CheckParity(withRenderer)
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("with RenderNative set, parity STILL reports nothing for a transaction that " +
			"observes 41 on the fork arm and 0 on the cold arm. The hook is not reaching the " +
			"comparison it exists for.")
	}
	var sawResult bool
	for _, w := range got {
		if w.Property == elpstest.ParityPropertyReturns {
			sawResult = true
		}
	}
	if !sawResult {
		t.Errorf("the witnesses do not include a RESULT divergence, which is where the transaction's "+
			"observation lives:\n%v", got)
	}
}

// The state channel takes the renderer too, which is the half a
// result-only threading would silently drop: a payload the transaction
// mutated but did not return is post-run STATE.
func TestForkParity_TheRendererReachesTheStateChannel(t *testing.T) {
	t.Parallel()
	// A fork that writes through the shared ledger without any transaction
	// returning it: the divergence is confined to reachable state.
	forkWrites := func(env *lisp.LEnv) (*lisp.LEnv, error) {
		f, err := env.Fork()
		if err != nil {
			return nil, err
		}
		if l, ok := f.GetGlobal(lisp.Symbol("led")).Native.(nativeLedger); ok {
			l["n"] = 41
		}
		return f, nil
	}
	c := elpstest.ParityCheck{
		NewEnv:  nativeObservationEnv,
		Program: `(set 'probe (list led))`,
		Tx:      [][]string{{`(length probe)`}},
		Fork:    forkWrites,
		Repro:   "a divergence confined to a native's contents",
	}
	if got, err := elpstest.CheckParity(c); err != nil {
		t.Fatalf("harness error: %v", err)
	} else if len(got) != 0 {
		t.Errorf("with no renderer, a divergence confined to a native's contents produced %d "+
			"witness(es); the default is supposed to compare the header only:\n%v", len(got), got)
	}
	c.RenderNative = func(payload any) string {
		if l, ok := payload.(nativeLedger); ok {
			return fmt.Sprintf("ledger(n=%d)", l["n"])
		}
		return fmt.Sprintf("%T", payload)
	}
	got, err := elpstest.CheckParity(c)
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	var sawState bool
	for _, w := range got {
		if w.Property == elpstest.ParityPropertyState {
			sawState = true
		}
	}
	if !sawState {
		t.Errorf("RenderNative did not reach the post-run STATE fingerprint: a fork whose shared "+
			"payload holds 41 where the cold arm's holds 0 produced no state witness.\n%v", got)
	}
}
