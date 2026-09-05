// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
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
