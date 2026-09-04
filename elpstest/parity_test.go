// Copyright © 2026 The ELPS authors

package elpstest_test

import (
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
