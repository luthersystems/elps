// Copyright © 2026 The ELPS authors

//go:build !elpscheck

// The over-sharing control for the parity oracle.  Excluded under `-tags
// elpscheck` for the reason aliasguard_templatefork_test.go gives: the
// defect it models -- a fork holding a value the template owns -- is an
// ownership violation, and the elpscheck checker panics with "LVal used by
// two Runtimes" before the oracle can report.  The control earns its place
// in the ordinary build an embedder ships, where nothing else refuses this
// class.

package elpstest_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
)

// brokenForkSharesABinding forks correctly, then re-points the fork's
// binding for `a` at the TEMPLATE's value.  Every fork so taken holds one
// map, so a write on one fork is a read on the next -- the leak the whole
// guard exists to catch, reproduced from outside Fork.
func brokenForkSharesABinding(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	sym := lisp.Symbol("a")
	v := env.GetGlobal(sym)
	if v.Type == lisp.LError {
		return nil, lisp.GoError(v)
	}
	if rc := f.PutGlobal(sym, v); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return f, nil
}

// TestForkParity_DetectsASharingFork: environment 0 writes through `a`,
// environment 1 reads it.  On cold environments the read sees nothing; on
// forks over one shared map it sees the write, so environment 1's RESULT
// diverges and so does its post-run state -- isolation failing, reported
// as a parity failure, which is the claim that isolation is a consequence.
// Run under both schedules, since a leak between forks is exactly the
// class whose visibility depends on ordering.
func TestForkParity_DetectsASharingFork(t *testing.T) {
	t.Parallel()
	for _, interleave := range []bool{false, true} {
		got, err := elpstest.CheckParity(elpstest.ParityCheck{
			NewEnv:     newFuzzEnv,
			Program:    parityAliasProgram,
			Tx:         [][]string{{`(assoc! a "y" 7) (get a "y")`}, {`(get a "y")`}},
			Interleave: interleave,
			Fork:       brokenForkSharesABinding,
		})
		if err != nil {
			t.Fatalf("interleave=%t: harness error: %v", interleave, err)
		}
		result, state := parityWitnessKinds(got)
		for _, w := range got {
			t.Logf("interleave=%t: %s", interleave, w)
		}
		if !result || !state {
			t.Fatalf("interleave=%t: a sharing fork produced result witness=%t, state witness=%t; the parity oracle has been weakened",
				interleave, result, state)
		}
	}
}

// TestForkParity_SequentialForksAreTakenLazily pins the schedule
// semantics ParityCheck.Interleave documents: off, fork i is taken after
// forks 0..i-1 ran their whole sequences; on, every fork is taken before
// any transaction runs.  Observed through a sharing fork: each fork holds
// the template's `shared` map, every transaction increments its "n", and
// the fork walker records the count it saw at each fork call.  Two
// environments of two transactions each therefore see [0 2] lazily and
// [0 0] eagerly.
func TestForkParity_SequentialForksAreTakenLazily(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		interleave bool
		want       string
	}{{false, "[0 2]"}, {true, "[0 0]"}} {
		var seen []int
		fork := func(env *lisp.LEnv) (*lisp.LEnv, error) {
			f, err := env.Fork()
			if err != nil {
				return nil, err
			}
			sym := lisp.Symbol("shared")
			v := env.GetGlobal(sym)
			n, ok := v.Map().Get(lisp.String("n"))
			if !ok {
				return nil, lisp.GoError(env.Errorf("no n"))
			}
			seen = append(seen, n.Int)
			if rc := f.PutGlobal(sym, v); rc.Type == lisp.LError {
				return nil, lisp.GoError(rc)
			}
			return f, nil
		}
		tick := `(assoc! shared "n" (+ 1 (get shared "n")))`
		if _, err := elpstest.CheckParity(elpstest.ParityCheck{
			NewEnv:     newFuzzEnv,
			Program:    `(set 'shared (sorted-map "n" 0))`,
			Tx:         [][]string{{tick, tick}, {tick, tick}},
			Interleave: tc.interleave,
			Fork:       fork,
		}); err != nil {
			t.Fatalf("interleave=%t: harness error: %v", tc.interleave, err)
		}
		if got := fmt.Sprint(seen); got != tc.want {
			t.Errorf("interleave=%t: forks were taken when the shared count read %s, want %s", tc.interleave, got, tc.want)
		}
	}
}
