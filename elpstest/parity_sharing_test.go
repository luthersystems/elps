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
