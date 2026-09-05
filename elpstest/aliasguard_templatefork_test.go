// Copyright © 2026 The ELPS authors

//go:build !elpscheck

// The negative control for the template -> fork isolation direction, in its
// own build-tagged file.
//
// It is excluded under `-tags elpscheck` because the defect it models --
// a fork holding a payload its template also holds -- is ALSO an ownership
// violation, and the elpscheck ownership checker panics on it before the
// isolation property can report anything:
//
//	panic: ownership violation: LVal used by two Runtimes
//
// That is worth stating plainly rather than working around, because it is
// a second line of defence and it bounds what this property adds. Both
// shapes were tried: handing the fork the template's own *LVal, and
// binding a FORK-OWNED header over the template's *MapData. Both panic,
// the second because the values inside the shared map are still the
// template's. So under elpscheck, elps already refuses this class.
//
// The property still earns its place: ownership checking is opt-in and
// build-tag-gated, exactly like RuntimeBound, so in an ordinary build --
// which is what an embedder ships -- nothing else asserts that a write
// through the template cannot reach a live fork.
package elpstest_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
)

// ---------------------------------------------------------------------------
// Control 11: a fork that SHARES a payload with its template.
//
// This is the negative control for the template -> fork direction, the
// fourth isolation direction and the one that went unasserted until now.
// Everything else drives writes FROM a fork; nothing wrote to the TEMPLATE
// and re-checked the forks that were already live.
//
// The asymmetry is structural. If a fork shares a payload with its
// template -- the #576 and #585 defect -- then a write through the
// template lands in the fork. Two ends of one pointer; only one end was
// covered.
// ---------------------------------------------------------------------------

// brokenForkSharesTemplatePayload forks properly and then binds a
// FORK-OWNED header whose payload is the TEMPLATE's `*MapData`.
//
// Two headers over one payload is the #576/#585 shape exactly, and it is
// the shape that matters here: handing the fork the template's own *LVal
// instead is a different defect, one elps already detects — under
// `-tags elpscheck` the ownership checker panics with "LVal used by two
// Runtimes". The first version of this control did that and was rejected
// by that checker, correctly. Sharing the payload beneath a fork-owned
// header is the version that is legal at the runtime level and still
// wrong at the isolation level, which is the gap this property covers.
func brokenForkSharesTemplatePayload(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	v := env.Get(lisp.Symbol("shared"))
	if v == nil || v.Type == lisp.LError {
		return nil, fmt.Errorf("the template does not bind `shared`: %v", v)
	}
	leaky := lisp.SortedMap()
	leaky.Native = v.Native // THE DEFECT: the template's payload.
	if rc := f.PutGlobal(lisp.Symbol("shared"), leaky); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return f, nil
}

func TestGuardDetectsATemplateWriteReachingAFork(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		Program: templateSharedProgram,
		// Two transactions, so the property is checked over more than one
		// fork. That is safe only because this check DECLARES that its
		// walker shares: two forks over one map, written in parallel, is a
		// data race by construction. Before the declaration existed this
		// had to be a single transaction.
		Tx:                []string{`(assoc! shared "k" 2)`, `(assoc! shared "k" 3)`},
		Fork:              brokenForkSharesTemplatePayload,
		SkipConcurrentArm: true,
		Repro:             "a fork that shares a payload with its template",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	const want = "a transaction on the template is invisible to every existing fork"
	var found *elpstest.Witness
	for i := range got {
		if got[i].Property == want {
			found = &got[i]
			break
		}
	}
	if found == nil {
		t.Fatalf("a fork sharing a payload with its template was NOT reported by the\n"+
			"template-to-fork property.\nThat direction is unasserted again: a write THROUGH the\n"+
			"template reaches a live fork and nothing says so.\nwitnesses: %v", got)
	}
	if !strings.Contains(found.Detail, "moved fork") {
		t.Errorf("the witness does not name which fork moved:\n%s", found)
	}
	if found.Leak == "" {
		t.Errorf("the witness carries no diverging path:\n%s", found)
	}
	t.Logf("detected:\n%s", found)
}

// ---------------------------------------------------------------------------
// Control 12: a fork that shares a payload with its template ONLY on the
// PRISTINE-SUCCESSOR fork.
//
// The successor is the fork property 3 takes after every transaction has
// run. It is still LIVE when property 5 writes to the template, so it
// belongs in the set property 5 sweeps -- and for one revision it was not
// in that set, while the property's Baseline string already promised
// "every live fork holds the value it held before the template was written
// to". A walker that over-shares only there produced ZERO witnesses.
//
// This control pins the successor into the set. It is deliberately
// narrower than control 11: every ordinary fork is faithful, so the ONLY
// thing that can report it is property 5 looking at the successor.
// ---------------------------------------------------------------------------

// brokenForkSharesOnlyWithTheSuccessor is faithful for the first nTx forks
// and shares the template's payload on every fork after them.
//
// CheckTransactions takes exactly len(Tx) forks before running the
// transactions and then ONE more for the pristine-successor property, so
// call nTx+1 is that successor.
//
// The counter needs no lock: CheckTransactions skips the concurrent arm
// whenever a fork walker is substituted, so every call here is sequential.
func brokenForkSharesOnlyWithTheSuccessor(nTx int) func(*lisp.LEnv) (*lisp.LEnv, error) {
	calls := 0
	return func(env *lisp.LEnv) (*lisp.LEnv, error) {
		calls++
		if calls <= nTx {
			return env.Fork()
		}
		return brokenForkSharesTemplatePayload(env)
	}
}

func TestGuardDetectsATemplateWriteReachingTheSuccessorFork(t *testing.T) {
	t.Parallel()
	tx := []string{`(assoc! shared "k" 2)`}
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		Program: templateSharedProgram,
		Tx:      tx,
		Fork:    brokenForkSharesOnlyWithTheSuccessor(len(tx)),
		// This walker shares on purpose, so it declares it: the counter it
		// closes over is unsynchronised, and its successor fork shares the
		// template's map.
		SkipConcurrentArm: true,
		Repro:             "a fork that shares with its template only on the successor",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	const want = "a transaction on the template is invisible to every existing fork"
	var found *elpstest.Witness
	for i := range got {
		if got[i].Property == want {
			found = &got[i]
			break
		}
	}
	if found == nil {
		t.Fatalf("a walker that over-shares ONLY on the pristine-successor fork was NOT\n"+
			"reported. The successor is live when property 5 writes to the template, so it\n"+
			"must be in the set that property sweeps -- otherwise the property's own\n"+
			"Baseline (\"every live fork\") is false. Drop the successor from the live set in\n"+
			"CheckTransactions and this is the failure you get.\nwitnesses: %v", got)
	}
	if !strings.Contains(found.Observed, "successor") {
		t.Errorf("the witness does not name the successor fork as the one that moved;\n"+
			"an index into forks[] cannot name it, which is why the live set carries names:\n%s", found)
	}
	t.Logf("detected:\n%s", found)
}
