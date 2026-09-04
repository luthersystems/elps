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
		// ONE transaction on purpose. Every fork this walker makes shares
		// one map, so two of them writing it in the concurrent arm is a
		// real data race -- the control's own doing, not a finding about
		// elps, and `-race` reports it as such. One fork is all this
		// property needs.
		Tx:    []string{`(assoc! shared "k" 2)`},
		Fork:  brokenForkSharesTemplatePayload,
		Repro: "a fork that shares a payload with its template",
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
