// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
)

// The parity channel of CheckTransactions (aliasguard_parity.go): the
// #579 shape as a guard test, the controls that prove the channel is wired,
// and the pin on how many environments it builds.

// parityEnvBuilds is how many environments the parity channel builds for a
// transaction set: one template plus one cold environment per transaction.
// The build-count tests in aliasguard_broken_test.go add it to their
// expectations; TestParityBuildsOneTemplatePlusOneColdPerTransaction pins
// it against CheckParity.
func parityEnvBuilds(tx []string) int {
	return 1 + len(tx)
}

func TestParityBuildsOneTemplatePlusOneColdPerTransaction(t *testing.T) {
	t.Parallel()
	for _, tx := range [][]string{{`a`}, {`a`, `b`, `(assoc! a "x" 1)`}} {
		builds := 0
		seqs := make([][]string, len(tx))
		for i := range tx {
			seqs[i] = []string{tx[i]}
		}
		if _, err := elpstest.CheckParity(elpstest.ParityCheck{
			NewEnv:  countingEnvBuilds(&builds),
			Program: parityAliasProgram,
			Tx:      seqs,
		}); err != nil {
			t.Fatalf("harness error: %v", err)
		}
		if builds != parityEnvBuilds(tx) {
			t.Errorf("%d transaction(s): CheckParity built %d environments, parityEnvBuilds says %d", len(tx), builds, parityEnvBuilds(tx))
		}
	}
}

// TestTransactionIsolation_SchemaValidatorCredential is issue #579 (fix
// 6ef3da5) as a GUARD test rather than only a RunForkCheck one.  A
// validator minted on the template must still be a validator on a fork; on
// the reverted fix `(s:validate T 3)` raises "Value is not a schema
// constraint" on the fork and returns on the cold load, which the parity
// channel reports as ParityPropertyRaises -- the needle
// scripts/mutation-proof.sh's 579 row can pin as a property instead of a
// test name.
//
// Two things here are deliberately not what TestForkCheck_SchemaValidatorCredential
// does, and nothing else is weaker than it (same program, same default
// environment, the other four transactions verbatim):
//
//   - The always-raising `(s:validate T "nope")` is absent: the sweep's
//     contract forbids a transaction that raises on both arms.  It stays
//     in the RunForkCheck test, which keeps covering it.
//   - ExpectNoSharedNatives is NOT set.  This is a documented exemption,
//     not a weakening: every validator's marker cell holds one process-wide
//     *validatorTag, a zero-size stateless credential that 6ef3da5's own
//     comment says is "shared process-wide because it carries no state
//     whatsoever", and Fork's documented default for a payload that
//     declares no protocol is to share it.  Setting the flag would report
//     that design as a finding.  A payload that DECLARES NativeCloner or
//     RuntimeBound and is shared anyway is a witness regardless of the
//     flag.
func TestTransactionIsolation_SchemaValidatorCredential(t *testing.T) {
	t.Parallel()
	elpstest.RunTransactionCheck(t, elpstest.TransactionCheck{
		Program: `
(s:deftype "T" s:int)
(set 'anon (s:make-validator "Anon" s:int (s:gt 1)))
`,
		Tx: []string{
			`(s:validate T 3)`,
			`(s:validate anon 3)`,
			`(s:deftype "U" s:string) (s:validate U "x")`,
			`(s:validate (s:make-validator "Fresh" s:string) "x")`,
		},
		// ExpectNoSharedNatives deliberately unset: see the exemption above.
	})
}

// brokenForkRevokes makes `b` an integer on the fork, so `(get b ...)`
// raises there and returns on a cold load: the #579 signature (an error
// where a fresh VM gives a value) reproduced from outside Fork.
func brokenForkRevokes(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	if rc := f.LoadString("revoke.lisp", `(set 'b 5)`); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return f, nil
}

func parityProperties(ws []elpstest.Witness) []string {
	var out []string
	for _, w := range ws {
		out = append(out, w.Property)
	}
	return out
}

// TestTransactionCheckCarriesParity: a de-aliasing fork through
// CheckTransactions -- the entry point every existing caller uses -- must
// report the parity value property.  Nothing else in the guard sees a
// de-aliased map through a transaction's RESULT; deleting the seam in
// CheckTransactions turns this red.
func TestTransactionCheckCarriesParity(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      []string{`(assoc! a "y" 7) (get b "y")`, `(assoc! a "z" 1)`},
		Fork:    brokenForkDealiases,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		t.Logf("%s", w)
	}
	if !strings.Contains(strings.Join(parityProperties(got), "\n"), elpstest.ParityPropertyReturns) {
		t.Fatalf("CheckTransactions did not carry the parity channel: no %q among %d witness(es)",
			elpstest.ParityPropertyReturns, len(got))
	}
}

// TestTransactionCheckSurvivesAForkThatRaises: a transaction that raises on
// the fork and not on the cold load used to be a HARNESS ERROR of the sweep
// ("transaction 0: ...") and so invisible as a finding.  It is a parity
// witness, and CheckTransactions returns it instead of the error.
func TestTransactionCheckSurvivesAForkThatRaises(t *testing.T) {
	t.Parallel()
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv:  newFuzzEnv,
		Program: parityAliasProgram,
		Tx:      []string{`(get b "k")`, `(assoc! a "z" 1)`},
		Fork:    brokenForkRevokes,
	})
	if err != nil {
		t.Fatalf("a fork that raises where a cold load returns must be a witness, not a harness error: %v", err)
	}
	for _, w := range got {
		t.Logf("%s", w)
	}
	props := strings.Join(parityProperties(got), "\n")
	if !strings.Contains(props, elpstest.ParityPropertyRaises) {
		t.Fatalf("no %q among %d witness(es)", elpstest.ParityPropertyRaises, len(got))
	}
}

// TestTransactionCheckReportsAParityLoadAsymmetry: CheckTransactions loads
// the template (build 1); the parity channel's template is build 2.  A
// NewEnv that fails there is the same source loading differently in two
// fresh environments, reported as ParityPropertyLoads rather than as an
// error -- and the sweep and concurrent arm still run afterwards.
func TestTransactionCheckReportsAParityLoadAsymmetry(t *testing.T) {
	t.Parallel()
	calls := 0
	got, err := elpstest.CheckTransactions(elpstest.TransactionCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			calls++
			if calls == 2 {
				return nil, errStagedBuild
			}
			return newFuzzEnv()
		},
		Program: parityAliasProgram,
		Tx:      []string{`(assoc! a "y" 7) (get b "y")`, `(assoc! a "z" 1)`},
	})
	if err != nil {
		t.Fatalf("a parity template that fails to build must be a witness, not a harness error: %v", err)
	}
	for _, w := range got {
		t.Logf("%s", w)
	}
	if len(got) != 1 || got[0].Property != elpstest.ParityPropertyLoads || !strings.Contains(got[0].Detail, errStagedBuild.Error()) {
		t.Fatalf("want exactly one %q witness carrying the build error, got %d witness(es)", elpstest.ParityPropertyLoads, len(got))
	}
	if calls < 3 {
		t.Errorf("the check stopped after the parity template failed (%d builds); the sweep and concurrent arm must still run", calls)
	}
}
