// Copyright © 2026 The ELPS authors

package elpstest

// Parity inside the alias guard.
//
// The owner's definition of the fork contract -- "for two VMs that get
// instantiated from the same source code, ELPS code should work identically
// to two forks from the same template" -- is the property every direction in
// aliasguard_isolation.go's matrix is a consequence of.  Until this file it
// lived only in parity.go (CheckParity) and in the older RunForkCheck, and
// the guard's headline oracle knew nothing of it.  That split is why the
// #579 revert (credential 6ef3da5, a schema validator revoked across a fork
// by header identity) could only be pinned by a TEST name in
// scripts/mutation-proof.sh: the guard's channels observe payload-pointer
// sharing, location bleed and isolation fingerprints, none of which sees a
// value that is wrong only when RUN.
//
// So CheckTransactions carries parity as a channel, over its own Tx set:
// transaction i on fork i against transaction i on cold environment i, the
// same substituted walker, one hop, sequential with forks taken lazily.
// The sweep already takes every fork eagerly before any transaction runs;
// lazily taken forks are the other shape, and the fork -> template -> later
// fork direction observed the way an embedder's loop observes it.  Every
// existing caller gains the channel with no call-site change, the way
// property 5 was added.
//
// WHICH WALKERS.  Fork only.  `copy`, Detach and the macro stamp rebuild a
// VALUE inside one environment; no transaction runs "on" their output, so
// there is no cold counterpart to compare against.  Their analogue of
// parity -- a write through the copy is seen exactly where a write through
// a value built from the same source is seen -- is the mutation-probe
// property CheckWalker already holds them to.  That sentence is a
// DEFINITION of what parity means for a value walker, not a claim about
// the code, so no test backs it; it is stated here so the scope is a
// decision and not an omission.  CheckWalker's Fork arm needs nothing
// either: with no transactions parity collapses to "a fresh fork is the
// cold load", and the cold load IS the template, which CheckForkTemplate
// asserts.
//
// WHY BEFORE THE SWEEP.  The sweep treats a transaction that raises as a
// harness error, since it cannot tell a raising transaction from a broken
// one.  Parity can: a transaction that raises on the fork and returns on
// the cold load is ParityPropertyRaises -- exactly #579's signature -- and
// once it is witnessed the sweep would only abort on the same transaction.
// CheckTransactions therefore runs this channel first and returns what it
// has when a raise asymmetry is found (TestTransactionCheckSurvivesAForkThatRaises).
//
// WHY THE TEMPLATE ERROR IS A WITNESS HERE.  CheckParity returns an error
// only when its template does not load.  CheckTransactions has already
// loaded the same program into its own template, so a second fresh
// environment refusing it is the load asymmetry ParityPropertyLoads names,
// not a harness failure (TestTransactionCheckReportsAParityLoadAsymmetry).

// The channel builds one template plus one cold environment per
// transaction.  The build-count tests in aliasguard_broken_test.go add that
// to their expectations through parityEnvBuilds in
// aliasguard_parity_test.go, which
// TestParityBuildsOneTemplatePlusOneColdPerTransaction pins against
// CheckParity.

// transactionParityWitnesses runs the parity channel over c and reports its
// witnesses, plus whether any of them is a raise asymmetry -- the case in
// which the sweep that follows cannot run.
func transactionParityWitnesses(c TransactionCheck) (out []Witness, raised bool) {
	seqs := make([][]string, len(c.Tx))
	for i, tx := range c.Tx {
		seqs[i] = []string{tx}
	}
	ws, err := CheckParity(ParityCheck{
		NewEnv:  c.NewEnv,
		Program: c.Program,
		Tx:      seqs,
		Fork:    c.Fork,
		Repro:   c.Repro,
	})
	if err != nil {
		return []Witness{{
			Walker:   "Fork",
			Property: ParityPropertyLoads,
			Detail: "the template CheckTransactions built loaded this program; the parity channel's did not: " +
				err.Error() + "\n    (the same source loaded differently in two fresh environments)",
			Repro: c.Repro,
		}}, false
	}
	for _, w := range ws {
		if w.Property == ParityPropertyRaises {
			raised = true
		}
	}
	return ws, raised
}
