// Copyright © 2026 The ELPS authors

package elpstest

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

// Fork parity: the property the rest of this guard is a consequence of.
//
// In the owner's words: "For two VMs that get instantiated from the same
// source code, ELPS code should work identically to two forks from the same
// template.  Fork was meant to be a performance optimisation to speed that
// up.  All this machinery is to guarantee and check when that's not the
// case."
//
// Stated as a check: for a program P and per-environment transaction
// sequences T_1..T_n, running T_i on fork i of ONE template that loaded P
// must give byte-identical per-transaction results, and byte-identical
// post-run reachable state, to running T_i on cold environment i that
// loaded P itself.  Isolation (a fork cannot see another fork) and
// fidelity (a fork shares exactly what a fresh load would share) are
// consequences: a fork that leaked into another would make that other
// fork's results diverge from its cold twin, and a fork that de-aliased or
// over-shared would leave state a cold load does not.
//
// Two oracles already stated halves of this.  RunForkCheck has a cold arm,
// but runs ONE transaction per fork over hand-written programs.
// CheckTransactions runs a generated program over n forks, but compares
// forks against the TEMPLATE, never against a cold load, and compares
// structure rather than execution results.  CheckParity is the two joined:
// n cold environments against n forks, per-transaction sequences, both
// results and state, under the same generator that feeds FuzzAliasGuard.
//
// What the structural oracles cannot see, and this one can, is a value
// whose identity is not a payload pointer.  Two list headers over ONE
// backing array -- `(set 'tail (cdr l))` is a view that shares its elements
// with l (docs/func.md, "Slices are views, not copies") -- have no shared
// pointer the fingerprint can key on: it memoises on the *LVal, and each
// header gets its own cells copy in the fork.  Sorting l in place then
// reorders tail's elements on a cold environment and not on a fork.  That
// is TestForkParity_ViewSortGapStillOpen, measured red on commit 74e4ac8,
// and it is invisible to every fingerprint comparison because the
// fingerprints of the cold and forked graphs are equal BEFORE the sort.
// Only running the program tells them apart.
//
// What is compared, and how.  A result is rendered by renderResult (type,
// value and captured environment, error text normalised of environment
// numbers).  Post-run state is FingerprintEnv under templateOpts -- the
// same options CheckForkTemplate compares a fork against its template
// with -- so sharing, seal bits and the package metadata tables are all in
// the comparison.  A cold environment and a fork number their environments
// on independent counters, and the fingerprint already normalises the one
// place that number reaches a token (funIDPattern).

// ParityCheck describes one run of the parity oracle.
type ParityCheck struct {
	// NewEnv builds an environment: once for the template, once per cold
	// arm.  Nil means NewForkCheckEnv.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into the template and into every cold environment.
	Program string
	// Tx[i] is the transaction sequence environment i runs, in order.
	// There are len(Tx) forks and len(Tx) cold environments.
	Tx [][]string
	// Interleave runs the sequences round-robin -- every environment's
	// first transaction, then every second -- with all forks taken before
	// any transaction runs, so every fork is live while every other one
	// writes.  Off, each fork runs its whole sequence before the next fork
	// is TAKEN: fork i is created after forks 0..i-1 finished, which is
	// the fork -> template -> later-fork direction observed the way an
	// embedder's transaction loop would observe it.
	Interleave bool
	// Hops is how many Fork calls separate a fork from the template: 1
	// (the default) or 2, a fork of a fork.  The two-hop arm exists for the
	// reason RunForkCheck gives: on a shared libtesting suite it was once
	// the only arm that noticed (commit d26953a,
	// TestForkCheck_TestingSuitePerFork).
	Hops int
	// ForkOptions are passed to every Fork call.
	ForkOptions []lisp.ForkOption
	// Fork substitutes the fork walker.  Nil means (*lisp.LEnv).Fork with
	// ForkOptions.  It exists so a deliberately broken fork can be driven
	// through this oracle to prove it is not vacuous
	// (TestForkParity_DetectsASharingFork, TestForkParity_DetectsADealiasingFork).
	Fork func(*lisp.LEnv) (*lisp.LEnv, error)
	// Repro is attached to every witness.
	Repro string
}

func (c ParityCheck) fork(env *lisp.LEnv) (*lisp.LEnv, error) {
	if c.Fork != nil {
		return c.Fork(env)
	}
	return env.Fork(c.ForkOptions...)
}

// parityStep is one transaction in the schedule: environment i's j-th.
type parityStep struct{ i, j int }

// paritySchedule orders the transactions.  Sequential: every transaction
// of environment 0, then of environment 1, and so on.  Interleaved: round
// robin, skipping an environment whose sequence has run out.
func paritySchedule(tx [][]string, interleave bool) []parityStep {
	var steps []parityStep
	if !interleave {
		for i, seq := range tx {
			for j := range seq {
				steps = append(steps, parityStep{i, j})
			}
		}
		return steps
	}
	longest := 0
	for _, seq := range tx {
		if len(seq) > longest {
			longest = len(seq)
		}
	}
	for j := range longest {
		for i, seq := range tx {
			if j < len(seq) {
				steps = append(steps, parityStep{i, j})
			}
		}
	}
	return steps
}

// RunParityCheck runs CheckParity and reports each witness.
func RunParityCheck(t TestingTB, c ParityCheck) {
	t.Helper()
	got, err := CheckParity(c)
	if err != nil {
		t.Fatalf("parity: %v", err)
		return
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// CheckParity runs the transaction sequences on forks and on cold
// environments and returns one witness per divergence: a transaction whose
// result differs, or an environment whose post-run reachable state
// differs.  A transaction that raises is a result like any other -- the
// cold arm defines what a fork must do, raising included -- so an error
// value is compared, not reported.  An error building an environment,
// loading the program or taking a fork is returned, since nothing after
// it would mean anything.
func CheckParity(c ParityCheck) ([]Witness, error) {
	if len(c.Tx) == 0 {
		return nil, errors.New("no transaction sequences: parity would hold vacuously")
	}
	hops := c.Hops
	switch hops {
	case 0:
		hops = 1
	case 1, 2:
	default:
		return nil, fmt.Errorf("hops must be 1 or 2, not %d", hops)
	}
	newEnv := c.NewEnv
	if newEnv == nil {
		newEnv = NewForkCheckEnv
	}
	build := func() (*lisp.LEnv, error) {
		env, err := newEnv()
		if err != nil {
			return nil, err
		}
		if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
		return env, nil
	}
	takeFork := func(tmpl *lisp.LEnv, i int) (*lisp.LEnv, error) {
		f := tmpl
		for h := range hops {
			var err error
			if f, err = c.fork(f); err != nil {
				return nil, fmt.Errorf("fork %d hop %d: %w", i, h+1, err)
			}
		}
		return f, nil
	}

	tmpl, err := build()
	if err != nil {
		return nil, fmt.Errorf("template: %w", err)
	}
	n := len(c.Tx)
	cold := make([]*lisp.LEnv, n)
	for i := range cold {
		if cold[i], err = build(); err != nil {
			return nil, fmt.Errorf("cold %d: %w", i, err)
		}
	}
	forks := make([]*lisp.LEnv, n)
	if c.Interleave {
		// Every fork live before anything runs.
		for i := range forks {
			if forks[i], err = takeFork(tmpl, i); err != nil {
				return nil, err
			}
		}
	}

	var out []Witness
	for _, st := range paritySchedule(c.Tx, c.Interleave) {
		if forks[st.i] == nil {
			// Sequential: taken when first needed, after every earlier
			// fork has run its whole sequence.
			if forks[st.i], err = takeFork(tmpl, st.i); err != nil {
				return nil, err
			}
		}
		tx := c.Tx[st.i][st.j]
		// The same file name on both arms: it reaches the fingerprint as a
		// source location on every value the transaction creates.
		name := fmt.Sprintf("env%d-tx%d.lisp", st.i, st.j)
		want := renderResult(cold[st.i].LoadString(name, tx))
		got := renderResult(forks[st.i].LoadString(name, tx))
		if want != got {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a transaction on a fork returns what it returns on a cold load of the same program",
				Detail: fmt.Sprintf("environment %d, transaction %d: %s\n    cold: %s\n    fork: %s",
					st.i, st.j, tx, clip(want), clip(got)),
				Repro: c.Repro,
			})
		}
	}
	for i := range n {
		if forks[i] == nil {
			// An empty sequence: the fork was never taken.  Take it now, so
			// the state comparison still covers it.
			if forks[i], err = takeFork(tmpl, i); err != nil {
				return nil, err
			}
		}
		want := FingerprintEnv(cold[i], templateOpts)
		got := FingerprintEnv(forks[i], templateOpts)
		if want.Equal(got) {
			continue
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "a fork's reachable state after its transactions is the cold load's",
			Detail:   fmt.Sprintf("environment %d\n%s", i, want.Diff(got)),
			Leak:     firstDivergentPath(want, got),
			Repro:    c.Repro,
		})
	}
	return out, nil
}
