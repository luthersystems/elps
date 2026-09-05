// Copyright © 2026 The ELPS authors

package elpstest

import (
	"errors"
	"fmt"
	"regexp"
	"strings"

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
// pointer the fingerprint can key on: it memoises on the *LVal.  Until PR
// #602 each header got its own cells copy in the fork, so sorting l in
// place reordered tail's elements on a cold environment and not on a fork
// -- measured red on commit 74e4ac8, and invisible to every fingerprint
// comparison because the fingerprints of the cold and forked graphs are
// equal BEFORE the sort.  Only running the program tells them apart, and
// only this oracle runs it.  #602 (lisp commit b9153c3) closed the gap by
// recording the view where it is made; viewSortGapSeed in
// parity_fuzz_test.go is the control that keeps it closed.
//
// What is compared, and how.  A result is rendered by renderResult (type,
// value and captured environment, error text normalised of environment
// numbers).  Post-run state is FingerprintEnv under templateOpts -- the
// same options CheckForkTemplate compares a fork against its template
// with -- so sharing, seal bits and the package metadata tables are all in
// the comparison.  A cold environment and a fork number their environments
// on independent counters, and the fingerprint already normalises the one
// place that number reaches a token (funIDPattern); libschema's
// process-wide validator gensym is normalised here for the same reason
// (parityGensymPattern).

// The parity property strings.  They are constants because the fold into
// CheckTransactions (aliasguard_parity.go) classifies witnesses by them,
// and because scripts/mutation-proof.sh pins them as needles: a rename
// here is a manifest change there.
const (
	// ParityPropertyRaises is the RAISE asymmetry: exactly one arm raised.
	// It is a separate property from ParityPropertyReturns because it is a
	// different signature -- a de-aliased or over-shared payload gives a
	// different VALUE (issue #576, the template-share mutation), while a
	// credential revoked across a fork gives an ERROR where a cold load
	// gives a value (issue #579, fix 6ef3da5) -- and a needle that both
	// emit distinguishes neither.
	ParityPropertyRaises = "a transaction on a fork raises exactly when it raises on a cold load of the same program"
	// ParityPropertyReturns is a value divergence: neither arm raised, or
	// both raised with different text.
	ParityPropertyReturns = "a transaction on a fork returns what it returns on a cold load of the same program"
	// ParityPropertyState is a post-run reachable-state divergence.
	ParityPropertyState = "a fork's reachable state after its transactions is the cold load's"
	// ParityPropertyLoads is a program that loaded on the template and not
	// on a fresh environment.
	ParityPropertyLoads = "the program loads on a cold environment exactly when it loads on the template"
	// ParityPropertyForkable is a template that loaded and could not be
	// forked.
	ParityPropertyForkable = "a fork can be taken from any template that loaded"
)

// parityGensymPattern matches libschema's validator gensyms.  GenSymbol
// (lisp/lisplib/libschema) mints "_validation_fun_<n>" from a PROCESS-WIDE
// counter, so a validator defined inside a transaction is numbered by
// minting order: the cold arm and the fork arm -- and two cold loads --
// get different numbers for the same definition.  That is the same class
// the fingerprint already normalises for the per-Runtime "_fun<envID>"
// (funIDPattern), and no more a parity divergence than an environment ID
// is.  Normalised here rather than in fingerprint.go because only a
// comparison ACROSS independently numbered environments needs it: the
// template-level checks compare a fork against the template it was
// numbered from.  TestTransactionIsolation_SchemaValidatorCredential is
// the pin: it defines a validator in a transaction, and without this it
// reports the counter as a state divergence at user:U.
var parityGensymPattern = regexp.MustCompile(`_validation_fun_\d+`)

const parityGensymMarker = "_validation_fun_"

func parityNormalize(s string) string {
	if !strings.Contains(s, parityGensymMarker) {
		return s
	}
	return parityGensymPattern.ReplaceAllString(s, parityGensymMarker+"#")
}

// parityFingerprint is the post-run state fingerprint the two arms are
// compared under: FingerprintEnv under the template-level options, with
// process-wide gensyms normalised (parityGensymPattern).
func parityFingerprint(env *lisp.LEnv) *Fingerprint {
	fp := FingerprintEnv(env, templateOpts)
	for i, tok := range fp.tokens {
		fp.tokens[i] = parityNormalize(tok)
	}
	return fp
}

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
// result differs, an environment whose post-run reachable state differs, a
// cold environment that did not load the program the template loaded, or
// a fork that could not be taken.  A transaction that raises is a result
// like any other -- the cold arm defines what a fork must do, raising
// included -- so an error value is compared, not reported.
//
// THE ONLY HARNESS ERROR IS A TEMPLATE THAT DOES NOT LOAD.  Then there is
// nothing to compare.  Once the template has loaded, every later failure
// is on-property and is a witness, not an error: the same source loading
// on the template and not on a cold environment is nondeterministic
// loading, and "a fresh VM runs this program but a fork of it cannot even
// be created" is a parity violation under the definition at the top of
// this file.  Both used to be returned as errors, and the fuzz target
// turned every error into a skip, so a run reported "0 failures" over
// inputs it had silently not compared.  TestForkParity_DetectsAForkRefusal
// and TestForkParity_DetectsAnAsymmetricLoad are the controls.
//
// The run CONTINUES past such a failure: the environment that lost an arm
// has its transactions and its state comparison skipped -- it was already
// reported, and there is nothing to compare it against -- and every other
// environment is compared in full, so one run reports everything that
// diverges, the way RunForkCheck's t.Errorf loop does.
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
			return nil, fmt.Errorf("new environment: %w", err)
		}
		if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
			return nil, fmt.Errorf("load: %w", lisp.GoError(rc))
		}
		return env, nil
	}

	tmpl, err := build()
	if err != nil {
		return nil, fmt.Errorf("template: %w", err)
	}
	var out []Witness
	n := len(c.Tx)
	cold := make([]*lisp.LEnv, n)
	for i := range cold {
		if cold[i], err = build(); err != nil {
			cold[i] = nil
			out = append(out, Witness{
				Walker:   "Fork",
				Property: ParityPropertyLoads,
				Detail: fmt.Sprintf("the template loaded this program; cold environment %d did not: %v\n"+
					"    (the same source loaded differently in two fresh environments; environment %d is not compared further)", i, err, i),
				Repro: c.Repro,
			})
		}
	}
	// forks[i] is nil until taken; dead[i] records a fork that could not be
	// taken, which is a witness and ends that environment's comparison.
	forks := make([]*lisp.LEnv, n)
	dead := make([]bool, n)
	takeFork := func(i int) {
		f := tmpl
		for h := range hops {
			var err error
			if f, err = c.fork(f); err != nil {
				dead[i] = true
				out = append(out, Witness{
					Walker:   "Fork",
					Property: ParityPropertyForkable,
					Detail: fmt.Sprintf("fork %d, hop %d of %d: %v\n"+
						"    (a cold environment runs this program; a fork of the template that loaded it cannot be created; environment %d is not compared further)",
						i, h+1, hops, err, i),
					Repro: c.Repro,
				})
				return
			}
		}
		forks[i] = f
	}
	if c.Interleave {
		// Every fork live before anything runs.
		for i := range forks {
			takeFork(i)
		}
	}

	for _, st := range paritySchedule(c.Tx, c.Interleave) {
		if cold[st.i] == nil || dead[st.i] {
			continue
		}
		if forks[st.i] == nil {
			// Sequential: taken when first needed, after every earlier
			// fork has run its whole sequence.
			if takeFork(st.i); dead[st.i] {
				continue
			}
		}
		tx := c.Tx[st.i][st.j]
		// The same file name on both arms: it reaches the fingerprint as a
		// source location on every value the transaction creates.
		name := fmt.Sprintf("env%d-tx%d.lisp", st.i, st.j)
		wantRC := cold[st.i].LoadString(name, tx)
		gotRC := forks[st.i].LoadString(name, tx)
		want, got := parityNormalize(renderResult(wantRC)), parityNormalize(renderResult(gotRC))
		if want == got {
			continue
		}
		// Exactly one arm raised is its own property: see ParityPropertyRaises.
		property := ParityPropertyReturns
		if (wantRC.Type == lisp.LError) != (gotRC.Type == lisp.LError) {
			property = ParityPropertyRaises
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: property,
			Detail: fmt.Sprintf("environment %d, transaction %d: %s\n    cold: %s\n    fork: %s",
				st.i, st.j, tx, clip(want), clip(got)),
			Repro: c.Repro,
		})
	}
	for i := range n {
		if cold[i] == nil || dead[i] {
			continue
		}
		if forks[i] == nil {
			// An empty sequence: the fork was never taken.  Take it now, so
			// the state comparison still covers it.
			if takeFork(i); dead[i] {
				continue
			}
		}
		want := parityFingerprint(cold[i])
		got := parityFingerprint(forks[i])
		if want.Equal(got) {
			continue
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: ParityPropertyState,
			Detail:   fmt.Sprintf("environment %d\n%s", i, want.Diff(got)),
			Leak:     firstDivergentPath(want, got),
			Repro:    c.Repro,
		})
	}
	return out, nil
}
