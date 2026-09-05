// Copyright © 2026 The ELPS authors

package elpstest

import (
	"errors"
	"fmt"
	"reflect"
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

// Transaction isolation: the guarantee the mechanisms serve.
//
// Fork exists for performance.  An embedder — substrate is the motivating
// one — loads a program ONCE into a template and runs every transaction on
// a fresh fork of it, because a fork costs a fraction of a load.  The
// product guarantee that arrangement has to keep is that NO TRANSACTION CAN
// OBSERVE OR AFFECT ANOTHER.  Aliasing, cloning, copying, location bleed
// and native sharing are all just mechanisms by which that guarantee can
// break; the other files in this guard test the mechanisms, and this one
// tests the guarantee directly.
//
// THE DIRECTION MATRIX COMES FIRST, because organising this list by
// MECHANISM is how a hole stayed invisible in it.  The participants are a
// template and its n forks — n+1 environments, but only TWO ROLES — and a
// write that leaks between environments must cross from one role to the
// other or between two occupants of the fork role.  That is exactly three
// SINGLE-HOP directions, and all three are asserted.  The fourth row is
// not a fourth direction: it is the SAME hop as the row above it, observed
// LATER, for the reason given below the matrix:
//
//	fork -> another fork             property 2, swept i x j
//	fork -> its template             property 1, sequential and concurrent
//	template -> an existing fork     property 5
//	fork -> template -> later fork   property 3 — the fork -> template hop
//	                                 again, observed after the fact, and
//	                                 NOT a composition of the rows above
//
// WHAT PROPERTY 3'S ROW IS, precisely, because an earlier revision got
// this wrong and called it "a composition of the two rows above it".  It
// is not.  Its second step is template -> a fork taken AFTERWARDS, and
// that is not a leak direction at all — it is ordinary fork semantics.  It
// is emphatically not property 5's row either: property 5 exists to sweep
// forks that are ALREADY LIVE when the template is written.  Property 3 is
// the single hop fork -> template, observed through a fork that did not
// exist when property 1 looked.
//
// Why it still earns its own property rather than being inferred from
// property 1.  Its hop can leak WITHOUT BEING OBSERVABLE at the moment
// property 1 looks: a fork can contaminate structure the template reaches
// without moving the template's fingerprint at that instant, and the
// damage surfaces only in a fork taken afterwards.  Property 1's
// observation point is wrong for it, not its direction.
//
// Why T -> F -> T and T -> F -> F' get no property.  NOT because "both
// their hops hold, so they hold" — that argument does not survive its own
// escape clause, since observational incompleteness applies to property 5
// at least as strongly (property 5 writes one transaction and looks once,
// at the end, with no fork transaction afterwards to surface a delayed
// effect).  The real reason is that each COLLAPSES ONTO A SINGLE HOP THE
// CHECK ALREADY SWEEPS.  T -> F -> T requires the payload to cross
// F -> T, which property 1 sweeps after every transaction.  T -> F -> F'
// means the template's payload is reachable from F', which is just
// T -> F', and property 5 sweeps every live fork.  Property 3's does not
// collapse, because its observation point is after the fact.
//
// What "complete" does and does not claim.  It is a claim about SINGLE
// HOPS between two roles, and nothing more.  It says nothing about
// completeness of the MECHANISMS by which any one direction can leak, and
// nothing about coverage WITHIN a direction: property 2 sweeps i x j,
// while property 5 writes a single transaction to the template.
//
// A reader can therefore tell at a glance whether a new property is needed
// or an existing one has moved.  Before this matrix was written down the
// list ran 1-4 by mechanism (immutability, independence, successor,
// natives), and the template -> fork direction was simply absent; nothing
// in the doc made the direction space explicit, so nobody could see a hole
// in it.  A reader eventually asked which directions were covered, which
// is how it was found.
//
// The asymmetry that hid it: the sequential loop mutates a fork and
// re-fingerprints the template on EVERY iteration, so fork -> template was
// continuously covered almost by accident, while template -> fork had no
// path to be exercised at all — nothing in the check ever wrote to the
// template.
//
// The properties, all expressed with the same fingerprint the mechanism
// checks use:
//
//  1. TEMPLATE IMMUTABILITY UNDER LOAD.  Take N forks, run a different
//     mutating transaction on each, and the template must fingerprint
//     byte-identically to its baseline.  Run both sequentially and
//     concurrently: a data race here is the same bug wearing a different
//     hat, which is why the concurrent arm belongs in the -race gate.
//  2. FORK INDEPENDENCE.  Snapshot every fork, run one transaction, and
//     only that fork may have moved.  Swept over every fork rather than
//     demonstrated on one.
//  3. PRISTINE SUCCESSOR.  A fork taken AFTER other forks have been
//     mutated must fingerprint identically to one taken from the untouched
//     template.  This is the shape that would silently contaminate a LATER
//     customer transaction: state that leaked back into the template and
//     then forward.
//  4. NO SHARED STATEFUL NATIVE.  Not a direction but a PRECONDITION for
//     several of them: a payload reachable by pointer from two forks makes
//     more than one direction leak at once.  No native payload that
//     declares neither NativeCloner nor RuntimeBound may be so reachable.
//     See the census below for why this is not the same check as the
//     runtime-affinity protocol.
//  5. TEMPLATE -> EXISTING FORK.  Run a mutating transaction ON THE
//     TEMPLATE and require every live fork to stay where it was.  If a
//     fork shares a payload with its template — the #576 and #585 defect —
//     a write through the template lands in the fork; that and property 1
//     are the two ends of one shared pointer.
//
// Two bounds on property 5, so this block does not overclaim:
//
//   - Under `-tags elpscheck` the ownership checker refuses this class
//     first, panicking with "LVal used by two Runtimes" before the
//     property can report.  Property 5 therefore earns its place in the
//     ORDINARY build an embedder ships, where ownership checking — like
//     RuntimeBound — is not compiled in.
//   - Reverting the #576 map memo does NOT fail property 5.  Measured with
//     the memo lookup disabled in forker.mapData, it trips exactly two
//     DISTINCT PROPERTIES: the UNNUMBERED fresh-fork precheck ("a fresh
//     fork is indistinguishable from its template") and property 3 ("a
//     fork taken after other forks were mutated is pristine").  Two
//     properties, not two witnesses -- the precheck reports once per fork,
//     so the witness count tracks len(Tx) and is not worth quoting.  It
//     does NOT fail property 1, and cannot: a
//     de-aliasing fork copies MORE, so a write on a fork can never reach
//     the template, and property 1 can only redden on OVER-sharing.  An
//     earlier revision of this bullet said "properties 1 and 3", which
//     asserted the very thing the next sentence argues is impossible.
//     Property 5 catches OVER-sharing; #576 is DE-aliasing, and a fork
//     that copies too eagerly is more isolated from its template, not
//     less.  Opposite ends of one axis, caught by different properties —
//     which is the argument for asserting every direction rather than
//     assuming one implies the others.

// TransactionCheck describes one run of the transaction-isolation oracle.
type TransactionCheck struct {
	// NewEnv builds the template.  Nil means NewForkCheckEnv.
	//
	// THE RETURNED ENVIRONMENT IS WRITTEN TO.  Property 5 runs a
	// transaction on the template on purpose -- that is the whole point of
	// the template -> fork direction -- so the environment does not survive
	// the check unmodified.  Return a FRESH environment on every call; do
	// not hand back a cached or shared one.  Every implementation in this
	// repository builds fresh, so nothing depends on the old behaviour, but
	// nothing said so either until property 5 made this function the first
	// exported entry point that mutates its own template.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into the template.
	Program string
	// Tx are the transactions, one per fork.  Each should MUTATE something
	// the template holds, or the properties pass vacuously; CheckTransactions
	// asserts that at least one of them moves its own fork.
	Tx []string
	// ExpectNoSharedNatives makes an UNDECLARED native payload reachable
	// from two forks a finding rather than a report.  Fork shares a native
	// payload by reference unless it implements NativeCloner or the
	// embedder substitutes it (lisp/fork.go), so for an embedder's own
	// values that sharing may be a deliberate choice.  For a program over
	// the standard library the expected count is zero, and the guard's own
	// test sets this.  A payload that DECLARES NativeCloner or RuntimeBound
	// and is shared anyway is always a finding: it stated that it must not
	// be.
	ExpectNoSharedNatives bool
	// Fork produces each fork.  Nil means (*lisp.LEnv).Fork.  It exists so
	// a deliberately broken reference fork can be driven through the same
	// oracle (aliasguard_broken_test.go).
	//
	// Substituting it does NOT change which properties run.  An earlier
	// revision keyed the concurrent arm off `Fork != nil`, so an embedder
	// substituting a benign walker -- fork options, instrumentation, a
	// counting wrapper -- silently lost the -race arm with no signal.  Use
	// SkipConcurrentArm to opt out, deliberately and visibly.
	Fork func(*lisp.LEnv) (*lisp.LEnv, error)
	// SkipConcurrentArm omits the concurrent repeat of property 1.
	//
	// Set it ONLY for a walker whose defect is that it SHARES a payload
	// between forks or with its template.  Driving two such forks in
	// parallel is a data race BY CONSTRUCTION: two goroutines mutate
	// environments over one *MapData, -race reports it against the guard's
	// own test rather than against anything in elps, and Go marks every
	// other in-flight parallel test failed alongside it (five of them, on
	// commit 9a73d6a, which is how this was found).
	//
	// It costs the interleaving hazard and nothing else: the sequential
	// arm above already checks every isolation property.  Leaving it false
	// is right for every correct walker, which is why false is the
	// default -- the -race arm is coverage, and coverage should not be
	// dropped as a side effect of substituting a fork walker.
	SkipConcurrentArm bool
	// Repro is attached to every witness.
	Repro string
	// onFork, when set, is told the ROLE of the fork about to be taken --
	// which property it serves -- immediately before Fork is called for
	// it.  In-package only: it exists so a control that models a walker
	// broken on one role (the pristine successor, say) can key on the
	// role rather than on the fork's ordinal.  The ordinal is not stable:
	// the parity channel (aliasguard_parity.go) takes len(Tx) forks of
	// its own between the sweep's fresh forks and the successor, so a
	// control that counted calls named a parity fork as "the successor"
	// the moment parity was folded in (TestGuardDetectsDealiasingOnTheSuccessorOnly,
	// red on the first restack).  Roles are what the harness knows and
	// what a control means.
	onFork func(role forkRole)
}

// forkRole names the property a fork is taken for.
type forkRole string

const (
	// forkRoleFresh is one of the len(Tx) forks the sweep runs a
	// transaction on, taken before any transaction runs.
	forkRoleFresh forkRole = "a fresh fork"
	// forkRoleSuccessor is the pristine-successor fork, taken after every
	// transaction has run (property 3).
	forkRoleSuccessor forkRole = "the pristine-successor fork"
	// forkRoleConcurrent is one of the concurrent arm's forks, over a
	// template of its own.
	forkRoleConcurrent forkRole = "a concurrent-arm fork"
	// forkRoleParity is one of the parity channel's forks
	// (aliasguard_parity.go), each compared against a cold load.
	forkRoleParity forkRole = "a parity-channel fork"
)

// fork applies the check's fork walker, defaulting to (*lisp.LEnv).Fork.
func (c TransactionCheck) fork(env *lisp.LEnv) (*lisp.LEnv, error) {
	if c.Fork != nil {
		return c.Fork(env)
	}
	return env.Fork()
}

// forkAs is fork, announcing the role first (see onFork).
func (c TransactionCheck) forkAs(role forkRole, env *lisp.LEnv) (*lisp.LEnv, error) {
	if c.onFork != nil {
		c.onFork(role)
	}
	return c.fork(env)
}

// RunTransactionCheck runs the sequential arm, and the concurrent arm
// unless the check sets SkipConcurrentArm, reporting each witness.
func RunTransactionCheck(t TestingTB, c TransactionCheck) {
	t.Helper()
	got, err := CheckTransactions(c)
	if err != nil {
		t.Fatalf("transaction isolation: %v", err)
		return
	}
	for _, w := range got {
		t.Errorf("%s", w)
	}
}

// CheckTransactions runs every isolation property and returns one witness
// per failure.
//
// It deliberately does NOT name a count.  A count in prose drifts the
// moment a property is added, and this doc comment proved it: it went on
// naming a count of 4 for the whole life of the fifth property, in the
// EXPORTED API doc, past the end of the window the header drift guard was
// scanning and in a casing its banned-phrase list did not cover.  The
// guard now scans the whole file, case-insensitively, by pattern.
func CheckTransactions(c TransactionCheck) ([]Witness, error) {
	if len(c.Tx) == 0 {
		return nil, errors.New("no transactions: the properties would pass vacuously")
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
	tmpl, err := build()
	if err != nil {
		return nil, err
	}
	baseline := FingerprintEnv(tmpl, templateOpts)

	forks := make([]*lisp.LEnv, len(c.Tx))
	for i := range forks {
		f, err := c.forkAs(forkRoleFresh, tmpl)
		if err != nil {
			return nil, fmt.Errorf("fork %d: %w", i, err)
		}
		forks[i] = f
	}

	var out []Witness
	// A fresh fork must be indistinguishable from its template.  This is
	// the whole fork contract in one comparison, and it subsumes the
	// alias-structure check at environment level because sharing is part of
	// the encoding.
	before := make([]*Fingerprint, len(forks))
	for i, f := range forks {
		before[i] = FingerprintEnv(f, templateOpts)
		if !baseline.Equal(before[i]) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a fresh fork is indistinguishable from its template",
				Detail:   fmt.Sprintf("fork %d diverges before any transaction ran\n%s", i, baseline.Diff(before[i])),
				Leak:     firstDivergentPath(baseline, before[i]),
				Repro:    c.Repro,
			})
		}
		// The Cells row of the contract table, asserted for Fork: a
		// fresh fork's views window the fork's own roots exactly as the
		// template's window the template's (aliasguard_cellview.go).
		// The fingerprint above cannot see this -- two headers over one
		// array share no pointer it keys on -- which is why it is its
		// own channel.
		out = append(out, cellViewWitnesses(c, tmpl, f, fmt.Sprintf("fork %d", i))...)
	}

	// Parity, the property every direction above is a consequence of, over
	// the same transactions against COLD environments (aliasguard_parity.go).
	// It runs before the sweep because a transaction that raises on a fork
	// and not on a cold load is a parity finding here and a harness error
	// there; the sweep cannot follow it and is not attempted.
	parity, raised := transactionParityWitnesses(c)
	out = append(out, parity...)
	if raised {
		return out, nil
	}

	// Properties 1 and 2, swept: run transaction i on fork i, then assert
	// that the template and every OTHER fork are where they were.
	moved := false
	for i, tx := range c.Tx {
		if rc := forks[i].LoadString(fmt.Sprintf("tx%d.lisp", i), tx); rc.Type == lisp.LError {
			return nil, fmt.Errorf("transaction %d: %v", i, rc)
		}
		after := FingerprintEnv(forks[i], templateOpts)
		if !after.Equal(before[i]) {
			moved = true
		}
		before[i] = after
		if got := FingerprintEnv(tmpl, templateOpts); !baseline.Equal(got) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "the template is unchanged by a transaction on a fork",
				Detail:   fmt.Sprintf("transaction %d moved the template\n%s", i, baseline.Diff(got)),
				Leak:     firstDivergentPath(baseline, got),
				Repro:    c.Repro,
			})
		}
		for j := range forks {
			if j == i {
				continue
			}
			if got := FingerprintEnv(forks[j], templateOpts); !before[j].Equal(got) {
				out = append(out, Witness{
					Walker:   "Fork",
					Property: "a transaction on one fork is invisible to every other fork",
					Detail:   fmt.Sprintf("transaction %d moved fork %d\n%s", i, j, before[j].Diff(got)),
					Leak:     firstDivergentPath(before[j], got),
					Repro:    c.Repro,
				})
				before[j] = got
			}
		}
	}
	if !moved {
		return nil, errors.New("no transaction changed its own fork; the isolation properties would pass vacuously")
	}

	// Property 3: a fork taken after all that must be pristine.
	successor, err := c.forkAs(forkRoleSuccessor, tmpl)
	if err != nil {
		return nil, err
	}
	if got := FingerprintEnv(successor, templateOpts); !baseline.Equal(got) {
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "a fork taken after other forks were mutated is pristine",
			Detail:   "state leaked back into the template and forward into a later transaction\n" + baseline.Diff(got),
			Leak:     firstDivergentPath(baseline, got),
			Repro:    c.Repro,
		})
	}
	out = append(out, cellViewWitnesses(c, tmpl, successor, "the pristine-successor fork")...)

	// Property 4: no stateful native shared between two forks.
	out = append(out, sharedNativeWitnesses(c, "the template", tmpl, forks)...)

	// Property 5: THE OTHER END OF THE SHARED POINTER.
	//
	// Everything above drives writes FROM a fork: fork -> other fork,
	// fork -> template, and fork -> template -> later fork.  None of them
	// writes to the TEMPLATE and re-checks the forks that are already
	// live, so that direction went unasserted.
	//
	// The asymmetry is structural, not incidental.  If a fork shares a
	// payload with its template -- which is exactly the #576 and #585
	// defect -- then a write through the template lands in the fork.
	// Those are the two ends of one shared pointer, and testing only one
	// end leaves the other to "the embedder is supposed to treat the
	// template as immutable after load", which is the kind of supposition
	// this guard exists so that nobody has to make.
	//
	// It runs after every check that reads the template's baseline,
	// because it deliberately mutates the template.  It runs BEFORE the
	// concurrent arm, which is independent of this template -- that arm
	// builds and forks its own -- and which is skipped entirely for a
	// substituted fork walker.  Ordering it after that skip put it behind
	// an early return and silently disabled it for exactly the broken
	// walkers it exists to catch; TestGuardDetectsATemplateWriteReachingAFork
	// caught that, and is the control that keeps it fixed.
	//
	// WHAT IT CATCHES, and what it does not.  This direction detects
	// OVER-sharing: a fork still holding a payload the template owns.  It
	// does NOT detect the DE-aliasing defects at the other extreme --
	// measured by reverting the #576 map memo, which fails "a fresh fork
	// is indistinguishable from its template" and the pristine-successor
	// property while leaving this one green.  That is not a gap: a fork
	// that de-aliases too eagerly is MORE isolated from its template, not
	// less, so a template write cannot reach it.  The two defects sit at
	// opposite ends of the same axis and are caught by different
	// properties, which is the point of asserting every direction rather
	// than assuming one implies the others.
	//
	// THE LIVE SET INCLUDES THE PRISTINE-SUCCESSOR FORK, and must.  It was
	// created above and is still live when the template is written to, so a
	// walker that over-shares only on a fork taken after the others were
	// mutated is invisible without it -- measured at zero witnesses before
	// it was added, against a Baseline string that already said "every live
	// fork".  TestGuardDetectsATemplateWriteReachingTheSuccessorFork keeps
	// it in the set.
	live := make([]liveFork, 0, len(forks)+1)
	for j, f := range forks {
		live = append(live, liveFork{env: f, before: before[j], name: fmt.Sprintf("fork %d", j)})
	}
	live = append(live, liveFork{
		env:    successor,
		before: FingerprintEnv(successor, templateOpts),
		name:   "the pristine-successor fork",
	})
	out = append(out, templateToForkWitnesses(c, tmpl, live)...)

	// Property 1 again, concurrently.  Same transactions, same template,
	// forks driven in parallel: under -race this is also the data-race
	// gate, and without it it still catches a template mutation that only
	// happens under interleaving.
	//
	// IT IS SKIPPED ONLY WHEN THE CALLER ASKS, via SkipConcurrentArm.
	//
	// A sharing walker driven in parallel races BY CONSTRUCTION: two
	// goroutines mutate environments over one *MapData.  The control asks
	// for that defect, -race duly reports it against the guard's own test
	// rather than against anything in elps, and Go marks every other
	// in-flight parallel test failed alongside it -- five of them, on the
	// run that proved this (commit 9a73d6a).
	//
	// THE AXIS USED TO BE `c.Fork != nil`, AND THAT WAS WRONG.  It
	// conflated "a walker was substituted" with "a walker shares on
	// purpose".  The two are not the same: an embedder substituting Fork
	// for a benign reason -- fork options, instrumentation, a counting
	// wrapper -- lost the -race arm silently, with nothing at the API
	// surface saying so.  The guard's own test made the point without
	// anyone noticing: it passed a FAITHFUL walker and asserted the arm
	// was skipped, which is precisely the benign case losing coverage.
	//
	// Sharing is now declared rather than inferred.  Two tests hold it --
	// TestTheConcurrentArmIsSkippedOnRequest, and
	// TestTheConcurrentArmStillRunsForASubstitutedWalker, which exists
	// because the way this could go wrong is by silently swallowing the
	// coverage it is meant to protect.
	if c.SkipConcurrentArm {
		return out, nil
	}

	conc, err := build()
	if err != nil {
		return nil, err
	}
	concBase := FingerprintEnv(conc, templateOpts)
	cforks := make([]*lisp.LEnv, len(c.Tx))
	for i := range cforks {
		f, err := c.forkAs(forkRoleConcurrent, conc)
		if err != nil {
			return nil, err
		}
		cforks[i] = f
	}
	errs := make([]*lisp.LVal, len(c.Tx))
	var wg sync.WaitGroup
	for i, tx := range c.Tx {
		wg.Add(1)
		go func(i int, tx string) {
			defer wg.Done()
			errs[i] = cforks[i].LoadString(fmt.Sprintf("tx%d.lisp", i), tx)
		}(i, tx)
	}
	wg.Wait()
	for i, rc := range errs {
		if rc != nil && rc.Type == lisp.LError {
			return nil, fmt.Errorf("concurrent transaction %d: %v", i, rc)
		}
	}
	if got := FingerprintEnv(conc, templateOpts); !concBase.Equal(got) {
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "the template is unchanged by concurrent transactions on its forks",
			Detail:   "run the -race gate on this arm: a template mutation under interleaving is a data race\n" + concBase.Diff(got),
			Leak:     firstDivergentPath(concBase, got),
			Repro:    c.Repro,
		})
	}

	return out, nil
}

// templateToForkWitnesses runs a transaction ON THE TEMPLATE and requires
// every live fork to stay where it was.
//
// The transaction is drawn from the check's existing Tx set rather than
// from a new field, so every caller gets the property without changing a
// call site.  The set is tried in order until one actually moves the
// template: a transaction that leaves the template alone would make the
// property pass for free, which is the same non-vacuity discipline the
// fork sweep applies with its `moved` flag.
// liveFork is one environment that is ALIVE when property 5 writes to the
// template, together with the fingerprint it held just before that write.
//
// It carries a NAME because the set is not just forks[i] any more.  The
// pristine-successor fork property 3 creates is also live at that moment, and
// leaving it out was a real hole: a walker that over-shares only on a fork
// taken AFTER the others were mutated wrote through the template into that
// fork and produced zero witnesses, while the Baseline string claimed "every
// live fork".  An index into forks could not have named it.
type liveFork struct {
	env    *lisp.LEnv
	before *Fingerprint
	name   string
}

func templateToForkWitnesses(c TransactionCheck, tmpl *lisp.LEnv, live []liveFork) []Witness {
	pre := FingerprintEnv(tmpl, templateOpts)
	moved := -1
	for i, tx := range c.Tx {
		if rc := tmpl.LoadString(fmt.Sprintf("tmpl-tx%d.lisp", i), tx); rc.Type == lisp.LError {
			// A transaction the template rejects tells us nothing; try
			// the next one.
			continue
		}
		if !FingerprintEnv(tmpl, templateOpts).Equal(pre) {
			moved = i
			break
		}
	}
	if moved < 0 {
		return []Witness{{
			Walker:   "Fork",
			Property: "a transaction on the template is invisible to every existing fork",
			Leak:     "<no transaction moved the template>",
			Baseline: "at least one transaction changes the template it runs on",
			Observed: "every transaction left the template's fingerprint unchanged",
			Detail: "This property would pass for free. It asserts that a write THROUGH THE TEMPLATE " +
				"does not reach a fork that is already live, so a transaction that does not write to " +
				"the template proves nothing. Give the check at least one transaction that mutates a " +
				"binding the template holds.",
			Repro: c.Repro,
		}}
	}

	var out []Witness
	for _, lf := range live {
		got := FingerprintEnv(lf.env, templateOpts)
		if lf.before.Equal(got) {
			continue
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "a transaction on the template is invisible to every existing fork",
			Leak:     firstDivergentPath(lf.before, got),
			Baseline: "every live fork holds the value it held before the template was written to",
			Observed: lf.name + " moved",
			Detail: fmt.Sprintf("transaction %d, run on the TEMPLATE, moved %s -- so that fork "+
				"shares a payload with the template it was forked from, and a template write reaches "+
				"it. This is the other end of the pointer the fork-to-template property covers.\n%s",
				moved, lf.name, lf.before.Diff(got)),
			Repro: c.Repro,
		})
	}
	return out
}

// CheckForkTemplate holds one loaded template to the fork contract stated
// as a single comparison: a fork, and a fork of that fork, must fingerprint
// identically to the template under the template-level fingerprint — every
// value, every sharing relation, every seal bit and the per-package
// metadata tables.
//
// Because sharing is part of the encoding, this subsumes an alias-structure
// comparison; because the fingerprint carries the package metadata channel,
// it also covers the three tables Fork copies rather than shares, which
// nothing compared before.  The two-hop arm is there because a fix that
// survived one fork hop and not two has happened (issue #579).
//
// It is what RunForkCheck delegates its aliasing and isolation properties
// to, so the two harnesses share one oracle instead of carrying two.
func CheckForkTemplate(env *lisp.LEnv, opts ...lisp.ForkOption) []Witness {
	baseline := FingerprintEnv(env, templateOpts)
	var out []Witness
	fork, err := env.Fork(opts...)
	if err != nil {
		return []Witness{{Walker: "Fork", Property: "the template forks", Detail: err.Error()}}
	}
	arms := []struct {
		name string
		env  *lisp.LEnv
	}{{"a fresh fork", fork}}
	fork2, err := fork.Fork(opts...)
	if err != nil {
		out = append(out, Witness{Walker: "Fork", Property: "a fork forks", Detail: err.Error()})
	} else {
		arms = append(arms, struct {
			name string
			env  *lisp.LEnv
		}{"a fork of a fork", fork2})
	}
	for _, arm := range arms {
		got := FingerprintEnv(arm.env, templateOpts)
		if !baseline.Equal(got) {
			out = append(out, Witness{
				Walker:   "Fork",
				Property: arm.name + " is indistinguishable from the template",
				Detail:   baseline.Diff(got),
				Leak:     firstDivergentPath(baseline, got),
			})
		}
	}
	// Macro-expansion metadata is dropped by Fork, so it is invisible to
	// the fingerprint comparison above -- see macroExpansionLeaks.
	for _, arm := range arms {
		out = append(out, macroExpansionLeaks(env, arm.env, arm.name)...)
	}
	// A payload that declared a duplication protocol or a runtime affinity
	// and reached two environments anyway contradicts its own declaration.
	for _, arm := range arms {
		for _, sh := range SharedNativePayloads(env, arm.env) {
			if !sh.Cloner && !sh.Bound {
				continue
			}
			out = append(out, Witness{
				Walker:   "Fork",
				Property: "a native payload that declared how it is duplicated is not shared with " + arm.name,
				Leak:     sh.PathB,
				Detail:   sh.String(),
			})
		}
	}
	return out
}

// SharedNative is one native payload reachable from two environments at
// once.
type SharedNative struct {
	// Type is the payload's Go type.
	Type string
	// PathA and PathB are where it was reached in each environment.
	PathA, PathB string
	// Cloner and Bound report which sharing protocol, if any, the payload
	// declares.
	Cloner, Bound bool
}

func (s SharedNative) String() string {
	return fmt.Sprintf("%s reachable at %s and at %s (NativeCloner=%t RuntimeBound=%t)",
		s.Type, s.PathA, s.PathB, s.Cloner, s.Bound)
}

// SharedNativePayloads reports every native payload held by pointer that is
// reachable from BOTH a and b.
//
// This is deliberately not the runtime-affinity check (lisp/runtime_bound.go).
// That protocol is OPT-IN — a payload that never implements RuntimeBound is
// never checked — and its enforcement lives entirely behind `-tags
// elpscheck`, so no production build checks anything.  Pointer identity
// across two environments needs neither: it is observable for every payload
// type, declared or not, in every build, and it is the exact shape of the
// contamination it matters about (one transaction's stateful handle also
// being another's).
//
// It is exported so an embedder can point it at its own values.  For an
// embedder, sharing may be a deliberate choice, so this REPORTS rather than
// judges; the standard library's own expected count is zero, which is what
// the guard's test asserts.
func SharedNativePayloads(a, b *lisp.LEnv) []SharedNative {
	na := reachableNatives(a)
	nb := reachableNatives(b)
	var out []SharedNative
	for payload, pa := range na {
		pb, ok := nb[payload]
		if !ok {
			continue
		}
		_, cloner := payload.(lisp.NativeCloner)
		_, bound := payload.(lisp.RuntimeBound)
		out = append(out, SharedNative{
			Type:   fmt.Sprintf("%T", payload),
			PathA:  pa,
			PathB:  pb,
			Cloner: cloner,
			Bound:  bound,
		})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].PathA < out[j].PathA })
	return out
}

// sharedNativeWitnesses reports a native payload reachable from two
// transactions at once.
//
// A payload that DECLARED a sharing protocol and is shared anyway is always
// a finding: a NativeCloner has stated what its duplicate is, and a
// RuntimeBound has stated which Runtime it belongs to, so either one
// arriving in two forks contradicts its own declaration.  (RuntimeBound is
// enforced by the kernel, but only under `-tags elpscheck`; this reports it
// in every build.)
//
// An UNDECLARED payload is Fork's documented default — share by reference —
// so it is a finding only when the caller says it expects none.
func sharedNativeWitnesses(c TransactionCheck, aName string, a *lisp.LEnv, forks []*lisp.LEnv) []Witness {
	var out []Witness
	report := func(what string, shared []SharedNative) {
		var lines []string
		var leak string
		for _, s := range shared {
			switch {
			case s.Cloner:
				lines = append(lines, s.String()+"  [declares NativeCloner and is shared anyway]")
			case s.Bound:
				lines = append(lines, s.String()+"  [declares RuntimeBound and is shared anyway]")
			case c.ExpectNoSharedNatives:
				lines = append(lines, s.String()+"  [declares nothing; shared by Fork's default policy]")
			default:
				continue
			}
			if leak == "" {
				leak = s.PathB
			}
		}
		if len(lines) == 0 {
			return
		}
		out = append(out, Witness{
			Walker:   "Fork",
			Property: "no stateful native payload is reachable from two transactions at once",
			Detail:   what + ":\n    " + strings.Join(lines, "\n    "),
			Leak:     leak,
			Repro:    c.Repro,
		})
	}
	for i, f := range forks {
		report(fmt.Sprintf("%s and fork %d", aName, i), SharedNativePayloads(a, f))
		for j := i + 1; j < len(forks); j++ {
			report(fmt.Sprintf("fork %d and fork %d", i, j), SharedNativePayloads(f, forks[j]))
		}
	}
	return out
}

// reachableNatives maps every pointer-held native payload reachable from
// env to the first path that reached it.  A payload held by value has no
// identity to share, so it is not collected.
func reachableNatives(env *lisp.LEnv) map[any]string {
	out := map[any]string{}
	walkReachable(env, func(v *lisp.LVal, path string) {
		// Keyed on the PAYLOAD, not on the type. Native is shared storage:
		// LBytes holds a *[]byte there, LSortMap a *MapData, and an embedder
		// can annotate an ordinary node. Keying on `v.Type == LNative` missed
		// all of it -- including the case measured in #603
		// (TestLoadCacheTopology_NativeAnnotationIsReported, which pinned
		// the gap as open until this change closed it), where a Reader's
		// annotation on a SEALED node reaches every fork by reference
		// because a sealed value is shared outright, before the native
		// policy runs, so its NativeCloner is never consulted. Nothing here
		// saw that, and this census is the surface that should have.
		//
		// A cell-view link is excluded: it is a reference to a root, not a
		// payload (isCellViewLink, elpstest/fingerprint.go).
		if isPointerPayload(v.Native) && !kernelOwnedPayload(v) {
			if _, dup := out[v.Native]; !dup {
				out[v.Native] = path
			}
		}
	})
	return out
}

// reachableValues maps every *LVal reachable from env to the first path
// that reached it.  It is the identity question the census asks of
// payloads, asked of headers -- which is what the macro-expansion leak
// check needs, since the leak is a fork holding a pointer to a TEMPLATE
// node rather than a shared payload.
func reachableValues(env *lisp.LEnv) map[*lisp.LVal]string {
	out := map[*lisp.LVal]string{}
	walkReachable(env, func(v *lisp.LVal, path string) {
		if _, dup := out[v]; !dup {
			out[v] = path
		}
	})
	return out
}

// walkReachable visits every value reachable from env exactly once, with
// the first path that reached it.
//
// ONE walk definition with two consumers, deliberately: the census
// (reachableNatives) and the value map (reachableValues) must agree about
// what "reachable" means, or a leak visible to one is invisible to the
// other for no reason a reader could discover.
func walkReachable(env *lisp.LEnv, visit func(v *lisp.LVal, path string)) {
	seenV := map[*lisp.LVal]bool{}
	seenE := map[*lisp.LEnv]bool{}
	var walk func(v *lisp.LVal, path string)
	var walkEnv func(e *lisp.LEnv, path string)
	walk = func(v *lisp.LVal, path string) {
		if v == nil || seenV[v] {
			return
		}
		seenV[v] = true
		visit(v, path)
		switch v.Type {
		case lisp.LFun:
			walkEnv(funraw.Env(v), path+"/env")
		case lisp.LSortMap:
			if md := v.Map(); md != nil {
				for _, k := range md.Keys().Cells {
					val, _ := md.Get(k)
					walk(val, path+"/"+k.String())
				}
			}
		default:
			// Every other type reaches its children only through cells,
			// which the loop below walks for every type.
		}
		for i, c := range v.Cells {
			walk(c, fmt.Sprintf("%s/%d", path, i))
		}
		// A cell view's root is reachable state: the view's Cells are a
		// window onto it, so a payload the root holds beyond the window is
		// reachable through the view.  Followed through the VALIDATED
		// resolver, as a reference with no identity of its own (the
		// convention on lisp.cellsView; isCellViewLink, fingerprint.go);
		// a stale link is walked as ordinary structure and not followed,
		// which is what Fork does with it too.
		if root, _, ok := v.CellView(); ok {
			walk(root, path+"/root")
		}
	}
	walkEnv = func(e *lisp.LEnv, path string) {
		if e == nil || seenE[e] {
			return
		}
		seenE[e] = true
		keys, vals := sortedBindings(e)
		for _, k := range keys {
			walk(vals[k], path+"/"+k)
		}
		walkEnv(e.Parent(), path+"/parent")
	}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		walk(v, pkg+":"+name)
	})
	walkEnv(env, "<env>")
}

// NativeDeclaration is one native payload TYPE reachable from an
// environment, and what it declares about being copied or shared.
type NativeDeclaration struct {
	// Type is the payload's Go type, as %T renders it.
	Type string
	// Path is the first place a payload of this type was reached.
	Path string
	// Cloner reports whether the type implements lisp.NativeCloner: it has
	// stated what its own duplicate is, so Fork, `copy` and detach all
	// duplicate it rather than sharing or refusing it.
	Cloner bool
	// Bound reports whether the type implements lisp.RuntimeBound: it has
	// declared a runtime affinity, which checked builds enforce.
	Bound bool
	// Stateless reports whether the payload's underlying type is a basic Go
	// type, which has no state to share.
	Stateless bool
}

// Declared reports whether the type has stated its sharing semantics one
// way or another.
func (d NativeDeclaration) Declared() bool { return d.Cloner || d.Bound || d.Stateless }

func (d NativeDeclaration) String() string {
	return fmt.Sprintf("%s at %s (NativeCloner=%t RuntimeBound=%t stateless=%t)",
		d.Type, d.Path, d.Cloner, d.Bound, d.Stateless)
}

// NativeDeclarations classifies every native payload type reachable from
// env.
//
// The point of the classification is that BOTH existing mechanisms miss the
// same case.  The runtime-affinity protocol is opt-in, so a payload that
// forgets to declare anything is never checked; and its enforcement is
// compiled only under `-tags elpscheck`, so no production build checks even
// the payloads that did declare.  A type that is neither a NativeCloner nor
// a RuntimeBound nor provably stateless has therefore said nothing about
// what happens when it is shared by every fork of a template — which is the
// default, and which for a stateful payload means every transaction shares
// it.
//
// Exported so an embedder can run the same census over its own loaded
// environment before shipping a phylum.
func NativeDeclarations(env *lisp.LEnv) []NativeDeclaration {
	byType := map[string]NativeDeclaration{}
	for payload, path := range reachableNatives(env) {
		key := fmt.Sprintf("%T", payload)
		if _, ok := byType[key]; ok {
			continue
		}
		_, cloner := payload.(lisp.NativeCloner)
		_, bound := payload.(lisp.RuntimeBound)
		byType[key] = NativeDeclaration{
			Type:      key,
			Path:      path,
			Cloner:    cloner,
			Bound:     bound,
			Stateless: isStatelessPayload(payload),
		}
	}
	out := make([]NativeDeclaration, 0, len(byType))
	for _, d := range byType {
		out = append(out, d)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Type < out[j].Type })
	return out
}

// isStatelessPayload reports whether a payload's underlying type is a basic
// Go type — a bool, an integer, a float, a complex or a string, possibly
// behind one pointer.  Such a payload holds no reference to anything else,
// so sharing it between two transactions shares nothing they can both
// write.  Anything else (a struct, a slice, a map, a channel, a func) may
// reach mutable state and has to declare.
func isStatelessPayload(payload any) bool {
	t := reflect.TypeOf(payload)
	if t == nil {
		return false
	}
	if t.Kind() == reflect.Pointer {
		t = t.Elem()
	}
	switch t.Kind() {
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr,
		reflect.Float32, reflect.Float64,
		reflect.Complex64, reflect.Complex128,
		reflect.String:
		return true
	default:
		return false
	}
}

// macroExpansionLeaks reports macro-expansion debug metadata on a fork
// whose recorded call-site arguments point at MUTABLE values owned by the
// template.
//
// WHY THIS CANNOT BE A FINGERPRINT COMPARISON, which is the reason the
// channel had no coverage at all. Fork DELIBERATELY drops the metadata
// (lisp/fork.go, `cp.macroExpansion = nil`), so a fork and its template are
// SUPPOSED to differ here. Property 2 asks the opposite question -- "a
// fresh fork is indistinguishable from its template" -- so a token for this
// field in the default fingerprint would fail on correct code, and leaving
// it out makes the field invisible. A field a walker is supposed to drop is
// invisible to a same-vs-same comparison by construction. It needs a
// direct property, which is this one.
//
// WHAT THE LEAK IS. macroExpansionContext.Args holds the unevaluated
// call-site arguments as []*LVal -- the ORIGINAL nodes, not copies -- and
// (*LVal).MacroExpansion hands them out to any embedder with a debugger
// attached. A fork that kept its template's metadata therefore hands a
// consumer pointers into the template's own tree.
//
// SEALED ARGS ARE NOT A LEAK, and the distinction is measured rather than
// assumed. Args are usually sealed parse-tree nodes: immutable, and shared
// with every fork outright by the same kernel policy, so a pointer to one
// conveys nothing a fork does not already have. But they are not always
// sealed -- a macro call built at runtime (`(macroexpand-1 (list 'defun
// 'f (list 'x) 'x))`) records the runtime-built list nodes, which are
// mutable and template-owned. Measured on that shape: 15 recorded Args, 10
// sealed, 5 not. So the property is about the unsealed ones, and
// TestForkDoesNotLeakMacroExpansionMetadata builds exactly that fixture.
//
// The witness names both ends -- where the metadata sits on the fork, and
// which template value it reaches -- because a boolean would leave the
// reader to rediscover the aliasing.
func macroExpansionLeaks(tmpl, fork *lisp.LEnv, arm string) []Witness {
	tmplVals := reachableValues(tmpl)
	var lines []string
	var leak string
	for v, path := range reachableValues(fork) {
		m, ok := v.MacroExpansion()
		if !ok {
			continue
		}
		for i, arg := range m.Args {
			if arg == nil || arg.IsSealed() {
				continue
			}
			tpath, shared := tmplVals[arg]
			if !shared {
				continue
			}
			lines = append(lines, fmt.Sprintf(
				"%s carries expansion metadata for %s whose arg %d is the template's %s",
				path, m.Name, i, tpath))
			if leak == "" {
				leak = path
			}
		}
	}
	if len(lines) == 0 {
		return nil
	}
	sort.Strings(lines)
	return []Witness{{
		Walker:   "Fork",
		Property: "no macro-expansion metadata on " + arm + " reaches a template value",
		Detail: "macroExpansionContext.Args holds the ORIGINAL call-site nodes and " +
			"(*LVal).MacroExpansion hands them out, so these are template values a fork " +
			"consumer can reach and write:\n    " + strings.Join(lines, "\n    "),
		Leak: leak,
	}}
}
