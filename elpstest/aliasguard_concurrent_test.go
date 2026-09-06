// Copyright © 2026 The ELPS authors

// Controls for the CONCURRENT ARM of the transaction-isolation oracle.
//
// The arm used to re-check property 1 and nothing else -- the template's
// fingerprint -- so a defect confined to the concurrent forks was
// unobserved by every channel.  The sequential sweep never sees those
// forks; the cell-view channel, the native census and parity all run on
// the SEQUENTIAL forks.  -race catches the shape only when two goroutines
// actually write in one window, and a fork that merely INHERITS another's
// state races with nothing.

package elpstest

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// concurrentSharingProgram holds two independent maps, one per
// transaction, so a fork can be handed another fork's map without the two
// transactions ever touching one payload at the same time.
const concurrentSharingProgram = `
(set 'm (sorted-map "n" 0))
(set 'k (sorted-map "n" 0))
(set 'probe (list m k))
`

// concurrentSharingTx: transaction 0 writes m, transaction 1 writes k.
// Nothing writes what another transaction reads, so the walker below is a
// LEAK WITHOUT A DATA RACE -- which is the whole point.  A control that
// raced would be caught by -race and would prove nothing about the oracle,
// and it would take every other parallel test down with it (see
// SkipConcurrentArm's doc).
var concurrentSharingTx = []string{
	`(assoc! m "n" 1)`,
	`(assoc! k "n" 2)`,
}

// forkSharingBetweenConcurrentForks is faithful for every role except
// forkRoleConcurrent, where it hands every fork after the first that
// FIRST fork's `m`.  Nothing is shared with any template, so the
// sequential sweep, the census, the cell-view channel and parity all see a
// correct walker.
func forkSharingBetweenConcurrentForks() (func(*lisp.LEnv) (*lisp.LEnv, error), func(forkRole)) {
	var role forkRole
	var first *lisp.LVal
	fork := func(env *lisp.LEnv) (*lisp.LEnv, error) {
		f, err := env.Fork()
		if err != nil {
			return nil, err
		}
		if role != forkRoleConcurrent {
			return f, nil
		}
		if first == nil {
			first = f.GetGlobal(lisp.Symbol("m"))
			return f, nil
		}
		if rc := f.PutGlobal(lisp.Symbol("m"), first); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
		return f, nil
	}
	return fork, func(r forkRole) { role = r }
}

// TestGuardDetectsSharingBetweenConcurrentForksOnly is the measurement the
// concurrent arm's independence check was added for: before it, this
// walker produced ZERO witnesses.
func TestGuardDetectsSharingBetweenConcurrentForksOnly(t *testing.T) {
	t.Parallel()
	fork, onFork := forkSharingBetweenConcurrentForks()
	got, err := CheckTransactions(TransactionCheck{
		Program: concurrentSharingProgram,
		Tx:      concurrentSharingTx,
		Fork:    fork,
		onFork:  onFork,
		Repro:   "a fork walker that shares one map between two CONCURRENT-arm forks",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	const want = "a fork that ran its transaction concurrently holds what it holds when it runs alone"
	var found bool
	for _, w := range got {
		if w.Property == want {
			found = true
			continue
		}
		t.Errorf("a walker faithful on every SEQUENTIAL role was reported by another channel:\n%s", w)
	}
	if !found {
		t.Fatalf("two concurrent-arm forks share one *MapData and the oracle reported %d witness(es), "+
			"none of them the concurrent-arm independence property.\n"+
			"The arm is back to re-checking the template's fingerprint and nothing else, so a defect "+
			"confined to the concurrent forks is unobserved outside -race -- and this one does not "+
			"race: the fork that inherits the map never touches it while the other writes.\n"+
			"witnesses: %v", len(got), got)
	}
}

// The other half: a CORRECT walker must be silent on the same arm, or the
// check above proves only that the property is noisy.  The default Fork is
// used deliberately -- this is the shape every caller of CheckTransactions
// now runs through the arm.
func TestACorrectForkIsSilentOnTheConcurrentArm(t *testing.T) {
	t.Parallel()
	got, err := CheckTransactions(TransactionCheck{
		Program: concurrentSharingProgram,
		Tx:      concurrentSharingTx,
		Repro:   "the real Fork walker over the concurrent arm",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, w := range got {
		if strings.Contains(w.Property, "concurrently") {
			t.Errorf("the real Fork walker was reported by the concurrent-arm independence "+
				"property:\n%s\nA guard red on a correct walker is a guard that gets switched off.", w)
		}
	}
	if len(got) != 0 {
		t.Errorf("the real Fork walker produced %d witness(es): %v", len(got), got)
	}
}

// The reference forks the independence check replays on must arrive under
// their OWN role.  If they came through forkRoleConcurrent, a walker
// broken on that role would break the reference in the same way and the
// comparison could never fail -- the control above would pass on an oracle
// that compares two identically broken environments.
func TestTheSoloReplayForksHaveTheirOwnRole(t *testing.T) {
	t.Parallel()
	seen := map[forkRole]int{}
	_, err := CheckTransactions(TransactionCheck{
		Program: concurrentSharingProgram,
		Tx:      concurrentSharingTx,
		Fork:    func(env *lisp.LEnv) (*lisp.LEnv, error) { return env.Fork() },
		onFork:  func(r forkRole) { seen[r]++ },
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if seen[forkRoleConcurrentSolo] != len(concurrentSharingTx) {
		t.Errorf("the concurrent arm announced %d solo-replay fork(s), want one per transaction (%d).\n"+
			"roles seen: %v", seen[forkRoleConcurrentSolo], len(concurrentSharingTx), seen)
	}
	if seen[forkRoleConcurrent] != len(concurrentSharingTx) {
		t.Errorf("the concurrent arm announced %d concurrent fork(s), want %d: %v",
			seen[forkRoleConcurrent], len(concurrentSharingTx), seen)
	}
}
