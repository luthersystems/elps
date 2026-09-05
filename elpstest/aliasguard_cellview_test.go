// Copyright © 2026 The ELPS authors

// Controls for the cell-view channel (aliasguard_cellview.go): the Cells
// row of the walker-contract table asserted for Fork, and the reference
// walk that follows a live view's root (walkReachable).

package elpstest

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// cellViewProgram holds a view of every producing shape at load scope.
const cellViewProgram = `
(set 'l (list 30 10 20 40))
(set 'tail (cdr l))
(set 'sl (slice 'list l 1 3))
(set 'v (vector 3 1 2))
(set 'r (rest v))
(set 'w (append 'vector v))
`

var cellViewTx = []string{"(stable-sort < l) tail", "(stable-sort > v) r"}

func cellViewWitnessesOf(got []Witness) []Witness {
	var out []Witness
	for _, w := range got {
		if w.Property == CellViewProperty {
			out = append(out, w)
		}
	}
	return out
}

// TestGuardAssertsCellViewSharingAcrossFork: the real Fork satisfies the
// contract on every fresh fork and on the pristine successor, and the
// premise -- that the program actually holds live views -- is checked so
// the assertion cannot pass on a tree that records nothing.
func TestGuardAssertsCellViewSharingAcrossFork(t *testing.T) {
	t.Parallel()
	env, err := NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", cellViewProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	for _, name := range []string{"tail", "sl", "r"} {
		if _, _, ok := env.Get(lisp.Symbol(name)).CellView(); !ok {
			t.Fatalf("%s is not a live view; the premise of this test is wrong", name)
		}
	}
	if _, _, ok := env.Get(lisp.Symbol("w")).Cells[1].CellView(); !ok {
		t.Fatal("(append 'vector v)'s holder is not a live view; the premise of this test is wrong")
	}

	got, err := CheckTransactions(TransactionCheck{
		Program: cellViewProgram,
		Tx:      cellViewTx,
		Repro:   "TestGuardAssertsCellViewSharingAcrossFork",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	if ws := cellViewWitnessesOf(got); len(ws) != 0 {
		t.Fatalf("the real Fork failed the cell-view contract:\n%v", ws)
	}
	for _, w := range got {
		t.Errorf("unexpected witness: %s", w)
	}
}

// brokenForkDealiasesViews is the pre-#602 Fork: a faithful fork whose views
// are then rebuilt over private arrays, with no link.
func brokenForkDealiasesViews(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	for _, name := range []string{"tail", "sl", "r"} {
		v := f.Get(lisp.Symbol(name))
		private := lisp.QExpr(append([]*lisp.LVal(nil), v.Cells...))
		if rc := f.PutGlobal(lisp.Symbol(name), private); rc.Type == lisp.LError {
			return nil, lisp.GoError(rc)
		}
	}
	return f, nil
}

// brokenForkWindowsTheTemplatesRoot re-points the fork's `tail` at the
// TEMPLATE's list: a live link (slot identity holds against that root),
// pointing at memory the fork must not share.
func brokenForkWindowsTheTemplatesRoot(env *lisp.LEnv) (*lisp.LEnv, error) {
	f, err := env.Fork()
	if err != nil {
		return nil, err
	}
	troot := env.Get(lisp.Symbol("l"))
	leaky := lisp.QExpr(troot.Cells[1:len(troot.Cells):len(troot.Cells)])
	leaky.Native = troot // THE DEFECT: the template's root.
	leaky.Int = 1
	if rc := f.PutGlobal(lisp.Symbol("tail"), leaky); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return f, nil
}

// TestGuardDetectsAForkThatDealiasesViews is the negative control: a walker
// that copies the root and the view separately -- exactly what forker.val
// did before #602 -- must be reported at the view's binding, under the
// contract's property, with the view's path as the leak.
func TestGuardDetectsAForkThatDealiasesViews(t *testing.T) {
	t.Parallel()
	got, err := CheckTransactions(TransactionCheck{
		Program:           cellViewProgram,
		Tx:                cellViewTx,
		Fork:              brokenForkDealiasesViews,
		SkipConcurrentArm: true,
		Repro:             "a fork that rebuilds views over private arrays",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	ws := cellViewWitnessesOf(got)
	if len(ws) == 0 {
		t.Fatalf("a fork that de-aliased every view was NOT reported under %q.\n"+
			"That is the pre-#602 Fork, and the parity oracle measured it from pure ELPS; the\n"+
			"structural guard must now see it too.\nwitnesses: %v", CellViewProperty, got)
	}
	w := ws[0]
	if w.Leak != "user:tail" && !strings.Contains(w.Detail, "user:tail") {
		t.Errorf("the witness does not name the de-aliased view:\n%s", w)
	}
	if !strings.Contains(w.Detail, "private array") {
		t.Errorf("the witness does not say what happened to the view:\n%s", w)
	}
	t.Logf("detected:\n%s", w)
}

// TestGuardDetectsAForkThatWindowsTheTemplatesRoot: the other half of the
// property.  A fork whose view is a LIVE view of the template's root passes
// "is the fork's view live" and must fail "is its root the fork's own".
func TestGuardDetectsAForkThatWindowsTheTemplatesRoot(t *testing.T) {
	t.Parallel()
	got, err := CheckTransactions(TransactionCheck{
		Program:           cellViewProgram,
		Tx:                cellViewTx,
		Fork:              brokenForkWindowsTheTemplatesRoot,
		SkipConcurrentArm: true,
		Repro:             "a fork whose view windows the template's root",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	ws := cellViewWitnessesOf(got)
	if len(ws) == 0 {
		t.Fatalf("a fork whose view windows the TEMPLATE's root was NOT reported under %q.\n"+
			"Slot identity holds against that root, so only the 'is the root the fork's own' half\n"+
			"can see it.\nwitnesses: %v", CellViewProperty, got)
	}
	if !strings.Contains(ws[0].Detail, "TEMPLATE") || !strings.Contains(ws[0].Detail, "user:tail") {
		t.Errorf("the witness does not say the view windows template memory at user:tail:\n%s", ws[0])
	}
	t.Logf("detected:\n%s", ws[0])
}

// TestReachableWalkFollowsALiveRoot pins SWAP 2 (fingerprint.go's
// isCellViewLink doc): a live view's root is reachable state, so a payload
// the root holds OUTSIDE the view's window is reachable through the view --
// and a stale link is not followed.
func TestReachableWalkFollowsALiveRoot(t *testing.T) {
	t.Parallel()
	env := payloadKeyEnv(t)
	probe := &payloadKeyProbe{n: 4}
	root := lisp.QExpr([]*lisp.LVal{lisp.Native(probe), lisp.Int(2), lisp.Int(3)})
	live := lisp.QExpr(root.Cells[1:len(root.Cells):len(root.Cells)])
	live.Native = root
	live.Int = 1
	stale := lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)})
	stale.Native = root
	stale.Int = 1 // element 0 is not root.Cells[1]: a link that no longer describes the header
	env.PutGlobal(lisp.Symbol("live"), live)
	env.PutGlobal(lisp.Symbol("stale"), stale)

	if _, _, ok := live.CellView(); !ok {
		t.Fatal("the live fixture does not resolve; the premise of this test is wrong")
	}
	if _, _, ok := stale.CellView(); ok {
		t.Fatal("the stale fixture resolves; the premise of this test is wrong")
	}
	paths := censusPaths(env)
	if _, ok := paths["user:live/root/0"]; !ok {
		t.Errorf("the reachable walk did not follow a live view's link to its root: the payload the\n"+
			"root holds outside the view's window is invisible to the census. census: %v", paths)
	}
	for path := range paths {
		if strings.HasPrefix(path, "user:stale/") {
			t.Errorf("the reachable walk followed a STALE link: %s. Fork copies such a header privately\n"+
				"and does not follow it either; the guard must agree by the same call.", path)
		}
	}
}

// dealiasingOn returns a Fork substitute that de-aliases the views on
// exactly the calls `when` selects (1-based call number), and is the real
// Fork otherwise.  CheckTransactions takes len(Tx) forks before running the
// transactions and then ONE more for the pristine successor, so calls
// 1..len(Tx) are the fresh forks and call len(Tx)+1 is the successor.
func dealiasingOn(when func(call int) bool) func(*lisp.LEnv) (*lisp.LEnv, error) {
	call := 0
	return func(env *lisp.LEnv) (*lisp.LEnv, error) {
		call++
		if when(call) {
			return brokenForkDealiasesViews(env)
		}
		return env.Fork()
	}
}

// TestGuardDetectsDealiasingOnFreshForksOnly pins the fresh-fork hook: a
// walker faithful on the pristine successor and de-aliasing on every
// earlier fork is visible ONLY to the check on fresh forks.
func TestGuardDetectsDealiasingOnFreshForksOnly(t *testing.T) {
	t.Parallel()
	nTx := len(cellViewTx)
	got, err := CheckTransactions(TransactionCheck{
		Program:           cellViewProgram,
		Tx:                cellViewTx,
		Fork:              dealiasingOn(func(call int) bool { return call <= nTx }),
		SkipConcurrentArm: true,
		Repro:             "a fork walker that de-aliases views on the fresh forks only",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	ws := cellViewWitnessesOf(got)
	if len(ws) == 0 {
		t.Fatalf("de-aliased views on the FRESH forks (the successor faithful) were not reported.\n"+
			"Only the fresh-fork check can see this shape.\nwitnesses: %v", got)
	}
	for _, w := range ws {
		if strings.Contains(w.Detail, "successor") {
			t.Errorf("the faithful successor was reported:\n%s", w)
		}
	}
}

// TestGuardDetectsDealiasingOnTheSuccessorOnly pins the successor hook: a
// walker faithful on every fresh fork and de-aliasing on the pristine
// successor is visible ONLY to the check on the successor.
func TestGuardDetectsDealiasingOnTheSuccessorOnly(t *testing.T) {
	t.Parallel()
	nTx := len(cellViewTx)
	got, err := CheckTransactions(TransactionCheck{
		Program:           cellViewProgram,
		Tx:                cellViewTx,
		Fork:              dealiasingOn(func(call int) bool { return call == nTx+1 }),
		SkipConcurrentArm: true,
		Repro:             "a fork walker that de-aliases views on the pristine successor only",
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	ws := cellViewWitnessesOf(got)
	if len(ws) == 0 {
		t.Fatalf("de-aliased views on the PRISTINE SUCCESSOR (every fresh fork faithful) were not\n"+
			"reported. Only the successor check can see this shape.\nwitnesses: %v", got)
	}
	for _, w := range ws {
		if !strings.Contains(w.Detail, "successor") {
			t.Errorf("a faithful fresh fork was reported:\n%s", w)
		}
	}
}
