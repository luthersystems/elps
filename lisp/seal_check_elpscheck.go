// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
	"sync"
)

// Checked-mode AST inspector — the dynamic verifier for the seal invariant
// (issue #372).
//
// The copy-on-write seal design (lisp/seal.go) promises that the bytes of
// a sealed program node never change after parsing completes.  Issues
// #369 and #370 shipped past a manual audit and the elpsvet census, so
// that promise now gets checked mechanism-blind: this file records the
// canonical fingerprint (lisp/sealfp.go) of every sealed parse and
// re-verifies it later, so ANY code path that writes a sealed node in
// place is caught — no matter which mutator, builtin, or metadata stamp
// performed the write.
//
// # Choke points
//
//   - RECORD: (*LVal).SealAST, which the parser calls on every completed
//     top-level expression at nesting depth zero.  Every parse path —
//     Reader.Read, LoadString, ReadProgram/ParseProgram, the REPL — exits
//     through it, and embedders that hand-build and seal trees call it
//     too, so recording there covers all of them without touching any
//     individual entry point.
//   - RE-VERIFY, per load: (*LEnv).load, the funnel every Load* entry
//     point and LoadProgram* evaluates through, re-verifies that load's
//     own top-level expressions after evaluation finishes (error or not).
//     A corruption is therefore reported at the load that caused it, not
//     at end of suite.  The check panics — like an ownership violation, a
//     seal violation found in a checked build must stop the run, not
//     decorate its output.
//   - RE-VERIFY, at teardown: Runtime and LEnv have no destructor to hook
//     (they are garbage collected), so end-of-lifetime verification is
//     the process-lifecycle owner's call: VerifySealedASTs re-checks
//     every recorded root and is invoked by the lisp package's TestMain
//     after the suite runs, by elpstest.Runner at the end of each test
//     file, and is exported for embedders to call at their own teardown
//     points.
//
// # Memory
//
// The table holds a strong reference to every recorded root, so recorded
// trees are not collectable while the table holds them.  Growth is
// bounded: past sealCheckMaxRoots the table is VERIFIED and then dropped.
// Unlike the ownership table's plain reset (see ownership_check_
// elpscheck.go, which trades a detection gap for boundedness), the
// verify-before-drop means no recorded tree is ever forgotten
// unverified — the reset only shrinks the window in which a future
// corruption of an old tree can still be detected.
//
// # Cost
//
// Checked builds pay one fingerprint walk per sealed parse at record
// time, one per top-level load at re-verify time, and one per root at
// each full verification.  All of it compiles out under the default
// build tag (see seal_check_default.go): release binaries carry empty
// inlined hooks and zero bookkeeping.
var sealCheck = sealCheckState{roots: make(map[*LVal]uint64)}

// sealCheckMaxRoots bounds the table: past this many recorded roots the
// table is verified and dropped (see the Memory section above).  2^20
// top-level expressions is far beyond a unit-test process; a long fuzz
// or soak run cycles through verify-and-drop instead of growing without
// bound.
const sealCheckMaxRoots = 1 << 20

type sealCheckState struct {
	roots map[*LVal]uint64 // sealed root → fingerprint at seal time
	mu    sync.Mutex
}

// permanentSealRoots are the process-wide singletons (lisp/singleton.go),
// which are born sealed (issue #376) and registered here at package init —
// before any user code can run — as PERMANENT inspector roots.  Unlike the
// bounded roots table they are never part of a verify-and-drop cycle:
// their fingerprints are held for the life of the process and re-verified
// at every top-level load (verifySealedLoadRoots) and at every teardown
// verification (VerifySealedASTs), so a singleton corruption is caught at
// the load that caused it, not just at the value-drift checkpoints
// checkSingleton covers.  The slice is written once at init and read-only
// afterwards, so verification needs no lock.
var permanentSealRoots = func() []permanentSealRoot {
	roots := make([]permanentSealRoot, 0, 3)
	for _, s := range []struct {
		v    *LVal
		name string
	}{
		{singletonNil, "Nil()"},
		{singletonTrue, "Bool(true)"},
		{singletonFalse, "Bool(false)"},
	} {
		roots = append(roots, permanentSealRoot{
			v:    s.v,
			name: s.name,
			fp:   SealedASTFingerprint([]*LVal{s.v}),
		})
	}
	return roots
}()

type permanentSealRoot struct {
	v    *LVal
	name string
	fp   uint64 // fingerprint at package init, before any user code
}

// verifyPermanentSealRoots re-fingerprints the three singleton roots and
// panics on the first mismatch.  Called on every top-level load; the
// singletons are tiny (three nodes, no cells), so the cost is noise even
// in checked builds.
func verifyPermanentSealRoots(context string) {
	for _, r := range permanentSealRoots {
		if got := SealedASTFingerprint([]*LVal{r.v}); got != r.fp {
			panic(sealViolationMessage(r.v, r.fp, got,
				"permanent singleton root "+r.name+", "+context))
		}
	}
}

// recordSealedRoot stores the parse-time fingerprint of a freshly sealed
// tree.  Called from SealAST with the tree's root; unsealed values (an
// LError from the parser, a runtime value SealAST declined to mark) are
// ignored.  Re-sealing an already-recorded root is a no-op — sealing is
// idempotent and the fingerprint cannot legally have changed.
func recordSealedRoot(v *LVal) {
	if v == nil || !v.sealed {
		return
	}
	// Fingerprint outside the lock: the tree was sealed by this goroutine
	// and has not been shared yet, so the walk needs no synchronization.
	fp := SealedASTFingerprint([]*LVal{v})
	sealCheck.mu.Lock()
	defer sealCheck.mu.Unlock()
	if _, ok := sealCheck.roots[v]; ok {
		return
	}
	if len(sealCheck.roots) >= sealCheckMaxRoots {
		verifySealCheckTableLocked()
		sealCheck.roots = make(map[*LVal]uint64)
	}
	sealCheck.roots[v] = fp
}

// verifySealedLoadRoots re-verifies the sealed roots evaluated by one
// top-level load, immediately after that load finishes.  Called from
// (*LEnv).load on every path, including error returns — an evaluation
// error does not excuse corrupting the parse.  Panics on a mismatch so
// the offending load is named while it is still on the stack.
func verifySealedLoadRoots(exprs []*LVal) {
	verifyPermanentSealRoots("re-verified after the load that just finished")
	for i, e := range exprs {
		if e == nil || !e.sealed {
			continue
		}
		sealCheck.mu.Lock()
		want, ok := sealCheck.roots[e]
		sealCheck.mu.Unlock()
		if !ok {
			// Recorded before a verify-and-drop cycle, or sealed outside
			// SealAST's record point; nothing to compare against.
			continue
		}
		if got := SealedASTFingerprint([]*LVal{e}); got != want {
			panic(sealViolationMessage(e, want, got,
				fmt.Sprintf("expression %d of the load that just finished", i)))
		}
	}
}

// VerifySealedASTs re-fingerprints every sealed parse recorded in this
// process and reports the first mismatch (plus a count of the rest).  A
// non-nil error means a sealed program tree was mutated in place after
// parsing — the corruption class of luthersystems/substrate#378 — and the
// run's results cannot be trusted.
//
// It is called by the lisp package's TestMain after the suite completes
// and by elpstest.Runner at the end of each lisp test file; embedders
// running checked-mode CI can call it at their own teardown points.  In
// production builds (no elpscheck tag) it records nothing and always
// returns nil.
func VerifySealedASTs() error {
	sealCheck.mu.Lock()
	type entry struct {
		root *LVal
		want uint64
	}
	entries := make([]entry, 0, len(sealCheck.roots))
	for r, w := range sealCheck.roots {
		entries = append(entries, entry{root: r, want: w})
	}
	sealCheck.mu.Unlock()

	var (
		bad   int
		first string
	)
	// The permanent singleton roots are verified with the same lens and
	// reported through the same error, never dropped and never skipped.
	for _, r := range permanentSealRoots {
		if got := SealedASTFingerprint([]*LVal{r.v}); got != r.fp {
			bad++
			if first == "" {
				first = sealViolationMessage(r.v, r.fp, got,
					"permanent singleton root "+r.name+", recorded at package init")
			}
		}
	}
	for _, e := range entries {
		if got := SealedASTFingerprint([]*LVal{e.root}); got != e.want {
			bad++
			if first == "" {
				first = sealViolationMessage(e.root, e.want, got, "recorded at parse time")
			}
		}
	}
	if bad > 0 {
		return fmt.Errorf("%d of %d sealed roots were mutated in place; first:\n%s",
			bad, len(entries)+len(permanentSealRoots), first)
	}
	return nil
}

// verifySealCheckTableLocked verifies every recorded root before the
// table is dropped.  Caller holds sealCheck.mu.  Panics on a mismatch:
// this runs inside recordSealedRoot, deep in a parse, and a detected
// corruption must not be lost to the reset that is about to happen.
func verifySealCheckTableLocked() {
	for r, want := range sealCheck.roots {
		if got := SealedASTFingerprint([]*LVal{r}); got != want {
			panic(sealViolationMessage(r, want, got, "detected during table verify-and-drop"))
		}
	}
}

// sealCheckSampleRoots returns up to max recorded roots for the -race
// seal write watchdog (lisp/seal_watchdog_test.go).  Go's randomized map
// iteration makes each call a fresh sample, so over many calls the
// watchdog's coverage rotates through the whole table.
func sealCheckSampleRoots(limit int) []*LVal {
	sealCheck.mu.Lock()
	defer sealCheck.mu.Unlock()
	if limit <= 0 || len(sealCheck.roots) == 0 {
		return nil
	}
	if limit > len(sealCheck.roots) {
		limit = len(sealCheck.roots)
	}
	roots := make([]*LVal, 0, limit)
	for r := range sealCheck.roots {
		roots = append(roots, r)
		if len(roots) == limit {
			break
		}
	}
	return roots
}

// sealViolationMessage renders a seal violation without calling
// LVal.String() — rendering a corrupted (possibly cyclic) tree inside a
// failure path is how one panic becomes two.  Same policy as
// ownershipViolationMessage.
func sealViolationMessage(v *LVal, want, got uint64, context string) string {
	str := v.Str
	if len(str) > 64 {
		str = str[:64] + "..."
	}
	loc := "<no source>"
	if src, ok := v.Source(); ok {
		loc = src.String()
	}
	return fmt.Sprintf("sealcheck: sealed program AST mutated after parse (%s)\n"+
		"  root: type=%s str=%q cells=%d source=%s\n"+
		"  fingerprint at seal time: %016x\n"+
		"  fingerprint now:          %016x\n"+
		"sealed bytes must never change after parse; see lisp/seal.go and lisp/seal_check_elpscheck.go",
		context, v.Type, str, len(v.Cells), loc, want, got)
}
