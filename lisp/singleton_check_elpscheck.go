// Copyright © 2025 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
)

// initSnapshot is the bit-pattern of all three singleton LVals captured
// at package init time, before any user code can mutate them.
var initSnapshot SingletonSnapshot

func init() {
	initSnapshot = TakeSingletonSnapshot()
}

// checkSingleton verifies that all three singleton LVals are
// bit-identical to their init-time snapshot. Called from Bool() and
// Nil() on every invocation — so any mutation is detected at the next
// read of any singleton, localizing the corruption to within a handful
// of evaluations of the offending write.
//
// Cost: three struct-field compares per call. Only enabled under the
// `elpscheck` build tag.
//
// # What this does NOT catch
//
// This is a value-drift detector, not a write detector. It compares
// field values against the snapshot, so a write that stores the value a
// field ALREADY HOLDS is invisible to it — and so is the same write in
// SingletonSnapshot.Verify, which shares the comparison.
//
// That gap is not theoretical. Issue #333 was `s.quoted = false`
// executed against singletonNil, whose Quoted was already false:
//
//	s = builtinConcat(nil, s) // returns Nil() when the result is empty
//	s.quoted = false
//
// A same-value write is still a write to shared memory, and it still
// races with every concurrent reader — in #333, with `if v.quoted` in
// (*LEnv).eval. Neither this function nor the "Singleton integrity
// (elpscheck)" CI job could ever have seen it, no matter how often they
// ran. Only `go test -race` did, and only on a loaded runner where two
// goroutines happened to overlap.
//
// Closing the gap here is not possible cheaply. A bool field has no
// spare bit patterns to poison, so there is no value we could store that
// a same-value write would disturb, and detecting the write itself needs
// either memory protection (mmap + mprotect: unsafe, platform-specific,
// and it breaks the tests that mutate singletons on purpose) or the race
// detector.
//
// So the coverage is split, and you should know which guard covers what:
//
//   - value drift, any build, cheap: this function + Verify.
//   - unsynchronized writes including same-value ones: `make race`, plus
//     the write watchdog in lisp/singleton_watchdog_test.go, which keeps
//     an unsynchronized read of all three singletons live for the whole
//     package test run so such a write races deterministically instead
//     of only when goroutines collide by luck.
//   - a specific helper handing a singleton out to a caller that will
//     mutate it: an identity assertion in a plain test, e.g.
//     TestLambdaVarsDoesNotReturnSingleton.
//
// If you are adding a mutation guard, prefer removing the mutation. The
// codebase's copy-on-write helpers (Quote, Splice, shallowUnquote) exist
// so that no caller has to reason about whether a value is shared.
//
// NOT LISP-REACHABLE (#367): a deliberate corruption detector, compiled only
// under the `elpscheck` build tag, whose trigger is elps's own code having
// written to shared memory. It stays a panic on purpose -- turning it into a
// condition would let the corrupted process keep running.
func checkSingleton(_ *LVal) {
	if drift := initSnapshot.Verify(); drift != "" {
		panic(fmt.Sprintf("singleton corruption detected: %s mutated\n  current Nil=%+v\n  current True=%+v\n  current False=%+v",
			drift, singletonNil, singletonTrue, singletonFalse))
	}
}
