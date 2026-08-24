// Copyright © 2025 The ELPS authors

package lisp

import (
	"fmt"
	"os"
	"testing"
)

// TestMain wraps the package test suite with five complementary guards over
// the state this process shares between every environment it builds.
//
// The singleton snapshot detects a singleton whose *value* differs at the
// end of the run from the start — a tree-walker that failed to guard
// against them, say. See issue #274.
//
// The write watchdog covers what the snapshot structurally cannot: a
// write that stores the value the field already held. Issue #333 was
// one of those, and only `-race` ever saw it, and only when two
// goroutines happened to collide. The watchdog makes any unsynchronized
// write to a singleton race deterministically under `make race`. See
// singleton_watchdog_test.go for its scope and limits.
//
// The builtin-formals snapshot extends the first check from the three
// singletons to the other process-wide shared structure: the formals lists
// in the builtin TABLES, which every environment copies out of.  A test that
// reaches one through a function value and writes to it corrupts every
// environment built afterwards, in this process, in any later test — and
// issue #398 is exactly that, arriving from a fuzz target's own seed corpus
// with every assertion in the package satisfied.  The templates are sealed,
// and since the sharing work (registrationFormals in env.go; issues #379,
// #514) every environment ALIASES the sealed template rather than carrying a
// private copy — so a write that gets past the copy-on-write guards corrupts
// every environment at once, and the snapshot is what says whether one did.
// See builtin_formals_test.go.
//
// The seal write watchdog and the sealed-AST verification extend the same
// two-guard pattern to sealed program trees (issue #372): the watchdog
// makes any unsynchronized write to a watched sealed node race
// deterministically under `-race` (see seal_watchdog_test.go), and
// VerifySealedASTs re-fingerprints every sealed parse recorded by the
// checked-mode inspector — a value-drift check with the same role the
// snapshot Verify plays for singletons.  Without the elpscheck tag the
// inspector records nothing and the verification is a free nil.
func TestMain(m *testing.M) {
	snap := TakeSingletonSnapshot()
	formals := takeBuiltinFormalsSnapshot()
	stopWatchdog := startSingletonWriteWatchdog()
	stopSealWatchdog := startSealWriteWatchdog()
	code := m.Run()
	stopSealWatchdog()
	stopWatchdog()
	if drift := snap.Verify(); drift != "" {
		fmt.Fprintf(os.Stderr,
			"FATAL: singleton %s was mutated during test run\n  Nil=%+v\n  True=%+v\n  False=%+v\n",
			drift, singletonNil, singletonTrue, singletonFalse)
		if code == 0 {
			code = 1
		}
	}
	if drift := formals.verify(); drift != "" {
		fmt.Fprintf(os.Stderr,
			"FATAL: the process-wide builtin formals were mutated during the test run;\n"+
				"every LEnv built after the write saw the mutated list:\n%s", drift)
		if code == 0 {
			code = 1
		}
	}
	if err := VerifySealedASTs(); err != nil {
		fmt.Fprintf(os.Stderr, "FATAL: sealed AST verification failed at end of test run\n%v\n", err)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}
