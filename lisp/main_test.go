// Copyright © 2025 The ELPS authors

package lisp

import (
	"fmt"
	"os"
	"testing"
)

// TestMain wraps the package test suite with two complementary
// singleton guards.
//
// The snapshot check detects a singleton whose *value* differs at the
// end of the run from the start — a tree-walker that failed to guard
// against them, say. See issue #274.
//
// The write watchdog covers what the snapshot structurally cannot: a
// write that stores the value the field already held. Issue #333 was
// one of those, and only `-race` ever saw it, and only when two
// goroutines happened to collide. The watchdog makes any unsynchronized
// write to a singleton race deterministically under `make race`. See
// singleton_watchdog_test.go for its scope and limits.
func TestMain(m *testing.M) {
	snap := TakeSingletonSnapshot()
	stopWatchdog := startSingletonWriteWatchdog()
	code := m.Run()
	stopWatchdog()
	if drift := snap.Verify(); drift != "" {
		fmt.Fprintf(os.Stderr,
			"FATAL: singleton %s was mutated during test run\n  Nil=%+v\n  True=%+v\n  False=%+v\n",
			drift, singletonNil, singletonTrue, singletonFalse)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}
