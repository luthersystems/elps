// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/fuzzwatch"
)

// TestAwaitSharedTreeFixedInputNeverSkips pins what an Inconclusive watchdog
// does to a regression test: it waits for the evaluation and reports a
// verdict, and it does not skip.  A skip would let TestSharedTreeSeedsAgree
// and the oracle tests pass on a starved runner without asserting anything
// (#791).
//
// The check stands in for a budget that is starved throughout.  It closes
// done as it returns, so the evaluation finishes after the watchdog gave up,
// and nothing here depends on how fast this process runs.
func TestAwaitSharedTreeFixedInputNeverSkips(t *testing.T) {
	t.Parallel()
	done := make(chan struct{})
	checks := 0
	inconclusive := func() fuzzwatch.CheckResult {
		checks++
		close(done)
		return fuzzwatch.CheckResult{Verdict: fuzzwatch.Inconclusive}
	}

	var sub *testing.T
	var skipped string
	t.Run("fixed", func(t *testing.T) {
		sub = t
		skipped = awaitSharedTree(t, done, fuzzwatch.Fixed, 0, inconclusive, "")
	})
	if sub.Skipped() {
		t.Fatal("a fixed input was skipped on an Inconclusive verdict")
	}
	if sub.Failed() {
		t.Fatal("a fixed input failed although its evaluation finished")
	}
	if skipped != "" {
		t.Fatalf("awaitSharedTree returned %q, want \"\" for a finished evaluation", skipped)
	}
	if checks != 1 {
		t.Fatalf("the watchdog was checked %d times, want 1: the Inconclusive path did not run", checks)
	}
}
