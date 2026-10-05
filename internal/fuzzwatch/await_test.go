// Copyright © 2026 The ELPS authors

package fuzzwatch

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"
)

// recordT is a T that records Skipf and Fatalf instead of ending the test.
// AwaitStarved returns after each of them, so nothing here needs Goexit.
type recordT struct {
	ctx      context.Context
	deadline time.Time
	skipped  string
	failed   string
}

func (r *recordT) Helper()                  {}
func (r *recordT) Logf(string, ...any)      {}
func (r *recordT) Context() context.Context { return r.ctx }
func (r *recordT) Deadline() (time.Time, bool) {
	return r.deadline, !r.deadline.IsZero()
}
func (r *recordT) Skipf(format string, args ...any) { r.skipped = fmt.Sprintf(format, args...) }
func (r *recordT) Fatalf(format string, args ...any) {
	r.failed = fmt.Sprintf(format, args...)
}

// TestAwaitStarvedSkipsAFuzzedInput pins the fuzzed rule: skip, and do not
// wait for the work.
func TestAwaitStarvedSkipsAFuzzedInput(t *testing.T) {
	t.Parallel()
	rt := &recordT{ctx: t.Context()}
	// A result that is ready must not be taken: a fuzzed input does not wait.
	ch := make(chan int, 1)
	ch <- 1
	if _, ok := AwaitStarved(rt, ch, StarvedInput{Input: Fuzzed, Report: Report{}, What: "the work", Describe: ""}); ok {
		t.Fatal("a fuzzed input returned a result after an Inconclusive verdict")
	}
	if rt.skipped == "" || rt.failed != "" {
		t.Fatalf("a fuzzed input: skipped %q, failed %q; want a skip only", rt.skipped, rt.failed)
	}
}

// TestAwaitStarvedFixedInputWaitsForTheResult pins the fixed rule when the
// work finishes: the result comes back, and t neither skips nor fails.
func TestAwaitStarvedFixedInputWaitsForTheResult(t *testing.T) {
	t.Parallel()
	rt := &recordT{ctx: t.Context()}
	ch := make(chan int, 1)
	go func() { ch <- 42 }()
	got, ok := AwaitStarved(rt, ch, StarvedInput{Input: Fixed, Report: Report{}, What: "the work", Describe: ""})
	if !ok || got != 42 {
		t.Fatalf("AwaitStarved returned (%d, %v), want (42, true)", got, ok)
	}
	if rt.skipped != "" || rt.failed != "" {
		t.Fatalf("a fixed input that finished: skipped %q, failed %q; want neither", rt.skipped, rt.failed)
	}
}

// TestAwaitStarvedFixedInputFailsAtTheBackstop pins the fixed rule when the
// work never finishes: t fails at the backstop and does not skip.  The fake
// deadline is 200ms away, so the backstop ends 50ms before it.
func TestAwaitStarvedFixedInputFailsAtTheBackstop(t *testing.T) {
	t.Parallel()
	rt := &recordT{ctx: t.Context(), deadline: time.Now().Add(200 * time.Millisecond)}
	if _, ok := AwaitStarved(rt, make(chan int), StarvedInput{Input: Fixed, Report: Report{}, What: "the work", Describe: ""}); ok {
		t.Fatal("AwaitStarved returned a result from a channel that never sends")
	}
	if rt.skipped != "" {
		t.Fatalf("a fixed input skipped: %q", rt.skipped)
	}
	if !strings.Contains(rt.failed, BackstopExpired) {
		t.Fatalf("a fixed input whose work never finished: failed %q, want a message containing %q",
			rt.failed, BackstopExpired)
	}
}

// TestAwaitStarvedFixedInputPrefersAReadyResult pins that a result ready when
// the backstop ends is taken, not failed: select picks at random between two
// ready cases, so AwaitStarved looks at the channel again before it fails.
func TestAwaitStarvedFixedInputPrefersAReadyResult(t *testing.T) {
	t.Parallel()
	ctx, cancel := context.WithCancel(t.Context())
	cancel()
	for i := range 200 {
		rt := &recordT{ctx: ctx}
		ch := make(chan int, 1)
		ch <- i
		got, ok := AwaitStarved(rt, ch, StarvedInput{Input: Fixed, Report: Report{}, What: "the work", Describe: ""})
		if !ok || got != i || rt.failed != "" {
			t.Fatalf("iteration %d: AwaitStarved returned (%d, %v), failed %q; want (%d, true) and no failure",
				i, got, ok, rt.failed, i)
		}
	}
}

func TestInputString(t *testing.T) {
	t.Parallel()
	if Fuzzed.String() != "fuzzed" || Fixed.String() != "fixed" {
		t.Fatalf("Input.String: %q, %q", Fuzzed, Fixed)
	}
}
