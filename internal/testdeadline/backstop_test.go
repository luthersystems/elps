// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"testing"
	"time"
)

// TestBackstopDeadlineScalesMargin pins the margin: at most backstopMargin,
// and a quarter of the time left when that is less.  A fixed 30s margin
// made a context that had already expired under -timeout=20s (#791).
func TestBackstopDeadlineScalesMargin(t *testing.T) {
	t.Parallel()
	now := time.Unix(1_000_000, 0)
	for _, tc := range []struct {
		remaining time.Duration
		margin    time.Duration
	}{
		{10 * time.Minute, backstopMargin},
		{2 * time.Minute, backstopMargin},
		{100 * time.Second, 25 * time.Second},
		{20 * time.Second, 5 * time.Second},
		{time.Second, 250 * time.Millisecond},
		{time.Nanosecond, 0},
	} {
		deadline := now.Add(tc.remaining)
		end, ok := backstopDeadline(now, deadline)
		if !ok {
			t.Errorf("remaining %v: no backstop, want one", tc.remaining)
			continue
		}
		if !end.After(now) {
			t.Errorf("remaining %v: backstop ends at %v, not after now %v", tc.remaining, end, now)
		}
		if got := deadline.Sub(end); got != tc.margin {
			t.Errorf("remaining %v: margin %v, want %v", tc.remaining, got, tc.margin)
		}
	}
}

// TestBackstopDeadlinePassed covers a test deadline at or before now: there
// is no live deadline to give, so backstopDeadline reports false and
// Backstop falls back to the test's own context.
func TestBackstopDeadlinePassed(t *testing.T) {
	t.Parallel()
	now := time.Unix(1_000_000, 0)
	for _, deadline := range []time.Time{now, now.Add(-time.Second)} {
		if end, ok := backstopDeadline(now, deadline); ok {
			t.Errorf("deadline %v before now %v: backstop ends at %v, want none", deadline, now, end)
		}
	}
}
