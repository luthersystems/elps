// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"context"
	"time"
)

// TB is the part of *testing.T that Backstop reads.
type TB interface {
	Deadline() (time.Time, bool)
	Context() context.Context
}

// backstopMargin is the most a Backstop context ends before the test binary's
// -timeout.  It leaves time to report the failure and clean up.
const backstopMargin = 30 * time.Second

// Backstop returns a context for a wait whose regression is a hang that uses
// no CPU: a goroutine or child process blocked on input or on a signal.  CPU
// bounds (Within, Watch, RunChild, Guard) cannot see such a hang.
//
// The context ends a margin before t's deadline, the binary's -timeout.  The
// margin is backstopMargin or a quarter of the time left, whichever is less,
// so a short -timeout (-timeout=20s) still gets a live context (#791).  A
// correct run that reaches it would have hit the -timeout anyway, so
// starvation alone cannot fail a test that the -timeout would pass.  What it
// adds is the failure message: the hung test fails by name instead of the
// binary panicking with every goroutine's stack.  If the binary has no
// -timeout, or its deadline has already passed, the context ends with the
// test.
func Backstop(t TB) (context.Context, context.CancelFunc) {
	deadline, ok := t.Deadline()
	if !ok {
		return context.WithCancel(t.Context())
	}
	end, ok := backstopDeadline(time.Now(), deadline)
	if !ok {
		return context.WithCancel(t.Context())
	}
	return context.WithDeadline(t.Context(), end)
}

// backstopDeadline returns when a Backstop context made at now ends, for a
// test deadline of deadline.  It reports false when no time is left, so the
// caller never makes a context that has already expired.
func backstopDeadline(now, deadline time.Time) (time.Time, bool) {
	remaining := deadline.Sub(now)
	if remaining <= 0 {
		return time.Time{}, false
	}
	return deadline.Add(-min(backstopMargin, remaining/4)), true
}
