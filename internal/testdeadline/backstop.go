// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"context"
	"testing"
	"time"
)

// backstopMargin is how long before the test binary's -timeout a Backstop
// context ends.  It leaves time to report the failure and clean up.
const backstopMargin = 30 * time.Second

// Backstop returns a context for a wait whose regression is a hang that uses
// no CPU: a goroutine or child process blocked on input or on a signal.  CPU
// bounds (Within, Watch, RunChild, Guard) cannot see such a hang.
//
// The context ends backstopMargin before t's deadline, the binary's -timeout.
// A correct run that reaches it would have hit the -timeout anyway, so
// starvation alone cannot fail a test that the -timeout would pass.  What it
// adds is the failure message: the hung test fails by name instead of the
// binary panicking with every goroutine's stack.  If the binary has no
// -timeout, the context ends with the test.
func Backstop(t *testing.T) (context.Context, context.CancelFunc) {
	deadline, ok := t.Deadline()
	if !ok {
		return context.WithCancel(t.Context())
	}
	return context.WithDeadline(t.Context(), deadline.Add(-backstopMargin))
}
