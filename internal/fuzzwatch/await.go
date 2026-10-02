// Copyright © 2026 The ELPS authors

package fuzzwatch

import (
	"context"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
)

// Input says where a harness's input came from.  It decides what an
// Inconclusive verdict does; see [AwaitStarved].
type Input int

const (
	// Fuzzed is a generated input, or a seed of a fuzz target.  An
	// Inconclusive verdict skips it: the fuzzer runs again and re-finds a
	// real hang.
	Fuzzed Input = iota
	// Fixed is a regression test's own input.  It never skips.
	Fixed
)

func (in Input) String() string {
	if in == Fixed {
		return "fixed"
	}
	return "fuzzed"
}

// T is the part of *testing.T that AwaitStarved uses.
type T interface {
	Helper()
	Logf(format string, args ...any)
	Skipf(format string, args ...any)
	Fatalf(format string, args ...any)
	Deadline() (time.Time, bool)
	Context() context.Context
}

// AwaitStarved is what a harness does when its watchdog returns Inconclusive.
// ch is the channel that carries the result of the work.  report is the
// verdict's Report.  what names the work at the start of a failure message,
// and describe ends it (for example, the source under test).
//
// It returns the result and true, or false after it skipped or failed t.
//
// A Fuzzed input skips.  A Fixed input is a regression test, and a skip would
// let it pass without asserting anything (luthersystems/elps#792).  So it
// waits on ch, up to testdeadline.Backstop, and fails if no result arrives.
// Starvation alone cannot fail it, because a run that reaches the backstop
// would hit the binary's -timeout anyway.
func AwaitStarved[V any](t T, ch <-chan V, input Input, report Report, what, describe string) (V, bool) {
	t.Helper()
	var zero V
	if input != Fixed {
		t.Skipf("no verdict: the process was starved throughout (%s)", report)
		return zero, false
	}
	t.Logf("the process was starved throughout (%s); a fixed input waits for the -timeout backstop", report)
	backstop, stop := testdeadline.Backstop(t)
	defer stop()
	select {
	case v := <-ch:
		return v, true
	case <-backstop.Done():
		t.Fatalf("%s did not terminate before the test's -timeout (%s)%s", what, report, describe)
		return zero, false
	}
}
