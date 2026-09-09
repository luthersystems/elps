// Copyright © 2026 The ELPS authors

package lisp_test

import "testing"

// The backstop collapse must be narrow: it exists so that two arms stopped by
// DIFFERENT budgets on the same runaway loop agree, and for nothing else.  A
// collapse that reached any further would blind FuzzSharedTreeEval to the
// divergences it exists to find, which is a worse outcome than the flake it
// removes (crasher 423b7dd9e421bd27).
func TestOnlyTwoBackstopsCollapse(t *testing.T) {
	const (
		iter     = "fuzz:1:28: s: tail-call iteration limit exceeded: 100001 (a single tail-recursive loop ran this many turns)"
		deadline = "fuzz:1:11: context-cancelled: context cancelled: context deadline exceeded"
		other    = "fuzz:1:11: context-cancelled: context cancelled: some other cause"
		value    = "'(1 2 3)"
		raised   = "fuzz:1:4: user error: rate limit exceeded"
	)
	for _, tt := range []struct {
		name     string
		a, b     string
		collapse bool
	}{
		{"the two budgets, either order", iter, deadline, true},
		{"the two budgets, reversed", deadline, iter, true},
		{"the same budget twice", iter, iter, true},
		// The asymmetric cases: one arm stopped by a budget and the other
		// not.  Scheduling does not explain those, so they stay divergences.
		{"a budget against a value", iter, value, false},
		{"a budget against an ordinary error", deadline, other, false},
		{"a value against a budget", value, deadline, false},
		// A program is free to RAISE an error whose text mentions a limit.
		// That is the program's meaning, not a backstop.
		{"a program-raised error naming a limit", raised, iter, false},
		{"two ordinary errors", other, value, false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			if got := bothHitAResourceBackstop(tt.a, tt.b); got != tt.collapse {
				t.Fatalf("collapse=%v, want %v\n  a: %s\n  b: %s", got, tt.collapse, tt.a, tt.b)
			}
		})
	}
}
