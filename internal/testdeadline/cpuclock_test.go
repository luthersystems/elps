// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"errors"
	"os"
	"os/exec"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

// spin burns CPU until stop is set.
func spin(stop *atomic.Bool) {
	for !stop.Load() {
	}
}

// TestWatchStopsASpin is the property every caller relies on: a call that
// does not return is reported once it has used the budget.
func TestWatchStopsASpin(t *testing.T) {
	var stop atomic.Bool
	defer stop.Store(true)
	const budget = 200 * time.Millisecond
	used, reason := watch(budget, 0, func() { spin(&stop) })
	if reason == "" {
		t.Fatal("watch returned success for a call that never returned")
	}
	if used <= budget {
		t.Fatalf("watch fired after %v, before the %v budget was used", used, budget)
	}
	if !strings.Contains(reason, "CPU time") {
		t.Fatalf("reason does not name the CPU budget: %q", reason)
	}
}

// TestWatchReportsHeapGrowth keeps the heap bound Watch had before it
// measured CPU time.
func TestWatchReportsHeapGrowth(t *testing.T) {
	release := make(chan struct{})
	defer close(release)
	var held atomic.Pointer[[]byte]
	_, reason := watch(time.Hour, 16<<20, func() {
		b := make([]byte, 256<<20)
		for i := range b {
			b[i] = 1
		}
		held.Store(&b)
		<-release
	})
	if !strings.Contains(reason, "heap grew") {
		t.Fatalf("watch did not report 256 MB of heap growth over a 16 MB limit: %q", reason)
	}
}

// TestWithinReturnsFast pins the success path and the CPU it reports.
func TestWithinReturnsFast(t *testing.T) {
	used, ok := Within(time.Minute, func() {})
	if !ok {
		t.Fatal("an empty call exceeded a one-minute budget")
	}
	if used < 0 || used > time.Minute {
		t.Fatalf("implausible CPU time for an empty call: %v", used)
	}
}

// childEnv selects what TestRunChildHelper does when the test binary is
// re-executed as a child.
const childEnv = "ELPS_TESTDEADLINE_CHILD"

// TestRunChildHelper is the child process for the RunChild tests.  It does
// nothing when run directly.
func TestRunChildHelper(t *testing.T) {
	switch os.Getenv(childEnv) {
	case "spin":
		var stop atomic.Bool
		spin(&stop)
	case "sleep":
		time.Sleep(2 * time.Second)
	}
}

func child(t *testing.T, mode string) *exec.Cmd {
	//nolint:gosec // Re-execute this test binary.
	cmd := exec.CommandContext(t.Context(), os.Args[0], "-test.run=^TestRunChildHelper$")
	cmd.Env = append(os.Environ(), childEnv+"="+mode)
	return cmd
}

// TestRunChildKillsASpin: a child that never exits is killed once it has used
// its CPU budget.
func TestRunChildKillsASpin(t *testing.T) {
	out, err := runChild(child(t, "spin"), time.Second)
	if !errors.Is(err, ErrOverBudget) {
		t.Fatalf("a spinning child was not killed on its CPU budget: %v\n%s", err, out)
	}
}

// TestGuardKillsASpin: Guard stops a started child that never exits.
func TestGuardKillsASpin(t *testing.T) {
	cmd := child(t, "spin")
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	stop := guard(cmd.Process, time.Second)
	_ = cmd.Wait()
	if used, killed := stop(); !killed || used <= time.Second {
		t.Fatalf("guard did not kill a spinning child on its budget: killed=%v used=%v", killed, used)
	}
}

// TestBackstopEndsBeforeTheTestDeadline: with a -timeout, the backstop ends
// at most backstopMargin before it and is still live, even under a short
// -timeout such as 20s (#791); without one, it has no deadline.
func TestBackstopEndsBeforeTheTestDeadline(t *testing.T) {
	ctx, cancel := Backstop(t)
	defer cancel()
	got, hasBackstop := ctx.Deadline()
	want, hasTimeout := t.Deadline()
	if hasBackstop != hasTimeout {
		t.Fatalf("backstop deadline %v, test deadline %v", hasBackstop, hasTimeout)
	}
	if err := ctx.Err(); err != nil {
		t.Fatalf("backstop context already done: %v", err)
	}
	if hasTimeout && (got.After(want) || got.Before(want.Add(-backstopMargin))) {
		t.Fatalf("backstop ends at %v, want within %v before %v", got, backstopMargin, want)
	}
}
