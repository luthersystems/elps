// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"os"
	"testing"
	"time"
)

// The two tests below are the reason for this package's CPU clock (#789).  A
// call that waits uses wall time but no CPU, which is what a starved process
// looks like to the code under test.  A wall-clock bound fails both; a CPU
// bound fails neither.  They are Linux-only because elsewhere the clock falls
// back to wall time.

// TestWatchDoesNotChargeWaiting: 500ms of wall time against a 100ms budget.
func TestWatchDoesNotChargeWaiting(t *testing.T) {
	used, reason := watch(100*time.Millisecond, 0, func() { time.Sleep(500 * time.Millisecond) })
	if reason != "" {
		t.Fatalf("watch charged wall time to the call: %s", reason)
	}
	if used >= 100*time.Millisecond {
		t.Fatalf("a sleeping call was charged %v of CPU time", used)
	}
}

// TestRunChildDoesNotChargeWaiting: a child that sleeps 2s against a 1s
// budget completes.
func TestRunChildDoesNotChargeWaiting(t *testing.T) {
	out, err := runChild(child(t, "sleep"), time.Second)
	if err != nil {
		t.Fatalf("RunChild charged wall time to the child: %v\n%s", err, out)
	}
}

// TestReadStatCPU parses a stat line whose command name holds spaces and
// parentheses, which is why fields are counted from the last ')'.
func TestReadStatCPU(t *testing.T) {
	// Fields 14 and 15 (utime, stime) are 150 and 50 ticks: 2s at 100 Hz.
	line := "1234 (a ) b (c) S 1 1234 1234 0 -1 4194304 100 0 0 0 150 50 0 0 20 0 1 0 1 1 1\n"
	path := t.TempDir() + "/stat"
	if err := os.WriteFile(path, []byte(line), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err := readStatCPU(path)
	if err != nil {
		t.Fatal(err)
	}
	if got != 2*time.Second {
		t.Fatalf("readStatCPU = %v, want 2s", got)
	}
}
