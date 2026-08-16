// Copyright © 2026 The ELPS authors

package fuzzwatch

import (
	"os"
	"strconv"
	"strings"
	"testing"
	"time"
)

// TestCPUShareProbe is the reproduction harness for #453. It is SKIPPED by
// default: it needs deliberate external CPU contention to say anything, and
// under CI's own load it would be a coin flip either way.
//
// To run it, load the machine and set the variable:
//
//	for i in $(seq 1 200); do ( while :; do :; done ) & done
//	FUZZWATCH_SHARE_PROBE=1 go test -run TestCPUShareProbe -v ./internal/fuzzwatch/
//	kill %1 %2 ... # or: pkill -f 'while :'
//
// What it demonstrates, measured on the 4-core sandbox with 200 spinners:
//
//	A sleep-window probe (the #453 probe)
//	  wall=3.003s scheduled=3.003s lost=0s longest=0s
//	B cpu-bound probe
//	  wall to complete=17.375s (169ms unloaded)  cpu=0.150s  share=0.9%
//	  fuzzwatch says: scheduled=17.357s lost=300ms
//
// A 103x slowdown at 0.9% CPU share, reported as seventeen seconds of normal
// scheduled execution. That is the boundary the package doc describes, and the
// reason MinHonestBudget exists. This test asserts NOTHING -- the numbers
// depend entirely on the load you apply -- it only prints, so that the next
// person can re-derive the boundary instead of taking the doc's word for it.
func TestCPUShareProbe(t *testing.T) {
	if os.Getenv("FUZZWATCH_SHARE_PROBE") == "" {
		t.Skip("set FUZZWATCH_SHARE_PROBE=1, under external CPU load, to run the #453 probe")
	}

	const window = 3 * time.Second
	const workIters = 200_000_000

	// A: exactly the probe in the issue -- a Budget with nothing but a sleep
	// in it. The heartbeat goroutine is near-idle, which is precisely the kind
	// of task CFS keeps latency good for, so no stall is ever seen.
	b := New(window)
	c0 := cpuSeconds(t)
	time.Sleep(window)
	cpu := cpuSeconds(t) - c0
	verdict, _, rep := b.Check()
	t.Logf("A sleep-window probe: %s", rep)
	t.Logf("A verdict=%v starved=%v; process consumed %.3fs of CPU over the window",
		verdict, rep.Starved(), cpu)

	// B: the same budget over CPU-BOUND work. This is what a fuzz target
	// actually does, and where stall and share come apart.
	b2 := New(window)
	c2 := cpuSeconds(t)
	t0 := time.Now()
	x := 0
	for i := range workIters {
		x += i * i
		x ^= x >> 3
	}
	took := time.Since(t0)
	cpu2 := cpuSeconds(t) - c2
	_ = x
	_, _, rep2 := b2.Check()
	t.Logf("B cpu-bound probe: %d iterations took %v using %.3fs of CPU (share %.1f%%)",
		workIters, took.Round(time.Millisecond), cpu2, 100*cpu2/took.Seconds())
	t.Logf("B fuzzwatch says: %s", rep2)
}

// cpuSeconds reads utime+stime for this process out of /proc/self/stat. Linux
// only; the probe is skipped by default so a non-Linux run never reaches it.
func cpuSeconds(t *testing.T) float64 {
	t.Helper()
	b, err := os.ReadFile("/proc/self/stat")
	if err != nil {
		t.Skipf("no /proc/self/stat on this platform: %v", err)
	}
	s := string(b)
	// comm is parenthesised and may contain spaces, so fields are counted from
	// after the final ')'.
	i := strings.LastIndex(s, ")")
	if i < 0 || i+2 >= len(s) {
		t.Fatalf("unparsable /proc/self/stat")
	}
	f := strings.Fields(s[i+2:])
	if len(f) < 13 {
		t.Fatalf("/proc/self/stat has %d fields after comm, want >= 13", len(f))
	}
	ut, err1 := strconv.ParseFloat(f[11], 64)
	st, err2 := strconv.ParseFloat(f[12], 64)
	if err1 != nil || err2 != nil {
		t.Fatalf("parsing utime/stime: %v %v", err1, err2)
	}
	const userHZ = 100
	return (ut + st) / userHZ
}
