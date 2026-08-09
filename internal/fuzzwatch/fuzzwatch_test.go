// Copyright © 2026 The ELPS authors

package fuzzwatch

import (
	"testing"
	"time"
)

// The accounting is tested by driving observe() directly rather than by
// sleeping: a test that manufactures a real multi-second scheduler stall is
// both slow and, on a loaded machine, exactly as unreliable as the thing this
// package exists to fix.

func TestMonitorIgnoresOrdinaryJitter(t *testing.T) {
	var m monitor
	for range 100 {
		m.observe(tick+20*time.Millisecond, tick)
	}
	// A tick landing 20ms late, a hundred times, is a healthy machine.
	if got := m.lost(); got != 0 {
		t.Fatalf("ordinary jitter was charged as scheduler stall: %s", got)
	}
	if got := m.longest(); got != 0 {
		t.Fatalf("ordinary jitter recorded a longest stall: %s", got)
	}
}

func TestMonitorChargesOnlyTheExcess(t *testing.T) {
	var m monitor
	m.observe(3*time.Second, tick)
	// The nominal interval would have elapsed anyway; only the excess is time
	// the process lost.
	if want, got := 3*time.Second-tick, m.lost(); got != want {
		t.Fatalf("lost = %s, want %s", got, want)
	}
	if want, got := 3*time.Second, m.longest(); got != want {
		t.Fatalf("longest = %s, want %s", got, want)
	}
}

func TestMonitorLongestIsAMaximum(t *testing.T) {
	var m monitor
	m.observe(5*time.Second, tick)
	m.observe(2*time.Second, tick)
	if want, got := 5*time.Second, m.longest(); got != want {
		t.Fatalf("longest = %s, want %s (a later shorter stall must not lower it)", got, want)
	}
}

func TestReportScheduled(t *testing.T) {
	r := Report{Wall: 10 * time.Second, Lost: 7 * time.Second}
	if want, got := 3*time.Second, r.Scheduled(); got != want {
		t.Fatalf("Scheduled = %s, want %s", got, want)
	}
	if !r.Starved() {
		t.Fatal("7s lost out of 10s wall is starved")
	}
	healthy := Report{Wall: 10 * time.Second, Lost: 100 * time.Millisecond}
	if healthy.Starved() {
		t.Fatal("100ms lost out of 10s wall is not starved")
	}
	// Defensive: the monitor is sampled at two instants, so Lost can in
	// principle exceed Wall by a hair. It must not produce a negative.
	odd := Report{Wall: time.Second, Lost: 2 * time.Second}
	if got := odd.Scheduled(); got != 0 {
		t.Fatalf("Scheduled = %s, want 0 for Lost > Wall", got)
	}
}

// checkAt exercises Budget.Check against a synthetic window, so the verdict
// logic is tested without a clock.
func checkAt(b *Budget, wall, lost time.Duration) (Verdict, time.Duration, Report) {
	b.startedAt = time.Now().Add(-wall)
	b.lostAt = defaultMonitor.lost() - lost
	return b.Check()
}

func TestCheckHungOnAHealthyMachine(t *testing.T) {
	b := New(10 * time.Second)
	v, _, r := checkAt(b, 10*time.Second, 0)
	if v != Hung {
		t.Fatalf("verdict = %v, want Hung (%s)", v, r)
	}
}

func TestCheckContinuesWhenDescheduled(t *testing.T) {
	b := New(10 * time.Second)
	// 10s of wall clock, but 6s of it the process was not running: only 4s of
	// the budget was actually spent.
	v, more, r := checkAt(b, 10*time.Second, 6*time.Second)
	if v != Continue {
		t.Fatalf("verdict = %v, want Continue (%s)", v, r)
	}
	if more < 5*time.Second || more > 7*time.Second {
		t.Fatalf("wait = %s, want the ~6s of budget still unspent", more)
	}
}

func TestCheckInconclusiveAtTheWallCap(t *testing.T) {
	b := New(10 * time.Second)
	// Four budgets of wall clock have gone by and the process was starved for
	// nearly all of it. Whether the call would terminate is unknowable.
	v, _, r := checkAt(b, 40*time.Second, 39*time.Second)
	if v != Inconclusive {
		t.Fatalf("verdict = %v, want Inconclusive (%s)", v, r)
	}
}

// The load-bearing negative: reaching the wall cap on a machine that WAS
// running us is a hang, not an excuse. Without this, a slow-but-real hang on a
// mildly noisy machine could be waved away.
func TestCheckHungAtTheWallCapWhenNotStarved(t *testing.T) {
	b := New(10 * time.Second)
	v, _, r := checkAt(b, 40*time.Second, time.Second)
	if v != Hung {
		t.Fatalf("verdict = %v, want Hung (%s)", v, r)
	}
}

func TestCheckNeverRearmsBelowAHeartbeat(t *testing.T) {
	b := New(10 * time.Second)
	// A sliver of budget left: re-arming for it would spin without ever
	// observing another heartbeat.
	v, more, _ := checkAt(b, 10*time.Second, time.Millisecond)
	if v != Continue {
		t.Fatalf("verdict = %v, want Continue", v)
	}
	if more < tick {
		t.Fatalf("wait = %s, want at least one heartbeat (%s)", more, tick)
	}
}

// End to end against the real clock, kept short. A budget that expires with
// the machine idle must say Hung: this is the path every existing watchdog
// takes today, and it must be unchanged.
func TestBudgetExpiresOnAnIdleMachine(t *testing.T) {
	b := New(150 * time.Millisecond)
	time.Sleep(b.Total() + 50*time.Millisecond)
	v, _, r := b.Check()
	if v != Hung {
		t.Fatalf("verdict = %v, want Hung on an idle machine (%s)", v, r)
	}
}

func TestVerdictString(t *testing.T) {
	for v, want := range map[Verdict]string{
		Continue: "continue", Hung: "hung", Inconclusive: "inconclusive",
	} {
		if got := v.String(); got != want {
			t.Errorf("Verdict(%d).String() = %q, want %q", int(v), got, want)
		}
	}
	if got := Verdict(9).String(); got != "Verdict(9)" {
		t.Errorf("unknown verdict rendered as %q", got)
	}
}
