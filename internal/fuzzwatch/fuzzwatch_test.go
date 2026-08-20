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

// #501: the ticker cannot charge a stall while the process is frozen, because
// the tick that would charge it cannot be delivered. pending is what a reader
// asks instead, and these pin its arithmetic without a clock.

func TestMonitorPendingSeesAStallTheTickerHasNotCharged(t *testing.T) {
	var m monitor
	m.origin = time.Now().Add(-3 * time.Second)
	// beatNanos still 0: the heartbeat has not run since origin, which is what
	// a three-second freeze starting at origin looks like from a reader.
	gap, lost := m.pending(time.Now(), tick)
	if gap < 3*time.Second {
		t.Fatalf("gap = %s, want at least the 3s since the last known heartbeat", gap)
	}
	if want := gap - tick; lost != want {
		t.Fatalf("pending lost = %s, want %s (the excess over the nominal interval)", lost, want)
	}
	// Nothing was written: the ticker's accounting is the ticker's, and the
	// delayed tick must charge this stall exactly once when it arrives.
	if got := m.lost(); got != 0 {
		t.Fatalf("pending charged %s to the accumulated total; it must only compute", got)
	}
	if got := m.longest(); got != 0 {
		t.Fatalf("pending recorded a longest stall (%s); it must only compute", got)
	}
}

func TestMonitorPendingIgnoresOrdinaryJitter(t *testing.T) {
	var m monitor
	m.origin = time.Now()
	// A read landing between heartbeats is the normal case, not a stall.
	if _, lost := m.pending(m.origin.Add(tick*tolerance), tick); lost != 0 {
		t.Fatalf("a gap of exactly tolerance*tick was charged %s; the floor is unchanged (#501 "+
			"fixes when a stall is READ, not how small a stall is visible)", lost)
	}
	if _, lost := m.pending(m.origin.Add(tick*tolerance+time.Millisecond), tick); lost == 0 {
		t.Fatal("a gap just past tolerance*tick was not charged at all")
	}
}

func TestMonitorPendingIsSilentBeforeTheHeartbeatStarts(t *testing.T) {
	var m monitor
	// No origin: there is no instant at which this monitor knows the process
	// was running, so it may not claim a stall of the whole epoch.
	gap, lost := m.pending(time.Now(), tick)
	if gap != 0 || lost != 0 {
		t.Fatalf("an unstarted monitor reported gap=%s lost=%s, want 0/0", gap, lost)
	}
}

// The property that makes the read-time charge safe to fold in: it is the SAME
// quantity the tick would charge for the same gap. A read at the instant a
// freeze ends and a read two heartbeats later therefore agree, and neither is
// the sum of both.
func TestPendingChargesWhatTheTickWouldCharge(t *testing.T) {
	for _, gap := range []time.Duration{
		tick, tick * tolerance, tick*tolerance + time.Millisecond,
		time.Second, 3 * time.Second, 30 * time.Second,
	} {
		var ticked, read monitor
		ticked.observe(gap, tick)
		read.origin = time.Now()
		_, pending := read.pending(read.origin.Add(gap), tick)
		if pending != ticked.lost() {
			t.Errorf("gap %s: read-time charge %s, tick charge %s -- the two must agree or a "+
				"window's verdict depends on which one happened to run first (#501)",
				gap, pending, ticked.lost())
		}
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

// The load-bearing negative: a long window on a machine that WAS running us is
// a hang, not an excuse. Without this, a slow-but-real hang on a mildly noisy
// machine could be waved away -- especially now that Starved() is consulted at
// both caps (#488) rather than only at the wall cap.
//
// Note what this window actually exercises: 40s of wall clock with only 1s
// lost is 39s SCHEDULED, so the budget is long spent and it is the
// scheduled-time cap that fires, not the wall cap. That is the arm #488
// changed, and it must still say Hung.
func TestCheckHungAtTheWallCapWhenNotStarved(t *testing.T) {
	b := New(10 * time.Second)
	v, _, r := checkAt(b, 40*time.Second, time.Second)
	if v != Hung {
		t.Fatalf("verdict = %v, want Hung (%s)", v, r)
	}
	if r.Starved() {
		t.Fatalf("window is starved, so it is not the not-starved negative it claims to be (%s)", r)
	}
}

// #488, as filed and as observed live: a 30s budget over a window of
// "wall 1m24.561s, of which 30.561s scheduled and 54s lost to scheduler stall
// (longest single stall 10.7s)". Starved() is true AND the budget of scheduled
// time is spent, so both facts are available to Check; before the fix the
// scheduled-time arm was tested first and returned Hung without ever asking.
func TestCheckDoesNotCallAStarvedWindowAHang(t *testing.T) {
	b := New(30 * time.Second)
	v, _, r := checkAt(b, 84561*time.Millisecond, 54*time.Second)
	// Pin that this really is the co-occurrence, not some neighbouring window:
	// the SCHEDULED cap is the one that fires, the wall cap is not reached,
	// and the window is starved.
	if r.Scheduled() < 30*time.Second {
		t.Fatalf("window does not spend the budget, so it is not the #488 case (%s)", r)
	}
	if r.Wall >= 4*30*time.Second {
		t.Fatalf("window reaches the hard wall cap, so it is not the #488 case (%s)", r)
	}
	if !r.Starved() {
		t.Fatalf("window is not starved, so it is not the #488 case (%s)", r)
	}
	if v == Hung {
		t.Fatalf("verdict = Hung for a window Check itself classifies as starved (%s) -- "+
			"Starved() means 'do not blame the code under test' (#488)", r)
	}
	if v != Inconclusive {
		t.Fatalf("verdict = %v, want Inconclusive (%s)", v, r)
	}
}

// The property, over the share space rather than at the one observed point:
// Starved() and Hung may never co-occur, and Inconclusive must be reachable at
// shares well above the ~25% ceiling the old ordering imposed.
func TestHungAndStarvedNeverCoOccur(t *testing.T) {
	const total = 10 * time.Second
	best := -1
	for _, wallSec := range []int{10, 15, 20, 30, 40, 60, 90, 120} {
		for lostPct := 0; lostPct <= 100; lostPct += 5 {
			wall := time.Duration(wallSec) * time.Second
			lost := wall * time.Duration(lostPct) / 100
			b := New(total)
			v, _, r := checkAt(b, wall, lost)
			if v == Hung && r.Starved() {
				t.Errorf("wall=%s lost=%s: verdict Hung for a starved window (%s)", wall, lost, r)
			}
			if v == Inconclusive && !r.Starved() {
				t.Errorf("wall=%s lost=%s: verdict Inconclusive for a window that is not starved (%s)", wall, lost, r)
			}
			if v == Inconclusive && r.Wall > 0 {
				if share := int(r.Scheduled() * 100 / r.Wall); share > best {
					best = share
				}
			}
		}
	}
	// Before #488 the highest CPU share that could ever yield Inconclusive was
	// under 25%: it required Wall >= 4*total with Scheduled < total. The point
	// of the fix is that the verdict is reachable above that ceiling.
	if best <= 25 {
		t.Fatalf("highest scheduled share yielding Inconclusive is %d%%, want above 25%% -- "+
			"the ceiling #488 describes is still in place", best)
	}
	t.Logf("Inconclusive reachable up to %d%% scheduled share (the old ordering capped it below 25%%)", best)
}

// Why the two caps can share one branch: reaching the hard wall cap without
// having spent the budget is, arithmetically, always a starved window. Wall >=
// 4*total with Scheduled < total gives Wall > 2*Scheduled, which is Starved().
// So the old wall-cap "not starved, therefore Hung" fallback could not fire.
func TestTheWallCapIsAlwaysStarved(t *testing.T) {
	const total = 10 * time.Second
	const hardWall = total * hardWallFactor
	checked := 0
	for _, wall := range []time.Duration{hardWall, hardWall + time.Millisecond, 60 * time.Second, 120 * time.Second, 600 * time.Second} {
		for lost := time.Duration(0); lost <= wall; lost += 250 * time.Millisecond {
			r := Report{Wall: wall, Lost: lost}
			if r.Scheduled() >= total {
				continue // the budget cap fires first; not the wall-cap case
			}
			checked++
			if !r.Starved() {
				t.Fatalf("wall=%s lost=%s reaches the wall cap unspent but is not starved (%s)", wall, lost, r)
			}
		}
	}
	if checked == 0 {
		t.Fatal("no wall-cap window was examined — the sweep is broken, not the property")
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
