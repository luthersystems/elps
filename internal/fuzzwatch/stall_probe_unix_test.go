// Copyright © 2026 The ELPS authors

//go:build unix

package fuzzwatch

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"
)

// The rest of this package's verdict tests drive Check against synthetic
// windows, which is the right way to test arithmetic. This one does not: it
// creates a REAL scheduler stall, of the exact kind fuzzwatch was built to
// detect, and asks Check what it makes of the window that results.
//
// It exists because #488 was found by running, not by reading, and the
// arithmetic alone does not show that the monitor, the Report and the verdict
// actually line up end to end on a live process. A synthetic Report cannot go
// wrong in the way a real one can.
//
// Two probes share the rig, differing only in WHEN the child reads its budget.
// TestCheckUnderARealSchedulerStall reads once the freeze has been accounted
// for, and asks what a window containing a real freeze is called (#488). The
// #501 probe reads at the instant the freeze ends, before any tick could have
// been delivered, and asks what Check is told about the freeze at the one
// moment the accounting has not caught up.
//
// The stall is manufactured with SIGSTOP/SIGCONT rather than with CPU load.
// That is deliberate and is the only honest way to do it here: PR #483
// measured that contention does NOT produce stalls this instrument can see
// (200 spinners on 4 cores reported lost=0s), because starvation is
// microseconds-at-a-time and the 400ms floor swallows it. SIGSTOP is the
// failure mode the package doc names -- a process genuinely frozen, VM steal
// or cgroup throttling in miniature -- so it is the real condition for this
// instrument, and it is deterministic.

const (
	stallChildEnv = "FUZZWATCH_STALL_CHILD"

	// The child spends this much SCHEDULED time before calling Check, i.e.
	// exactly what a caller does when its timer fires.
	stallChildBudget = 1500 * time.Millisecond
	// How long the parent freezes the child. Must exceed the budget, so that
	// Lost > Scheduled and the window is Starved; and the whole window must
	// stay under hardWallFactor*budget so it is the SCHEDULED cap that fires,
	// which is the #488 shape.
	stallFreeze = 3 * time.Second
	// Let the child accumulate a little scheduled time before freezing it, so
	// the budget is provably not spent at the moment the stall begins.
	stallRunBefore = 300 * time.Millisecond
	// How often a child looks at its budget. Short enough that the #501 child
	// wakes essentially at the instant SIGCONT lands, and far below
	// stallFreeze/2, which is how that child tells the sleep the freeze
	// swallowed from an ordinary one.
	stallPoll = 25 * time.Millisecond
)

// TestStallProbeChildProcess is not a test. It is the body of the child
// process TestCheckUnderARealSchedulerStall spawns, and it skips out
// immediately in any other context.
func TestStallProbeChildProcess(t *testing.T) {
	if os.Getenv(stallChildEnv) != "1" {
		t.Skip("helper process for TestCheckUnderARealSchedulerStall; not run directly")
	}
	b := New(stallChildBudget)
	fmt.Println("FUZZWATCH-CHILD READY")

	// Poll the way a caller re-arms: the watchdog is meant to fire once the
	// budget of SCHEDULED time is spent. Whatever stall happens in between is
	// the parent's doing, not ours.
	//
	// This loop used to settle for two heartbeats before trusting a read that
	// said the budget was spent, because at the instant a freeze ended the
	// monitor had not charged it yet and Report counted the whole freeze as
	// scheduled time. That was #501, and Report now charges an unexplained gap
	// at read time, so a single read is enough and the settle is gone. The
	// #488 assertions below are unchanged and never depended on it.
	deadline := time.Now().Add(60 * time.Second)
	for {
		if time.Now().After(deadline) {
			fmt.Println("FUZZWATCH-CHILD ABORT budget never spent")
			return
		}
		if b.Report().Scheduled() >= b.Total() {
			break
		}
		time.Sleep(stallPoll)
	}
	v, _, r := b.Check()
	fmt.Printf("FUZZWATCH-CHILD RESULT verdict=%s wall=%d lost=%d scheduled=%d starved=%t hardwall=%d\n",
		v, r.Wall, r.Lost, r.Scheduled(), r.Starved(), b.hardWall)
}

// TestStallResumeProbeChildProcess is not a test either. It is the body of the
// child TestReportAtTheInstantAStallEnds spawns.
//
// It differs from the child above in one respect, which is the entire point:
// it reads at the WORST possible instant rather than the most convenient one.
// It polls in short sleeps and calls Check as the very first thing after a
// sleep that took far longer than it asked for -- i.e. the sleep the freeze
// swallowed. That is not a contrived moment: a caller arms its timer for the
// budget, so a freeze longer than the budget always straddles the deadline and
// the first Check after it always lands here, with the caller's timer and the
// heartbeat ticker both overdue and no ordering between them.
//
// Before #501 the answer at that instant was "lost=0s, starved=false, hung":
// the freeze was real, the monitor was about to charge it, and the verdict was
// taken a moment too early and blamed the code under test.
func TestStallResumeProbeChildProcess(t *testing.T) {
	if os.Getenv(stallChildEnv) != "1" {
		t.Skip("helper process for TestReportAtTheInstantAStallEnds; not run directly")
	}
	b := New(stallChildBudget)
	fmt.Println("FUZZWATCH-CHILD READY")

	deadline := time.Now().Add(60 * time.Second)
	for {
		before := time.Now()
		time.Sleep(stallPoll)
		// FIRST, before anything else gets a chance to let the heartbeat catch
		// up: this read is the measurement. Whether the sleep was the ordinary
		// one or the one the freeze swallowed is decided afterwards, from the
		// same instant.
		v, _, r := b.Check()
		if time.Since(before) < stallFreeze/2 {
			if time.Now().After(deadline) {
				fmt.Println("FUZZWATCH-CHILD ABORT never observed a freeze")
				return
			}
			continue
		}
		fmt.Printf("FUZZWATCH-CHILD RESUME verdict=%s wall=%d lost=%d scheduled=%d starved=%t hardwall=%d\n",
			v, r.Wall, r.Lost, r.Scheduled(), r.Starved(), b.hardWall)
		return
	}
}

// stallResult is one parsed RESULT line from the child.
type stallResult struct {
	verdict   string
	wall      time.Duration
	lost      time.Duration
	scheduled time.Duration
	starved   bool
	hardWall  time.Duration
}

func (s stallResult) String() string {
	return fmt.Sprintf("verdict=%s wall=%s lost=%s scheduled=%s starved=%t (%d%% of wall scheduled, hard wall cap %s)",
		s.verdict, s.wall.Round(time.Millisecond), s.lost.Round(time.Millisecond),
		s.scheduled.Round(time.Millisecond), s.starved,
		int(s.scheduled*100/max(s.wall, 1)), s.hardWall)
}

// isTheIssue488Shape reports whether this window is the one #488 describes:
// the SCHEDULED cap is spent, the hard wall cap is NOT reached, and the window
// is starved. Only in that shape does the old ordering differ from the new
// one, so only in that shape is this test load-bearing.
func (s stallResult) isTheIssue488Shape(budget time.Duration) bool {
	return s.scheduled >= budget && s.wall < s.hardWall && s.starved
}

func TestCheckUnderARealSchedulerStall(t *testing.T) {
	if testing.Short() {
		t.Skip("spawns a child process and freezes it for several seconds")
	}

	const attempts = 3
	var last stallResult
	var got bool
	for i := range attempts {
		res, err := runStallProbe(t, "TestStallProbeChildProcess", "FUZZWATCH-CHILD RESULT ")
		if err != nil {
			t.Fatalf("attempt %d: %v", i+1, err)
		}
		t.Logf("attempt %d: %s", i+1, res)

		// THE INVARIANT, asserted on every window the child produced,
		// whatever its shape: a window Check itself classifies as starved may
		// not be reported as a hang.
		if res.starved && res.verdict == Hung.String() {
			t.Fatalf("attempt %d: a real scheduler stall produced %s -- Starved() means "+
				"'do not blame the code under test' (#488)", i+1, res)
		}
		last = res
		if res.isTheIssue488Shape(stallChildBudget) {
			got = true
			if res.verdict != Inconclusive.String() {
				t.Fatalf("attempt %d: %s, want verdict=%s", i+1, res, Inconclusive)
			}
			break
		}
	}
	if !got {
		// Not "nothing to check": the harness could not build the window this
		// test exists to examine, so the assertion above never had the case it
		// was written for. That is a failure, not a pass.
		t.Fatalf("could not produce the #488 window in %d attempts (last: %s) -- the harness "+
			"could not run, which is not the same as the check passing", attempts, last)
	}
}

// #501, end to end: a Report taken at the INSTANT a freeze ends must already
// show the freeze.
//
// The mechanism is accounting lag, not arm ordering, so #488's fix cannot
// reach it: at that instant Starved() is honestly false, because the evidence
// has not landed. The monitor learns of a stall from a tick and a tick cannot
// be delivered while the process is frozen, so the whole freeze read as
// scheduled time and Check called it a hang -- observed 3/3 on the unfixed
// accounting as "verdict=hung wall=3.306s lost=0s scheduled=3.306s
// starved=false", against a 1.5s budget and a 3s freeze.
//
// Every attempt is asserted rather than searched: unlike the #488 probe there
// is no window shape to hunt for, so a single attempt that comes back healthy
// is a regression, not a miss. Repeating simply gives the race more chances to
// land the read before the heartbeat catches up.
func TestReportAtTheInstantAStallEnds(t *testing.T) {
	if testing.Short() {
		t.Skip("spawns a child process and freezes it for several seconds")
	}

	const attempts = 3
	for i := range attempts {
		res, err := runStallProbe(t, "TestStallResumeProbeChildProcess", "FUZZWATCH-CHILD RESUME ")
		if err != nil {
			t.Fatalf("attempt %d: %v", i+1, err)
		}
		t.Logf("attempt %d: %s", i+1, res)

		// The rig, first: the read has to have happened at the end of a real
		// freeze, or the assertions below are vacuous.
		if res.wall < stallFreeze {
			t.Fatalf("attempt %d: %s -- the window is shorter than the %s freeze, so the child "+
				"did not read at the end of it and this attempt asserts nothing", i+1, res, stallFreeze)
		}

		// The floor is deliberately generous: what is being distinguished is
		// zero from three seconds, not one measurement from another. The
		// charge is quantised to the last heartbeat OBSERVED, so a fraction of
		// a tick either way is expected and uninteresting.
		if res.lost < stallFreeze/2 {
			t.Fatalf("attempt %d: %s -- a %s freeze was still unaccounted at the instant it "+
				"ended, so the window reads as scheduled time and the freeze is charged to the "+
				"code under test (#501)", i+1, res, stallFreeze)
		}
		if !res.starved {
			t.Fatalf("attempt %d: %s -- a window that was frozen for most of its wall clock "+
				"must be starved at the instant it resumes, not two heartbeats later (#501)", i+1, res)
		}
		if res.verdict == Hung.String() {
			t.Fatalf("attempt %d: %s -- Check read at the end of a real freeze called it a hang "+
				"(#501)", i+1, res)
		}
	}
}

// runStallProbe spawns this test binary as a child running childTest, freezes
// it mid-window with SIGSTOP, resumes it, and returns the result line the child
// emitted under prefix.
func runStallProbe(t *testing.T, childTest, prefix string) (stallResult, error) {
	t.Helper()

	//nolint:gosec // os.Args[0] is this test binary, re-executed as its own child; no external input reaches it
	cmd := exec.CommandContext(t.Context(), os.Args[0], "-test.run=^"+childTest+"$", "-test.count=1", "-test.v")
	cmd.Env = append(os.Environ(), stallChildEnv+"=1")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return stallResult{}, fmt.Errorf("stdout pipe: %w", err)
	}
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		return stallResult{}, fmt.Errorf("starting child: %w", err)
	}
	// A frozen child would otherwise outlive the test run.
	defer func() {
		_ = cmd.Process.Signal(syscall.SIGCONT)
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	}()

	lines := make(chan string, 64)
	go func() {
		defer close(lines)
		sc := bufio.NewScanner(stdout)
		for sc.Scan() {
			lines <- sc.Text()
		}
		if err := sc.Err(); err != nil {
			t.Logf("child stdout: %v", err)
		}
	}()

	if _, err := awaitLine(lines, "FUZZWATCH-CHILD READY", 30*time.Second); err != nil {
		return stallResult{}, fmt.Errorf("waiting for the child to arm its budget: %w", err)
	}

	time.Sleep(stallRunBefore)
	if err := cmd.Process.Signal(syscall.SIGSTOP); err != nil {
		return stallResult{}, fmt.Errorf("SIGSTOP: %w", err)
	}
	time.Sleep(stallFreeze)
	if err := cmd.Process.Signal(syscall.SIGCONT); err != nil {
		return stallResult{}, fmt.Errorf("SIGCONT: %w", err)
	}

	line, err := awaitLine(lines, prefix, 60*time.Second)
	if err != nil {
		return stallResult{}, fmt.Errorf("waiting for the child's verdict: %w", err)
	}
	return parseStallResult(strings.TrimPrefix(line, prefix))
}

func awaitLine(lines <-chan string, prefix string, timeout time.Duration) (string, error) {
	deadline := time.After(timeout)
	for {
		select {
		case line, ok := <-lines:
			if !ok {
				return "", fmt.Errorf("child exited before emitting %q", prefix)
			}
			if strings.HasPrefix(line, prefix) {
				return line, nil
			}
			if strings.HasPrefix(line, "FUZZWATCH-CHILD ABORT") {
				return "", fmt.Errorf("child aborted: %s", line)
			}
		case <-deadline:
			return "", fmt.Errorf("timed out after %s waiting for %q", timeout, prefix)
		}
	}
}

func parseStallResult(s string) (stallResult, error) {
	var res stallResult
	seen := map[string]bool{}
	for _, field := range strings.Fields(s) {
		k, v, ok := strings.Cut(field, "=")
		if !ok {
			return res, fmt.Errorf("malformed result field %q", field)
		}
		seen[k] = true
		switch k {
		case "verdict":
			res.verdict = v
		case "starved":
			b, err := strconv.ParseBool(v)
			if err != nil {
				return res, fmt.Errorf("starved: %w", err)
			}
			res.starved = b
		case "wall", "lost", "scheduled", "hardwall":
			n, err := strconv.ParseInt(v, 10, 64)
			if err != nil {
				return res, fmt.Errorf("%s: %w", k, err)
			}
			switch k {
			case "wall":
				res.wall = time.Duration(n)
			case "lost":
				res.lost = time.Duration(n)
			case "scheduled":
				res.scheduled = time.Duration(n)
			case "hardwall":
				res.hardWall = time.Duration(n)
			}
		default:
			return res, fmt.Errorf("unexpected result field %q", k)
		}
	}
	// A result line that lost a field would otherwise read as zeros, and zeros
	// satisfy neither branch loudly.
	for _, k := range []string{"verdict", "wall", "lost", "scheduled", "starved", "hardwall"} {
		if !seen[k] {
			return res, fmt.Errorf("result line is missing %q: %q", k, s)
		}
	}
	return res, nil
}
