// Copyright © 2026 The ELPS authors

// Package fuzzwatch bounds a fuzz target's watchdog by SCHEDULED time rather
// than by wall-clock time.
//
// # The problem
//
// Three fuzz targets in this repository assert that a call terminates, using
// the same shape:
//
//	select {
//	case v := <-done:
//	        ...
//	case <-time.After(watchdog):
//	        t.Fatalf("did not terminate")
//	}
//
// The property being asserted is real and worth asserting: `go test -fuzz` has
// no per-input deadline, the interpreter's own step and allocation budgets are
// blind to a builtin that loops inside Go, and two of the three defects
// FuzzApplyStdlib found were exactly that shape. But `time.After` measures the
// wall clock, and the wall clock counts time during which this process was not
// running at all. On a contended CI runner a call that would finish in a
// millisecond can be descheduled for seconds, and the watchdog cannot tell
// that from a genuine hang. It fails, no input is really to blame, and the
// board goes red for a reason nobody can reproduce -- the failure mode this
// repository has spent a lot of effort eliminating.
//
// Simply raising the watchdog does not fix it. The headroom is already
// enormous: measured on a 4-core box, FuzzEval averages 0.33ms per input
// against a 30s watchdog (~90,000x), FuzzApplyStdlib 0.73ms against 15s
// (~20,000x) and FuzzSchemaValidate 0.91ms against 20s (~22,000x). A bound
// four orders of magnitude above the mean is not too tight; it is measuring
// the wrong thing.
//
// # What this measures instead
//
// A single heartbeat goroutine per process ticks at a fixed interval and
// records how much wall clock passed during which it did NOT run on schedule.
// That is a direct measurement of "this process was not being given the CPU",
// and it is exactly the quantity a wall-clock watchdog wrongly charges to the
// code under test.
//
// A [Budget] then spends only SCHEDULED time. When the caller's timer fires,
// [Budget.Check] answers one of three things:
//
//   - Continue: the process was descheduled for part of the window, so the
//     budget is not spent. Wait the returned amount longer.
//   - Hung: the full budget of scheduled time elapsed with the process running
//     normally. The call under test did not terminate; fail.
//   - Inconclusive: the hard wall-clock cap was reached and the process was
//     starved throughout. Nothing can be concluded about this input, so
//     nothing is asserted about it.
//
// On a healthy machine no stall is ever recorded, Check returns Hung at
// exactly the configured budget, and every target detects precisely what it
// detected before. Detection is reduced only on a machine that is not running
// us -- where the alternative is not detection but a coin flip.
//
// # What this does NOT measure: CPU share
//
// This instrument resolves scheduler STALL -- intervals during which the
// process ran not at all. It does not measure CPU SHARE, and under Linux CFS
// the two come apart completely. Do not reach for it as a load-aware budget.
//
// A stall is visible here only if it exceeds tolerance*tick, i.e. 400ms. What
// starvation actually looks like on a busy machine is not one 400ms freeze but
// a few microseconds of CPU handed out every millisecond, forever -- and every
// one of those gaps is far below the resolution floor. Worse, the heartbeat
// goroutine is by construction almost entirely idle, and CFS gives a waking,
// near-idle task excellent latency no matter how long the run queue is. The
// probe is precisely the kind of task starvation does not touch. It reports a
// healthy machine while the worker beside it gets nothing.
//
// Measured on the 4-core sandbox, 200 competing spinners (load average > 50):
//
//   - The #453 probe -- create a Budget, sleep 3s, Check -- reports
//     "wall=3.003s scheduled=3.003s lost=0s longest=0s". Zero lost time, under
//     load heavy enough to make the machine unusable.
//
//   - A fixed lump of CPU work that takes 169ms on the idle box took 17.375s
//     under that load: a 103x slowdown, at 0.9% CPU share. Over that window
//     fuzzwatch charged 300ms as lost and reported scheduled=17.357s. It
//     described a process that was starved for seventeen seconds as one that
//     ran normally for seventeen seconds. A repeat run was starker still --
//     12.279s for the same work at 1.2% share, reported as
//     "12.66s scheduled, 0s lost". Not under-counted: not counted at all.
//
// So "scheduled time" is a truthful name only for the failure mode this
// package was built for -- a process genuinely frozen (VM steal, cgroup
// throttling with long periods, an SMR pause), where the gaps are seconds and
// land well above the 400ms floor. Against that, it works as designed. Against
// contention it degenerates to the wall clock, silently.
//
// # The regime in which it is honest, and the floor
//
// The nine (now eleven) watchdogs using this package are unaffected, because
// they are sized 20,000x-90,000x above the mean work they bound: FuzzEval
// averages 0.33ms per input against 30s, FuzzApplyStdlib 0.73ms against 15s,
// FuzzSchemaValidate 0.91ms against 20s. A 100x slowdown still leaves two to
// three orders of magnitude of headroom, so whether the instrument
// distinguishes stall from share never arises. That headroom -- not the
// accounting -- is what makes those targets robust.
//
// A budget close to the work it bounds gets no such protection, and this
// package cannot supply it. That is not hypothetical: the first proposed fix
// in #435 was to give a 2s budget this treatment, at ~57x headroom rather than
// ~90,000x. PR #447 measured it, found it does not work, and rejected it for
// that reason; #453 records the general boundary. Do not re-propose it.
//
// [MinHonestBudget] is the floor, enforced by a guard test over every call
// site in the repository. If you want a budget below it, this is the wrong
// instrument -- measure utime+stime deltas from /proc/self/stat against wall
// clock instead, and note that those over-count whenever several goroutines of
// the process are legitimately busy (parallel subtests), so it is not a
// drop-in replacement either.
package fuzzwatch

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

const (
	// tick is the heartbeat interval. Short enough to resolve the sub-second
	// stalls that matter, long enough that one goroutine per fuzz worker
	// process is free.
	tick = 100 * time.Millisecond

	// tolerance is how late a tick may land before it counts as the process
	// having been descheduled. A timer is always a little late; 4x the
	// interval is comfortably above ordinary jitter (measured p100 tick lag
	// during a live 4-worker sweep on a 4-core box was well under this) and
	// far below the multi-second stalls a contended runner produces.
	tolerance = 4

	// hardWallFactor caps total wall clock at a multiple of the budget, so a
	// permanently starved process cannot wait forever -- `go test -timeout`
	// would eventually kill the binary with no crasher written, which is worse
	// than any verdict. A process given less than a quarter of the CPU for
	// four consecutive budgets is comprehensively broken, and Inconclusive is
	// the honest answer for it.
	hardWallFactor = 4
)

// MinHonestBudget is the smallest budget this instrument may be used for.
//
// It is not a property of the accounting -- New will happily construct
// anything -- but of what the accounting can SEE. Stalls shorter than
// tolerance*tick (400ms) are invisible, and CPU starvation, which is the
// common case on a busy runner, is invisible at any duration; see the package
// doc for the measurement. A watchdog is therefore only as trustworthy as its
// headroom over the work it bounds, and below this floor there is not enough
// headroom left for "the machine was busy" and "the code hung" to be
// distinguishable at all.
//
// 10s is chosen to sit just under the smallest budget in the tree (15s, in
// lisp/cycle_fuzz_test.go and lisp/lisplib/fuzz_test.go) and far above the
// 2s budget that #435 proposed and PR #447 measured and rejected. It is a
// backstop against a new call site, not a target to design against: every
// existing watchdog clears it by 50x or more because it is sized against
// sub-millisecond work, which is the property that actually makes it sound.
//
// TestEveryBudgetIsAboveTheHonestFloor enforces this across the repository.
const MinHonestBudget = 10 * time.Second

// monitor accumulates scheduler stall observed by a heartbeat goroutine.
//
// Both fields are cumulative and monotonic, so a caller records a snapshot at
// the start of a window and subtracts: no history, no lock, O(1).
type monitor struct {
	lostNanos    atomic.Int64
	longestNanos atomic.Int64
}

var (
	defaultMonitor monitor
	startOnce      sync.Once
)

// start launches the process-wide heartbeat. Idempotent; every constructor
// calls it, so a target never has to remember to.
func start() {
	startOnce.Do(func() {
		go defaultMonitor.run(tick)
	})
}

func (m *monitor) run(d time.Duration) {
	t := time.NewTicker(d)
	defer t.Stop()
	last := time.Now()
	for now := range t.C {
		m.observe(now.Sub(last), d)
		last = now
	}
}

// observe records one heartbeat interval. Split out from run so the accounting
// is testable without waiting on a real clock.
func (m *monitor) observe(gap, nominal time.Duration) {
	if gap <= nominal*tolerance {
		return
	}
	// Charge only the EXCESS as lost: the nominal interval would have elapsed
	// even on an idle machine.
	m.lostNanos.Add(int64(gap - nominal))
	for {
		cur := m.longestNanos.Load()
		if int64(gap) <= cur || m.longestNanos.CompareAndSwap(cur, int64(gap)) {
			return
		}
	}
}

func (m *monitor) lost() time.Duration {
	return time.Duration(m.lostNanos.Load())
}

func (m *monitor) longest() time.Duration {
	return time.Duration(m.longestNanos.Load())
}

// Verdict is what a fired watchdog timer actually means.
type Verdict int

const (
	// Continue means the process was descheduled for part of the window, so
	// the budget of scheduled time is not spent. Wait longer.
	Continue Verdict = iota
	// Hung means the budget of scheduled time elapsed while the process was
	// running normally. The call under test did not terminate.
	Hung
	// Inconclusive means the hard wall-clock cap was reached with the process
	// starved throughout. Whether the call would have terminated is unknown,
	// so nothing may be asserted about this input.
	Inconclusive
)

func (v Verdict) String() string {
	switch v {
	case Continue:
		return "continue"
	case Hung:
		return "hung"
	case Inconclusive:
		return "inconclusive"
	}
	return fmt.Sprintf("Verdict(%d)", int(v))
}

// Report describes what the wall clock was doing during a watchdog window. It
// exists to be printed in a failure message: "did not terminate in 15s" is a
// much weaker claim than "did not terminate in 15s of scheduled time, during
// which the process was never descheduled by more than 120ms".
type Report struct {
	// Wall is the total wall-clock time since the budget was created.
	Wall time.Duration
	// Lost is the part of Wall during which this process was demonstrably not
	// being scheduled.
	Lost time.Duration
	// LongestStall is the longest single heartbeat gap in the window.
	LongestStall time.Duration
}

// Scheduled is the part of the window during which the process was actually
// running -- the quantity a watchdog should be spending.
func (r Report) Scheduled() time.Duration {
	if r.Lost > r.Wall {
		return 0
	}
	return r.Wall - r.Lost
}

// Starved reports whether scheduler stall dominated the window.
func (r Report) Starved() bool {
	return r.Lost*2 >= r.Wall
}

func (r Report) String() string {
	return fmt.Sprintf("wall %s, of which %s scheduled and %s lost to scheduler stall (longest single stall %s)",
		r.Wall.Round(time.Millisecond),
		r.Scheduled().Round(time.Millisecond),
		r.Lost.Round(time.Millisecond),
		r.LongestStall.Round(time.Millisecond))
}

// Budget is a watchdog budget denominated in scheduled time.
//
// Create one before starting the work, arm an ordinary timer for [Budget.Total],
// and call [Budget.Check] when it fires.
type Budget struct {
	total     time.Duration
	hardWall  time.Duration
	startedAt time.Time
	lostAt    time.Duration
	longestAt time.Duration
}

// New returns a Budget of d scheduled time.
func New(d time.Duration) *Budget {
	start()
	return &Budget{
		total:     d,
		hardWall:  d * hardWallFactor,
		startedAt: time.Now(),
		lostAt:    defaultMonitor.lost(),
		longestAt: defaultMonitor.longest(),
	}
}

// Total is the budget the caller should arm its first timer for.
func (b *Budget) Total() time.Duration { return b.total }

// Report snapshots the window so far.
func (b *Budget) Report() Report {
	longest := defaultMonitor.longest()
	if longest < b.longestAt {
		longest = b.longestAt
	}
	r := Report{
		Wall: time.Since(b.startedAt),
		Lost: defaultMonitor.lost() - b.lostAt,
	}
	// longestNanos is a process-wide maximum, so it is only meaningful for this
	// window when it grew during it.
	if longest > b.longestAt {
		r.LongestStall = longest
	}
	return r
}

// Check interprets a fired watchdog timer. When it returns Continue, the
// second value is how much longer to wait before checking again.
func (b *Budget) Check() (Verdict, time.Duration, Report) {
	r := b.Report()
	switch {
	case r.Scheduled() >= b.total:
		return Hung, 0, r
	case r.Wall >= b.hardWall:
		// Out of wall clock without ever spending the budget. Either the
		// machine never gave us the CPU (Inconclusive), or the stall
		// accounting under-counted and this really is a hang.
		if r.Starved() {
			return Inconclusive, 0, r
		}
		return Hung, 0, r
	}
	remaining := b.total - r.Scheduled()
	// Never re-arm for less than a heartbeat: a shorter timer cannot observe
	// any new stall, so it would spin.
	if remaining < tick {
		remaining = tick
	}
	return Continue, remaining, r
}
