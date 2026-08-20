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
//   - Hung: a cap was reached and the process was NOT starved during the
//     window. The call under test did not terminate; fail.
//   - Inconclusive: a cap was reached but scheduler stall dominated the
//     window ([Report.Starved]). Nothing can be concluded about this input, so
//     nothing is asserted about it.
//
// On a healthy machine no stall is ever recorded, Check returns Hung at
// exactly the configured budget, and every target detects precisely what it
// detected before. Detection is reduced only on a machine that is not running
// us -- where the alternative is not detection but a coin flip.
//
// # Starvation is consulted at BOTH caps
//
// [Budget.Check] has two caps: the budget of scheduled time, and a hard
// wall-clock cap at [hardWallFactor] times it. Which one a window hits does
// not change what the window means, so [Report.Starved] is consulted for both.
//
// It was not always. Check originally tested the scheduled-time cap first and
// returned Hung from it unconditionally, without ever asking Starved. That
// made Inconclusive unreachable for any process receiving more than about a
// quarter of the CPU, because reaching the wall cap with the budget UNSPENT
// requires Wall >= 4*total while Scheduled < total -- i.e. a CPU share below
// 25%. Above that share the first cap always won and every window, however
// starved, was reported as a hang. Observed live (#488): a 30s budget over a
// window of "wall 1m24.561s, of which 30.561s scheduled and 54s lost to
// scheduler stall (longest single stall 10.7s)" -- 36% of wall clock actually
// scheduled, Starved() true -- was reported as Hung and failed a test.
//
// Starved() and Hung must not be able to co-occur: Starved() exists precisely
// to mean "do not blame the code under test". A window in which measured stall
// outweighs measured running time cannot support "this call did not
// terminate", whichever cap it reached first.
//
// The same arithmetic shows the wall cap can only ever be reached in a starved
// window: Wall >= 4*total with Scheduled < total gives Wall >= 4*total >
// 2*Scheduled, which is Starved() by definition. So the two caps share one
// branch rather than each carrying their own -- the old wall-cap "not starved,
// therefore Hung" fallback was unreachable code.
//
// The cost is deliberate and is a real reduction in detection: a genuine hang
// on a machine that was stalling for more than half the window now reports
// Inconclusive instead of failing. That is the error this project prefers.
// Fuzzing is a repeated-trial process -- a real hang is re-found by the next
// run, or by the same input on a quieter machine -- whereas a false Hung
// writes a crasher blaming an innocent input, which costs an investigation and
// pollutes the corpus permanently. It also does not touch the healthy case at
// all: with no stall recorded, Starved() is false and Hung fires at exactly
// the configured budget, as before.
//
// # A stall is charged when it is READ, not when its tick lands
//
// The monitor only learns about a stall from a tick, and a tick cannot be
// delivered while the process is frozen. So at the instant a freeze ends the
// accumulated total has not grown yet: [Budget.Report] read at that moment
// used to count the whole freeze as scheduled time, and [Budget.Check] read at
// that moment returned Hung with Starved() false -- the evidence exonerating
// the input existed but had not landed. Observed with SIGSTOP (#501): a 1.5s
// budget over a 3s freeze, read at resume, gave "wall=3.306s lost=0s
// scheduled=3.306s starved=false", and the same freeze read two heartbeats
// later gave "wall=4.704s lost=3s scheduled=1.704s starved=true". Same window,
// opposite verdict, decided by whether the ticker goroutine happened to run
// first.
//
// That instant is reachable and not merely a harness artefact: a caller arms
// its timer for [Budget.Total] and calls Check when it fires, so any freeze
// LONGER than the budget straddles the deadline and the first Check after it
// necessarily runs at resume, with both timers overdue and no ordering between
// them.
//
// Report therefore charges the unexplained interval since the last heartbeat
// itself, by the same rule the ticker applies (see [monitor.pending]). The
// charge is transient -- computed for that one Report, never written back --
// so the ticker's accounting is untouched and the delayed tick, when it
// arrives, charges the same stall exactly once. The fix is in the READING; the
// resolution floor is unchanged, and no budget or threshold was widened
// (#443/#452, #435/#447).
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
// One more floor sits underneath, and it is a property of the accounting
// rather than of the load: the instrument resolves stall no finer than one
// heartbeat. Report now charges a stall as soon as it is READ rather than
// waiting for the tick that would explain it (#501, see the package doc), so a
// Check landing exactly at the instant a freeze ends no longer charges the
// freeze to the code under test. But the charge is still measured from the
// last heartbeat OBSERVED, so it is quantised to within a tick and it still
// only appears at all once the gap passes tolerance*tick. A budget sized close
// enough to its work for a 400ms quantum to matter cannot be adjudicated by
// this instrument, and no amount of verdict logic can lift that; it is the
// same argument as the floor above, arriving from the accounting side.
//
// TestEveryBudgetIsAboveTheHonestFloor enforces this across the repository.
const MinHonestBudget = 10 * time.Second

// monitor accumulates scheduler stall observed by a heartbeat goroutine.
//
// The two charged fields are cumulative and monotonic, so a caller records a
// snapshot at the start of a window and subtracts: no history, no lock, O(1).
// beatNanos is not cumulative: it is when the heartbeat was last known to have
// run, which is what lets a reader see a stall the ticker has not been able to
// charge yet.
type monitor struct {
	lostNanos    atomic.Int64
	longestNanos atomic.Int64

	// beatNanos is the last observed heartbeat, as nanoseconds since origin.
	// Zero means no tick has been received yet, in which case origin itself is
	// the last instant the process was demonstrably running.
	beatNanos atomic.Int64

	// origin is set once, before the heartbeat goroutine starts, and read-only
	// afterwards. Offsets from it are monotonic-clock differences, so the
	// read-time accounting cannot be confused by a wall-clock step the way a
	// stored Unix timestamp could.
	origin time.Time
}

var (
	defaultMonitor monitor
	startOnce      sync.Once
)

// start launches the process-wide heartbeat. Idempotent; every constructor
// calls it, so a target never has to remember to.
func start() {
	startOnce.Do(func() {
		// Set before the goroutine starts, so every reader that reached here
		// through New -- which is all of them -- sees it. Until the first tick
		// lands this is also the last instant the process is known to have been
		// running, which is exactly what pending needs.
		defaultMonitor.origin = time.Now()
		go defaultMonitor.run(tick)
	})
}

func (m *monitor) run(d time.Duration) {
	t := time.NewTicker(d)
	defer t.Stop()
	last := m.origin
	for now := range t.C {
		// Publish the heartbeat BEFORE charging the interval it closes, and
		// note that pending's reader does the opposite: it reads the charged
		// total first and this timestamp second. Those two orders together are
		// what make double-counting impossible. If a reader's total already
		// includes this Add, then this Store -- which precedes it -- had
		// already happened when the reader looked, so the reader sees an
		// up-to-date beat and computes no pending charge. The reverse
		// interleaving costs one read that misses a stall still in flight,
		// which is the direction this package already errs in.
		m.beatNanos.Store(int64(now.Sub(m.origin)))
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

// pending returns the interval since the last observed heartbeat, and the part
// of it that is already scheduler stall by the same rule observe applies. It is
// the answer to "what has this monitor not been able to tell me yet": a tick
// cannot be delivered while the process is frozen, so a stall in progress, or
// one that ended within the last heartbeat, is invisible in lostNanos and
// visible only here (#501).
//
// It computes; it does not accumulate. Nothing here writes to the charged
// totals, so the tick that eventually arrives charges the same stall exactly
// once and a caller folding this into a Report cannot double-count it.
func (m *monitor) pending(now time.Time, nominal time.Duration) (gap, lost time.Duration) {
	if m.origin.IsZero() {
		// Never started -- no heartbeat, and no instant at which the process
		// was known to be running. Nothing may be claimed.
		return 0, 0
	}
	gap = now.Sub(m.origin) - time.Duration(m.beatNanos.Load())
	if gap <= nominal*tolerance {
		return gap, 0
	}
	// Charge the same quantity observe would when the tick finally lands: the
	// EXCESS over the nominal interval. Matching it is what makes the read at
	// resume agree with the read two heartbeats later, which is the whole
	// point.
	return gap, gap - nominal
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
	// Hung means a cap was reached while the process was running normally --
	// the budget of scheduled time was spent, or the hard wall-clock cap was
	// hit, without scheduler stall dominating the window. The call under test
	// did not terminate.
	Hung
	// Inconclusive means a cap was reached in a window that Report.Starved
	// reports as dominated by scheduler stall. Whether the call would have
	// terminated is unknown, so nothing may be asserted about this input.
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
	// being scheduled. It includes a gap since the last heartbeat that the
	// ticker has not been able to charge yet; see Budget.Report.
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
//
// The snapshot includes stall the heartbeat has not charged yet. A tick cannot
// be delivered while the process is frozen, so at the instant a freeze ends the
// accumulated total still reads zero and the window looks perfectly healthy --
// which is #501: a Check landing there charged the whole freeze to the code
// under test and called it Hung. Report therefore asks the monitor what it has
// not been able to account for and folds it in.
//
// The fold is transient: monitor.pending only computes, so the ticker's
// accounting is untouched and the delayed tick charges the stall exactly once
// whenever it does arrive. The one interleaving that could count it twice --
// reading a total that already includes the tick's charge alongside a beat
// timestamp from before it -- is excluded by the store order in monitor.run
// together with the read order below.
func (b *Budget) Report() Report {
	longest := defaultMonitor.longest()
	if longest < b.longestAt {
		longest = b.longestAt
	}
	now := time.Now()
	r := Report{
		Wall: now.Sub(b.startedAt),
		// Read the charged total FIRST, the beat timestamp second: see
		// monitor.run.
		Lost: defaultMonitor.lost() - b.lostAt,
	}
	// longestNanos is a process-wide maximum, so it is only meaningful for this
	// window when it grew during it.
	if longest > b.longestAt {
		r.LongestStall = longest
	}
	if gap, unaccounted := defaultMonitor.pending(now, tick); unaccounted > 0 {
		// A window cannot have lost more time than it has existed, and a gap
		// that began before this budget did is not this window's to charge, so
		// cap the read-time charge at whatever part of the window is not
		// accounted for already.
		if room := r.Wall - r.Lost; unaccounted > room {
			unaccounted = room
		}
		if unaccounted > 0 {
			r.Lost += unaccounted
			if stall := min(gap, r.Wall); stall > r.LongestStall {
				r.LongestStall = stall
			}
		}
	}
	return r
}

// Check interprets a fired watchdog timer. When it returns Continue, the
// second value is how much longer to wait before checking again.
func (b *Budget) Check() (Verdict, time.Duration, Report) {
	r := b.Report()
	// Two independent caps, one meaning. `spent` is the budget of scheduled
	// time actually elapsing; `outOfWall` is the backstop that stops a
	// permanently starved process waiting until `go test -timeout` kills the
	// binary with no crasher written.
	spent := r.Scheduled() >= b.total
	outOfWall := r.Wall >= b.hardWall
	if spent || outOfWall {
		// Starvation is consulted for BOTH caps. Testing `spent` first and
		// returning Hung from it without asking Starved is #488: it made
		// Inconclusive unreachable above ~25% CPU share and reported a window
		// that was 64% scheduler stall as a hang. See the package doc.
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
