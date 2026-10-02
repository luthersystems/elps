// Copyright © 2026 The ELPS authors

package testdeadline

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"time"
)

// cpuClock reports CPU time used so far.  ok is false when the reading failed,
// for example because the thread it reads has already exited.
type cpuClock func() (used time.Duration, ok bool)

// wallClock is the fallback cpuClock: wall time since it was created.  It is
// what every bound in this package measured before it measured CPU time.
func wallClock() cpuClock {
	start := time.Now()
	return func() (time.Duration, bool) { return time.Since(start), true }
}

// pollEvery is how often a watchdog reads its clock.
const pollEvery = 10 * time.Millisecond

// Within runs fn on its own OS thread and waits until fn returns or has used
// Scale(budget) of CPU time.  It returns the CPU time fn used, and ok=false if
// fn was still running when the budget was spent.  On ok=false fn's goroutine
// is left running: the callers use Within for code that cannot be
// interrupted, which is the defect they test for.
//
// It bounds CPU time, not wall time, because a test asserts what the code
// does, and a starved process does the same work in more wall time.  A
// process at 0.1% CPU share (-cpu=1 beside a CPU hog) can take minutes of wall
// time over milliseconds of work, so no wall-clock bound is safe from it.  CPU
// time is charged only while fn's thread runs, so it does not grow while the
// process waits for a CPU.
//
// Within bounds only work done on fn's goroutine.  It does not bound time fn
// spends blocked: a call that waits forever uses no CPU, and the `go test`
// -timeout is the backstop for it.  On a platform where this package cannot
// read thread CPU time (any OS but Linux), it bounds wall time instead.
func Within(budget time.Duration, fn func()) (time.Duration, bool) {
	used, reason := watch(Scale(budget), 0, fn)
	return used, reason == ""
}

// watch runs fn on a locked OS thread and returns the CPU time it used.  The
// reason is empty if fn returned, and otherwise says which bound fn exceeded:
// budget of CPU time, or, if maxHeap is not zero, maxHeap bytes of heap
// growth.
func watch(budget time.Duration, maxHeap uint64, fn func()) (time.Duration, string) {
	base := heapBytes()
	clocks := make(chan cpuClock, 1)
	done := make(chan time.Duration, 1)
	go func() {
		// Never unlocked: the thread ends with this goroutine, so no other
		// goroutine runs on it while its CPU time is read.
		runtime.LockOSThread()
		clock := threadCPU()
		// A thread can come from the runtime's pool with CPU time already
		// spent, so measure from here.
		start, _ := clock()
		clocks <- func() (time.Duration, bool) {
			now, ok := clock()
			return now - start, ok
		}
		fn()
		used, _ := clock()
		done <- used - start
	}()
	clock := <-clocks
	tick := time.NewTicker(pollEvery)
	defer tick.Stop()
	for {
		select {
		case used := <-done:
			return used, ""
		case <-tick.C:
			if used, ok := clock(); ok && used > budget {
				return used, fmt.Sprintf("used %v of CPU time without returning (budget %v)", used, budget)
			}
			if maxHeap == 0 {
				continue
			}
			if h := heapBytes(); h > base && h-base > maxHeap {
				used, _ := clock()
				return used, fmt.Sprintf("heap grew by %d MB while it ran (limit %d MB)", (h-base)>>20, maxHeap>>20)
			}
		}
	}
}

// ErrOverBudget is the error RunChild wraps when it kills a child for using
// its CPU budget.
var ErrOverBudget = errors.New("over its CPU budget")

// RunChild runs cmd and returns its combined output, as cmd.CombinedOutput
// does.  It kills the child once the child has used Scale(budget) of CPU time,
// summed over its threads, and then returns an error that wraps
// ErrOverBudget.
//
// It is for tests that re-execute the test binary to contain a regression
// that does not return.  A wall-clock deadline on the child counts process
// start-up and every moment the child waits for a CPU, so it fails a correct
// build on a starved runner; CPU time does not grow while the child waits.  A
// child that blocks without using CPU is not bounded here: callers keep
// exec.CommandContext(t.Context(), ...) so the child ends with the test.  On
// a platform where this package cannot read process CPU time (any OS but
// Linux), RunChild bounds wall time instead.
func RunChild(cmd *exec.Cmd, budget time.Duration) ([]byte, error) {
	return runChild(cmd, Scale(budget))
}

func runChild(cmd *exec.Cmd, budget time.Duration) ([]byte, error) {
	// One writer for both streams: exec then lets at most one goroutine
	// write at a time.
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	stop := guard(cmd.Process, budget)
	err := cmd.Wait()
	if used, killed := stop(); killed {
		return out.Bytes(), fmt.Errorf("killed: the child used %v of CPU time, %w of %v: %w", used, ErrOverBudget, budget, err)
	}
	return out.Bytes(), err
}

// Guard kills the started process p once it has used Scale(budget) of CPU
// time, summed over its threads.  Call the returned stop function after
// cmd.Wait returns; it reports whether Guard killed p.
//
// It is RunChild for a test that drives the child itself (pipes, signals).
// As with RunChild, a child that blocks without using CPU is not bounded:
// pair Guard with Backstop for that.
func Guard(p *os.Process, budget time.Duration) func() bool {
	stopUsed := guard(p, Scale(budget))
	return func() bool {
		_, killed := stopUsed()
		return killed
	}
}

func guard(p *os.Process, budget time.Duration) func() (time.Duration, bool) {
	clock := processCPU(p.Pid)
	quit := make(chan struct{})
	done := make(chan struct{})
	var used time.Duration
	var killed bool
	go func() {
		defer close(done)
		tick := time.NewTicker(pollEvery)
		defer tick.Stop()
		for {
			select {
			case <-quit:
				return
			case <-tick.C:
				u, ok := clock()
				if !ok || u <= budget {
					continue
				}
				used, killed = u, true
				_ = p.Kill()
				return
			}
		}
	}()
	return func() (time.Duration, bool) {
		close(quit)
		<-done
		return used, killed
	}
}
