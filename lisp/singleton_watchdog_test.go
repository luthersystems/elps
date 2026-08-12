// Copyright © 2026 The ELPS authors

package lisp

import (
	"sync"
	"time"
)

// The singleton write watchdog closes the blind spot described on
// checkSingleton: SingletonSnapshot.Verify compares *values*, so a write
// that stores the value a field already holds is invisible to it. That
// is not a hypothetical -- issue #333 was exactly that write
// (`s.quoted = false` into singletonNil, whose Quoted was already
// false), and the only thing that ever caught it was `go test -race` on
// a loaded CI runner where two goroutines happened to overlap.
//
// The watchdog removes the "happened to overlap" part. It runs for the
// lifetime of the package test binary and periodically reads all three
// singletons without synchronizing against anything else. Under -race
// that read sits in the race detector's shadow memory, so ANY
// unsynchronized write to a singleton -- from a single-goroutine test,
// storing any value, same or different -- is reported as a data race
// with a stack trace naming the offending line.
//
// Scope and limits, so nobody over-trusts this either:
//
//   - It only has teeth under -race (`make race`). Without -race the
//     goroutine is a no-op that burns a few microseconds.
//   - It only watches package lisp's test binary. Writes originating in
//     other packages' tests are not covered; the singletons and every
//     historical offender (macro.go #274, lisp.go #333) live here.
//   - It detects unsynchronized writes, not writes as such. A write made
//     while holding singletonWatchdogMu is ordered against the watchdog
//     and stays silent -- which is what pauseSingletonWatchdog is for.

// singletonWatchdogInterval is how often the watchdog re-reads the
// singletons. It does not need to be tight: the race detector reports on
// happens-before, not wall-clock overlap, so a read from a moment ago is
// still in shadow memory and still races with a later unsynchronized
// write. Only the watchdog and a would-be offender ever touch these
// words, so nothing else evicts the read.
const singletonWatchdogInterval = 200 * time.Microsecond

// singletonWatchdogMu orders the watchdog's reads against the handful of
// tests that mutate a singleton on purpose to prove the value-drift
// guard works. Deliberate mutators take it via pauseSingletonWatchdog;
// nothing else does, so an accidental write anywhere else stays
// unordered and gets reported.
var singletonWatchdogMu sync.Mutex

// singletonWatchdogSink receives the watchdog's loads so the compiler
// cannot elide them. Only the watchdog goroutine writes it.
var singletonWatchdogSink SingletonSnapshot

// startSingletonWriteWatchdog launches the watchdog and returns a
// function that stops it and waits for it to exit.
func startSingletonWriteWatchdog() (stop func()) {
	done := make(chan struct{})
	stopped := make(chan struct{})
	go func() {
		defer close(stopped)
		tick := time.NewTicker(singletonWatchdogInterval)
		defer tick.Stop()
		for {
			select {
			case <-done:
				return
			case <-tick.C:
			}
			singletonWatchdogMu.Lock()
			// A whole-struct copy reads every field, including the
			// Cells slice header -- no field can be added to LVal and
			// silently fall outside the watch.
			singletonWatchdogSink = TakeSingletonSnapshot()
			singletonWatchdogMu.Unlock()
		}
	}()
	return func() {
		close(done)
		<-stopped
	}
}

// pauseSingletonWatchdog blocks the watchdog so a test can deliberately
// mutate a singleton without tripping the race detector. Call the
// returned function to resume:
//
//	defer pauseSingletonWatchdog()()
//
// Only tests that are *about* singleton corruption should use this.
// Production code has no business writing to a singleton at all.
func pauseSingletonWatchdog() (resume func()) {
	singletonWatchdogMu.Lock()
	return singletonWatchdogMu.Unlock
}
