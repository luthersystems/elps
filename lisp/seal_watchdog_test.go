// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"sync"
	"time"
)

// The seal write watchdog generalizes the singleton write watchdog
// (singleton_watchdog_test.go, issue #334) from three fixed values to
// arbitrary sealed AST nodes (issue #372).
//
// The checked-mode inspector compares fingerprint *values*, so — exactly
// like the singleton snapshot before it — it is structurally blind to a
// write that stores what a field already holds, and to a metadata write
// that swaps a pointer for an equal-content one.  Issue #370's write
// (stampMacroExpansion restamping a shared node's source) is precisely
// that class: under concurrent environments it is a data race whether or
// not any fingerprint moves.  The watchdog removes the "two goroutines
// happened to collide" requirement for catching it: it keeps an
// unsynchronized read of registered sealed nodes live for the lifetime of
// the package test binary, so under -race ANY unsynchronized write to a
// watched node — same-value and metadata writes included — is reported
// with a stack naming the offender.
//
// Registration is two-channel:
//
//   - EXPLICIT: registerSealWatch (exported to the external test package
//     as RegisterSealWatchForTest) pins specific trees for the tests that
//     prove a guard, e.g. the stampMacroExpansion sealed-skip tests.
//     Deterministic: a registered tree is read on every tick.
//   - AUTOMATIC, elpscheck builds only: each tick additionally samples a
//     bounded batch of roots from the checked-mode inspector's registry
//     (seal_check_elpscheck.go), so under `go test -race -tags elpscheck`
//     the whole suite's parses take turns under watch.  Go's randomized
//     map iteration rotates the sample for free.  Untagged builds record
//     nothing, so the sample is empty and the watchdog only reads
//     explicit registrations (see seal_watchdog_sample_*_test.go).
//
// Scope and limits, honestly:
//
//   - Teeth only under -race; without it the goroutine burns microseconds.
//   - Only this package's test binary.  The historical offenders
//     (macro.go's stamp, builtins.go's mutators) all live here.
//   - Only SEALED nodes are read.  An unsealed node is runtime storage
//     the evaluator may legitimately write; reading it here would turn
//     legal mutation into a false positive.  The walk reads each sealed
//     node's scalar fields, its source-Location contents, its metadata
//     pointers, and its Cells backing array, then recurses into sealed
//     children only.
//   - The automatic channel samples; it does not exhaustively cover every
//     parse on every tick.  The race detector needs a watchdog read of
//     the written word to be live in shadow memory around the write, so
//     sampled coverage is probabilistic for suite-wide writes — the
//     deterministic red-proof tests use explicit registration.
//   - A write made while holding sealWatchdogMu is ordered against the
//     watchdog and stays silent, which is what pauseSealWatchdog is for.
//     Only tests that are ABOUT deliberate sealed-node mutation (e.g.
//     fingerprint sensitivity tests) may use it, and they must restore
//     the bytes they touched.

// sealWatchdogInterval matches the singleton watchdog's cadence: the race
// detector reports on happens-before, not wall-clock overlap, so a read a
// tick ago still races with a later unsynchronized write.
const sealWatchdogInterval = 200 * time.Microsecond

// sealWatchdogSampleRoots bounds how many inspector-recorded roots one
// tick samples in elpscheck builds; sealWatchdogMaxNodes bounds the total
// nodes (explicit + sampled) one tick reads, so a huge parse cannot turn
// a tick into a full-tree scan; sealWatchdogMaxDepth bounds recursion on
// adversarial hand-sealed graphs (parser nesting is capped at 10,000).
const (
	sealWatchdogSampleRoots = 64
	sealWatchdogMaxNodes    = 8192
	sealWatchdogMaxDepth    = 10000
)

// sealWatchdogMu orders the watchdog's reads against registration and
// against the few tests that deliberately mutate a sealed node to prove
// a detector works.  Deliberate mutators take it via pauseSealWatchdog;
// nothing else does, so an accidental write anywhere stays unordered and
// gets reported.
var sealWatchdogMu sync.Mutex

// sealWatchExplicit is the explicit watch list.  Guarded by
// sealWatchdogMu.
var sealWatchExplicit []*LVal

// sealWatchdogSink receives each tick's accumulated loads so the compiler
// cannot elide them.  Only the watchdog goroutine writes it.
var sealWatchdogSink uint64

// startSealWriteWatchdog launches the watchdog and returns a function
// that stops it and waits for it to exit.  Started by TestMain next to
// the singleton watchdog.
func startSealWriteWatchdog() (stop func()) {
	done := make(chan struct{})
	stopped := make(chan struct{})
	go func() {
		defer close(stopped)
		tick := time.NewTicker(sealWatchdogInterval)
		defer tick.Stop()
		for {
			select {
			case <-done:
				return
			case <-tick.C:
			}
			sealWatchdogMu.Lock()
			budget := sealWatchdogMaxNodes
			var acc uint64
			for _, r := range sealWatchExplicit {
				acc = sealWatchRead(r, 0, &budget, acc)
			}
			// elpscheck builds: rotate through the inspector's recorded
			// parses.  The sample call itself synchronizes on the
			// inspector's own mutex, so every write the SEALING goroutine
			// made before recording a root happens-before our reads of it —
			// the only unordered writers left are the illegal ones.
			for _, r := range sealWatchSampleRoots(sealWatchdogSampleRoots) {
				acc = sealWatchRead(r, 0, &budget, acc)
			}
			sealWatchdogSink = acc
			sealWatchdogMu.Unlock()
		}
	}()
	return func() {
		close(done)
		<-stopped
	}
}

// wdBits reinterprets a signed load as bits for the watchdog's
// accumulator.  Sign change is deliberate: the value only exists so the
// compiler cannot elide the load.
func wdBits(i int) uint64 {
	return uint64(i) //nolint:gosec // G115: deliberate bit reinterpretation of a watched load
}

// sealWatchRead performs unsynchronized loads of every field of v and,
// recursively, of its sealed descendants, mixing the loads into acc so
// they cannot be optimized away.  Unsealed nodes contribute only the one
// flag load that identified them — their contents are legitimately
// mutable and must not be watched.
func sealWatchRead(v *LVal, depth int, budget *int, acc uint64) uint64 {
	if v == nil || *budget <= 0 || depth > sealWatchdogMaxDepth {
		return acc
	}
	if !v.sealed {
		return acc ^ 1
	}
	*budget--
	acc ^= uint64(v.Type)<<1 ^ uint64(v.FunType)<<9
	if v.quoted {
		acc ^= 1 << 17
	}
	if v.spliced {
		acc ^= 1 << 18
	}
	acc ^= wdBits(len(v.Str)) // loads the string header
	acc += wdBits(v.Int)
	acc ^= math.Float64bits(v.Float)
	if src := v.source; src != nil {
		acc += wdBits(len(src.File)) + wdBits(len(src.Path))
		acc ^= wdBits(src.Pos)<<1 ^ wdBits(src.Line)<<11 ^ wdBits(src.Col)<<21
		acc ^= wdBits(src.EndPos)<<2 ^ wdBits(src.EndLine)<<12 ^ wdBits(src.EndCol)<<22
	}
	if v.meta != nil {
		acc ^= 1 << 19
	}
	if v.macroExpansion != nil {
		acc ^= 1 << 20
	}
	if v.Native != nil {
		acc ^= 1 << 21
	}
	for _, c := range v.Cells { // loads the slice header and backing array
		acc = sealWatchRead(c, depth+1, budget, acc)
	}
	return acc
}

// registerSealWatch adds roots to the explicit watch list and returns a
// function that removes exactly those entries.  A registered tree is read
// on every watchdog tick, which is what makes detection deterministic for
// the guard-reversal red-proofs.
func registerSealWatch(roots ...*LVal) (unregister func()) {
	sealWatchdogMu.Lock()
	sealWatchExplicit = append(sealWatchExplicit, roots...)
	sealWatchdogMu.Unlock()
	return func() {
		sealWatchdogMu.Lock()
		defer sealWatchdogMu.Unlock()
		remove := make(map[*LVal]int, len(roots))
		for _, r := range roots {
			remove[r]++
		}
		kept := sealWatchExplicit[:0]
		for _, r := range sealWatchExplicit {
			if n := remove[r]; n > 0 {
				remove[r] = n - 1
				continue
			}
			kept = append(kept, r)
		}
		for i := len(kept); i < len(sealWatchExplicit); i++ {
			sealWatchExplicit[i] = nil
		}
		sealWatchExplicit = kept
	}
}

// pauseSealWatchdog blocks the watchdog so a test can deliberately write
// a sealed node without tripping the race detector.  Call the returned
// function to resume:
//
//	defer pauseSealWatchdog()()
//
// Only tests that are *about* sealed-node corruption may use this, and
// they must restore the mutated bytes before resuming — the checked-mode
// inspector's end-of-suite verification does not pause with the watchdog.
func pauseSealWatchdog() (resume func()) {
	sealWatchdogMu.Lock()
	return sealWatchdogMu.Unlock
}
