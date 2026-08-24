// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The LOAD-CACHE differential target: one cached file, several environments.
//
// This is FuzzSharedProgramMultiEnv's sibling, and the reason it is a
// sibling rather than a fourth arm of that target is that the two share a
// property but not a code path.  FuzzSharedProgramMultiEnv shares a
// lisp.Program the test itself holds and evaluates through LoadProgram; this
// target shares nothing itself — it installs a lisp.LoadCache and lets
// (*LEnv).readCached decide what each environment gets.  Everything between
// "the bytes" and "the expressions the evaluator sees" is under test here
// and is stubbed out there: the key derivation, the admission walk, the
// store, the hit, and the decision to alias rather than copy.
//
// The property is FuzzSharedProgramMultiEnv's, restated for the hook:
//
//	For a source file S and independent environments E1..En sharing one
//	LoadCache, loading S in Ei must produce exactly what loading S with NO
//	cache produces in a virgin environment, the cached tree's sealed bytes
//	must be unchanged after every one of those loads, and Ei must not be
//	able to observe anything Ej wrote.
//
// The determinism control is the same and for the same reason: the mutator
// writes whatever it likes, `(time:utc-now)` legitimately differs between
// runs, and a differential assertion is only sound over deterministic
// programs.  Two no-cache runs in two virgin environments are the baseline
// and its control; a divergence is only reported when the control agreed
// AND the divergence survives a second isolation.
//
// Assertions:
//
//  1. Sealed stability of the cache entry, re-checked after EVERY load in
//     EVERY environment.  Asserted unconditionally, deterministic or not.
//  2. Result equality against the no-cache baseline, per environment and
//     per repetition.
//  3. No cross-environment state, through the same whole-binding-state
//     digest.
//  4. The cache is actually being exercised: the file is parsed exactly
//     once across every environment.  Without this the target would still
//     pass if readCached silently reparsed, and would then be asserting
//     nothing about the hook at all.
//  5. Termination and no recovered Go panic, from the shared budgeted
//     harness.
//
// Not asserted: concurrency.  Environments run one after another — the
// substrate topology, and what keeps a crasher reproducible — so a shared
// cache under concurrent load remains uncovered here, exactly as it is in
// FuzzSharedProgramMultiEnv.

const (
	loadCacheFuzzMaxEnvs = 3
	loadCacheFuzzMaxReps = 3
	loadCacheFuzzName    = "fuzz.lisp"
)

// fuzzLoadCache is the LoadCache under test: a plain map with a mutex,
// which is the shape an embedder writes, plus the store counter assertion 4
// reads.
type fuzzLoadCache struct {
	entries map[string]*lisp.CachedSource
	mu      sync.Mutex
	stores  int
}

func newFuzzLoadCache() *fuzzLoadCache {
	return &fuzzLoadCache{entries: make(map[string]*lisp.CachedSource)}
}

func (c *fuzzLoadCache) Load(key string) (*lisp.CachedSource, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	src, ok := c.entries[key]
	return src, ok
}

func (c *fuzzLoadCache) Store(key string, src *lisp.CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.stores++
	c.entries[key] = src
}

func (c *fuzzLoadCache) sole() *lisp.CachedSource {
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.entries) != 1 {
		return nil
	}
	for _, src := range c.entries {
		return src
	}
	return nil
}

// FuzzLoadCacheMultiEnv loads one source file into several independent
// environments through a shared lisp.LoadCache, against a no-cache
// baseline.  See the file comment for the property and the determinism
// control.
func FuzzLoadCacheMultiEnv(f *testing.F) {
	// The same corpus FuzzSharedProgramMultiEnv seeds from, and for the
	// same reasons: programs that actually run are the only ones that can
	// exercise a shared parse, the runaway half is deliberately excluded
	// (a program the budget stops has no result to compare and leaves no
	// state behind), and the knob byte spreads the environment/repetition
	// choice across the corpus.
	knob := uint8(0)
	add := func(src string) {
		f.Add(knob, []byte(src))
		knob++
	}
	for _, src := range fuzzseed.EvalTerminating() {
		add(src)
	}
	for _, src := range fuzzseed.EvalErroring() {
		add(src)
	}
	for _, src := range fuzzseed.EvalAdversarial() {
		add(src)
	}
	// The hand-written shapes aimed at a SHARED parse — a literal sorted in
	// place, a slice through a literal's capacity, a macro over a shared
	// node — apply unchanged when the sharing is the cache's doing.
	for _, src := range sharedProgramSeeds {
		add(src)
	}

	f.Fuzz(func(t *testing.T, knob uint8, src []byte) {
		// Cheap reject first, before any environment is built: unparsable
		// input is most of what the mutator produces and environment
		// construction is this target's whole cost.  Parsing here rather
		// than pattern-matching the reader's error message out of a load
		// result keeps evaluation errors — which ARE good differential
		// subjects — in the corpus.
		if _, err := lisp.ReadProgram(parser.NewReader(), loadCacheFuzzName, bytes.NewReader(src)); err != nil {
			return
		}

		baseline, ok := runFileFresh(t, src, 2)
		if !ok {
			return
		}
		control, ok := runFileFresh(t, src, 2)
		if !ok {
			return
		}
		deterministic := baseline.equal(control)

		nenv := 2 + int(knob)%(loadCacheFuzzMaxEnvs-1)
		reps := 2 + int(knob/8)%(loadCacheFuzzMaxReps-1)

		cache := newFuzzLoadCache()
		var entry *lisp.CachedSource
		var fp uint64
		for i := range nenv {
			got, ok := runFileCached(t, src, cache, &entry, &fp, i, reps)
			if !ok {
				return
			}
			// Assertion 4: the hook is doing its job, or the rest of this
			// target is asserting nothing about it.  A parse that the
			// admission legitimately refuses stores nothing, and entry
			// stays nil — that case is skipped rather than failed, and
			// TestLoadCacheRefusesUncacheableParse covers it deterministically.
			if entry != nil && cache.stores != 1 {
				t.Fatalf("the source was parsed %d times across %d environments;"+
					" a shared cache must parse it once\n--- source (%d bytes) ---\n%q",
					cache.stores, nenv, len(src), src)
				return
			}
			if !sharedDivergenceReportable(deterministic, got, baseline) {
				continue
			}
			if !confirmCachedDivergence(t, src, cache, &entry, &fp, i, reps, baseline) {
				continue
			}
			t.Fatalf("environment %d loading a CACHED file diverged from the uncached baseline,"+
				" reproducibly and with the uncached control agreeing:"+
				" a cached load must be indistinguishable from a fresh parse"+
				"\n--- baseline ---\n%s\n--- cached, env %d ---\n%s"+
				"\n--- source (%d bytes) ---\n%q",
				i, baseline, i, got, len(src), src)
			return
		}
	})
}

// runFileFresh loads src in a virgin environment with NO cache installed,
// reps times.  This is what a cached load has to be indistinguishable from.
func runFileFresh(t *testing.T, src []byte, reps int) (programRun, bool) {
	t.Helper()
	return runFileIn(t, src, nil, nil, nil, -1, reps)
}

// runFileCached loads src in a virgin environment with the shared cache
// installed, re-checking the entry's sealed fingerprint after every load.
// The first call populates entry/fp; later calls assert against them.
func runFileCached(t *testing.T, src []byte, cache *fuzzLoadCache, entry **lisp.CachedSource, fp *uint64, envIdx, reps int) (programRun, bool) {
	t.Helper()
	return runFileIn(t, src, cache, entry, fp, envIdx, reps)
}

// runFileIn is the shared body.  A nil cache disables both the hook and the
// seal oracle, which is correct for the baseline: nothing else can observe
// its parse.
func runFileIn(t *testing.T, src []byte, cache *fuzzLoadCache, entry **lisp.CachedSource, fp *uint64, envIdx, reps int) (programRun, bool) {
	t.Helper()

	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
		return programRun{}, false
	}
	if cache != nil {
		env.Runtime.LoadCache = cache
	}

	run := programRun{results: make([]string, 0, reps)}
	for rep := range reps {
		result, ok := loadFileBudgeted(t, env, src, envIdx, rep)
		if !ok {
			return programRun{}, false
		}
		run.results = append(run.results, valueFingerprint([]*lisp.LVal{result}))

		if cache == nil {
			continue
		}
		if *entry == nil {
			*entry = cache.sole()
			if *entry != nil {
				*fp = lisp.SealedASTFingerprint(lisp.CachedSourceExprs(*entry))
				if *fp != (*entry).Fingerprint() {
					t.Fatalf("the cache entry's admission fingerprint (%016x) does not describe the tree it holds (%016x)",
						(*entry).Fingerprint(), *fp)
					return programRun{}, false
				}
			}
			continue
		}
		// Assertion 1, after EVERY load rather than once at the end.
		if after := lisp.SealedASTFingerprint(lisp.CachedSourceExprs(*entry)); after != *fp {
			t.Fatalf("load %d in environment %d corrupted the SHARED cached parse"+
				" (fingerprint %016x -> %016x): one environment wrote storage every other"+
				" environment shares (the substrate#378 class)"+
				"\n--- source (%d bytes) ---\n%q",
				rep+1, envIdx, *fp, after, len(src), src)
			return programRun{}, false
		}
	}
	run.state = envStateFingerprint(env)
	return run, true
}

// loadFileBudgeted performs one load under the evaluation budget, on its own
// goroutine, with the scheduled-time watchdog running.  It is
// loadProgramBudgeted's sibling; the difference is that the source goes in
// as BYTES, so the cache hook is in the path.
//
// A parse failure returns (nil, true): most mutator output is unparsable and
// that is not a finding.
func loadFileBudgeted(t *testing.T, env *lisp.LEnv, src []byte, envIdx, rep int) (*lisp.LVal, bool) {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()

	ch := make(chan *lisp.LVal, 1)
	go func() {
		ch <- env.LoadLocationContext(ctx, loadCacheFuzzName, loadCacheFuzzName, bytes.NewReader(src))
	}()

	budget := fuzzwatch.New(watchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case result := <-ch:
			if result == nil {
				t.Fatalf("load %d in environment %d returned a nil LVal", rep+1, envIdx)
				return nil, false
			}
			if lisp.IsInternalPanic(result) {
				t.Fatalf("load %d in environment %d recovered a Go panic"+
					" (a host-code defect, not a lisp error)\n--- error ---\n%v",
					rep+1, envIdx, result)
				return nil, false
			}
			if containerRenderable([]*lisp.LVal{result}) {
				_ = result.String()
			}
			return result, true
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return nil, false
			default:
				t.Fatalf("load %d in environment %d did not terminate within %s of SCHEDULED"+
					" time despite a %s context deadline (%s)",
					rep+1, envIdx, budget.Total(), fuzzDeadline, report)
				return nil, false
			}
		}
	}
}

// confirmCachedDivergence re-runs the divergence with a matched pair — one
// uncached run, one cached run — and reports true only when the uncached run
// still agrees with the baseline AND the cached run still disagrees.  Same
// discipline as confirmSharedDivergence: the target refuses to report a
// crasher it cannot attribute to the cache.
func confirmCachedDivergence(t *testing.T, src []byte, cache *fuzzLoadCache, entry **lisp.CachedSource, fp *uint64, envIdx, reps int, baseline programRun) bool {
	t.Helper()
	fresh, ok := runFileFresh(t, src, reps)
	if !ok || !fresh.equal(baseline) {
		return false
	}
	again, ok := runFileCached(t, src, cache, entry, fp, envIdx, reps)
	return ok && !again.equal(baseline)
}
