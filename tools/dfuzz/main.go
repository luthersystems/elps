// Copyright © 2026 The ELPS authors

// Command dfuzz is a DIFFERENTIAL fuzzing harness for the elps sealed-AST /
// copy-on-write work.
//
// It evaluates the same generated program in two interpreters -- STOCK
// (origin/main) and SEALED (the copy-on-write branch) -- inside ONE process,
// and reports every observable difference.  See README.md for the two-module
// mechanism that makes a single-process diff possible, and for how to run it.
//
// Why differential rather than more unit tests: every one of the four
// corruption / semantics defects found by hand during the seal work was caught
// by a differential or structural check and NONE by a unit test.  A unit test
// asserts what its author already suspected; a differential oracle asserts the
// thing nobody thought to suspect, which is that the rewrite changed nothing
// it did not mean to change.
package main

import (
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

// Evaluation budget.  Shared by both trees so a divergence can never be an
// artefact of one side being given more room than the other.  Sized like the
// in-tree eval fuzz target's budget: generous enough that every seed
// completes, tight enough that a runaway program dies in milliseconds.
const (
	maxSteps          = 2_000_000
	maxTailIterations = 100_000
	maxPhysicalHeight = 2_000
	maxEvalNesting    = 20_000
	maxAlloc          = 1_000_000
	maxMacroDepth     = 100

	// evalDeadline is generous rather than tight: the step, tail, height and
	// alloc budgets above already bound every program that reaches an
	// interpreter check, so the deadline is only the backstop for a builtin
	// that loops inside Go.  Sized to survive a starved sandbox, because a
	// deadline that fires because the machine was busy produces a divergence
	// that is about the machine.
	evalDeadline = 10 * time.Second
	// watchdog is the outer bound on one evaluation.  Reaching it means the
	// interpreter ignored every budget it was given, which is itself the
	// finding.  The goroutine is leaked, which is acceptable because the run
	// has already failed.
	watchdog = 60 * time.Second
)

type result struct {
	// Starved marks a pair decided by wall clock on both attempts, which is
	// not comparable.
	Starved  bool
	Seed     int64
	Src      string
	Stock    Outcome
	Sealed   Outcome
	Class    Classification
	Diverged []Divergence
}

func main() {
	var (
		start     = flag.Int64("start", 1, "first seed")
		count     = flag.Int64("n", 0, "number of programs to generate (0 = until -duration elapses)")
		duration  = flag.Duration("duration", 0, "run for this long (0 = -n programs only)")
		workers   = flag.Int("workers", 4, "concurrent program pairs")
		seedsOnly = flag.Bool("seeds", false, "evaluate only the built-in seed corpus and exit")
		reproFile = flag.String("repro", "", "evaluate a single program read from this file and print both outcomes")
		maxReport = flag.Int("max-report", 40, "stop after this many distinct findings")
		allowFlag = flag.String("allow", "", "comma-separated allowlist rules to enable in addition to the defaults")
		listAllow = flag.Bool("list-allow", false, "print the allowlist and exit")
		shrink    = flag.Bool("shrink", true, "shrink a diverging program to a minimal reproducer")
		elpspath  = flag.Bool("elpspath", false, "generate elpspath shapes (only valid when the left tree has the package: b7ad5ca or later)")
		tally     = flag.Bool("tally", false, "print the most common agreeing-error messages and exit (generator tuning)")
		selfEvery = flag.Int("selfcheck", 100, "self-check every Nth program: evaluate it TWICE in the same tree and report any difference as harness nondeterminism (0 disables)")
		verbose   = flag.Bool("v", false, "print progress")
	)
	flag.Parse()
	enableElpspath = *elpspath

	if *listAllow {
		def := defaultAllow()
		for _, r := range allowRules {
			state := "OFF"
			if def[r.Name] {
				state = "ON "
			}
			fmt.Printf("%s  %s\n      %s\n", state, r.Name, r.Why)
		}
		return
	}

	allow := defaultAllow()
	for _, name := range strings.Split(*allowFlag, ",") {
		if name = strings.TrimSpace(name); name != "" {
			allow[name] = true
		}
	}

	if *reproFile != "" {
		src, err := os.ReadFile(*reproFile)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(2)
		}
		runOne(-1, string(src), allow, true)
		return
	}

	if *seedsOnly {
		findings := 0
		for i, src := range seedCorpus() {
			r := runOne(int64(i), src, allow, false)
			if len(r.Class.Findings) > 0 {
				findings++
				reportResult(os.Stdout, r, "SEED")
			}
		}
		fmt.Printf("\nseeds=%d findings=%d\n", len(seedCorpus()), findings)
		if findings > 0 {
			os.Exit(1)
		}
		return
	}

	if *tally {
		counts := map[string]int{}
		n := int64(1000)
		if *count > 0 {
			n = *count
		}
		ok := 0
		for i := int64(0); i < n; i++ {
			g := NewGen(*start + i)
			src := g.Program()
			if (*start+i)%3 == 0 {
				src = g.TopLevelProgram()
			}
			o := evalStock(src)
			if !o.IsError {
				ok++
				continue
			}
			counts[trunc(firstLine(o.Msg))]++
		}
		type kv struct {
			k string
			v int
		}
		var ks []kv
		for k, v := range counts {
			ks = append(ks, kv{k, v})
		}
		sort.Slice(ks, func(i, j int) bool { return ks[i].v > ks[j].v })
		fmt.Printf("clean=%d/%d\n", ok, n)
		for i, e := range ks {
			if i >= 25 {
				break
			}
			fmt.Printf("%5d  %s\n", e.v, e.k)
		}
		return
	}

	deadline := time.Time{}
	if *duration > 0 {
		deadline = time.Now().Add(*duration)
	}

	var (
		execs    atomic.Int64
		diverged atomic.Int64
		intended atomic.Int64
		findings atomic.Int64
		errs     atomic.Int64
		starved  atomic.Int64
		classMu  sync.Mutex
		classN   = map[string]int{}
		classEg  = map[string]result{}
		selfBad  atomic.Int64
		selfRun  atomic.Int64
		nextSeed atomic.Int64
		mu       sync.Mutex
		reported = map[string]bool{}
		stop     atomic.Bool
	)
	nextSeed.Store(*start)

	// The seed corpus runs first, on every run, single-threaded: a harness
	// that cannot reproduce its own known shapes has nothing useful to say
	// about generated ones.
	for i, src := range seedCorpus() {
		r := runOne(int64(-1000-i), src, allow, false)
		execs.Add(1)
		if len(r.Diverged) > 0 {
			diverged.Add(1)
			if len(r.Class.Findings) > 0 {
				findings.Add(1)
				reportResult(os.Stdout, r, "SEED")
			} else {
				intended.Add(1)
			}
		}
	}

	var wg sync.WaitGroup
	progress := time.Now()
	for w := 0; w < *workers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for {
				if stop.Load() {
					return
				}
				seed := nextSeed.Add(1) - 1
				if *count > 0 && seed >= *start+*count {
					return
				}
				if !deadline.IsZero() && time.Now().After(deadline) {
					return
				}
				g := NewGen(seed)
				var src string
				if seed%3 == 0 {
					src = g.TopLevelProgram()
				} else {
					src = g.Program()
				}
				if *selfEvery > 0 && seed%int64(*selfEvery) == 0 {
					selfRun.Add(1)
					if ds := selfCheck(src); len(ds) > 0 {
						selfBad.Add(1)
						mu.Lock()
						fmt.Printf("\n===== HARNESS NONDETERMINISM seed=%d =====\n%s--- same tree, two runs ---\n", seed, src)
						for _, d := range ds {
							fmt.Printf("  %s\n", d)
						}
						mu.Unlock()
					}
				}
				r := runOne(seed, src, allow, false)
				execs.Add(1)
				if r.Starved {
					starved.Add(1)
					continue
				}
				if r.Stock.IsError && r.Sealed.IsError {
					errs.Add(1)
				}
				if len(r.Diverged) == 0 {
					continue
				}
				diverged.Add(1)
				if len(r.Class.Intended) > 0 {
					classMu.Lock()
					for name := range r.Class.Intended {
						classN[name]++
						if _, seen := classEg[name]; !seen {
							classEg[name] = r
						}
					}
					classMu.Unlock()
				}
				if len(r.Class.Findings) == 0 {
					intended.Add(1)
					continue
				}
				key := findingKey(r)
				mu.Lock()
				dup := reported[key]
				reported[key] = true
				n := len(reported)
				mu.Unlock()
				if dup {
					findings.Add(1)
					continue
				}
				findings.Add(1)
				if *shrink {
					r = shrinkResult(r, allow)
				}
				mu.Lock()
				reportResult(os.Stdout, r, "FINDING")
				mu.Unlock()
				if n >= *maxReport {
					stop.Store(true)
					return
				}
			}
		}()
	}

	if *verbose {
		go func() {
			t := time.NewTicker(30 * time.Second)
			defer t.Stop()
			for range t.C {
				if stop.Load() {
					return
				}
				fmt.Fprintf(os.Stderr, "[%s] execs=%d diverged=%d intended=%d findings=%d\n",
					time.Since(progress).Round(time.Second), execs.Load(), diverged.Load(),
					intended.Load(), findings.Load())
			}
		}()
	}

	wg.Wait()
	fmt.Printf("\n== dfuzz summary ==\n")
	fmt.Printf("elapsed            %s\n", time.Since(progress).Round(time.Second))
	fmt.Printf("program pairs      %d\n", execs.Load())
	fmt.Printf("evaluations        %d  (2 per pair)\n", 2*execs.Load())
	fmt.Printf("agreeing errors    %d\n", errs.Load())
	fmt.Printf("starved (dropped)  %d\n", starved.Load())
	fmt.Printf("self-checks        %d, nondeterministic %d\n", selfRun.Load(), selfBad.Load())
	fmt.Printf("diverged           %d\n", diverged.Load())
	fmt.Printf("  intended         %d\n", intended.Load())
	classMu.Lock()
	names := make([]string, 0, len(classN))
	for n := range classN {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		fmt.Printf("    %-34s %d programs\n", n, classN[n])
	}
	egs := make([]result, 0, len(names))
	for _, n := range names {
		egs = append(egs, classEg[n])
	}
	classMu.Unlock()
	// One exemplar per intended class.  A classified divergence is never
	// hidden -- only excused from failing the run.
	for i, n := range names {
		fmt.Printf("\n===== INTENDED exemplar (%s) =====\n", n)
		reportResult(os.Stdout, egs[i], "INTENDED")
	}
	fmt.Printf("  findings         %d (%d distinct)\n", findings.Load(), len(reported))
	if findings.Load() > 0 {
		os.Exit(1)
	}
}

// seedCorpus is the seed set for this run: the core shapes always, plus the
// elpspath shapes when the left-hand tree has that package.
func seedCorpus() []string {
	if !enableElpspath {
		return SeedCorpus
	}
	return append(append([]string{}, SeedCorpus...), ElpspathSeedCorpus...)
}

// runOne evaluates src in both trees and classifies the difference.
//
// A pair where either side was STARVED (see Outcome.Starved) is retried once:
// a wall-clock outcome is not a semantic outcome.  If it starves again the
// pair is marked non-comparable and counted separately rather than reported.
func runOne(seed int64, src string, allow map[string]bool, print bool) result {
	r := evalPair(seed, src, allow)
	if r.Stock.Starved || r.Sealed.Starved {
		r = evalPair(seed, src, allow)
		if r.Stock.Starved || r.Sealed.Starved {
			r.Starved = true
			r.Diverged = nil
			r.Class = Classification{}
		}
	}
	if print {
		reportResult(os.Stdout, r, "REPRO")
	}
	return r
}

func evalPair(seed int64, src string, allow map[string]bool) result {
	r := result{Seed: seed, Src: src}
	r.Stock = evalStock(src)
	r.Sealed = evalSealed(src)
	r.Diverged = Compare(r.Stock, r.Sealed)
	r.Class = Classify(src, r.Diverged, r.Stock, r.Sealed, allow)
	return r
}

// selfCheck evaluates src in the SAME tree twice and reports any difference.
//
// This is the oracle's own smoke alarm.  Every divergence this harness reports
// rests on the assumption that one interpreter, given one program, produces
// one answer.  If that is false -- an address, a counter, a map order leaking
// into a printed value -- then every report is suspect.  Sampling it turns the
// assumption into an assertion, and it costs one extra evaluation per sampled
// program.
func selfCheck(src string) []Divergence {
	a := evalStock(src)
	b := evalStock(src)
	if a.Starved || b.Starved {
		return nil
	}
	return Compare(a, b)
}

// findingKey collapses findings that are the same defect seen through
// different generated programs, so a single systematic divergence does not
// bury the report under thousands of near-identical entries.
func findingKey(r result) string {
	var b strings.Builder
	fields := make([]string, 0, len(r.Class.Findings))
	for _, d := range r.Class.Findings {
		fields = append(fields, d.Field)
	}
	sort.Strings(fields)
	b.WriteString(strings.Join(fields, "|"))
	b.WriteString("##")
	b.WriteString(r.Stock.Cond + ">" + r.Sealed.Cond)
	b.WriteString("##")
	b.WriteString(firstLine(r.Stock.Msg) + ">" + firstLine(r.Sealed.Msg))
	return b.String()
}

// shrinkResult removes statements from a diverging program for as long as the
// SAME finding key survives.  A shrunk reproducer is the difference between a
// report someone can act on and a wall of generated noise.
func shrinkResult(r result, allow map[string]bool) result {
	want := findingKey(r)
	lines := strings.Split(strings.TrimRight(r.Src, "\n"), "\n")
	for changed := true; changed; {
		changed = false
		for i := 0; i < len(lines); i++ {
			cand := make([]string, 0, len(lines)-1)
			cand = append(cand, lines[:i]...)
			cand = append(cand, lines[i+1:]...)
			src := strings.Join(cand, "\n") + "\n"
			if strings.TrimSpace(src) == "" {
				continue
			}
			c := runOne(r.Seed, src, allow, false)
			if len(c.Class.Findings) > 0 && findingKey(c) == want {
				lines = cand
				r = c
				r.Src = src
				changed = true
				i--
			}
		}
	}
	return r
}

func reportResult(w *os.File, r result, tag string) {
	fmt.Fprintf(w, "\n===== %s seed=%d =====\n", tag, r.Seed)
	fmt.Fprintf(w, "--- program ---\n%s", r.Src)
	if !strings.HasSuffix(r.Src, "\n") {
		fmt.Fprintln(w)
	}
	isFinding := make(map[Divergence]bool, len(r.Class.Findings)) //nolint:gocritic
	for _, f := range r.Class.Findings {
		isFinding[f] = true
	}
	fmt.Fprintf(w, "--- divergences ---\n")
	for _, d := range r.Diverged {
		mark := "intended"
		if isFinding[d] {
			mark = "FINDING"
		}
		fmt.Fprintf(w, "  [%s] %s\n", mark, d)
	}
	if len(r.Class.Intended) > 0 {
		names := make([]string, 0, len(r.Class.Intended))
		for n := range r.Class.Intended {
			names = append(names, n)
		}
		sort.Strings(names)
		fmt.Fprintf(w, "--- allowlist rules applied: %s\n", strings.Join(names, ", "))
	}
	fmt.Fprintf(w, "--- stock  --- type=%s err=%v cond=%s panic=%v steps=%d\n  value: %s\n",
		r.Stock.Type, r.Stock.IsError, r.Stock.Cond, r.Stock.InternalPanic, r.Stock.Steps, trunc(r.Stock.Value))
	if r.Stock.Msg != "" {
		fmt.Fprintf(w, "  msg:   %s\n", trunc(r.Stock.Msg))
	}
	fmt.Fprintf(w, "--- sealed --- type=%s err=%v cond=%s panic=%v steps=%d\n  value: %s\n",
		r.Sealed.Type, r.Sealed.IsError, r.Sealed.Cond, r.Sealed.InternalPanic, r.Sealed.Steps, trunc(r.Sealed.Value))
	if r.Sealed.Msg != "" {
		fmt.Fprintf(w, "  msg:   %s\n", trunc(r.Sealed.Msg))
	}
}

func trunc(s string) string {
	const n = 400
	if len(s) > n {
		return s[:n] + "...<truncated>"
	}
	return s
}
