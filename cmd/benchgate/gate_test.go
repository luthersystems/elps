package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// This file is the fixture corpus both repositories accumulated for their
// shell gates, replayed against the Go one.
//
// It is not a new test suite written to suit the port. Every case below is a
// transcription of an assertion that was in scripts/ci-gates-test.sh in elps
// or in substrate, against the same fixture, with the same environment and the
// same expected exit code -- because "the tool reaches the same verdict the
// shell gate reached, on everything either repository had ever pinned" is the
// acceptance bar for replacing it (issue #538).
//
// Fixture provenance:
//   testdata/elps/       moved verbatim from elps' scripts/testdata/
//   testdata/substrate/  copied verbatim from substrate's scripts/testdata/,
//                        so this repository can prove parity for the consumer
//                        it is about to take over the gating of

const (
	elpsFixtures = "testdata/elps"
	subFixtures  = "testdata/substrate"
	// The shipped elps waiver list, as CI passes it. Several cases below exist
	// precisely to prove it rescues nothing it was not written for, so they
	// must run against the real file rather than a copy.
	shippedWaivers = "../../scripts/benchstat-waivers.txt"
)

// benchEnvVars is every environment variable the tool reads. Each case starts
// from all of them UNSET, because BENCH_WAIVERS distinguishes "unset" from
// "set to empty" and a leaked value would quietly change a verdict.
var benchEnvVars = []string{
	"BENCH_REGRESSION_THRESHOLD_PCT",
	"BENCH_ALLOC_THRESHOLD_PCT",
	"BENCH_ALPHA",
	"BENCH_WAIVERS",
	"BENCH_WAIVER_TODAY",
}

type gateCase struct {
	name string
	// env entries; a value of nil means "leave unset".
	env map[string]*string
	// args after the standard -waivers-default, ending in the fixture path.
	args []string
	// want is the exit code: 0 clean, 1 regression, 2 uninterpretable.
	want        int
	contains    []string
	notContains []string
}

func s(v string) *string { return &v }

func runGate(t *testing.T, c gateCase) (int, string) {
	t.Helper()
	for _, k := range benchEnvVars {
		old, had := os.LookupEnv(k)
		t.Cleanup(func() {
			if had {
				_ = os.Setenv(k, old)
			} else {
				_ = os.Unsetenv(k)
			}
		})
		_ = os.Unsetenv(k)
	}
	for k, v := range c.env {
		if v == nil {
			continue
		}
		_ = os.Setenv(k, *v)
	}
	var out bytes.Buffer
	rc := run(c.args, &out, &out)
	return rc, out.String()
}

func check(t *testing.T, cases []gateCase) {
	t.Helper()
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			rc, out := runGate(t, c)
			if rc != c.want {
				t.Errorf("exit %d, want %d\n%s", rc, c.want, out)
			}
			for _, want := range c.contains {
				if !strings.Contains(out, want) {
					t.Errorf("output does not contain %q\n%s", want, out)
				}
			}
			for _, bad := range c.notContains {
				if strings.Contains(out, bad) {
					t.Errorf("output unexpectedly contains %q\n%s", bad, out)
				}
			}
		})
	}
}

// elpsArgs builds the argument list CI uses: the shipped waiver list as the
// default, then the fixture.
func elpsArgs(fixture string, extra ...string) []string {
	a := []string{"-waivers-default", shippedWaivers}
	a = append(a, extra...)
	return append(a, filepath.Join(elpsFixtures, fixture))
}

func subArgs(fixture string, extra ...string) []string {
	a := []string{"-waivers-default", shippedWaivers}
	a = append(a, extra...)
	return append(a, filepath.Join(subFixtures, fixture))
}

// noWaivers switches the shipped list off. Disabling waivers can only ever
// make the gate stricter, so it is not a bypass -- it is how a case proves the
// row it is about was judged on its own merits.
func noWaivers() map[string]*string { return map[string]*string{"BENCH_WAIVERS": s("")} }

func waivers(path string) map[string]*string {
	return map[string]*string{"BENCH_WAIVERS": s(path)}
}

func TestFiresOnRegressions(t *testing.T) {
	check(t, []gateCase{{
		name: "new-format table with a +83.31% significant timing regression",
		args: elpsArgs("benchstat-regression-new.txt"),
		want: 1, contains: []string{"REGRESSION"},
	}, {
		name: "old-format table with a +83.31% significant timing regression",
		args: elpsArgs("benchstat-regression-old.txt"),
		want: 1,
	}, {
		// The exact sample from the original bug report: a 50% regression at
		// p=0.000 that the old inline grep waved through. This is the headline
		// assertion of the suite.
		name: "the reported +50.00% (p=0.000 n=10) sample FIRES the gate",
		args: elpsArgs("benchstat-task-sample.txt"),
		want: 1, contains: []string{"+50.00%"},
	}})
}

func TestQuietOnCleanComparisons(t *testing.T) {
	check(t, []gateCase{{
		name: "improvements (negative timing deltas) never fire",
		args: elpsArgs("benchstat-improvement-new.txt"), want: 0,
	}, {
		name: "large deltas with p above alpha never fire",
		args: elpsArgs("benchstat-insignificant-new.txt"), want: 0,
	}, {
		name: "old-format table whose deltas are all under the gate",
		args: elpsArgs("benchstat-clean-old.txt"), want: 0,
	}, {
		// benchstat-clean-ci.txt is the REAL CI comparison from the commit
		// that added the shell gate. That commit changed no Go code, so it is
		// a genuine null comparison on the real infrastructure -- every delta
		// in it is noise, and the gate must not fire while still parsing the
		// whole table. This is the fixture that keeps the DEFAULT thresholds
		// honest: if a future retune drops them below the real CI noise floor,
		// this case flips.
		name: "REAL CI null comparison does not fire at the default gates",
		args: elpsArgs("benchstat-clean-ci.txt"), want: 0,
		contains: []string{
			"interpreted 22 delta row(s) + 148 no-change row(s)",
			"3 significant move(s) in the bad direction",
		},
	}, {
		// The same command on a CONTENDED machine is an order of magnitude
		// noisier. Pinned so nobody re-derives the thresholds from a local run
		// and concludes the gate is broken: it fires here, and that is the
		// machine's fault, not the gate's.
		name: "the same comparison on a CONTENDED machine DOES fire (noise, not a bug)",
		args: elpsArgs("benchstat-noisy-sandbox.txt"), want: 1,
	}})
}

func TestMetricDirection(t *testing.T) {
	zero := map[string]*string{
		"BENCH_REGRESSION_THRESHOLD_PCT": s("0"),
		"BENCH_ALLOC_THRESHOLD_PCT":      s("0"),
	}
	check(t, []gateCase{{
		// The adaptation elps needs and the upstream reference did not. B/s is
		// higher-is-better: a +178% delta is a 2.8x throughput GAIN. A gate
		// that reads the raw sign fails an improving PR.
		name: "B/s throughput GAINS are not regressions, even at a 0% gate",
		env:  zero, args: elpsArgs("benchstat-bps-improvement.txt"),
		want: 0, notContains: []string{"REGRESSION"},
	}, {
		name: "B/s throughput COLLAPSE does fire the gate",
		args: elpsArgs("benchstat-bps-regression.txt"),
		want: 1, contains: []string{"higher is better"},
	}, {
		// A B/s DIP of a few percent is the case between those two extremes,
		// and the one that got mistaken for a gate bug on elps PR #310: it is
		// the only column in that run with magnitudes above the allocation
		// threshold, which invites the conclusion that B/s is being judged
		// against the allocation gate. It is not -- B/s has no "/op" suffix.
		name: "a several-percent B/s DIP with flat B/op and allocs/op does NOT fire",
		args: elpsArgs("benchstat-bps-dip-only.txt"), want: 0,
		contains:    []string{"gate 15%"},
		notContains: []string{"REGRESSION"},
	}, {
		// Belt and braces: even if the allocation gate were tightened to zero,
		// the B/s rows must be unaffected by it. If this ever fails, B/s has
		// been mis-classified into the allocation bucket.
		name: "B/s dip is untouched by the ALLOCATION gate, even at 0%",
		env:  map[string]*string{"BENCH_ALLOC_THRESHOLD_PCT": s("0")},
		args: elpsArgs("benchstat-bps-dip-only.txt"), want: 0,
	}})
}

func TestPerMetricClassThresholds(t *testing.T) {
	check(t, []gateCase{{
		// elps' allocation metrics are deterministic while sec/op noise
		// reaches 33.83%. A single threshold cannot serve both. This fixture
		// holds an +8% allocation regression: below the loose timing gate,
		// above the tight allocation one.
		name: "an +8% ALLOCATION regression fires the tight allocation gate",
		args: elpsArgs("benchstat-alloc-regression.txt"),
		want: 1, contains: []string{"allocs/op"},
	}, {
		name: "the same table passes once the allocation gate is raised to 20%",
		env:  map[string]*string{"BENCH_ALLOC_THRESHOLD_PCT": s("20")},
		args: elpsArgs("benchstat-alloc-regression.txt"), want: 0,
	}, {
		name: "a single 50% gate would have missed it entirely",
		env:  map[string]*string{"BENCH_ALLOC_THRESHOLD_PCT": s("50")},
		args: elpsArgs("benchstat-alloc-regression.txt"), want: 0,
	}, {
		// The live case: the shell gate's first real firing, on elps PR #310.
		// A +8.44% B/op regression with an IDENTICAL allocation count -- the
		// same allocations, made bigger by a field added to CallFrame.
		name: "the LIVE PR #310 allocation regression fires",
		args: elpsArgs("benchstat-alloc-regression-live.txt"),
		want: 1, contains: []string{"REGRESSION", "EnvFunCallRecursion-4"},
	}, {
		name: "with only the ALLOCATION gate raised, the live table passes",
		env:  map[string]*string{"BENCH_ALLOC_THRESHOLD_PCT": s("10")},
		args: elpsArgs("benchstat-alloc-regression-live.txt"), want: 0,
	}})
}

func TestReviewedWaivers(t *testing.T) {
	const fx = "benchstat-libjson-encode-411.txt"
	wpath := func(n string) string { return filepath.Join(elpsFixtures, n) }

	check(t, []gateCase{{
		// The before half of the round trip. With waivers switched off, the
		// real run has TWO rows at or above a gate. Note this is not the "one
		// failing row" the change was described as: B/op is an ALLOCATION
		// metric, judged against the 5% allocation gate rather than the 15%
		// timing one, and +12.45% is over it.
		name: "PR #411's REAL benchstat output fires the gate with no waivers",
		env:  noWaivers(), args: elpsArgs(fx),
		want: 1, contains: []string{"+7.94%", "+12.45%"},
	}, {
		// The after half: the shipped list no longer carries the two #411
		// entries. What the DEFAULT path must prove now is the deletion's
		// other side -- with them gone, the same comparison fires again.
		name: "with the shipped waiver list empty, PR #411's comparison fires again",
		args: elpsArgs(fx), want: 1, contains: []string{"+7.94%", "+12.45%"},
	}, {
		// A WAIVED row must still be visible: reported by name, with its
		// delta, its tracking issue, and counted in the summary. Anchored on
		// the per-ROW marker, not the bare word: the summary line already says
		// "row(s) WAIVED", so a substring check for "WAIVED" stays green even
		// after the per-row lines are deleted -- which is exactly the
		// regression that matters.
		name: "a waived row is still reported by name, not silently dropped",
		env:  waivers(wpath("waivers-libjson-both.txt")), args: elpsArgs(fx),
		want: 0,
		contains: []string{
			"WAIVED      github.com/luthersystems/elps/lisp/lisplib/libjson",
			"+7.94%", "elps#412", "row(s) WAIVED",
		},
	}, {
		// NARROWNESS. A waiver covers one package, one benchmark, one metric
		// column. Waiving allocs/op alone must leave B/op of the SAME
		// benchmark failing -- otherwise "per-row" is a description rather
		// than a property.
		name: "waiving allocs/op does NOT waive B/op of the same benchmark",
		env:  waivers(wpath("waivers-libjson-allocs.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"B/op"},
	}, {
		name: "a waiver for another package does not reach libjson",
		env:  waivers(wpath("waivers-wrong-pkg.txt")), args: elpsArgs(fx), want: 1,
	}, {
		// BOUNDEDNESS. The ceiling is what makes a waiver an accepted COST
		// rather than a blessed benchmark.
		name: "a regression that EXCEEDS its waiver ceiling fails",
		env:  waivers(wpath("waivers-libjson-tight-ceiling.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"EXCEEDS its waiver ceiling"},
	}, {
		// EXPIRY. Past its date a waiver stops suppressing and the row is
		// judged normally again, so the decision is re-made rather than
		// inherited.
		name: "an EXPIRED waiver no longer suppresses its row",
		env:  waivers(wpath("waivers-expired.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"WAIVER EXPIRED"},
	}, {
		// JUSTIFICATION. A waiver with no tracking reference is a threshold
		// increase with better manners; the gate must refuse to run rather
		// than honour it. Note the exit code: 2, the same "cannot be
		// interpreted" hard failure as an unreadable table, because a waiver
		// list that does not parse must never be treated as an empty one.
		name: "a waiver with NO issue reference is rejected",
		env:  waivers(wpath("waivers-no-issue.txt")), args: elpsArgs(fx),
		want: 2, contains: []string{"not a tracking reference"},
	}, {
		name: "malformed waiver entries are refused, not skipped",
		env:  waivers(wpath("waivers-malformed.txt")), args: elpsArgs(fx),
		want: 2, contains: []string{
			"expected 7 |-separated fields",
			"is not a positive percentage",
			"is not a YYYY-MM-DD date",
			"reason is missing or too short",
			"empty pkg field",
		},
	}, {
		// `go test` appends -<GOMAXPROCS> to every benchmark name, so a waiver
		// written with the suffix would silently unbind the day `runs-on` or
		// the GOMAXPROCS pin changes -- the single failure mode these
		// repositories have been bitten by most. Rejected at parse time rather
		// than left to fail open years later.
		name: "a waiver naming Encode-2 (with the GOMAXPROCS suffix) is rejected",
		env:  waivers(wpath("waivers-gomaxprocs-suffix.txt")), args: elpsArgs(fx),
		want: 2, contains: []string{"GOMAXPROCS"},
	}, {
		// An explicitly-named waiver file that is not there is an error.
		// Silently adjudicating with no waivers would be the strict direction,
		// but it would also mean a typo'd path reads as "no waivers
		// configured".
		name: "BENCH_WAIVERS pointing at a missing file is an error",
		env:  waivers(wpath("waivers-does-not-exist.txt")), args: elpsArgs(fx), want: 2,
	}, {
		// STALENESS. A waiver that protects nothing must not rot quietly.
		name: "a waiver whose benchmark no longer exists is REPORTED",
		env:  waivers(wpath("waivers-stale.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"WAIVER-STALE"},
	}, {
		name: "a waiver aimed at the wrong package is reported as stale too",
		env:  waivers(wpath("waivers-wrong-pkg.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"WAIVER-STALE"},
	}, {
		// ...and the softer half: the row is there and simply is not
		// regressing, which is the signal to delete the entry.
		name: "a waiver whose row is no longer regressing is REPORTED",
		env:  waivers(wpath("waivers-unused.txt")), args: elpsArgs(fx),
		want: 1, contains: []string{"waiver-unused"},
	}, {
		// Reporting a stale waiver must not, by itself, turn a clean
		// comparison red -- otherwise a renamed benchmark reds every PR until
		// someone edits a file, and the pressure is to delete the mechanism
		// rather than the entry.
		name: "a stale waiver is reported but does not fail an otherwise clean run",
		env:  waivers(wpath("waivers-stale.txt")),
		args: elpsArgs("benchstat-clean-ci.txt"), want: 0,
	}, {
		name: "with both allocation columns waived, the same table passes",
		env:  waivers(wpath("waivers-libjson-both.txt")), args: elpsArgs(fx), want: 0,
	}})
}

// TestShippedWaiversRescueNothing is the hole this could have been. The
// shipped waiver file must not rescue any of the fixtures the gate is supposed
// to fail on. If a waiver ever widens into something that matches broadly,
// this is where it shows up.
func TestShippedWaiversRescueNothing(t *testing.T) {
	var cases []gateCase
	for _, fx := range []string{
		"benchstat-regression-new.txt", "benchstat-regression-old.txt",
		"benchstat-task-sample.txt", "benchstat-alloc-regression.txt",
		"benchstat-alloc-regression-live.txt", "benchstat-bps-regression.txt",
		"benchstat-noisy-sandbox.txt",
	} {
		cases = append(cases, gateCase{
			name: "the shipped waivers do NOT rescue " + fx,
			args: elpsArgs(fx), want: 1,
		})
	}
	cases = append(cases, gateCase{
		name: "the shipped waivers do NOT turn an uninterpretable table green",
		args: elpsArgs("benchstat-crash.txt"), want: 2,
	})
	check(t, cases)
}

// TestResolutionCheck is elps#443: a threshold is one number for a whole
// metric class, and it is only as good as the assumption that rows in that
// class have comparable noise. On elps' timing rows they do not.
func TestResolutionCheck(t *testing.T) {
	check(t, []gateCase{{
		// The noise-only half: #443's row, +15.96% p=0.035 over a 15% gate,
		// with the ±24%/±25% spread the null comparison measured.
		name: "a timing move INSIDE the row's own measured spread is not a regression",
		env:  noWaivers(), args: elpsArgs("benchstat-parallel-noise-443.txt"),
		want: 0,
		// ...and it is NOT silence. A benchmark that cannot be adjudicated is
		// a standing problem with the benchmark, and a gate that quietly drops
		// rows is the exact defect this whole thing exists to prevent.
		contains: []string{"NOISE-FLOOR", "+15.96%", "spread ±25%", "cannot resolve them"},
	}, {
		// The true-regression half. Same benchmark, same spread, same flat
		// allocation columns; only the size of the move differs. If this ever
		// stops failing, the resolution check has become an off switch.
		name: "a timing move LARGER than the row's spread is still a regression",
		env:  noWaivers(), args: elpsArgs("benchstat-parallel-true-regression.txt"),
		want: 1, contains: []string{"REGRESSION", "+48.00%"},
	}, {
		// The check is about RESOLUTION, not about size.
		name: "the noise row stays unresolvable even with the gate lowered to 1%",
		env: map[string]*string{
			"BENCH_WAIVERS":                  s(""),
			"BENCH_REGRESSION_THRESHOLD_PCT": s("1"),
		},
		args: elpsArgs("benchstat-parallel-noise-443.txt"),
		want: 0, contains: []string{"NOISE-FLOOR"},
	}, {
		// CLASS BOUNDARY. Allocation metrics are exempt, explicitly. One
		// fixture, three rows, same delta and same spread, differing only in
		// class.
		name: "an ALLOCATION row is judged on its threshold even with a large spread",
		env:  noWaivers(), args: elpsArgs("benchstat-alloc-with-spread.txt"),
		want: 1, contains: []string{
			"REGRESSION  github.com/luthersystems/elps/lisp             B/op",
			"REGRESSION  github.com/luthersystems/elps/lisp             allocs/op",
			"NOISE-FLOOR github.com/luthersystems/elps/lisp             sec/op",
		},
	}, {
		// WHEN THERE IS NO INTERVAL. benchstat prints "± ∞ ¹" below 6 samples,
		// so there is no resolution to check against. Those rows fall back to
		// the threshold alone and must SAY SO -- a check that did not run must
		// never look like one that ran and passed.
		name: "a row with no computable interval is still gated on the threshold",
		env:  noWaivers(), args: elpsArgs("benchstat-regression-new.txt"),
		want: 1, contains: []string{"resolution check did not run"},
	}, {
		// THE MEASUREMENT ITSELF. A real null comparison -- one tree, two
		// interleaved runs, CI's sampling parameters.
		name: "a measured null comparison on identical code is clean",
		env:  noWaivers(), args: elpsArgs("benchstat-null-parallel-sandbox.txt"), want: 0,
	}, {
		// AND THE ONE THAT ACTUALLY FIRED. Trial 8 verbatim: +18.48% p=0.009
		// over a 15% gate, on code that did not change, with the offending arm
		// measuring itself at ±19%.
		name: "a NULL comparison that fired the old gate no longer reds the build",
		env:  noWaivers(), args: elpsArgs("benchstat-null-spurious-firing.txt"),
		want: 0, contains: []string{"NOISE-FLOOR", "+18.48%", "no-change row"},
	}, {
		name: "the noise-floor fixture passes with the SHIPPED waivers too",
		args: elpsArgs("benchstat-parallel-noise-443.txt"), want: 0,
	}, {
		name: "the shipped waivers do NOT rescue the true regression",
		args: elpsArgs("benchstat-parallel-true-regression.txt"), want: 1,
	}})
}

// TestQuantisationCheck is elps#537: `go test` reports allocs/op as integer
// division, so a row whose true cost sits near an integer prints either side
// of it with no code change at all.
func TestQuantisationCheck(t *testing.T) {
	check(t, []gateCase{{
		name: "a ONE-ALLOCATION move on a row that does not reproduce its count is QUANTISED",
		env:  noWaivers(), args: elpsArgs("benchstat-allocs-quantised-null.txt"),
		want: 0, contains: []string{"QUANTISED", "not gateable at one allocation"},
	}, {
		// The other half: a row that DOES reproduce its own count still reds
		// the build at the same one-allocation move. The check keys on a row
		// disagreeing WITH ITSELF, which a real regression does not do.
		name: "a ONE-ALLOCATION move on a row that reproduces its count is a REGRESSION",
		env:  noWaivers(), args: elpsArgs("benchstat-allocs-one-step-real.txt"),
		want: 1, contains: []string{"REGRESSION"},
	}, {
		// Two counts is not reachable by truncation -- that needs the true
		// values to differ by more than a whole allocation -- so it is
		// adjudicated normally.
		name: "the quantisation rule stops at ONE count",
		env:  noWaivers(), args: elpsArgs("benchstat-allocs-quantisation-boundary.txt"),
		want: 1,
	}})
}

func TestThresholdIsTheOnlyThingHoldingItBack(t *testing.T) {
	zero := map[string]*string{
		"BENCH_REGRESSION_THRESHOLD_PCT": s("0"),
		"BENCH_ALLOC_THRESHOLD_PCT":      s("0"),
	}
	check(t, []gateCase{{
		// Proves the parser genuinely SEES the real comparison's significant
		// deltas and is silent because of the threshold, not because it failed
		// to parse. If the table format ever changes out from under it, this
		// case flips.
		name: "the REAL clean fixture DOES fire once the gates are lowered to 0%",
		env:  zero, args: elpsArgs("benchstat-clean-ci.txt"), want: 1,
	}, {
		name: "old-format clean fixture DOES fire at 0%",
		env:  zero, args: elpsArgs("benchstat-clean-old.txt"), want: 1,
	}, {
		name: "improvements still do not fire at a 0% gate",
		env:  zero, args: elpsArgs("benchstat-improvement-new.txt"), want: 0,
	}, {
		name: "p-insignificant rows still do not fire at a 0% gate",
		env:  zero, args: elpsArgs("benchstat-insignificant-new.txt"), want: 0,
	}})
}

func TestUninterpretableInputFailsLoudly(t *testing.T) {
	empty := filepath.Join(t.TempDir(), "empty.txt")
	if err := os.WriteFile(empty, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	check(t, []gateCase{{
		name: "benchstat crash output (no comparison rows) is an error, not 'clean'",
		args: elpsArgs("benchstat-crash.txt"), want: 2,
	}, {
		// A p-value this parser cannot read must fail closed. Truncating at
		// the first non-digit turns p=1.5e-05 into 1.5 and drops a +99%
		// regression as insignificant -- the one parse path that would fail
		// OPEN.
		name: "scientific-notation p-value is read as significant, not dropped",
		args: elpsArgs("benchstat-sci-pvalue.txt"),
		want: 1, contains: []string{"+99.00%"},
	}, {
		name: "a p-value that is not a number at all fails closed",
		args: elpsArgs("benchstat-badpvalue.txt"), want: 2,
	}, {
		// A table where nothing moved is a SUCCESSFUL comparison, not an
		// unreadable one.
		name: "an all-'~' table with no geomean row is clean, not an error",
		args: elpsArgs("benchstat-tilde-only.txt"),
		want: 0, contains: []string{"no-change row"},
	}, {
		// Old-format "(all equal)" rows are DATA, not footnotes, but they
		// contain the words "all equal"/"samples". A footnote filter written
		// as a substring match discards them, the table parses to zero rows,
		// and a perfectly clean comparison reports a spurious exit 2.
		name: "old-format all-'(all equal)' table is clean, not an exit-2 error",
		args: elpsArgs("benchstat-allequal-old.txt"),
		want: 0, contains: []string{"3 no-change row(s)"},
	}, {
		name: "empty benchstat output is an error, not 'clean'",
		args: []string{"-waivers-default", shippedWaivers, empty}, want: 2,
	}, {
		name: "missing input file is an error, not 'clean'",
		args: elpsArgs("does-not-exist.txt"), want: 2,
	}, {
		name: "missing argument is a usage error",
		args: []string{"-waivers-default", shippedWaivers}, want: 2,
	}, {
		name: "-base without -head is a usage error",
		args: []string{"-base", "x.txt"}, want: 2,
	}, {
		name: "a non-numeric threshold in the environment is a usage error",
		env:  map[string]*string{"BENCH_REGRESSION_THRESHOLD_PCT": s("fifteen")},
		args: elpsArgs("benchstat-clean-ci.txt"), want: 2,
	}, {
		name: "a BENCH_WAIVER_TODAY that is not a date is a usage error",
		env:  map[string]*string{"BENCH_WAIVER_TODAY": s("yesterday")},
		args: elpsArgs("benchstat-clean-ci.txt"), want: 2,
	}})
}

// TestSubstrateFixtureParity replays substrate's corpus. substrate's shell
// gate had ONE threshold (10%) and no metric-class split, so the cases below
// pin both classes at that same 10% -- which is what its workflows now pass.
// Every expected exit code here was produced by substrate's own bash gate
// before this tool existed; the whole point is that adopting the tool changed
// none of them.
func TestSubstrateFixtureParity(t *testing.T) {
	sub := map[string]*string{
		"BENCH_REGRESSION_THRESHOLD_PCT": s("10"),
		"BENCH_ALLOC_THRESHOLD_PCT":      s("10"),
	}
	withSub := func(extra map[string]*string) map[string]*string {
		m := map[string]*string{}
		for k, v := range sub {
			m[k] = v
		}
		for k, v := range extra {
			m[k] = v
		}
		return m
	}
	wpath := func(n string) string { return filepath.Join(subFixtures, n) }
	const fx = "benchstat-waived-regression-392.txt"

	check(t, []gateCase{{
		name: "new-format table with a +83.31% significant regression",
		env:  sub, args: subArgs("benchstat-regression-new.txt"),
		want: 1, contains: []string{"REGRESSION"},
	}, {
		name: "old-format table with a +83.31% significant regression",
		env:  sub, args: subArgs("benchstat-regression-old.txt"), want: 1,
	}, {
		// The clean fixture is the verbatim benchstat comment from substrate
		// PR #357 (which merged): it carries REAL significant deltas of
		// +7.14%/+3.92%/+1.19% plus positive geomean rows. None reach 10%.
		name: "new-format table whose significant deltas are all under the gate",
		env:  sub, args: subArgs("benchstat-clean-new.txt"), want: 0,
	}, {
		name: "old-format table whose significant deltas are all under the gate",
		env:  sub, args: subArgs("benchstat-clean-old.txt"), want: 0,
	}, {
		name: "improvements (negative deltas) never fire",
		env:  sub, args: subArgs("benchstat-improvement-new.txt"), want: 0,
	}, {
		name: "large deltas with p above alpha never fire",
		env:  sub, args: subArgs("benchstat-insignificant-new.txt"), want: 0,
	}, {
		name: "clean fixture DOES fire once the gate is lowered to 0%",
		env: withSub(map[string]*string{
			"BENCH_REGRESSION_THRESHOLD_PCT": s("0"),
			"BENCH_ALLOC_THRESHOLD_PCT":      s("0"),
		}),
		args: subArgs("benchstat-clean-new.txt"), want: 1,
	}, {
		name: "old-format clean fixture DOES fire at 0%",
		env: withSub(map[string]*string{
			"BENCH_REGRESSION_THRESHOLD_PCT": s("0"),
			"BENCH_ALLOC_THRESHOLD_PCT":      s("0"),
		}),
		args: subArgs("benchstat-clean-old.txt"), want: 1,
	}, {
		name: "benchstat crash output is an error, not 'clean'",
		env:  sub, args: subArgs("benchstat-crash.txt"), want: 2,
	}, {
		name: "scientific-notation p-value is read as significant, not dropped",
		env:  sub, args: subArgs("benchstat-sci-pvalue.txt"),
		want: 1, contains: []string{"+99.00%"},
	}, {
		name: "a p-value that is not a number at all fails closed",
		env:  sub, args: subArgs("benchstat-badpvalue.txt"), want: 2,
	}, {
		name: "an all-'~' table with no geomean row is clean, not an error",
		env:  sub, args: subArgs("benchstat-tilde-only.txt"),
		want: 0, contains: []string{"no-change row"},
	}, {
		// The before half of substrate's waiver round trip: with waivers off,
		// PR #392's comparison has THREE rows at or above the 10% gate.
		name: "PR #392's comparison fires the gate with no waivers",
		env:  withSub(noWaivers()), args: subArgs(fx),
		want: 1, contains: []string{"+17.82%", "+11.50%", "+12.85%"},
	}, {
		// The after half: the retired #392 waivers make that same comparison
		// pass, and it passes because it was WAIVED, not because the gate
		// stopped looking.
		name: "the retired #392 waivers make PR #392's comparison pass",
		env:  withSub(waivers(wpath("waivers-392.txt"))), args: subArgs(fx),
		want: 0, contains: []string{
			"WAIVED      github.com/luthersystems/substrate/internal/substrate/shirocore",
			"+17.82%", "substrate#392", "row(s) WAIVED",
			// The rows the bump did NOT buy a waiver for stay gated.
			"below-gate",
		},
	}, {
		name: "waiving B/op does NOT waive allocs/op of the same benchmark",
		env:  withSub(waivers(wpath("waivers-load-bop-only.txt"))), args: subArgs(fx),
		want: 1, contains: []string{
			"REGRESSION  github.com/luthersystems/substrate/internal/substrate/shiro/preheat",
		},
	}, {
		name: "a waiver naming a package this comparison does not cover rescues nothing",
		env:  withSub(waivers(wpath("waivers-wrong-pkg.txt"))), args: subArgs(fx), want: 1,
	}, {
		name: "a regression that EXCEEDS its waiver ceiling fails",
		env:  withSub(waivers(wpath("waivers-tight-ceiling.txt"))), args: subArgs(fx),
		want: 1, contains: []string{"EXCEEDS its waiver ceiling"},
	}, {
		name: "an EXPIRED waiver no longer suppresses its row",
		env:  withSub(waivers(wpath("waivers-expired.txt"))), args: subArgs(fx),
		want: 1, contains: []string{"WAIVER EXPIRED"},
	}, {
		// Wind the clock past the #392 waivers' expiry and the comparison reds
		// again. Without this, "expires" could be a field nothing reads.
		name: "the #392 waivers genuinely expire (clock wound past the date)",
		env: withSub(map[string]*string{
			"BENCH_WAIVERS":      s(wpath("waivers-392.txt")),
			"BENCH_WAIVER_TODAY": s("2099-01-01"),
		}),
		args: subArgs(fx), want: 1,
	}, {
		name: "a waiver with NO issue reference is rejected",
		env:  withSub(waivers(wpath("waivers-no-issue.txt"))), args: subArgs(fx),
		want: 2, contains: []string{"not a tracking reference"},
	}, {
		name: "malformed waiver entries are refused, not skipped",
		env:  withSub(waivers(wpath("waivers-malformed.txt"))), args: subArgs(fx),
		want: 2, contains: []string{
			"expected 7 |-separated fields",
			"is not a positive percentage",
			"is not a YYYY-MM-DD date",
			"reason is missing or too short",
			"empty pkg field",
		},
	}, {
		name: "a waiver carrying the GOMAXPROCS suffix is rejected",
		env:  withSub(waivers(wpath("waivers-gomaxprocs-suffix.txt"))), args: subArgs(fx),
		want: 2, contains: []string{"GOMAXPROCS"},
	}, {
		name: "BENCH_WAIVERS pointing at a missing file is an error",
		env:  withSub(waivers(wpath("waivers-does-not-exist.txt"))), args: subArgs(fx), want: 2,
	}, {
		name: "a waiver whose benchmark no longer exists is REPORTED",
		env:  withSub(waivers(wpath("waivers-stale.txt"))), args: subArgs(fx),
		want: 1, contains: []string{"WAIVER-STALE"},
	}, {
		name: "a waiver whose row is no longer regressing is REPORTED",
		env:  withSub(waivers(wpath("waivers-unused.txt"))), args: subArgs(fx),
		want: 1, contains: []string{"waiver-unused"},
	}, {
		name: "a stale waiver is reported but does not fail an otherwise clean run",
		env:  withSub(waivers(wpath("waivers-stale.txt"))),
		args: subArgs("benchstat-clean-new.txt"), want: 0,
	}, {
		// substrate's baseline workflow points BENCH_WAIVERS at a SECOND file.
		// It must parse, and it must rescue nothing it was not written for.
		name: "substrate's baseline waiver file parses and rescues nothing here",
		env:  withSub(waivers("../../../substrate-baseline-waivers-not-present.txt")),
		args: subArgs(fx), want: 2,
	}})
}

// TestOldGrepPatternStillCannotMatch is the regression proof for the gate this
// replaced, twice over: an inline `grep -E '^\S.*\+$'` that no benchstat line
// can match. Documented here so nobody reintroduces it.
func TestOldGrepPatternStillCannotMatch(t *testing.T) {
	for _, fx := range []string{
		"benchstat-regression-new.txt", "benchstat-task-sample.txt",
		"benchstat-regression-old.txt", "benchstat-alloc-regression.txt",
		"benchstat-bps-regression.txt",
	} {
		b, err := os.ReadFile(filepath.Join(elpsFixtures, fx)) //#nosec G304 -- test reads a fixture from a fixed testdata dir
		if err != nil {
			t.Fatal(err)
		}
		for _, line := range strings.Split(string(b), "\n") {
			if line == "" || strings.HasPrefix(line, "name") {
				continue
			}
			if !strings.HasPrefix(line, " ") && !strings.HasPrefix(line, "\t") && strings.HasSuffix(line, "+") {
				t.Errorf("%s: the old inline grep would have matched %q -- the fixture no longer reproduces the bug", fx, line)
			}
		}
	}
}
