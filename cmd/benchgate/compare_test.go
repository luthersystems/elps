package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/perf/benchmath"
)

// These exercise the structured front end: raw `go test -bench` output in,
// verdict out, with benchfmt doing the parsing and benchmath doing the
// statistics. The numbers below are chosen so the expected delta, spread and
// significance are arithmetic rather than opinion.

// arm renders `go test -bench` output for one package: a header plus one line
// per sample per benchmark.
func arm(pkg string, benches map[string][]sampleLine) string {
	var b strings.Builder
	b.WriteString("goos: linux\ngoarch: arm64\n")
	fmt.Fprintf(&b, "pkg: %s\n", pkg)
	b.WriteString("cpu: Neoverse-N1\n")
	// Deterministic order so a failure is reproducible.
	for _, name := range sortedKeys(benches) {
		for _, s := range benches[name] {
			fmt.Fprintf(&b, "Benchmark%s-2\t%d\t%s\n", name, s.iters, s.metrics)
		}
	}
	b.WriteString("PASS\nok  \t" + pkg + "\t1.234s\n")
	return b.String()
}

type sampleLine struct {
	metrics string
	iters   int
}

func sortedKeys(m map[string][]sampleLine) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	for i := range out {
		for j := i + 1; j < len(out); j++ {
			if out[j] < out[i] {
				out[i], out[j] = out[j], out[i]
			}
		}
	}
	return out
}

// repeat produces n identical samples, which is what an exactly reproducing
// benchmark looks like on the wire.
func repeat(n int, metrics string) []sampleLine {
	out := make([]sampleLine, n)
	for i := range out {
		out[i] = sampleLine{iters: 1000, metrics: metrics}
	}
	return out
}

// alternate produces n samples flipping between two metric lines, which is
// what a row that does not reproduce its own count looks like.
func alternate(n int, a, b string) []sampleLine {
	out := make([]sampleLine, n)
	for i := range out {
		m := a
		if i%2 == 1 {
			m = b
		}
		out[i] = sampleLine{iters: 1000, metrics: m}
	}
	return out
}

func writeArms(t *testing.T, base, head string) (string, string) {
	t.Helper()
	dir := t.TempDir()
	bp := filepath.Join(dir, "base.txt")
	hp := filepath.Join(dir, "head.txt")
	if err := os.WriteFile(bp, []byte(base), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(hp, []byte(head), 0o600); err != nil {
		t.Fatal(err)
	}
	return bp, hp
}

func runRaw(t *testing.T, base, head string, env map[string]*string) (int, string) {
	t.Helper()
	bp, hp := writeArms(t, base, head)
	return runGate(t, gateCase{
		env:  env,
		args: []string{"-base", bp, "-head", hp},
	})
}

const pkg = "github.com/luthersystems/elps/lisp"

func TestRawModeRegression(t *testing.T) {
	// Ten samples each, exactly reproducing, 100ns -> 150ns. Mann-Whitney on
	// two fully separated samples of 10 is far below alpha, the delta is
	// exactly +50.00%, and both arms have a zero-width interval.
	base := arm(pkg, map[string][]sampleLine{
		"Eval": repeat(10, "100 ns/op\t16 B/op\t2 allocs/op"),
	})
	head := arm(pkg, map[string][]sampleLine{
		"Eval": repeat(10, "150 ns/op\t16 B/op\t2 allocs/op"),
	})
	rc, out := runRaw(t, base, head, nil)
	if rc != 1 {
		t.Fatalf("exit %d, want 1\n%s", rc, out)
	}
	for _, want := range []string{"REGRESSION", "+50.00%", "sec/op", "Eval-2"} {
		if !strings.Contains(out, want) {
			t.Errorf("output does not contain %q\n%s", want, out)
		}
	}
	// The allocation columns did not move, so they must not be reported as
	// significant at all.
	if strings.Contains(out, "allocs/op") {
		t.Errorf("an unmoved allocs/op row was reported\n%s", out)
	}
}

func TestRawModeImprovement(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "150 ns/op")})
	head := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "100 ns/op")})
	rc, out := runRaw(t, base, head, nil)
	if rc != 0 {
		t.Fatalf("exit %d, want 0\n%s", rc, out)
	}
	if strings.Contains(out, "REGRESSION") {
		t.Errorf("an improvement was reported as a regression\n%s", out)
	}
}

// TestRawModeThroughputDirection is the B/s rule reaching the structured front
// end: `go test` emits MB/s when a benchmark calls b.SetBytes, benchfmt tidies
// it to B/s, and a GAIN must not be a regression at any threshold.
func TestRawModeThroughputDirection(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"Parse": repeat(10, "1000 ns/op\t100.00 MB/s")})
	head := arm(pkg, map[string][]sampleLine{"Parse": repeat(10, "1000 ns/op\t300.00 MB/s")})
	zero := map[string]*string{
		"BENCH_REGRESSION_THRESHOLD_PCT": s("0"),
		"BENCH_ALLOC_THRESHOLD_PCT":      s("0"),
	}
	rc, out := runRaw(t, base, head, zero)
	if rc != 0 {
		t.Fatalf("exit %d, want 0\n%s", rc, out)
	}
	if strings.Contains(out, "REGRESSION") {
		t.Errorf("a 3x throughput gain was reported as a regression\n%s", out)
	}

	// ...and the mirror: a collapse IS a regression, and is labelled.
	rc, out = runRaw(t, head, base, nil)
	if rc != 1 {
		t.Fatalf("exit %d, want 1\n%s", rc, out)
	}
	if !strings.Contains(out, "higher is better") {
		t.Errorf("the throughput collapse is not labelled by direction\n%s", out)
	}
}

// TestRawModeQuantisation drives elps#537 from raw samples rather than from a
// rendered table: a row whose allocs/op flips between 9 and 10 from GC cadence
// alone, compared against a row that flips the other way, is a one-allocation
// move on a row that does not reproduce its own count.
func TestRawModeQuantisation(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{
		"Encode": alternate(10, "1000 ns/op\t9 allocs/op", "1000 ns/op\t10 allocs/op"),
	})
	head := arm(pkg, map[string][]sampleLine{
		"Encode": alternate(10, "1000 ns/op\t10 allocs/op", "1000 ns/op\t9 allocs/op"),
	})
	// Both arms hold the same multiset, so this comparison cannot be
	// significant -- which is itself the point: the row is unable to say
	// anything smaller than one allocation.
	rc, out := runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s("")})
	if rc != 0 {
		t.Fatalf("exit %d, want 0\n%s", rc, out)
	}

	// Now the shape that reddened a real PR: the base arm mostly 9s, the head
	// arm mostly 10s, on code that did not change.
	base = arm(pkg, map[string][]sampleLine{
		"Encode": append(repeat(8, "1000 ns/op\t9 allocs/op"), repeat(2, "1000 ns/op\t10 allocs/op")...),
	})
	head = arm(pkg, map[string][]sampleLine{
		"Encode": append(repeat(8, "1000 ns/op\t10 allocs/op"), repeat(2, "1000 ns/op\t9 allocs/op")...),
	})
	rc, out = runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s("")})
	if rc != 0 {
		t.Fatalf("exit %d, want 0 (QUANTISED, not a regression)\n%s", rc, out)
	}
	if !strings.Contains(out, "QUANTISED") {
		t.Errorf("the one-allocation move on a non-reproducing row was not reported as QUANTISED\n%s", out)
	}

	// ...and the control: a row that DOES reproduce its own count still reds
	// the build at the same one-allocation move.
	base = arm(pkg, map[string][]sampleLine{"Encode": repeat(10, "1000 ns/op\t9 allocs/op")})
	head = arm(pkg, map[string][]sampleLine{"Encode": repeat(10, "1000 ns/op\t10 allocs/op")})
	rc, out = runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s("")})
	if rc != 1 {
		t.Fatalf("exit %d, want 1\n%s", rc, out)
	}
	if !strings.Contains(out, "REGRESSION") || !strings.Contains(out, "+11.11%") {
		t.Errorf("a real one-allocation regression on a stable row was not gated\n%s", out)
	}
}

// TestRawModeNoiseFloor is elps#443 from raw samples: a timing row whose own
// measured spread is at or above the move it made cannot resolve that move.
//
// The numbers are chosen to sit exactly on the boundary the rule cares about,
// which makes this case load-bearing twice over. Base is ten samples spread
// linearly over ±20% of 900ns; head is the same samples scaled by 1.16.
// benchmath then reports delta +16.00%, p=0.035 -- significant, and over the
// 15% timing gate -- with each arm's 95%-CI half-width at 15.56%, which
// benchstat PRINTS as "± 16%". So the verdict turns on adjudicating the spread
// a reader would see rather than the raw float: at 15.56 the move is larger
// than the spread and this reds the build, at 16 it does not. If roundSpread
// is ever dropped, this case fails.
func TestRawModeNoiseFloor(t *testing.T) {
	var baseS, headS []sampleLine
	for i := range 10 {
		b := float64(720 + 40*i)
		baseS = append(baseS, sampleLine{iters: 1000, metrics: fmt.Sprintf("%g ns/op", b)})
		headS = append(headS, sampleLine{iters: 1000, metrics: fmt.Sprintf("%g ns/op", b*1.16)})
	}
	base := arm(pkg, map[string][]sampleLine{"Parallel": baseS})
	head := arm(pkg, map[string][]sampleLine{"Parallel": headS})
	rc, out := runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s("")})
	if rc != 0 {
		t.Fatalf("exit %d, want 0 (NOISE-FLOOR, not a regression)\n%s", rc, out)
	}
	for _, want := range []string{"NOISE-FLOOR", "+16.00%", "spread ±16%", "cannot resolve them"} {
		if !strings.Contains(out, want) {
			t.Errorf("output does not contain %q\n%s", want, out)
		}
	}

	// The other half of the pair: the same row, a bigger move. If this ever
	// stops failing the resolution check has become an off switch.
	headS = nil
	for i := range 10 {
		headS = append(headS, sampleLine{iters: 1000, metrics: fmt.Sprintf("%g ns/op", float64(720+40*i)*1.5)})
	}
	head = arm(pkg, map[string][]sampleLine{"Parallel": headS})
	rc, out = runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s("")})
	if rc != 1 {
		t.Fatalf("exit %d, want 1\n%s", rc, out)
	}
	if !strings.Contains(out, "REGRESSION") || !strings.Contains(out, "+50.00%") {
		t.Errorf("a move larger than the row's spread was not gated\n%s", out)
	}
}

// TestRawModeUnpairedRowsAreReported: a cell present in only one arm is not
// adjudicable, and must not vanish. scripts/bench-arms-check.sh is the
// designated guard for arm comparability; this only proves the count is said
// out loud.
func TestRawModeUnpairedRowsAreReported(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "100 ns/op")})
	head := arm(pkg, map[string][]sampleLine{
		"Eval":  repeat(10, "100 ns/op"),
		"Added": repeat(10, "200 ns/op"),
	})
	rc, out := runRaw(t, base, head, nil)
	if rc != 0 {
		t.Fatalf("exit %d, want 0\n%s", rc, out)
	}
	if !strings.Contains(out, "appear in only one arm") {
		t.Errorf("the unpaired cell was not reported\n%s", out)
	}
}

// TestRawModeEmptyComparisonFailsLoudly: two arms that pair nothing must be
// exit 2, not "no regression". A gate that cannot interpret its input must
// never report success -- the founding defect of the gates this replaces.
func TestRawModeEmptyComparisonFailsLoudly(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"OnlyInBase": repeat(10, "100 ns/op")})
	head := arm(pkg, map[string][]sampleLine{"OnlyInHead": repeat(10, "100 ns/op")})
	rc, out := runRaw(t, base, head, nil)
	if rc != 2 {
		t.Fatalf("exit %d, want 2\n%s", rc, out)
	}
	if !strings.Contains(out, "found NO comparison rows") {
		t.Errorf("the empty comparison was not named\n%s", out)
	}
}

// TestRawModeWaiversApply: the waiver machinery is front-end independent.
func TestRawModeWaiversApply(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "100 ns/op")})
	head := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "150 ns/op")})
	dir := t.TempDir()
	wf := filepath.Join(dir, "waivers.txt")
	body := pkg + " | Eval | sec/op | 60 | 2099-01-01 | elps#538 | accepted while the faster path lands\n"
	if err := os.WriteFile(wf, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	rc, out := runRaw(t, base, head, map[string]*string{"BENCH_WAIVERS": s(wf)})
	if rc != 0 {
		t.Fatalf("exit %d, want 0\n%s", rc, out)
	}
	if !strings.Contains(out, "WAIVED") || !strings.Contains(out, "+50.00%") {
		t.Errorf("the waived row was not reported with its delta\n%s", out)
	}
}

// TestBothFrontEndsAgree renders the same comparison two ways -- once as raw
// samples through benchfmt/benchmath, once as the benchstat table those
// samples produce -- and requires the same verdict. The two front ends share
// an adjudicator, so this pins the thing they do NOT share: that the delta and
// the spread come out the same to the precision benchstat prints.
func TestBothFrontEndsAgree(t *testing.T) {
	base := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "100 ns/op\t16 B/op\t2 allocs/op")})
	head := arm(pkg, map[string][]sampleLine{"Eval": repeat(10, "150 ns/op\t16 B/op\t2 allocs/op")})
	rcRaw, outRaw := runRaw(t, base, head, nil)

	// The table benchstat renders for exactly those samples: median 100n vs
	// 150n, both arms exactly reproducing (± 0%), +50.00%, and a Mann-Whitney
	// p-value of 0.000 for two fully separated samples of 10.
	table := "goos: linux\ngoarch: arm64\npkg: " + pkg + "\ncpu: Neoverse-N1\n" +
		"        │    base     │                 pr                 │\n" +
		"        │   sec/op    │   sec/op     vs base               │\n" +
		"Eval-2    100.0n ± 0%   150.0n ± 0%  +50.00% (p=0.000 n=10)\n"
	dir := t.TempDir()
	tp := filepath.Join(dir, "table.txt")
	if err := os.WriteFile(tp, []byte(table), 0o600); err != nil {
		t.Fatal(err)
	}
	rcTable, outTable := runGate(t, gateCase{args: []string{tp}})

	if rcRaw != rcTable {
		t.Fatalf("front ends disagree: raw exit %d, table exit %d\nRAW:\n%s\nTABLE:\n%s", rcRaw, rcTable, outRaw, outTable)
	}
	for _, want := range []string{"REGRESSION", "+50.00%"} {
		if !strings.Contains(outRaw, want) || !strings.Contains(outTable, want) {
			t.Errorf("front ends disagree on %q\nRAW:\n%s\nTABLE:\n%s", want, outRaw, outTable)
		}
	}
}

// TestPctRangeMatchesBenchstat pins the spread computation against
// benchmath's own renderer, so a future benchstat change to PctRangeString
// shows up here rather than as a silently different verdict.
func TestPctRangeMatchesBenchstat(t *testing.T) {
	for _, tc := range []struct {
		values []float64
		want   string
	}{
		{[]float64{1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, "0%"},
		{[]float64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, ""}, // any finite value
		{[]float64{1, 2, 3, 4, 5}, "∞"},                // below 6 samples
	} {
		sample := newSampleForTest(tc.values)
		got := pctRange(sample)
		rendered := sample.PctRangeString()
		switch tc.want {
		case "∞":
			if got != unknownSpread || rendered != "∞" {
				t.Errorf("values %v: pctRange=%v rendered=%q, want unknown/∞", tc.values, got, rendered)
			}
		case "0%":
			if fmt.Sprintf("%.0f%%", got) != rendered {
				t.Errorf("values %v: pctRange=%v does not render as %q", tc.values, got, rendered)
			}
		default:
			if got < 0 || fmt.Sprintf("%.0f%%", got) != rendered {
				t.Errorf("values %v: pctRange=%v does not render as %q", tc.values, got, rendered)
			}
		}
	}
}

// newSampleForTest summarises values exactly as compareArms does.
func newSampleForTest(values []float64) benchmath.Summary {
	th := benchmath.Thresholds{CompareAlpha: 0.05}
	return benchmath.AssumeNothing.Summary(benchmath.NewSample(append([]float64(nil), values...), &th), confidence)
}
