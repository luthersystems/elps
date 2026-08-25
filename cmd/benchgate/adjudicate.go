package main

import (
	"fmt"
	"io"
)

// defaultVarianceCeiling is the per-row fitness ceiling: a timing row whose own
// confidence interval is at or above this cannot be adjudicated at all.
//
// The number is derived from this repository's own measurements rather than
// picked, and the two constraints on it are both empirical:
//
//	the floor   elps' noisiest LEGITIMATE row, BenchmarkPackageGetFunParallel,
//	            measures itself at ±24%/±25% on IDENTICAL code (#443, and
//	            preserved as testdata/elps/benchstat-parallel-noise-443.txt).
//	            A ceiling at or below that would call this repository's own
//	            true-regression control (+48.00% on that same ±25% row)
//	            unmeasurable and turn a real finding into a re-measure loop.
//	the target  the substrate#424 incident measured ±71% on one arm, and ±30%
//	            on the re-run. A ceiling above 30 misses half the incident it
//	            was written for.
//
// 30, tested at-or-above, is the only round number that satisfies both. It is a
// flag because the right number is a property of the runner and the benchmark
// set: a repository whose rows are quieter than elps' parallel map lookup
// should tighten it, and BENCH_VARIANCE_CEILING_PCT is how.
const defaultVarianceCeiling = 30.0

// policy is the whole adjudication policy: six numbers and a waiver list.
// See the "Why there is no config file" note in the package doc.
type policy struct {
	alpha             float64
	threshold         float64
	allocThreshold    float64
	varianceCeiling   float64
	thresholdStr      string
	allocThresholdStr string
	ceilingStr        string
	waivers           *waiverSet
	waiverSource      string
}

// unmeasurable reports whether this row's own confidence interval is too wide
// for the comparison to say anything about its delta.
//
// TIMING ROWS ONLY, and that is the same class boundary the resolution check
// draws, for the same reason: a jittery ALLOCATION column is a benchmark defect
// rather than a sick machine, and this repository's doctrine is that it must
// not be able to buy itself the timing metrics' treatment (see the
// resolution-check note in the package doc). The one benign way an allocation
// count jitters -- `go test` truncating a fractional allocs/op -- is handled by
// the quantisation check, which can only ever discard a ONE-COUNT move.
//
// A row with no computable interval (unknownSpread, benchstat's "± ∞ ¹" below 6
// samples) is NOT unmeasurable by this rule: nothing was measured to compare
// against the ceiling. Those rows fall back to the threshold alone and the
// report says so, exactly as they already did for the resolution check.
func (p *policy) unmeasurable(metric string, spread float64) bool {
	return !isAllocMetric(metric) && spread >= 0 && spread >= p.varianceCeiling
}

// verdict is what an adjudication produced: the report lines, the counters,
// and the exit code they imply.
type verdict struct {
	lines []string

	regressions int
	significant int
	compared    int
	tilde       int
	badPValue   int
	waived      int
	unresolved  int
	quantised   int

	// unmeasurable counts rows at or above their gate whose own interval was
	// too wide to adjudicate. Those are what exit 3 is drawn from: a delta big
	// enough to matter, measured on a row that could not measure it.
	unmeasurable int
	// unmeasurableInfo counts rows whose interval was equally unfit but whose
	// delta was BELOW the gate. Reported as a warning and deliberately NOT
	// folded into the exit code: a noisy row can hide a real regression, but
	// failing every slightly-noisy minor row makes the gate brittle, and a
	// brittle gate gets switched off. Whole-machine fitness is burn-in's job
	// (benchgate burnin), which is where that gap is closed.
	unmeasurableInfo int

	waiverBad        int
	waiverStale      int
	waiverUnused     int
	waiverExpired    int
	waiverCount      int
	waiverOutOfScope int
}

// awkNum renders a float the way awk's %s renders a number (CONVFMT,
// "%.6g"), so the report text is identical to the shell gate's.
func awkNum(v float64) string { return fmt.Sprintf("%.6g", v) }

// gateFor returns the threshold for a metric's class, and the string the
// report prints for it.
func (p *policy) gateFor(metric string) (float64, string) {
	if isAllocMetric(metric) {
		return p.allocThreshold, p.allocThresholdStr
	}
	return p.threshold, p.thresholdStr
}

// adjudicate applies the policy to a comparison. It makes no I/O and no
// decisions about exit codes; report() and the caller do that.
func adjudicate(c *comparison, p *policy) *verdict {
	v := &verdict{}
	ws := p.waivers
	v.waiverCount = len(ws.waivers)
	v.waiverBad = len(ws.bad)
	v.lines = append(v.lines, ws.bad...)

	for _, r := range c.rows {
		switch r.kind {
		case kindBadPValue:
			v.badPValue++
			v.lines = append(v.lines, fmt.Sprintf("  BAD-PVALUE  unparseable p-value %s on row: %s", r.pvalStr, r.name))
			continue
		case kindTilde:
			// A "~" row still counts as "the waiver found its row": that is
			// what separates a waiver that is merely no longer needed (delete
			// it) from one pointing at a benchmark that no longer exists.
			v.tilde++
			if w := ws.find(r.pkg, baseName(r.name), r.metric); w != nil {
				w.seen = true
			}
			continue
		case kindDelta:
		}

		v.compared++
		// Looked up BEFORE the significance and threshold tests, for the same
		// reason as the tilde case above.
		w := ws.find(r.pkg, baseName(r.name), r.metric)
		if w != nil {
			w.seen = true
		}

		// Fold metric direction in: regr is how much WORSE this row got, so a
		// positive regr always means "worse" regardless of metric.
		regr := r.delta
		dir := ""
		if higherIsBetter(r.metric) {
			regr = -r.delta
			dir = "(higher is better)"
		}

		gate, gateStr := p.gateFor(r.metric)

		if !r.hasP {
			// geomean and other summary rows carry no significance
			// information.
			v.lines = append(v.lines, fmt.Sprintf("  info        %-46s %-9s %-40s delta=%s (no p-value; informational) %s",
				r.pkg, r.metric, r.name, r.deltaTok, dir))
			continue
		}

		if r.pval > p.alpha {
			continue // not statistically significant
		}
		if regr <= 0 {
			continue // improvement or no change
		}

		v.significant++
		if regr < gate {
			// A row below its gate is not adjudicated, so an unfit interval
			// changes nothing about the verdict -- but it is still worth
			// saying, because "small move" and "move too noisy to size" are
			// different statements and only one of them is reassuring.
			unfit := ""
			if p.unmeasurable(r.metric, r.spread) {
				v.unmeasurableInfo++
				unfit = fmt.Sprintf(" [spread ±%s%% is at or above the ±%s%% fitness ceiling, so this row could not have been sized either way -- below-gate here means UNMEASURED, not small]",
					awkNum(r.spread), p.ceilingStr)
			}
			v.lines = append(v.lines, fmt.Sprintf("  below-gate  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s%s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, dir, unfit))
			continue
		}

		// RESOLUTION. A timing row is only adjudicated when the move is larger
		// than what that row can measure. See the resolution-check note in the
		// package doc: the threshold is one number for a whole metric class,
		// and on a sub-100ns RunParallel body it sits BELOW the row's own null
		// distribution, so p<=alpha over the threshold happens there by chance
		// (#443).
		//
		// Deliberately not applied to the allocation metrics, and deliberately
		// skipped when no interval could be computed -- in that case the
		// regression line says so, so a check that did not run is never
		// mistaken for one that ran and passed.
		if !isAllocMetric(r.metric) && r.spread >= 0 && regr <= r.spread {
			v.unresolved++
			v.lines = append(v.lines, fmt.Sprintf("  NOISE-FLOOR %-46s %-9s %-40s delta=%s p=%s (gate %s%%) spread ±%s%% -- the OWN measured spread of this row on these samples is at or above this move, so the comparison cannot resolve it; not a regression, and not suppressed either: make the benchmark quieter (longer -benchtime, or keep it out of the comparison set) if this row needs to be gateable %s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, awkNum(r.spread), dir))
			continue
		}

		// QUANTISATION. `go test` reports allocs/op as
		// int64(memstats.Mallocs)/int64(b.N) -- INTEGER DIVISION of a quantity
		// that is not an integer. A row whose true cost is 9.99 allocations
		// per operation prints 9 on one sample and 10 on the next, from GC
		// cadence alone, and benchstat reads that as an 11.11% move. See the
		// quantisation-check note in the package doc for the measurement.
		//
		// So a move of ONE COUNT on an integer-count metric is only
		// adjudicated when the row is reproducing that count exactly. When
		// either arm disagrees with itself -- a nonzero spread on a metric
		// that is supposed to be exact -- one count is the smallest thing it
		// can say and the change is indistinguishable from the reported
		// integer landing on the other side of the boundary.
		if isCountMetric(r.metric) && r.baseVal > 0 && r.spread > 0 && r.baseVal*regr/100.0 < 1.5 {
			v.quantised++
			v.lines = append(v.lines, fmt.Sprintf("  QUANTISED   %-46s %-9s %-40s delta=%s p=%s (gate %s%%) base %s allocs/op, so this is a ONE-ALLOCATION step -- and the row does not reproduce its own count (spread ±%s%% on a metric that should be exact), so a one-step move cannot be told from `go test` truncating a fractional allocs/op to the other integer; not a regression, and not suppressed either: pin the count (drop the per-op allocation below the GC-cadence noise, or cut b.N variance) if this row needs to be gateable at one allocation %s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, awkNum(r.baseVal), awkNum(r.spread), dir))
			continue
		}

		noQuant := ""
		if isCountMetric(r.metric) && regr > 0 {
			switch {
			case r.baseVal <= 0:
				noQuant = " [base cell unreadable, so the quantisation check did not run]"
			case r.spread < 0:
				noQuant = " [no interval: benchstat needs >= 6 samples, so the quantisation check did not run]"
			}
		}
		noSpread := ""
		if !isAllocMetric(r.metric) && r.spread < 0 {
			noSpread = fmt.Sprintf(" [no interval: benchstat needs >= 6 samples, so the resolution check did not run, and neither did the ±%s%% fitness ceiling]", p.ceilingStr)
		}

		// FITNESS (#542). Everything above asks whether the CODE moved. This
		// asks whether the MACHINE was in a state to tell -- the question that
		// went unasked when a runner mid-job produced a base arm at ±71% and a
		// pr arm at ±3% on a tree with no Go-code delta, and this gate compared
		// the two medians and called the +83% difference a REGRESSION.
		//
		// The order of the three row-level checks is load-bearing:
		//
		//	NOISE-FLOOR first  it is the STRONGER statement -- not merely "this
		//	                   row is noisy" but "this row cannot resolve THIS
		//	                   move" -- and it is already the verdict on those
		//	                   rows. Re-labelling them would change no exit code
		//	                   and lose the more specific reason.
		//	QUANTISED next     an integer-count row is exempt from this check by
		//	                   class anyway; the ordering just keeps the reason
		//	                   printed on such a row the specific one.
		//	a live WAIVER      a waiver is a standing decision that this row's
		//	  before this      regression, up to a recorded ceiling, is already
		//	                   accepted. There is nothing left to certify, so
		//	                   there is nothing for an unfit measurement to
		//	                   invalidate -- and this way the fitness check can
		//	                   only ever WITHHOLD a regression finding (exit 1
		//	                   -> exit 3). It can never turn a run that would
		//	                   have passed into one that fails, which is what
		//	                   makes it safe to switch on everywhere.
		//
		// Note what a waiver does NOT buy: an expired one, or a move past the
		// recorded ceiling, is a NEW finding drawn from this measurement, and a
		// new finding needs a measurement worth drawing from. Those fall
		// through to UNMEASURABLE like any other row.
		waivedNow := w != nil && !w.expired && regr <= w.ceiling
		if !waivedNow && p.unmeasurable(r.metric, r.spread) {
			v.unmeasurable++
			waiverNote := ""
			if w != nil {
				// The row HAS a waiver that did not apply -- expired, or the
				// move is past its ceiling. Recorded so the waiver-reporting
				// loop below does not go on to advise deleting it, which would
				// be advice drawn from the measurement this line has just
				// declared worthless.
				w.unmeasured = true
				waiverNote = fmt.Sprintf(" [its waiver (%s, ceiling %s%%, expires %s) neither applied nor was outgrown here: both are claims about a size this comparison could not measure]",
					w.issue, w.ceilStr, w.expires)
			}
			v.lines = append(v.lines, fmt.Sprintf("  UNMEASURABLE %-45s %-9s %-40s delta=%s p=%s (gate %s%%) spread ±%s%% is at or above the ±%s%% fitness ceiling -- an arm this dispersed carries no information about the size of the move, so this delta is NOT adjudicated in either direction: not a regression, and not a pass. Re-measure on a fit runner (`benchgate burnin` before the run says whether it is one) %s%s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, awkNum(r.spread), p.ceilingStr, dir, waiverNote))
			continue
		}

		// At or above the gate. A waiver can turn this into a PASS, but only a
		// live one, and only while the move stays inside the ceiling it
		// recorded. Every outcome below is printed either way: the waiver
		// changes the verdict, never the visibility.
		switch {
		case w != nil && w.expired:
			v.regressions++
			w.expiredHit = true
			v.lines = append(v.lines, fmt.Sprintf("  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) WAIVER EXPIRED %s (%s), no longer suppressing %s%s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, w.expires, w.issue, dir, noSpread+noQuant))
		case w != nil && regr > w.ceiling:
			v.regressions++
			w.exceeded = true
			v.lines = append(v.lines, fmt.Sprintf("  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) EXCEEDS its waiver ceiling %s%% (%s) %s%s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, w.ceilStr, w.issue, dir, noSpread+noQuant))
		case w != nil:
			v.waived++
			w.used = true
			v.lines = append(v.lines, fmt.Sprintf("  WAIVED      %-46s %-9s %-40s delta=%s p=%s (gate %s%%) accepted: ceiling %s%%, expires %s, %s %s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, w.ceilStr, w.expires, w.issue, dir))
		default:
			v.regressions++
			v.lines = append(v.lines, fmt.Sprintf("  REGRESSION  %-46s %-9s %-40s delta=%s p=%s (gate %s%%) %s%s",
				r.pkg, r.metric, r.name, r.deltaTok, r.pvalStr, gateStr, dir, noSpread+noQuant))
		}
	}

	// A waiver that matched no row in this comparison, or matched a row it did
	// not need to suppress, is reported EVERY run. A stale waiver that rots
	// quietly is how a per-row exception turns back into a blanket one.
	for _, w := range ws.waivers {
		switch {
		case !c.pkgSeen[w.pkg]:
			// The comparison did not cover this package AT ALL, so there is
			// nothing to say about the waiver -- it was not exercised, and
			// calling that "stale" would flood every partial comparison with
			// warnings about waivers that are perfectly healthy. Counted, not
			// printed, so a package that has genuinely disappeared still shows
			// up as a nonzero number rather than as silence.
			v.waiverOutOfScope++
		case !w.seen:
			v.waiverStale++
			v.lines = append(v.lines, fmt.Sprintf("  WAIVER-STALE  %s:%d waives %s / %s / %s -- that package IS in this comparison and that row is not, so the benchmark was renamed or removed and the waiver is protecting nothing. %s",
				ws.source, w.line, w.pkg, w.bench, w.metric, w.issue))
		case !w.used && !w.exceeded && !w.expiredHit && !w.unmeasured:
			v.waiverUnused++
			v.lines = append(v.lines, fmt.Sprintf("  waiver-unused %s:%d waives %s / %s / %s, and that row is not regressing above its gate -- the waiver can be deleted. %s",
				ws.source, w.line, w.pkg, w.bench, w.metric, w.issue))
		}
		if w.expired && c.pkgSeen[w.pkg] {
			v.waiverExpired++
			v.lines = append(v.lines, fmt.Sprintf("  WAIVER-EXPIRED %s:%d %s / %s / %s expired on %s and no longer suppresses anything. %s",
				ws.source, w.line, w.pkg, w.bench, w.metric, w.expires, w.issue))
		}
	}

	return v
}

// report writes the human-readable verdict and returns the process exit code.
// stdout carries the report; stderr carries the reasons a verdict could not be
// reached.
func (v *verdict) report(stdout, stderr io.Writer, p *policy, inputDesc string) int {
	for _, l := range v.lines {
		pln(stdout, l)
	}

	if v.waiverBad > 0 {
		pf(stderr, `benchgate: %d malformed entr(y/ies) in %s -- see the
WAIVER-BAD line(s) above.

Refusing to report a verdict rather than skipping the bad entries: a waiver
list that cannot be read must never be treated as an empty one, and a waiver
that silently does not parse is a regression nobody is told about. Fix the
entry, or delete it. The format is documented at the top of that file.
`, v.waiverBad, p.waiverSource)
		return 2
	}

	if v.badPValue > 0 {
		pf(stderr, "benchgate: %d row(s) carried a p-value this gate cannot read -- refusing to report a verdict.\n", v.badPValue)
		return 2
	}

	if v.compared+v.tilde == 0 {
		pf(stderr, `benchgate: found NO comparison rows in %s (no delta rows and no
"~" no-change rows).

Either the comparison failed, or benchstat's output format changed and this
gate can no longer read it.  Failing rather than reporting "no regression" -- a
gate that cannot parse its input must never report success.  Run the benchgate
tests and refresh the fixtures in cmd/benchgate/testdata/ if benchstat's table
format has genuinely changed.
`, inputDesc)
		return 2
	}

	pf(stdout, "benchgate: interpreted %d delta row(s) + %d no-change row(s); %d significant move(s) in the bad direction; %d at or above the gate (timing %s%%, allocation %s%%).\n",
		v.compared, v.tilde, v.significant, v.regressions, p.thresholdStr, p.allocThresholdStr)

	// Printed whenever it is nonzero, and never folded into the regression
	// count. A NOISE-FLOOR row is a benchmark this comparison cannot
	// adjudicate at all, which is a standing problem with the benchmark rather
	// than a clean result. Silence here would turn "we could not measure it"
	// into "it was fine", which is the shape of defect this whole gate exists
	// to fix.
	if v.unresolved > 0 {
		pf(stdout, "benchgate: %d timing row(s) moved past the gate but by LESS than their own measured spread, so this comparison cannot resolve them (reported as NOISE-FLOOR above, excluded from the verdict). They are not gateable as sampled; make them quieter or keep them out of the comparison set.\n", v.unresolved)
	}

	// Same doctrine again, for the rows the MACHINE could not measure. This one
	// carries the exit code with it, so it says which way it went.
	if v.unmeasurable > 0 {
		pf(stdout, "benchgate: %d timing row(s) moved at or above the gate on an interval at or above the ±%s%% fitness ceiling, so the comparison cannot size the move at all (reported as UNMEASURABLE above). They are NOT counted as regressions and NOT counted as passes: this run certified nothing about them. Re-measure -- `benchgate burnin` on the runner first will say whether it can measure anything.\n", v.unmeasurable, p.ceilingStr)
	}
	if v.unmeasurableInfo > 0 {
		pf(stdout, "benchgate: %d further row(s) were equally unfit to measure but moved LESS than their gate, so they change no verdict and are reported as warnings only. A noisy row can hide a real regression; whole-machine fitness is what `benchgate burnin` is for.\n", v.unmeasurableInfo)
	}

	// Same doctrine as NOISE-FLOOR above, for the integer-count metrics.
	if v.quantised > 0 {
		pf(stdout, "benchgate: %d allocation-count row(s) moved past the gate by exactly ONE allocation on a row that does not reproduce its own count, so the move cannot be told from `go test` truncating a fractional allocs/op (reported as QUANTISED above, excluded from the verdict). They are not gateable at one allocation as sampled.\n", v.quantised)
	}

	// Printed on EVERY run, including clean ones. A waiver is a standing
	// decision, and a standing decision that stops being visible stops being
	// reviewed.
	if v.waiverCount > 0 || v.waived > 0 {
		pf(stdout, "benchgate: %d reviewed waiver(s) loaded from %s; %d row(s) WAIVED (measured, reported, and excluded from the verdict), %d stale, %d currently unused, %d expired, %d for a package this comparison does not cover.\n",
			v.waiverCount, p.waiverSource, v.waived, v.waiverStale, v.waiverUnused, v.waiverExpired, v.waiverOutOfScope)
	}

	// Exit-code precedence, and it only goes this way round: a regression found
	// on a row that COULD be measured is a finding, and an unmeasurable row
	// elsewhere in the same table does not make it less of one. Reporting
	// "re-measure" over the top of it would postpone a real result and invite a
	// re-run that finds it again.
	if v.regressions > 0 {
		return 1
	}
	if v.unmeasurable > 0 {
		return 3
	}
	return 0
}
