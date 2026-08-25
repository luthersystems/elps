package main

import (
	"math"
	"regexp"
	"strconv"
	"strings"
)

// rowKind separates the three things a comparison line can be. The
// distinction is load-bearing rather than cosmetic: a "~" row IS a
// successfully interpreted comparison that found no significant difference,
// and counting it as "nothing parsed" is what turns a perfectly clean
// all-equal table into a spurious exit 2.
type rowKind int

const (
	// kindDelta is a row carrying a signed percentage delta.
	kindDelta rowKind = iota
	// kindTilde is a row benchstat rendered as "~" -- compared, not
	// significant.
	kindTilde
	// kindBadPValue is a row whose p-value could not be read at all. It is a
	// format change, and it fails the run closed.
	kindBadPValue
)

// unknownSpread is the sentinel for "no confidence interval could be
// computed" (benchstat prints "± ∞ ¹" below 6 samples). It is NOT zero:
// treating "no interval" as "a perfect interval" would suppress nothing while
// looking like it had checked.
const unknownSpread = -1.0

// unreadableMagnitude is the sentinel for a value cell this gate cannot parse.
// It is NOT zero, so a cell that could not be read can never be mistaken for a
// row with no allocations -- which would suppress it.
const unreadableMagnitude = -1.0

// A row is one metric column of one benchmark in one package, as adjudicated.
// Both front ends (the benchstat-table reader and the benchfmt/benchmath
// comparison) produce these, and the policy in adjudicate.go sees nothing
// else.
type row struct {
	pkg      string
	name     string // as printed, including the -<GOMAXPROCS> suffix
	metric   string // sec/op, B/op, allocs/op, B/s, time/op, ...
	deltaTok string // "+7.94%", exactly as benchstat renders it
	pvalStr  string // "0.008", "1.5e-05", or "n/a"
	delta    float64
	pval     float64
	// spread is the LARGEST per-arm 95%-CI half-width benchstat printed for
	// this row, as a percent, or unknownSpread.
	//
	// The larger of the two arms, not the base arm alone: dispersion in both
	// arms feeds the uncertainty of the delta. Taking the max is the lenient
	// direction of the two (it suppresses more), but the alternative -- base
	// only -- would let a change that ADDS variance be judged against the
	// quiet arm it replaced, and this rule must never be easier to trip by
	// making a benchmark noisier.
	spread float64
	// baseVal is the magnitude of the base arm's summary value, or
	// unreadableMagnitude. The quantisation rule needs it to know how many
	// WHOLE allocations a percentage delta stands for.
	baseVal float64
	kind    rowKind
	hasP    bool
}

// comparison is everything one front end hands the adjudicator.
type comparison struct {
	// pkgSeen is every package the comparison covered, whether or not any of
	// its rows moved. A waiver for a package that is not in here was not
	// exercised at all, which is different from a waiver whose row has gone.
	pkgSeen map[string]bool
	rows    []row
}

// baseName strips the -<GOMAXPROCS> suffix `go test` appends to every
// benchmark name (and omits entirely at GOMAXPROCS=1), so the suffix follows
// the RUNNER, not the code. Waivers are written without it and rows are
// stripped down to match; that is what keeps a waiver from silently unbinding
// when `runs-on` changes.
var gomaxprocsSuffix = regexp.MustCompile(`-[0-9]+$`)

func baseName(n string) string {
	return gomaxprocsSuffix.ReplaceAllString(n, "")
}

// higherIsBetter reports whether a LARGER value of this metric is better
// (throughput). elps emits B/s because the parser benchmarks call b.SetBytes();
// the old benchstat table labelled the same column "speed".
func higherIsBetter(m string) bool {
	return strings.HasSuffix(m, "/s") || m == "speed"
}

var allocMetric = regexp.MustCompile(`^(B|MB|KB|bytes)/op$`)
var countMetric = regexp.MustCompile(`^allocs?/op$`)

// isAllocMetric reports whether this is one of the near-deterministic
// allocation metrics, which get the tight threshold. Everything else --
// timing (sec/op, ns/op, time/op), throughput (B/s), and any unrecognised or
// custom b.ReportMetric column -- gets the loose timing threshold.
// Unrecognised falls to the LOOSE side on purpose: a custom metric of unknown
// noise must not red PRs on arrival.
func isAllocMetric(m string) bool {
	return allocMetric.MatchString(m) || countMetric.MatchString(m)
}

// isCountMetric reports whether this metric counts WHOLE THINGS, which today
// means allocs/op alone. B/op is an allocation metric too, but it is not a
// count: its quantum is one byte out of thousands, so the quantisation rule is
// a no-op there and is not applied to it.
func isCountMetric(m string) bool {
	return countMetric.MatchString(m)
}

// magnitudeRe matches a benchstat value cell: a number with an optional
// SI/IEC scale suffix.
var magnitudeRe = regexp.MustCompile(`^([0-9]+(?:\.[0-9]+)?)([A-Za-z]*)$`)

var magnitudeScale = map[string]float64{
	"":   1,
	"k":  1e3,
	"K":  1e3,
	"M":  1e6,
	"G":  1e9,
	"Ki": 1024,
	"Mi": 1024 * 1024,
	"Gi": 1024 * 1024 * 1024,
}

// parseMagnitude returns the numeric magnitude of a benchstat value cell, or
// unreadableMagnitude. benchstat SCALES large values and prints the scale as a
// suffix -- "128.0k" is 128000 allocations, not 128 -- so the printed token is
// not the number, and the quantisation rule needs the number.
func parseMagnitude(s string) float64 {
	m := magnitudeRe.FindStringSubmatch(s)
	if m == nil {
		return unreadableMagnitude
	}
	mant, err := strconv.ParseFloat(m[1], 64)
	if err != nil {
		return unreadableMagnitude
	}
	mult, ok := magnitudeScale[m[2]]
	if !ok {
		return unreadableMagnitude
	}
	return mant * mult
}

// roundDelta rounds to the two decimal places benchstat prints
// (fmt.Sprintf("%+.2f%%")). Both front ends apply it so a boundary case cannot
// be decided differently by whichever of them happened to carry more digits
// than the reader can see.
func roundDelta(v float64) float64 {
	return math.Round(v*100) / 100
}

// roundSpread rounds to the whole percent benchstat prints
// (Summary.PctRangeString, fmt.Sprintf("%.0f%%")). Same reason as roundDelta.
func roundSpread(v float64) float64 {
	return math.Round(v)
}
