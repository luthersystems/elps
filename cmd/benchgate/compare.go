package main

import (
	"fmt"
	"io"
	"math"
	"os"
	"sort"

	"golang.org/x/perf/benchfmt"
	"golang.org/x/perf/benchmath"
)

// This file is the structured front end: raw `go test -bench` output in,
// adjudicable rows out, with no benchstat binary and no text in between.
//
// It uses benchstat's own libraries and reproduces what benchstat itself does
// for each (package, benchmark, unit) cell:
//
//	summary    benchmath.AssumeNothing.Summary(sample, 0.95)
//	           -> median, and the 95% confidence interval of that median
//	spread     max(Hi/Center-1, 1-Lo/Center) * 100, per arm, as
//	           benchmath.Summary.PctRangeString computes it
//	p-value    benchmath.AssumeNothing.Compare(base, head) -- Mann-Whitney U
//	delta      (head/base - 1) * 100, as benchmath.Comparison.FormatDelta
//	           computes it
//
// and then rounds both to the precision benchstat PRINTS, so this front end
// and the table front end cannot disagree on a boundary case merely because
// one of them carried digits the reader never sees.

// confidence is benchstat's default confidence level for the per-arm interval.
const confidence = 0.95

// cellKey identifies one adjudicable cell: one metric column of one benchmark
// in one package. The benchmark name carries its -<GOMAXPROCS> suffix, exactly
// as `go test` emitted it, because that is how benchstat pairs arms.
type cellKey struct {
	pkg  string
	name string
	unit string
}

type cellOrder struct {
	key cellKey
	seq int
}

// samples reads one arm.
func readArm(r io.Reader, fileName string, stderr io.Writer) (map[cellKey][]float64, []cellOrder, map[string]bool, error) {
	vals := map[cellKey][]float64{}
	var order []cellOrder
	pkgs := map[string]bool{}
	seq := 0

	br := benchfmt.NewReader(r, fileName)
	for br.Scan() {
		switch rec := br.Result().(type) {
		case *benchfmt.SyntaxError:
			// Non-fatal by design in benchfmt: a `go test` log legitimately
			// carries lines that are not results. Reported, never silently
			// dropped -- a parser that quietly discards half its input is the
			// failure mode this whole gate exists to prevent.
			pf(stderr, "benchgate: %s\n", rec.Error())
		case *benchfmt.Result:
			pkg := rec.GetConfig("pkg")
			pkgs[pkg] = true
			for _, v := range rec.Values {
				k := cellKey{pkg: pkg, name: rec.Name.String(), unit: v.Unit}
				if _, ok := vals[k]; !ok {
					order = append(order, cellOrder{key: k, seq: seq})
					seq++
				}
				vals[k] = append(vals[k], v.Value)
			}
		}
	}
	if err := br.Err(); err != nil {
		return nil, nil, nil, err
	}
	return vals, order, pkgs, nil
}

// pctRange is benchmath.Summary.PctRangeString as a number: the half-width of
// the arm's confidence interval relative to its center, in percent. It returns
// unknownSpread for the cases benchstat renders as "∞" or "?" -- no interval
// could be computed, which is not the same as a perfect one.
func pctRange(s benchmath.Summary) float64 {
	if math.IsInf(s.Lo, 0) || math.IsInf(s.Hi, 0) {
		return unknownSpread
	}
	if sign(s.Center) != sign(s.Lo) || sign(s.Center) != sign(s.Hi) {
		return unknownSpread
	}
	if s.Center == 0 {
		return 0
	}
	return 100 * math.Max(s.Hi/s.Center-1, 1-s.Lo/s.Center)
}

func sign(v float64) int {
	switch {
	case v < 0:
		return -1
	case v > 0:
		return 1
	}
	return 0
}

// compareArms builds a comparison from two raw `go test -bench` outputs.
// unpaired counts cells present in exactly one arm; they are not adjudicable
// and the caller reports the count rather than letting them vanish.
func compareArms(baseFile, headFile string, alpha float64, stderr io.Writer) (c *comparison, unpaired int, err error) {
	bf, err := os.Open(baseFile) //#nosec G304 -- benchgate is a CLI given the arm file paths to read
	if err != nil {
		return nil, 0, err
	}
	defer func() { _ = bf.Close() }()
	hf, err := os.Open(headFile) //#nosec G304 -- benchgate is a CLI given the arm file paths to read
	if err != nil {
		return nil, 0, err
	}
	defer func() { _ = hf.Close() }()

	baseVals, _, basePkgs, err := readArm(bf, baseFile, stderr)
	if err != nil {
		return nil, 0, err
	}
	headVals, headOrder, headPkgs, err := readArm(hf, headFile, stderr)
	if err != nil {
		return nil, 0, err
	}

	c = &comparison{pkgSeen: map[string]bool{}}
	for p := range basePkgs {
		c.pkgSeen[p] = true
	}
	for p := range headPkgs {
		c.pkgSeen[p] = true
	}

	// Report in the order the head arm produced the cells, so the report reads
	// like the benchmark run rather than like a map iteration.
	sort.Slice(headOrder, func(i, j int) bool { return headOrder[i].seq < headOrder[j].seq })

	th := benchmath.Thresholds{CompareAlpha: alpha}
	for _, o := range headOrder {
		k := o.key
		bv, ok := baseVals[k]
		if !ok {
			unpaired++
			continue
		}
		hv := headVals[k]

		baseSample := benchmath.NewSample(append([]float64(nil), bv...), &th)
		headSample := benchmath.NewSample(append([]float64(nil), hv...), &th)
		baseSum := benchmath.AssumeNothing.Summary(baseSample, confidence)
		headSum := benchmath.AssumeNothing.Summary(headSample, confidence)
		cmp := benchmath.AssumeNothing.Compare(baseSample, headSample)

		// The LARGER of the two arms' spreads, matching what the table front
		// end reads off the printed row; see the note on row.spread.
		spread := unknownSpread
		for _, s := range []float64{pctRange(baseSum), pctRange(headSum)} {
			if s >= 0 && (spread < 0 || s > spread) {
				spread = s
			}
		}
		if spread >= 0 {
			spread = roundSpread(spread)
		}

		r := row{
			pkg:     k.pkg,
			name:    k.name,
			metric:  k.unit,
			pval:    cmp.P,
			pvalStr: fmt.Sprintf("%.3f", cmp.P),
			hasP:    true,
			spread:  spread,
			baseVal: baseSum.Center,
		}

		// benchstat renders a non-significant comparison as "~" rather than as
		// a delta, and the shell gate counted those separately. Same here, so
		// the two front ends produce the same summary counts on the same data.
		if cmp.P > alpha || baseSum.Center == headSum.Center || baseSum.Center == 0 {
			r.kind = kindTilde
			c.rows = append(c.rows, r)
			continue
		}

		r.kind = kindDelta
		r.delta = roundDelta((headSum.Center/baseSum.Center - 1) * 100)
		r.deltaTok = fmt.Sprintf("%+.2f%%", r.delta)
		c.rows = append(c.rows, r)
	}

	// Cells the base arm had and the head arm did not are unpaired too.
	for k := range baseVals {
		if _, ok := headVals[k]; !ok {
			unpaired++
		}
	}

	return c, unpaired, nil
}
