package main

import (
	"regexp"
	"strconv"
	"strings"
)

// This file reads a benchstat TABLE -- the human-formatted text benchstat
// prints -- and turns it into rows. It is a direct port of the awk program
// that used to be scripts/benchstat-gate.sh in both repositories, kept
// deliberately faithful so the fixture corpora those repositories accumulated
// remain valid evidence: every one of them is replayed through this reader in
// table_parity_test.go and must reach the same verdict the shell gate reached.
//
// Both benchstat table formats are handled:
//
//	new (golang.org/x/perf, box-drawing columns):
//	  EnvGet-4   27.13m ± ∞ ¹   29.16m ± ∞ ¹  +7.14% (p=0.008 n=5)
//	old (pre-2022 "name/delta" table):
//	  EnvGet-4   27.1ms ± 2%   29.2ms ± 1%   +7.14%  (p=0.008 n=5+5)

var (
	blankRe     = regexp.MustCompile(`^[ \t]*$`)
	hashRe      = regexp.MustCompile(`^[ \t]*#`)
	contextRe   = regexp.MustCompile(`^(goos|goarch|pkg|cpu):`)
	footnoteRe  = regexp.MustCompile(`^[ \t]*(¹|²|³|⁴|⁵|⁶|⁷|⁸|⁹)`)
	oldHeaderRe = regexp.MustCompile(`^name[ \t]`)
	signedPctRe = regexp.MustCompile(`[+-][0-9]+(\.[0-9]+)?%`)
	spreadPctRe = regexp.MustCompile(`^[ \t]*[0-9]+(\.[0-9]+)?%`)
	pvalueRe    = regexp.MustCompile(`^[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?$`)
	tokenEndRe  = regexp.MustCompile(`[ \t)].*$`)
)

// maxSpread returns the LARGEST per-arm spread benchstat printed on this row,
// as a percent, or unknownSpread when it printed none it could compute.
//
// benchstat writes each arm as "<median> ± <pct>%": the half-width of the 95%
// confidence interval of the median of that arm, relative to it. That is how
// finely this comparison can see on this row, measured on the same samples the
// verdict is drawn from. Below 6 samples it cannot compute one and prints
// "± ∞ ¹" instead; that is reported as unknown rather than as 0, because
// treating "no interval" as "a perfect interval" would suppress nothing while
// looking like it had checked.
func maxSpread(s string) float64 {
	best := unknownSpread
	sawInf := false
	rest := s
	for {
		off := strings.Index(rest, "±")
		if off < 0 {
			break
		}
		rest = rest[off+len("±"):]
		// "± 24%" and "±24%" both occur; "± ∞ ¹" is the no-interval case.
		if tok := spreadPctRe.FindString(rest); tok != "" {
			num, err := strconv.ParseFloat(strings.TrimRight(strings.Trim(tok, " \t"), "%"), 64)
			if err == nil && num > best {
				best = num
			}
		} else {
			sawInf = true
		}
	}
	if best < 0 && sawInf {
		return unknownSpread
	}
	return best
}

// lastSignedPct returns the last signed-percentage token (e.g. +7.14% /
// -1.20%) in s, or "".
func lastSignedPct(s string) string {
	all := signedPctRe.FindAllString(s, -1)
	if len(all) == 0 {
		return ""
	}
	return all[len(all)-1]
}

// fields splits on runs of whitespace, as awk's default FS does.
func fields(s string) []string { return strings.Fields(s) }

// parseTable reads a benchstat table. It never returns an error: an
// uninterpretable table is reported by producing no delta and no tilde rows,
// which the caller turns into exit 2. That is deliberate -- see the exit-code
// note in the package doc.
func parseTable(content string) *comparison {
	c := &comparison{pkgSeen: map[string]bool{}}
	pkg := ""
	metric := ""

	for _, line := range strings.Split(content, "\n") {
		if blankRe.MatchString(line) {
			continue
		}

		// `#` comments. benchstat never emits one, but the fixtures are
		// annotated with the history of the run they capture -- and those
		// annotations QUOTE benchstat rows, deltas and p-values included.
		// Without this, the explanation a fixture carries is adjudicated as
		// data: the note above the table in benchstat-libjson-encode-411.txt
		// produced four phantom comparison rows, one of them a "below-gate"
		// verdict on a sentence. A fixture must not be able to move the
		// verdict by explaining itself.
		if hashRe.MatchString(line) {
			continue
		}

		// Context headers. elps benchmark output carries all four (the `cpu:`
		// line names the runner CPU).
		if contextRe.MatchString(line) {
			if strings.HasPrefix(line, "pkg:") {
				pkg = strings.TrimSpace(line[len("pkg:"):])
				c.pkgSeen[pkg] = true
			}
			continue
		}

		// Footnotes always START with a superscript marker ("¹ need >= 6
		// samples ...", "² all samples are equal"). Anchor on that rather than
		// on the substring: an old-format DATA row reads "~ (all equal)", and
		// a substring rule discarded it, so an all-equal comparison counted
		// zero rows and tripped the exit-2 "cannot interpret" path.
		if footnoteRe.MatchString(line) {
			continue
		}

		// Table header rows. The new format draws them with box characters and
		// always carries the literal "vs base"; the old format starts with
		// "name" and ends with a "delta" column. Remember the metric name
		// (sec/op, B/op, allocs/op, B/s, ...) -- it sets the direction in which
		// a change counts as a regression.
		if strings.Contains(line, "vs base") || oldHeaderRe.MatchString(line) {
			metric = "?"
			f := fields(line)
			for i := 1; i < len(f); i++ {
				if f[i] == "vs" || f[i] == "delta" {
					metric = f[i-1]
					break
				}
			}
			continue
		}
		// Any other box-drawing line is a header continuation, not data.
		if strings.Contains(line, "│") {
			continue
		}

		f := fields(line)
		if len(f) == 0 {
			continue
		}
		name := f[0]
		// The median of the base arm, as benchstat printed it. Both table
		// formats put it in the first column after the name ("9.000",
		// "128.0k"), and the quantisation rule needs its magnitude to know how
		// many WHOLE allocations a percentage delta stands for.
		baseVal := unreadableMagnitude
		if len(f) > 1 {
			baseVal = parseMagnitude(f[1])
		}

		// Bound the delta search: stop before "(p=" so the p-value is never
		// scanned, and start after the last "±" so an old-format "± 2%" spread
		// column (and any "%" inside a benchmark name) cannot be mistaken for
		// the delta.
		region := line
		hasP := false
		pval := 0.0
		pvalStr := "n/a"
		if pidx := strings.Index(region, "(p="); pidx >= 0 {
			// Take the whole token up to the next space or ")". Do NOT
			// truncate at the first non-digit: that turns "p=1.5e-05" into 1.5
			// and drops a real regression as insignificant -- the one parse
			// path that would fail OPEN. benchstat formats %.3f today, so
			// scientific notation is unreachable, but it is handled rather
			// than silently mis-read. Anything that is not a number at all is
			// a format change: flag it and let the caller exit 2.
			tok := tokenEndRe.ReplaceAllString(region[pidx+len("(p="):], "")
			if pvalueRe.MatchString(tok) {
				pval, _ = strconv.ParseFloat(tok, 64)
				pvalStr = tok
				hasP = true
			} else {
				c.rows = append(c.rows, row{kind: kindBadPValue, name: name, pvalStr: tok})
				continue
			}
			region = region[:pidx]
		}

		// Read the per-arm spreads BEFORE the region is truncated past them:
		// this is the measurement resolution of the row, and it is what the
		// resolution check is judged against.
		spread := maxSpread(region)

		if lastpm := strings.LastIndex(region, "±"); lastpm >= 0 {
			region = region[lastpm+len("±"):]
		}

		deltaTok := lastSignedPct(region)
		if deltaTok == "" {
			// A "~" row IS a successfully interpreted comparison -- it just
			// found no significant difference. Tally it separately so a table
			// in which nothing moved is not mistaken for "could not parse
			// anything" and turned into a spurious exit 2.
			if strings.Contains(line, "~") {
				c.rows = append(c.rows, row{
					kind: kindTilde, pkg: pkg, name: name, metric: metric,
				})
			}
			continue
		}

		delta, err := strconv.ParseFloat(strings.TrimSuffix(strings.TrimPrefix(deltaTok, "+"), "%"), 64)
		if err != nil {
			// Unreachable: the token matched signedPctRe. Treated as
			// uninterpretable rather than assumed to be zero.
			c.rows = append(c.rows, row{kind: kindBadPValue, name: name, pvalStr: deltaTok})
			continue
		}

		c.rows = append(c.rows, row{
			kind:     kindDelta,
			pkg:      pkg,
			name:     name,
			metric:   metric,
			deltaTok: deltaTok,
			delta:    delta,
			pval:     pval,
			pvalStr:  pvalStr,
			hasP:     hasP,
			spread:   spread,
			baseVal:  baseVal,
		})
	}
	return c
}
