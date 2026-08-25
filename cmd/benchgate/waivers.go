package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// A waiver is a reviewed, bounded, expiring per-row exception. See the
// "Reviewed waivers" section of the package doc for why it is shaped this way.
type waiver struct {
	pkg     string
	bench   string
	metric  string
	ceilStr string
	expires string
	issue   string
	reason  string
	ceiling float64
	line    int
	expired bool

	// Adjudication bookkeeping, filled in as rows are judged.
	seen       bool // the row this waiver names appeared in the comparison
	used       bool // it suppressed a regression
	exceeded   bool // the row moved past the recorded ceiling
	expiredHit bool // the row regressed and the waiver had expired
	// unmeasured records that the row was at or above its gate but the
	// comparison could not size the move (issue #542). Without it, the report
	// would tell the reader the waiver "can be deleted" on the strength of a
	// measurement it had just declared worthless.
	unmeasured bool
}

// waiverSet is a parsed waiver file plus the diagnostics from parsing it. A
// file with any bad entry is a hard failure: a waiver list that cannot be read
// must never be treated as an empty one, and a waiver that silently does not
// parse is a regression nobody is told about.
type waiverSet struct {
	source  string
	waivers []*waiver
	bad     []string
}

var (
	dateRe     = regexp.MustCompile(`^[0-9]{4}-[0-9]{2}-[0-9]{2}$`)
	pctRe      = regexp.MustCompile(`^[0-9]+(\.[0-9]+)?$`)
	issueRefRe = regexp.MustCompile(`^[A-Za-z0-9._/-]*#[0-9]+$`)
	issueURLRe = regexp.MustCompile(`^https://github\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/(issues|pull)/[0-9]+$`)
	fieldSplit = regexp.MustCompile(`[ \t,]+`)
	commentRe  = regexp.MustCompile(`^[ \t]*(#|$)`)
)

// issuesOK accepts one or more tracking references, space separated:
// elps#412, #412, luthersystems/elps#412, or a GitHub issue/PR URL. Anything
// else is not a reference somebody can be sent to.
//
// It COUNTS good references rather than merely not rejecting: a field of ","
// splits into two empty tokens, every one of which passes a not-rejected test.
func issuesOK(s string) bool {
	good := 0
	for _, t := range fieldSplit.Split(s, -1) {
		if t == "" {
			continue
		}
		if issueRefRe.MatchString(t) || issueURLRe.MatchString(t) {
			good++
			continue
		}
		return false
	}
	return good > 0
}

// parseWaivers reads a waiver file. today is the YYYY-MM-DD the expiry check
// is made against; ISO dates compare correctly as strings, so there is no date
// arithmetic and no dependency on how the platform parses dates.
func parseWaivers(source, content, today string) *waiverSet {
	ws := &waiverSet{source: source}
	if source == "" {
		ws.source = "<none>"
	}

	report := func(lineno int, msg string) {
		ws.bad = append(ws.bad, fmt.Sprintf("  WAIVER-BAD  %s:%d  %s", ws.source, lineno, msg))
	}

	lines := strings.Split(content, "\n")
	// A trailing newline yields a final empty element; it is a comment/blank
	// by the rule below, so it needs no special case.
	for i, raw := range lines {
		lineno := i + 1
		wl := strings.TrimSuffix(raw, "\r")
		if commentRe.MatchString(wl) {
			continue
		}

		wf := strings.Split(wl, "|")
		if len(wf) != 7 {
			report(lineno, fmt.Sprintf("expected 7 |-separated fields (pkg | benchmark | metric | ceiling | expires | issue | reason), found %d: %s", len(wf), strings.TrimSpace(wl)))
			continue
		}
		for j := range wf {
			wf[j] = strings.Trim(wf[j], " \t")
		}

		bad := false
		if wf[0] == "" {
			report(lineno, "empty pkg field; a waiver must name the package it covers")
			bad = true
		}
		switch {
		case wf[1] == "":
			report(lineno, "empty benchmark field; a waiver must name the benchmark it covers")
			bad = true
		case gomaxprocsSuffix.MatchString(wf[1]):
			report(lineno, fmt.Sprintf("benchmark %s carries a -<GOMAXPROCS> suffix; write it as %s so the waiver does not unbind when the runner changes", wf[1], baseName(wf[1])))
			bad = true
		}
		if wf[2] == "" {
			report(lineno, "empty metric field; a waiver covers one metric column, not the whole row")
			bad = true
		}
		ceiling := 0.0
		if !pctRe.MatchString(wf[3]) {
			report(lineno, fmt.Sprintf("ceiling %s is not a positive percentage; an unbounded waiver is a threshold increase in disguise", orEmpty(wf[3])))
			bad = true
		} else {
			ceiling, _ = strconv.ParseFloat(wf[3], 64)
			if ceiling <= 0 {
				report(lineno, fmt.Sprintf("ceiling %s is not a positive percentage; an unbounded waiver is a threshold increase in disguise", orEmpty(wf[3])))
				bad = true
			}
		}
		if !dateRe.MatchString(wf[4]) {
			report(lineno, fmt.Sprintf("expires %s is not a YYYY-MM-DD date; a waiver with no end date is never revisited", orEmpty(wf[4])))
			bad = true
		}
		if !issuesOK(wf[5]) {
			report(lineno, fmt.Sprintf("issue %s is not a tracking reference (elps#412, substrate#392, #412, owner/repo#412 or a github.com issue/PR URL); a waiver nobody has to come back to is just a silent threshold increase", orEmpty(wf[5])))
			bad = true
		}
		if len(wf[6]) < 10 {
			report(lineno, "reason is missing or too short; say what the regression buys and what the alternative cost")
			bad = true
		}
		if bad {
			continue
		}

		ws.waivers = append(ws.waivers, &waiver{
			pkg:     wf[0],
			bench:   wf[1],
			metric:  wf[2],
			ceiling: ceiling,
			ceilStr: wf[3],
			expires: wf[4],
			issue:   wf[5],
			reason:  wf[6],
			line:    lineno,
			expired: today > wf[4],
		})
	}
	return ws
}

func orEmpty(s string) string {
	if s == "" {
		return "<empty>"
	}
	return s
}

// find returns the waiver covering this row, or nil. Exact on all three keys,
// so a waiver cannot reach a row it was not written for.
func (ws *waiverSet) find(pkg, bench, metric string) *waiver {
	for _, w := range ws.waivers {
		if w.pkg == pkg && w.bench == bench && w.metric == metric {
			return w
		}
	}
	return nil
}
