// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestCoWCounterRecordsEverySite is the census's own anti-vacuity check:
// every instrumented copy-on-write-on-sealed site is reachable from lisp
// and is counted exactly once per event, attributed to the lisp location
// that triggered it.  A census whose sites never fire proves nothing about
// production code, so the sites are exercised deliberately here.
//
// The counters are process-global; each case measures a delta.
func TestCoWCounterRecordsEverySite(t *testing.T) {
	env := newCowTestEnv(t)
	if v := env.LoadString("cow.lisp", `(set 'lit '(3 1 2))`); v.Type == lisp.LError {
		t.Fatalf("setup: %v", v)
	}
	for _, tc := range []struct {
		site string
		src  string
	}{
		{"stable-sort", `(stable-sort > lit)`},
		{"slice-vector", `(slice 'vector lit 0 2)`},
		{"append-vector", `(append 'vector lit 4)`},
	} {
		t.Run(tc.site, func(t *testing.T) {
			before := lisp.CoWOnSealedCounts()
			if _, ok := before[tc.site]; !ok {
				t.Fatalf("site %q is not in the census: %v", tc.site, before)
			}
			if v := env.LoadString("cow.lisp", tc.src); v.Type == lisp.LError {
				t.Fatalf("eval %q: %v", tc.src, v)
			}
			after := lisp.CoWOnSealedCounts()
			if got := after[tc.site] - before[tc.site]; got != 1 {
				t.Errorf("%s recorded %d events for %q, want 1", tc.site, got, tc.src)
			}
			for site, n := range after {
				if site == tc.site {
					continue
				}
				if n != before[site] {
					t.Errorf("%q also moved the %s counter (%d -> %d)", tc.src, site, before[site], n)
				}
			}

			// The same expression over a copy takes the ordinary,
			// non-copying path: this is the remedy the flip would name.
			before = lisp.CoWOnSealedCounts()
			src := strings.Replace(tc.src, "lit", "(copy lit)", 1)
			if v := env.LoadString("cow.lisp", src); v.Type == lisp.LError {
				t.Fatalf("eval %q: %v", src, v)
			}
			if got := lisp.CoWOnSealedCounts()[tc.site] - before[tc.site]; got != 0 {
				t.Errorf("%q still recorded %d events; copy must clear the seal", src, got)
			}
		})
	}
}

// TestCoWCounterReportNamesLispLocation checks the report is usable as
// evidence: it names every site (including quiet ones) and attributes an
// event to the lisp source that caused it, not to Go code.
func TestCoWCounterReportNamesLispLocation(t *testing.T) {
	env := newCowTestEnv(t)
	const src = `
(defun sort-a-literal () (stable-sort > '(5 4 6)))
(sort-a-literal)
`
	if v := env.LoadString("attribution.lisp", src); v.Type == lisp.LError {
		t.Fatalf("eval: %v", v)
	}
	report := lisp.CoWOnSealedReport()
	for _, want := range []string{"stable-sort", "slice-vector", "append-vector"} {
		if !strings.Contains(report, want) {
			t.Errorf("report does not name the %s site:\n%s", want, report)
		}
	}
	if !strings.Contains(report, "attribution.lisp") {
		t.Errorf("report does not attribute the event to the lisp file that caused it:\n%s", report)
	}
	if !strings.Contains(report, "sort-a-literal") {
		t.Errorf("report does not name the lisp function that caused the event:\n%s", report)
	}
}
