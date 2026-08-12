// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"
)

// Checked-mode copy-on-write census — the empirical half of issue #378.
//
// # What it measures
//
// The seal design (lisp/seal.go) resolves a mutation attempt on a sealed
// program literal by copying instead of writing: `(stable-sort < '(3 1
// 2))` sorts a fresh list and leaves the literal alone.  That choice kept
// every existing program working, but it bifurcates the semantics of one
// expression by the provenance of its input, and a silent copy can mask a
// real bug — code that meant to mutate in place, and now quietly does not.
// Racket and R7RS raise an error instead.
//
// Whether elps should follow them is an empirical question, not an
// aesthetic one: it depends on whether any real program ever reaches those
// sites.  This file answers it.  Every copy-on-write-on-sealed site calls
// recordCoWOnSealed with its own name; the recorder counts the event,
// remembers the lisp-level location that caused it, and prints the first
// sightings to stderr so that a suite run — including one driven by an
// external test binary with no access to this API — leaves an auditable
// trail in its log.
//
// # Reading the output
//
// Each distinct (site, lisp location) pair prints one line:
//
//	elps-cow: site=stable-sort loc=case.lisp:120:9 lisp:stable-sort <- case.lisp:118:3 case:latest-key
//
// `grep -c 'elps-cow:'` over a suite log therefore counts distinct
// trigger points, and CoWOnSealedReport renders exact per-site totals for
// callers that can reach the API.  Repeated events at an already-reported
// pair are counted but not reprinted, so a hot loop cannot flood the log.
//
// # Cost
//
// Zero when the tag is off.  The production build compiles
// recordCoWOnSealed to an empty inlined function (lisp/cow_check_
// default.go) and the site strings die with the call, so no counter
// symbol, table or string reaches a release binary — asserted by
// TestNoCoWCounterSymbolsUntagged in lisp/cow_counter_test.go and by the
// interleaved benchmarks on the pull request.

// cowSites lists every instrumented site, so a report can name a site
// with zero events instead of silently omitting it — "no line in the log"
// and "site does not exist" must not look the same.
var cowSites = []string{cowSiteSortStable, cowSiteSliceVector, cowSiteAppendVector}

// cowCheck holds the census.  Contention is irrelevant: in the expected
// (zero-event) case the lock is never taken at all, and a build carrying
// this file is already paying for the seal fingerprint verifier.
var cowCheck = cowCheckState{
	counts: make(map[string]uint64),
	seen:   make(map[cowEventKey]uint64),
}

type cowCheckState struct {
	counts map[string]uint64      // site → total events
	seen   map[cowEventKey]uint64 // (site, location) → events at that location
	mu     sync.Mutex
}

type cowEventKey struct {
	site string
	loc  string
}

// recordCoWOnSealed records one copy-on-write-on-sealed event at site,
// attributing it to the lisp-level location currently on the call stack.
//
// env may be nil (a site reached from Go code with no environment in
// hand); the event is still counted, with an unknown location.
func recordCoWOnSealed(env *LEnv, site string) {
	loc := cowLispLocation(env)
	cowCheck.mu.Lock()
	cowCheck.counts[site]++
	key := cowEventKey{site: site, loc: loc}
	n := cowCheck.seen[key]
	cowCheck.seen[key] = n + 1
	cowCheck.mu.Unlock()
	if n == 0 {
		// First sighting of this pair: report it where an external test
		// binary's log will keep it.  Later events at the same pair are
		// counted silently.
		fmt.Fprintf(os.Stderr, "elps-cow: site=%s loc=%s\n", site, loc)
	}
}

// cowLispLocation renders the lisp-level call site of an event: the frame
// that ran the mutating builtin, and the frame that called it.  Go-level
// attribution is deliberately absent — the site name already identifies
// the Go code; what the flip decision needs to know is which lisp code
// asked for the mutation.
func cowLispLocation(env *LEnv) string {
	if env == nil || env.Runtime == nil || env.Runtime.Stack == nil {
		return "<no lisp stack>"
	}
	frames := env.Runtime.Stack.Frames
	if len(frames) == 0 {
		return "<empty lisp stack>"
	}
	var sb strings.Builder
	// Innermost first, at most two frames: enough to name the call and its
	// caller without turning every event into a stack dump.
	for i := len(frames) - 1; i >= 0 && i > len(frames)-3; i-- {
		if sb.Len() > 0 {
			sb.WriteString(" <- ")
		}
		sb.WriteString(frames[i].String())
	}
	return sb.String()
}

// CoWOnSealedCounts returns a snapshot of the per-site event totals.
// Sites with no events are present with a zero count.
func CoWOnSealedCounts() map[string]uint64 {
	out := make(map[string]uint64, len(cowSites))
	for _, site := range cowSites {
		out[site] = 0
	}
	cowCheck.mu.Lock()
	defer cowCheck.mu.Unlock()
	for site, n := range cowCheck.counts {
		out[site] = n
	}
	return out
}

// CoWOnSealedReport renders the census: one line per site with its total,
// then one line per distinct lisp location that reached it.  Embedders
// print this at the end of a suite run; `go test -tags elpscheck` runs get
// the same information from the stderr lines.
func CoWOnSealedReport() string {
	cowCheck.mu.Lock()
	defer cowCheck.mu.Unlock()
	var sb strings.Builder
	for _, site := range cowSites {
		fmt.Fprintf(&sb, "cow-on-sealed %s: %d\n", site, cowCheck.counts[site])
		var locs []cowEventKey
		for key := range cowCheck.seen {
			if key.site == site {
				locs = append(locs, key)
			}
		}
		sort.Slice(locs, func(i, j int) bool { return locs[i].loc < locs[j].loc })
		for _, key := range locs {
			fmt.Fprintf(&sb, "    %6d  %s\n", cowCheck.seen[key], key.loc)
		}
	}
	return sb.String()
}
