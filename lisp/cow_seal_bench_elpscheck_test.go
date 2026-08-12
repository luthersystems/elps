// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestSealedLiteralCoWBenchmarkCoversEverySite is the anti-vacuity gate for
// the checked-mode benchmark smoke run (`make bench-elpscheck-smoke`,
// .github/workflows/elps.yml).
//
// That run's whole purpose is to execute the benchmarked paths with
// VerifySealedASTs armed, so that an in-place write to the sealed program
// elpstest.RunBenchmark now SHARES across runtimes fails the run.  A
// fingerprint oracle over programs that never reach a copy-on-write site is
// green for the same reason an unplugged smoke detector is quiet, and
// nothing distinguishes the two from the outside.  So: assert the
// benchmark's program actually moves every counter in the census.
//
// The site list is READ FROM the census (CoWOnSealedCounts pre-populates
// every site, including quiet ones) rather than written out here.  A fourth
// CoW site added to lisp/cow_sites.go therefore fails this test until
// sealedLiteralCoWProgram covers it — which is the ordering that keeps the
// smoke run's coverage from silently falling behind the guards it backstops.
func TestSealedLiteralCoWBenchmarkCoversEverySite(t *testing.T) {
	before := lisp.CoWOnSealedCounts()
	if len(before) == 0 {
		t.Fatal("the CoW census reported no sites at all; it is the source of the site list here")
	}

	env := newCowTestEnv(t)
	if v := env.LoadString("cow_bench.lisp", sealedLiteralCoWProgram); v.Type == lisp.LError {
		t.Fatalf("evaluating the benchmark program: %v", v)
	}

	after := lisp.CoWOnSealedCounts()
	for site, was := range before {
		if after[site] <= was {
			t.Errorf("the benchmark program never reaches the %q copy-on-write site "+
				"(census %d -> %d); the checked-mode benchmark smoke run cannot "+
				"catch a regression at a site no benchmark exercises — extend "+
				"sealedLiteralCoWProgram", site, was, after[site])
		}
	}
}
