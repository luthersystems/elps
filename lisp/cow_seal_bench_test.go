// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// sealedLiteralCoWProgram reaches every copy-on-write-on-sealed site in
// lisp/cow_sites.go: stable-sort over a quoted list, (slice 'vector ...)
// over one, and (append 'vector ...) over storage sliced out of one.  Each
// takes the CoW branch and copies rather than writing the program literal.
//
// Shared with TestSealedLiteralCoWBenchmarkCoversEverySite (checked builds
// only), which asserts that "every site" claim against the census instead of
// leaving it as a comment that can quietly stop being true.
const sealedLiteralCoWProgram = `
  (dotimes (n 200)
    (stable-sort < '(5 3 1 4 2))
    (slice 'vector '(1 2 3 4 5) 0 4)
    (append 'vector (slice 'list '(1 2 3 4) 0 2) 9))
`

// BenchmarkSealedLiteralCoW measures the three builtins above on their
// sealed-input path.
//
// Its second job matters more than its first.  elpstest.RunBenchmark shares
// ONE sealed parse across every iteration's Runtime rather than deep-copying
// it per iteration, and what licenses that share is VerifySealedASTs
// re-fingerprinting the shared tree afterwards: an in-place write fails the
// benchmark instead of passing silently.  But an oracle only reports on the
// writes it is given, and before this benchmark existed no benchmarked
// program reached ANY CoW site — so the fingerprint check ran over programs
// that could not have exercised the guards it exists to backstop.  With this
// one in the set, `make bench-elpscheck-smoke` fails when a guard is removed.
//
// Untagged — the arm the regression gate compares — the verifier is a nil
// call and this is an ordinary benchmark of three builtins, nothing more.
// That asymmetry is the point: correctness is adjudicated by the checked
// smoke run, performance by the untagged measurement run, and neither
// perturbs the other.
func BenchmarkSealedLiteralCoW(b *testing.B) {
	elpstest.RunBenchmark(b, sealedLiteralCoWProgram)
}
