// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// sealedLiteralWriteErrorProgram reaches every guarded sealed-write site in
// the kernel: stable-sort over a quoted list, (slice 'vector ...) over one,
// and (append 'vector ...) over storage sliced out of one.  Each raises the
// catchable modify-literal-error condition (issue #378; these sites used to
// copy-on-write silently), and the program catches every one with a named
// handler-bind and counts it.
//
// The final assert is the anti-vacuity gate that used to live in the
// checked-mode copy-on-write census (deleted with the error flip): if any
// guard regresses — to a silent copy, or all the way back to an in-place
// write — its handler stops firing, the count comes up short, and the
// program itself fails the benchmark.  In `make bench-elpscheck-smoke`
// (elpscheck builds) the sealed-fingerprint verifier additionally fails the
// run if a regressed site wrote the shared program tree in place.
const sealedLiteralWriteErrorProgram = `
  (set 'caught 0)
  (defun count-literal-error (thunk)
    (handler-bind ([modify-literal-error
                    (lambda (c &rest args) (set! caught (+ caught 1)))])
      (thunk)))
  (dotimes (n 200)
    (count-literal-error (lambda () (stable-sort < '(5 3 1 4 2))))
    (count-literal-error (lambda () (slice 'vector '(1 2 3 4 5) 0 4)))
    (count-literal-error (lambda () (append 'vector (slice 'list '(1 2 3 4) 0 2) 9))))
  (assert (= caught 600))
`

// BenchmarkSealedLiteralWriteError measures the sealed-write error path of
// the three guarded builtins: raising the modify-literal-error condition and
// catching it with a named handler-bind.  It replaces
// BenchmarkSealedLiteralCoW, which measured the copy the same inputs used to
// take before issue #378's flip from silent copy-on-write to a catchable
// error.
//
// Its second job matters more than its first.  elpstest.RunBenchmark shares
// ONE sealed parse across every iteration's Runtime rather than deep-copying
// it per iteration, and what licenses that share is VerifySealedASTs
// re-fingerprinting the shared tree afterwards: an in-place write fails the
// benchmark instead of passing silently.  An oracle only reports on the
// writes it is given, and before this benchmark existed no benchmarked
// program reached ANY guarded sealed-write site — so the fingerprint check
// ran over programs that could not have exercised the guards it exists to
// backstop.  With this one in the set, `make bench-elpscheck-smoke` fails
// when a guard is removed — and the program's own catch-counting assert
// fails in ANY build, tagged or not.
//
// Untagged — the arm the regression gate compares — the verifier is a nil
// call and this is an ordinary benchmark of the error path, nothing more.
// That asymmetry is the point: correctness is adjudicated by the checked
// smoke run, performance by the untagged measurement run, and neither
// perturbs the other.
func BenchmarkSealedLiteralWriteError(b *testing.B) {
	elpstest.RunBenchmark(b, sealedLiteralWriteErrorProgram)
}
