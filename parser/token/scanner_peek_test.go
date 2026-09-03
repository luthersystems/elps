// Copyright © 2026 The ELPS authors

package token

import (
	"strings"
	"testing"
)

// TestScannerPeekBufferDoesNotReallocate pins the fix for the scanner's
// one-rune peek buffer.  ScanRune used to consume the buffered rune with
// s.peek = s.peek[1:], which on a one-element slice leaves length AND capacity
// zero, so the append in Peek allocated a fresh backing array for every rune
// of source.  ScanRune now shifts the buffer down in place, keeping the slot
// the first Peek allocated.
//
// The lexer drives the scanner as an alternating Peek/ScanRune pair (see
// Accept and friends), so the test drives it the same way over a few KB of
// source and asserts that a full scan is allocation-free in steady state.
//
// Each measured run needs its own scanner -- a scanner that has reached the
// end of its input cannot be rescanned -- so the scanners are built up front,
// outside the measured closure, and the closure only scans.  The bound is 2
// rather than 0 to leave slack for incidental allocation from the runtime or
// from future scanner bookkeeping: the regression this pins costs one
// allocation per rune (thousands per run), so anything in single digits
// distinguishes it unambiguously.
func TestScannerPeekBufferDoesNotReallocate(t *testing.T) {
	const runs = 10

	// ~4KB of ASCII source.  ASCII means the rune count equals the byte count.
	src := strings.Repeat("(defun add-one (x) (+ x 1)) ; increment\n", 100)
	nrune := len(src)

	// testing.AllocsPerRun calls f once to warm up and then runs more times.
	scanners := make([]*Scanner, runs+1)
	for i := range scanners {
		scanners[i] = NewScannerString("scanner_peek_test.lisp", src)
	}
	var next int

	avg := testing.AllocsPerRun(runs, func() {
		if next >= len(scanners) {
			t.Fatalf("AllocsPerRun called the closure more than %d times", len(scanners))
		}
		s := scanners[next]
		next++
		for i := range nrune {
			if _, ok := s.Peek(); !ok {
				t.Fatalf("Peek failed at rune %d of %d", i, nrune)
			}
			if err := s.ScanRune(); err != nil {
				t.Fatalf("ScanRune failed at rune %d of %d: %v", i, nrune, err)
			}
		}
	})

	if avg > 2 {
		t.Errorf("scanning %d runes allocated %v times per run; want <= 2 "+
			"(the peek buffer is being reallocated per rune)", nrune, avg)
	}
}
