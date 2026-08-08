// Copyright © 2026 The ELPS authors

package token

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
)

// Scanner is a hand-rolled sliding window over an io.Reader: a byte buffer, a
// token start offset, a current-rune offset and a lookahead offset, all
// shifted in place by extend().  Every one of those offsets is derived
// arithmetic, and the lexer drives them in an order the unit tests only
// partially cover.  That is where the "index out of range [-1]" in
// checkRuneError lived: after Ignore() moved start past the current rune, the
// next extend() left pos pointing before the buffer.
//
// This target drives the exported state machine with an operation script taken
// from the fuzzer's second argument, so the ordering of Peek/ScanRune/Ignore/
// EmitToken is explored rather than fixed.

// maxScannerOps bounds the driver loop.  Scanner has no terminal state -- it
// answers EOF() forever -- so the loop needs its own bound.  Kept small on
// purpose: Text() and EmitToken copy the whole pending token, so a long script
// over a large input spends the entire budget allocating strings instead of
// exploring orderings.
const maxScannerOps = 256

func FuzzScanner(f *testing.F) {
	scripts := []string{
		"S",     // scan only
		"SI",    // scan, ignore (the shape that strands pos behind start)
		"SIP",   // scan, ignore, peek (peek triggers extend)
		"SIPS",  // ...then scan again: the checkRuneError crash
		"PSE",   // peek, scan, emit
		"SSSE",  // multi-rune token then emit
		"ETSIP", // emit an empty token, read text, then the strand shape
	}
	// One script per source, rotating, rather than the full cross product:
	// the baseline-coverage pass runs every seed before fuzzing starts, and a
	// cross product of 100+ sources with 7 scripts spends that whole pass on
	// near-duplicates.
	for i, src := range fuzzseed.All() {
		f.Add(src, []byte(scripts[i%len(scripts)]))
	}
	// Every script at least once, against input that reaches all of the
	// scanner's branches.
	for _, script := range scripts {
		f.Add([]byte("(a \"b\" 1.5 #xFF)\n; c\n\xe4\xb8"), []byte(script))
	}
	f.Fuzz(func(t *testing.T, src []byte, script []byte) {
		if len(script) == 0 {
			script = []byte("SIPS")
		}
		s := NewScanner("fuzz", bytes.NewReader(src))
		s.SetPath("fuzz/path")
		n := len(script)
		if n > maxScannerOps {
			n = maxScannerOps
		}
		for i := 0; i < n; i++ {
			switch script[i] % 6 {
			case 0:
				_ = s.ScanRune()
			case 1:
				s.Ignore()
			case 2:
				_, _ = s.Peek()
			case 3:
				_ = s.EmitToken(SYMBOL)
			case 4:
				_ = s.Text()
			case 5:
				_ = s.Accept(func(rune) bool { return true })
			}
			// Locations are consumed by every diagnostic the interpreter
			// prints.  Line numbering starts at 1 and only ever increments
			// (Scanner.line / Scanner.startLine), so a non-positive line means
			// the offset bookkeeping has come apart.
			if loc := s.Loc(); loc.Line < 1 {
				t.Fatalf("op %d (%q): Loc().Line = %d", i, script[i], loc.Line)
			}
			if loc := s.LocStart(); loc.Line < 1 {
				t.Fatalf("op %d (%q): LocStart().Line = %d", i, script[i], loc.Line)
			}
		}
	})
}

// TestScannerStrandedPositionDoesNotPanic pins the minimised reproduction of
// the checkRuneError crash, driven through the exported API only.  "\xe4\xb8"
// is a three-byte UTF-8 sequence truncated to two: ScanRune advances onto the
// undecodable rune, Ignore leaves it behind the token start, Peek's extend()
// then discards it, and the next ScanRune formatted an error message from a
// buffer index that no longer existed.
func TestScannerStrandedPositionDoesNotPanic(t *testing.T) {
	s := NewScanner("fuzz", bytes.NewReader([]byte("\xe4\xb8")))
	if err := s.ScanRune(); err == nil {
		t.Fatal("expected an invalid-utf8 error from the first ScanRune")
	}
	s.Ignore()
	if _, ok := s.Peek(); ok {
		t.Fatal("expected Peek to fail on the truncated sequence")
	}
	if err := s.ScanRune(); err == nil {
		t.Fatal("expected an invalid-utf8 error from the second ScanRune")
	}
}
