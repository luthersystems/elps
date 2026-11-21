// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"io"
	"log"
	"math/rand/v2"
	"os"
	"strconv"
	"testing"
)

// referenceTerminalFID is a verbatim copy of the original linear-scan
// CallStack.TerminalFID algorithm, returning (length, panicked) instead of
// calling log.Panicf on TROBlock.
func referenceTerminalFID(frames []CallFrame, fid string) (int, bool) {
	if len(frames) == 0 {
		return 0, false
	}
	for i := len(frames) - 1; i >= 0; i-- {
		if !frames[i].Terminal {
			return 0, false
		}
		if frames[i].TROBlock {
			return 0, true
		}
		if frames[i].FID == fid {
			return len(frames) - i, false
		}
	}
	return 0, false
}

// callTerminalFID invokes s.TerminalFID(fid) and captures whether it panicked.
func callTerminalFID(s *CallStack, fid string) (int, bool) {
	panicked := true
	var n int
	func() {
		defer func() { _ = recover() }()
		n = s.TerminalFID(fid)
		panicked = false
	}()
	return n, panicked
}

// maxLinearWorkRatio bounds termWork(10n) / termWork(n) in the scaling tests.
// Linear work gives a ratio near 10.  Quadratic work gives a ratio near 100.
const maxLinearWorkRatio = 15

// directChainWork pushes a chain of n distinct terminal frames, calling
// TerminalFID before each push as funCallFrame does, and returns termWork.
func directChainWork(t *testing.T, fids []string, n int) int {
	t.Helper()
	s := CallStack{Frames: make([]CallFrame, 0, n)}
	for i := range n {
		if got := s.TerminalFID(fids[i]); got != 0 {
			t.Fatalf("unexpected match %d at step %d", got, i)
		}
		if err := s.PushFID(nil, fids[i], DefaultUserPackage, ""); err != nil {
			t.Fatal(err)
		}
		s.setTopTerminal(true)
	}
	if got := s.TerminalFID(fids[0]); got != n {
		t.Fatalf("TerminalFID(fids[0]) = %d, want %d", got, n)
	}
	for range n {
		s.Pop()
	}
	return s.termWork
}

// TestTerminalFIDDirectScaling checks that a chain of distinct terminal frames
// costs linear work in chain length (luthersystems/elps#1009).  It counts
// frames visited, so it does not depend on timing, -race or machine load.
func TestTerminalFIDDirectScaling(t *testing.T) {
	const (
		smallN = 1000
		largeN = 10 * smallN
	)
	fids := make([]string, largeN+1)
	for i := range fids {
		fids[i] = "_fun" + strconv.Itoa(i)
	}
	wSmall := directChainWork(t, fids, smallN)
	wLarge := directChainWork(t, fids, largeN)
	t.Logf("work n=%d: %d, n=%d: %d", smallN, wSmall, largeN, wLarge)
	if wLarge > 4*largeN {
		t.Fatalf("n=%d visited %d frames, want at most %d", largeN, wLarge, 4*largeN)
	}
	if wLarge > maxLinearWorkRatio*wSmall {
		t.Fatalf("work grew super-linearly: n=%d visited %d frames, n=%d visited %d",
			smallN, wSmall, largeN, wLarge)
	}
}

// nestedChainWork runs an inner terminal chain of innerN frames above each of
// n outer frames, while the top outer frame is non-terminal, and returns
// termWork.
func nestedChainWork(t *testing.T, outerFIDs, innerFIDs []string, n int) int {
	t.Helper()
	var s CallStack
	for i := range n {
		if got := s.TerminalFID(outerFIDs[i]); got != 0 {
			t.Fatalf("unexpected outer match %d at %d", got, i)
		}
		if err := s.PushFID(nil, outerFIDs[i], DefaultUserPackage, ""); err != nil {
			t.Fatal(err)
		}
		// Simulate argument evaluation that runs an inner terminal chain
		// while the top outer frame is temporarily non-terminal.
		for j := range innerFIDs {
			if got := s.TerminalFID(innerFIDs[j]); got != 0 {
				t.Fatalf("unexpected inner match %d at (%d,%d)", got, i, j)
			}
			if err := s.PushFID(nil, innerFIDs[j], DefaultUserPackage, ""); err != nil {
				t.Fatal(err)
			}
			s.Top().Terminal = true
		}
		for range innerFIDs {
			s.Pop()
		}
		s.Top().Terminal = true
	}
	if got := s.TerminalFID(outerFIDs[0]); got != n {
		t.Fatalf("TerminalFID(outerFIDs[0]) = %d, want %d", got, n)
	}
	for range n {
		s.Pop()
	}
	return s.termWork
}

// TestTerminalFIDNestedChainScaling checks that repeatedly running a nested
// terminal chain above a non-terminal frame on top of a long outer terminal
// chain costs linear work.  A rebuild of the outer index on every step costs
// quadratic work.
func TestTerminalFIDNestedChainScaling(t *testing.T) {
	const (
		smallN = 500
		largeN = 10 * smallN
		innerN = 24
	)
	outerFIDs := make([]string, largeN+1)
	for i := range outerFIDs {
		outerFIDs[i] = "_outer" + strconv.Itoa(i)
	}
	innerFIDs := make([]string, innerN)
	for i := range innerFIDs {
		innerFIDs[i] = "_inner" + strconv.Itoa(i)
	}
	wSmall := nestedChainWork(t, outerFIDs, innerFIDs, smallN)
	wLarge := nestedChainWork(t, outerFIDs, innerFIDs, largeN)
	t.Logf("nested work n=%d: %d, n=%d: %d", smallN, wSmall, largeN, wLarge)
	if wLarge > maxLinearWorkRatio*wSmall {
		t.Fatalf("nested work grew super-linearly: n=%d visited %d frames, n=%d visited %d",
			smallN, wSmall, largeN, wLarge)
	}
}

// TestTerminalFIDParity compares CallStack.TerminalFID against the reference
// linear scan across many seeded random operation sequences covering duplicates,
// non-terminal gaps, TROBlock frames, hold/restore pairs, and direct slice
// truncations.
func TestTerminalFIDParity(t *testing.T) {
	// Silence os.Stderr and log output while intentionally triggering TROBlock
	// panics in the differential check.
	devNull, err := os.OpenFile(os.DevNull, os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	origStderr := os.Stderr
	origLog := log.Writer()
	os.Stderr = devNull
	log.SetOutput(io.Discard)
	t.Cleanup(func() {
		os.Stderr = origStderr
		log.SetOutput(origLog)
		_ = devNull.Close()
	})

	rng := rand.New(rand.NewPCG(1009, 2026)) //nolint:gosec // G404: deterministic seed for differential parity test
	smallPool := []string{"", "if", "let", "f0", "f1", "f2", "f3"}

	pickFID := func(uniqueCounter *int) string {
		if rng.IntN(3) == 0 {
			return smallPool[rng.IntN(len(smallPool))]
		}
		*uniqueCounter++
		return fmt.Sprintf("u%d", *uniqueCounter)
	}

	checkAll := func(t *testing.T, trial, step int, s *CallStack) {
		t.Helper()
		queries := []string{"", "missing-fid", "if", "f0"}
		if n := len(s.Frames); n > 0 {
			queries = append(queries,
				s.Frames[0].FID,
				s.Frames[n/2].FID,
				s.Frames[n-1].FID,
			)
			if n > 1 {
				queries = append(queries, s.Frames[n-2].FID)
			}
			for range 3 {
				queries = append(queries, s.Frames[rng.IntN(n)].FID)
			}
		}
		for _, q := range queries {
			wantLen, wantPanic := referenceTerminalFID(s.Frames, q)
			gotLen, gotPanic := callTerminalFID(s, q)
			if gotLen != wantLen || gotPanic != wantPanic {
				t.Fatalf("trial %d step %d len=%d query=%q: got (%d, panic=%v), want (%d, panic=%v)",
					trial, step, len(s.Frames), q, gotLen, gotPanic, wantLen, wantPanic)
			}
		}
	}

	uniqueCounter := 0
	for trial := range 200 {
		var s CallStack
		type savedHold struct {
			idx      int
			terminal bool
		}
		var holds []savedHold
		for step := range 200 {
			op := rng.IntN(100)
			switch {
			case op < 55:
				// Push a frame, usually marking it terminal to grow long
				// terminal chains well past the index threshold.
				fid := pickFID(&uniqueCounter)
				if err := s.PushFID(nil, fid, DefaultUserPackage, ""); err != nil {
					t.Fatal(err)
				}
				if rng.IntN(10) < 8 {
					if rng.IntN(2) == 0 {
						s.setTopTerminal(true)
					} else {
						s.Top().Terminal = true
					}
				}
				if rng.IntN(40) == 0 {
					if rng.IntN(2) == 0 {
						s.setTopTROBlock()
					} else {
						s.Top().TROBlock = true
					}
				}
			case op < 75 && len(s.Frames) > 0:
				// Pop a frame (and drop any holds above the new top).
				s.Pop()
				for len(holds) > 0 && holds[len(holds)-1].idx >= len(s.Frames) {
					holds = holds[:len(holds)-1]
				}
			case op < 85 && len(s.Frames) > 0:
				// Flip top frame Terminal via stack methods or directly.
				switch rng.IntN(3) {
				case 0:
					s.setTopTerminal(!s.Top().Terminal)
				case 1:
					wasTerm := s.Top().Terminal
					if got := s.clearTopTerminal(); got != wasTerm {
						t.Fatalf("trial %d step %d: clearTopTerminal() = %v, want %v", trial, step, got, wasTerm)
					}
				default:
					s.Top().Terminal = !s.Top().Terminal
				}
			case op < 90 && len(s.Frames) > 0:
				// Toggle top frame TROBlock.
				if !s.Top().TROBlock && rng.IntN(2) == 0 {
					s.setTopTROBlock()
				} else {
					s.Top().TROBlock = !s.Top().TROBlock
				}
			case op < 95 && len(s.Frames) > 0:
				// Use holdNonTerminal / restoreTerminal.
				if len(holds) > 0 && rng.IntN(2) == 0 {
					h := holds[len(holds)-1]
					holds = holds[:len(holds)-1]
					s.restoreTerminal(h.idx, h.terminal)
				} else {
					idx, term := s.holdNonTerminal()
					holds = append(holds, savedHold{idx: idx, terminal: term})
				}
			case len(s.Frames) > 0:
				// Direct truncation without Pop, as embedders or tests may do.
				newLen := rng.IntN(len(s.Frames))
				s.Frames = s.Frames[:newLen]
				for len(holds) > 0 && holds[len(holds)-1].idx >= len(s.Frames) {
					holds = holds[:len(holds)-1]
				}
			}
			checkAll(t, trial, step, &s)
		}
	}
}

// TestTerminalFIDShortChainNoIndex checks that short terminal chains stay
// allocation-free (termIdx remains nil up to terminalIndexThreshold+1 frames),
// that Copy never retains termIdx, and that unwinding clears termIdx.
func TestTerminalFIDShortChainNoIndex(t *testing.T) {
	var s CallStack
	for i := range terminalIndexThreshold + 1 {
		fid := "_short" + strconv.Itoa(i)
		if err := s.PushFID(nil, fid, DefaultUserPackage, ""); err != nil {
			t.Fatal(err)
		}
		s.setTopTerminal(true)
		if got := s.TerminalFID("_miss"); got != 0 {
			t.Fatalf("step %d: got %d, want 0", i, got)
		}
		if s.termIdx != nil {
			t.Fatalf("step %d: termIdx allocated at depth %d (threshold %d)", i, len(s.Frames), terminalIndexThreshold)
		}
	}
	// One more frame crosses the threshold and builds the index on miss.
	if err := s.PushFID(nil, "_cross", DefaultUserPackage, ""); err != nil {
		t.Fatal(err)
	}
	s.setTopTerminal(true)
	if got := s.TerminalFID("_miss"); got != 0 {
		t.Fatalf("cross step: got %d, want 0", got)
	}
	if s.termIdx == nil {
		t.Fatal("expected termIdx to be allocated past terminalIndexThreshold")
	}
	// Copy must leave termIdx nil so error snapshots retain no index.
	cp := s.Copy()
	if cp.termIdx != nil {
		t.Fatal("Copy retained termIdx")
	}
	// Unwinding the stack must release termIdx back to nil.
	for len(s.Frames) > 0 {
		s.Pop()
	}
	if s.termIdx != nil {
		t.Fatal("expected termIdx to be nil after unwinding stack")
	}
}

// pushTerminalChain pushes terminal frames with FIDs prefix0..prefix(n-1) and
// queries a missing FID before each push, so that a long chain builds termIdx.
func pushTerminalChain(t *testing.T, s *CallStack, prefix string, n int) {
	t.Helper()
	for i := range n {
		if got := s.TerminalFID("_miss"); got != 0 {
			t.Fatalf("%s%d: TerminalFID(_miss) = %d, want 0", prefix, i, got)
		}
		if err := s.PushFID(nil, prefix+strconv.Itoa(i), DefaultUserPackage, ""); err != nil {
			t.Fatal(err)
		}
		s.setTopTerminal(true)
	}
}

// TestTerminalFIDTruncateThenRegrow checks a stack whose Frames an embedder
// truncates and then regrows with append, past the old index end.  The index
// then describes frames that are gone.  The query for f20 finds a stale match,
// fails the chain check and discards the index.  So it and the later queries
// give the answer of the linear scan.
func TestTerminalFIDTruncateThenRegrow(t *testing.T) {
	var s CallStack
	pushTerminalChain(t, &s, "f", 40)
	if got := s.TerminalFID("_miss"); got != 0 || s.termIdx == nil {
		t.Fatalf("TerminalFID(_miss) = %d, termIdx = %v; want 0 and an index", got, s.termIdx)
	}
	s.Frames = s.Frames[:10]
	for i := range 30 {
		s.Frames = append(s.Frames, CallFrame{FID: "g" + strconv.Itoa(i), Terminal: true})
	}
	for _, fid := range []string{"f20", "f5", "g0", "g29", "_miss"} {
		want, _ := referenceTerminalFID(s.Frames, fid)
		if got := s.TerminalFID(fid); got != want {
			t.Errorf("TerminalFID(%q) = %d, want %d", fid, got, want)
		}
	}
}

// TestTerminalFIDExternalWrites checks the contract for an embedder that
// writes Frames directly (see CallStack.Frames).  TerminalFID can miss a
// chain, but a count it returns must equal the linear scan, and it must panic
// only when the linear scan panics.
func TestTerminalFIDExternalWrites(t *testing.T) {
	devNull, err := os.OpenFile(os.DevNull, os.O_WRONLY, 0)
	if err != nil {
		t.Fatal(err)
	}
	origStderr := os.Stderr
	origLog := log.Writer()
	os.Stderr = devNull
	log.SetOutput(io.Discard)
	t.Cleanup(func() {
		os.Stderr = origStderr
		log.SetOutput(origLog)
		_ = devNull.Close()
	})

	rng := rand.New(rand.NewPCG(1009, 7)) //nolint:gosec // G404: deterministic seed for the contract test
	pool := []string{"a", "b", "c", "d"}
	for trial := range 300 {
		var s CallStack
		pushTerminalChain(t, &s, "f", 20+rng.IntN(40))
		for step := range 60 {
			n := len(s.Frames)
			switch op := rng.IntN(6); {
			case op == 0 && n > 0:
				// Truncate, then regrow with append.
				s.Frames = s.Frames[:rng.IntN(n)]
				for range rng.IntN(n + 1) {
					s.Frames = append(s.Frames, CallFrame{FID: pool[rng.IntN(len(pool))], Terminal: rng.IntN(8) != 0})
				}
			case op == 1 && n > 0:
				s.Frames[rng.IntN(n)].FID = pool[rng.IntN(len(pool))]
			case op == 2 && n > 0:
				f := &s.Frames[rng.IntN(n)]
				f.Terminal = !f.Terminal
			case op == 3 && n > 0 && rng.IntN(4) == 0:
				s.Frames[rng.IntN(n)].TROBlock = true
			case op == 4 && n > 0:
				s.Pop()
			default:
				if err := s.PushFID(nil, pool[rng.IntN(len(pool))], DefaultUserPackage, ""); err != nil {
					t.Fatal(err)
				}
				s.setTopTerminal(true)
			}
			for _, q := range append([]string{"_miss", "f3", "f30"}, pool...) {
				wantLen, wantPanic := referenceTerminalFID(s.Frames, q)
				gotLen, gotPanic := callTerminalFID(&s, q)
				if gotPanic && !wantPanic {
					t.Fatalf("trial %d step %d query %q: panicked, linear scan returns %d", trial, step, q, wantLen)
				}
				if gotLen != 0 && (gotLen != wantLen || wantPanic) {
					t.Fatalf("trial %d step %d query %q: got %d, linear scan gives (%d, panic=%v)",
						trial, step, q, gotLen, wantLen, wantPanic)
				}
			}
		}
	}
}
