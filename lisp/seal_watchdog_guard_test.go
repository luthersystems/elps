// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/parser/token"
)

// Red-proof anchors for the seal write watchdog (issue #372).
//
// These tests are the deterministic half of the #370 red-proof: each one
// registers a sealed tree with the watchdog and then drives the exact
// write path that corrupted shared parse metadata before the sealed-skip
// guard existed.  With the guard in place the stamp never writes and the
// tests assert that at the value level; with the guard reverted the stamp
// writes a watched node, and under `-race` the watchdog's unsynchronized
// reads turn that single write into a deterministic race report naming
// stampMacroExpansion — no second evaluating goroutine required.

// sealWatchdogSettle is how long a test waits for the watchdog to take
// passes over a registered tree.  Several ticks, so a slow scheduler
// cannot starve the read out of the exchange — and note the race
// detector's happens-before model makes the direction irrelevant: a
// pre-write read or a post-write read both race with an unsynchronized
// write.
const sealWatchdogSettle = 10 * sealWatchdogInterval

// TestSealWatchdogStampMacroExpansionGuard drives stampMacroExpansion
// directly at a sealed tree whose nodes carry no source location — the
// shape of a parser-synthesized Pos<0 node spliced into a macro
// expansion, which is exactly what issue #370's write landed on.  The
// sealed-skip guard must keep the stamp from writing anything.
func TestSealWatchdogStampMacroExpansionGuard(t *testing.T) {
	expr := SExpr([]*LVal{
		Symbol("guarded"),
		Int(1),
		SExpr([]*LVal{Symbol("nested"), String("payload")}),
	})
	expr.SealAST()
	if !expr.IsSealed() {
		t.Fatal("anti-vacuity: hand-built tree did not seal")
	}
	if _, ok := expr.Source(); ok {
		t.Fatal("anti-vacuity: constructors must not stamp source; the stamp below would have nothing to write")
	}

	// Anti-vacuity for the stamp itself: the same stamp against an
	// UNSEALED clone must stamp (on its copy-on-write result; issue #582),
	// proving the only thing standing between stampMacroExpansion and the
	// sealed tree is the sealed-skip guard.
	callSite := &token.Location{File: "stamp.lisp", Pos: 7, Line: 1, Col: 1}
	rt := StandardRuntime()
	unsealed := SExpr([]*LVal{Symbol("guarded"), Int(1)})
	stamped := stampMacroExpansion(unsealed, callSite, nil, rt)
	if loc, ok := stamped.Source(); !ok || loc.Pos != callSite.Pos {
		t.Fatalf("anti-vacuity: stamp did not stamp the unsealed clone (source=%v ok=%v)", loc, ok)
	}

	unregister := registerSealWatch(expr)
	defer unregister()

	// Let the watchdog read the tree, stamp it, then let the watchdog
	// read it again.  Guard present: zero writes, -race stays silent.
	// Guard reverted: stampMacroExpansion writes .source on every node,
	// and either read pass races with it deterministically.
	time.Sleep(sealWatchdogSettle)
	stampMacroExpansion(expr, callSite, nil, rt)
	time.Sleep(sealWatchdogSettle)

	seen := 0
	var assertUnstamped func(v *LVal)
	assertUnstamped = func(v *LVal) {
		seen++
		if _, ok := v.Source(); ok {
			t.Errorf("stampMacroExpansion wrote source metadata into sealed node type=%s str=%q", v.Type, v.Str)
		}
		if v.macroExpansion != nil {
			t.Errorf("stampMacroExpansion attached macroExpansionInfo to sealed node type=%s str=%q", v.Type, v.Str)
		}
		for _, c := range v.Cells {
			assertUnstamped(c)
		}
	}
	assertUnstamped(expr)
	if seen < 5 {
		t.Fatalf("anti-vacuity: walked only %d nodes", seen)
	}
}

// TestSealWatchdogPauseOrdersDeliberateWrites proves the pause mechanism:
// a test that must mutate a sealed node on purpose takes the watchdog's
// mutex, which orders its writes against every watchdog read, keeping
// -race silent for exactly that window.  The bytes are restored before
// resuming so the checked-mode inspector's end-of-suite verification
// still holds.
func TestSealWatchdogPauseOrdersDeliberateWrites(t *testing.T) {
	expr := SExpr([]*LVal{Symbol("pause-proof"), Int(42)})
	expr.SealAST()
	if !expr.IsSealed() {
		t.Fatal("anti-vacuity: tree did not seal")
	}
	unregister := registerSealWatch(expr)
	defer unregister()

	time.Sleep(sealWatchdogSettle)

	resume := pauseSealWatchdog()
	original := expr.Cells[1].Int
	expr.Cells[1].Int = original + 1 //elps:mutates -- deliberate corruption under pauseSealWatchdog; restored two lines down
	if expr.Cells[1].Int != original+1 {
		resume()
		t.Fatal("anti-vacuity: deliberate write did not land")
	}
	expr.Cells[1].Int = original //elps:mutates -- restoring the deliberately corrupted byte before resuming
	resume()

	time.Sleep(sealWatchdogSettle)
}
