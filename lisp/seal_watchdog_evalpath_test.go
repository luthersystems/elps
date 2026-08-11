// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestSealWatchdogMacroStampEvalPath is the real-pipeline half of the
// #370 red-proof (the direct-drive half lives in
// seal_watchdog_guard_test.go).  It parses a program whose macro
// expansion returns a sealed argument containing a parser-SYNTHESIZED
// node — the lisp:function head symbol behind #', which carries the
// native Pos == -1 location — and evaluates it while the whole parse
// tree is registered with the seal write watchdog.
//
// stampMacroExpansion only writes nodes whose source is synthetic
// (Pos < 0), so this shape reaches the stamp's write with shared sealed
// storage through nothing but ordinary parsing and evaluation.  With the
// sealed-skip guard the stamp never writes and -race stays silent; with
// the guard reverted the stamp writes the watched node's .source on
// every evaluation, and the watchdog's unsynchronized reads report it
// deterministically.
func TestSealWatchdogMacroStampEvalPath(t *testing.T) {
	const src = `
(defmacro pass-through (x) x)
(pass-through #'list)
`
	exprs := parseCached(t, src)

	// Anti-vacuity: the parse must contain a sealed node with a synthetic
	// location — the node an unguarded stamp would write.
	synthetic := 0
	seen := make(map[*lisp.LVal]bool)
	for _, e := range exprs {
		walkAST(e, seen, func(v *lisp.LVal) {
			if loc, ok := v.Source(); ok && loc.Pos < 0 && v.IsSealed() {
				synthetic++
			}
		})
	}
	if synthetic == 0 {
		t.Fatal("anti-vacuity: no sealed synthetic-location node in the parse; the stamp write path is unreachable")
	}

	unregister := lisp.RegisterSealWatchForTest(exprs...)
	defer unregister()

	// A handful of fresh environments evaluating the same cached parse —
	// the substrate cache shape.  Each evaluation macroexpands
	// (pass-through #'list) and runs the stamp over the sealed argument.
	// Under elpscheck the ownership checker forbids cross-runtime AST
	// sharing by design, so the checked build evaluates repeatedly in ONE
	// environment instead; the stamp runs on every iteration either way.
	iterations := 4
	var env *lisp.LEnv
	for i := range iterations {
		if env == nil || !elpscheckActive {
			env = newCowTestEnv(t)
		}
		for j, e := range exprs {
			if r := env.Eval(e); r.Type == lisp.LError {
				t.Fatalf("iteration %d expr %d: %v", i, j, r)
			}
		}
	}

	// Value-level assertion (works without -race): the synthetic
	// locations must be exactly as parsed — still Pos < 0, never
	// restamped to the macro call site.
	after := 0
	seen = make(map[*lisp.LVal]bool)
	for _, e := range exprs {
		walkAST(e, seen, func(v *lisp.LVal) {
			if loc, ok := v.Source(); ok && loc.Pos < 0 && v.IsSealed() {
				after++
			}
		})
	}
	if after != synthetic {
		t.Fatalf("macro expansion restamped sealed synthetic locations: %d before, %d after", synthetic, after)
	}
}

// TestSealedASTFingerprintCrossParse pins the property the checked-mode
// inspector and the fuzz oracle both lean on: the canonical fingerprint
// is content-based, so two independent parses of the same source — and
// the same parse before and after being evaluated — produce the same
// digest.
func TestSealedASTFingerprintCrossParse(t *testing.T) {
	exprs1 := parseCached(t, cowProgram)
	exprs2 := parseCached(t, cowProgram)
	fp1 := lisp.SealedASTFingerprint(exprs1)
	fp2 := lisp.SealedASTFingerprint(exprs2)
	if fp1 != fp2 {
		t.Fatalf("independent parses of identical source fingerprint differently: %016x vs %016x", fp1, fp2)
	}

	env := newCowTestEnv(t)
	for i, e := range exprs1 {
		if r := env.Eval(e); r.Type == lisp.LError {
			t.Fatalf("eval expr %d: %v", i, r)
		}
	}
	if fp := lisp.SealedASTFingerprint(exprs1); fp != fp1 {
		t.Fatalf("evaluation moved the sealed fingerprint: %016x -> %016x", fp1, fp)
	}
}
