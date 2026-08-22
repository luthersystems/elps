// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// TestSealWatchdogMacroStampEvalPath is the real-pipeline half of the #370
// red-proof (the direct-drive half lives in seal_watchdog_guard_test.go).  It
// evaluates a macro call whose argument is a SEALED subtree containing a node
// with a SYNTHETIC location (Pos < 0) -- the one shape stampMacroExpansion
// still writes -- with the whole tree registered with the seal write watchdog.
//
// stampMacroExpansion writes a node whose source is absent or synthetic
// (Pos < 0), so this shape reaches the stamp's write with shared sealed
// storage.  With the sealed-skip guard the stamp never writes and -race stays
// silent; with the guard reverted the stamp writes the watched node's .source
// on every evaluation, and the watchdog's unsynchronized reads report it
// deterministically.
//
// THE ARGUMENT IS HAND-BUILT, AND THAT IS A CHANGE.  This test used to parse
// `(pass-through #'list)` and rely on the reader emitting the lisp:function
// head with the native Pos == -1 location.  The reader does not do that any
// more: locateSynthesized gives every synthesized head the PREFIX TOKEN's own
// real location (#419/#426), and rdparser's
// TestParserEmitsNoSyntheticSourceLocations is the standing guard that it
// stays that way.  A probe over the old source confirms it -- sixteen nodes,
// not one of them synthetic.
//
// So the reader can no longer produce the shape, and a red-proof that waits
// for it is a red-proof that never fires.  The anti-vacuity check below is
// what caught that, which is the whole reason it is written as a hard failure
// rather than a skip.  The shape is therefore constructed directly, with
// SealAST doing to a hand-built tree exactly what the parser does to its own
// output -- which is a contract the design doc states for embedders anyway
// (docs/sealed-ast.md 2.5).  What is lost is the claim that the reader
// reaches this write path on ordinary input; what is kept, and is the point,
// is that EVALUATION does not write a sealed node when it does.
func TestSealWatchdogMacroStampEvalPath(t *testing.T) {
	// The macro comes from ordinary source; only its ARGUMENT is built by
	// hand, because that is the part the reader will no longer produce.
	macroExprs := parseCached(t, `(defmacro pass-through (x) x)`)

	// A sealed subtree carrying a synthetic location, in the shape the reader
	// used to emit for #': (lisp:function list), whose head has Pos == -1.
	head := lisp.Symbol("lisp:function")
	nativeLoc := token.NativeLocation()
	head.SetSource(&nativeLoc)
	arg := lisp.Quote(lisp.SExpr([]*lisp.LVal{head, lisp.Symbol("list")}))
	arg.SealAST()

	// The call node needs a REAL location: macroCall passes env.loc as the
	// stamp's callSite and stampGuarded returns early on a nil one, so a
	// location-less call would make this test vacuous in a second way.
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol("pass-through"), arg})
	call.SetSource(&token.Location{File: "synthetic.lisp", Path: "synthetic.lisp", Pos: 0, Line: 1, Col: 1})
	// Seal the WHOLE call, as the reader would have, and after stamping the
	// location -- SetSource is a no-op on a sealed value.  Not decoration:
	// the loop below evaluates this tree in a fresh environment per
	// iteration, so an unsealed node crossing runtimes trips the elpscheck
	// ownership checker.  A real parse hands over a tree that is sealed
	// throughout; a hand-built one has to say so.  (arg is already sealed;
	// SealAST stops at it, which is the monotone-flag contract.)
	call.SealAST()

	exprs := append(append([]*lisp.LVal{}, macroExprs...), call)

	// Anti-vacuity: the tree must contain a sealed node the stamp would
	// write -- source absent, or present and synthetic.
	synthetic := 0
	seen := make(map[*lisp.LVal]bool)
	for _, e := range exprs {
		walkAST(e, seen, func(v *lisp.LVal) {
			if loc, ok := v.Source(); v.IsSealed() && (!ok || loc.Pos < 0) {
				synthetic++
			}
		})
	}
	if synthetic == 0 {
		t.Fatal("anti-vacuity: no sealed synthetic-location node in the tree; the stamp write path is unreachable")
	}

	unregister := lisp.RegisterSealWatchForTest(exprs...)
	defer unregister()

	// A handful of fresh environments evaluating the same cached tree -- the
	// substrate cache shape.  Each evaluation macroexpands the call and runs
	// the stamp over the sealed argument.
	//
	// The checked build runs the same cross-runtime sharing, and used to
	// decline to: it evaluated repeatedly in ONE environment because the
	// ownership checker forbade one *LVal reaching two Runtimes.  The
	// sealed-node exemption (see the Allowlist section of
	// lisp/ownership_check_elpscheck.go) removed the reason, so the
	// single-environment fallback and the build-tagged constant behind it are
	// gone and the checked build now covers the topology it used to skip.
	iterations := 4
	for i := range iterations {
		env := newCowTestEnv(t)
		for j, e := range exprs {
			if r := env.Eval(e); r.Type == lisp.LError {
				t.Fatalf("iteration %d expr %d: %v", i, j, r)
			}
		}
	}

	// Value-level assertion (works without -race): the synthetic locations
	// must be exactly as built -- still absent or Pos < 0, never restamped to
	// the macro call site.
	after := 0
	seen = make(map[*lisp.LVal]bool)
	for _, e := range exprs {
		walkAST(e, seen, func(v *lisp.LVal) {
			if loc, ok := v.Source(); v.IsSealed() && (!ok || loc.Pos < 0) {
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
