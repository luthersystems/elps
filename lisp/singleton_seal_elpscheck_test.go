// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"
)

// Red-proof for the permanent singleton inspector roots (issue #376): a
// deliberate write to a singleton field must be caught by the checked-mode
// inspector at BOTH of its verification points — the per-load re-check
// (verifySealedLoadRoots, which every LEnv.load funnels through) and the
// teardown re-check (VerifySealedASTs, called by TestMain and
// elpstest.Runner).  Each test mutates a singleton under the paused write
// watchdog (the mutation is the point; see singleton_watchdog_test.go),
// proves detection, and restores the bytes before returning so the rest
// of the suite — and TestMain's own drift check — stays clean.

func TestPermanentSingletonRoots_LoadVerifyCatchesMutation(t *testing.T) {
	defer pauseSingletonWatchdog()()

	orig := singletonTrue.Str
	defer func() { singletonTrue.Str = orig }()

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected verifySealedLoadRoots to panic on a mutated permanent singleton root")
		}
		msg, ok := r.(string)
		if !ok {
			t.Fatalf("expected panic to be a string, got %T: %v", r, r)
		}
		if !strings.Contains(msg, "permanent singleton root Bool(true)") {
			t.Fatalf("panic should name the permanent singleton root; got: %s", msg)
		}
		if !strings.Contains(msg, "sealed program AST mutated after parse") {
			t.Fatalf("panic should use the seal-violation report; got: %s", msg)
		}
	}()

	singletonTrue.Str = "corrupted-by-red-proof"
	// The empty load: the per-load hook must catch the corruption even
	// when the load itself evaluated nothing — the roots are permanent,
	// not tied to any recorded parse.
	verifySealedLoadRoots(nil)
}

func TestPermanentSingletonRoots_TeardownVerifyCatchesMutation(t *testing.T) {
	defer pauseSingletonWatchdog()()

	orig := singletonNil.Quoted
	defer func() { singletonNil.Quoted = orig }()

	// The #333 write shape (s.Quoted on the Nil singleton) — but storing a
	// CHANGED value so the fingerprint moves; the same-value flavor remains
	// -race/watchdog territory by design (see checkSingleton's notes).
	singletonNil.Quoted = true

	err := VerifySealedASTs()
	if err == nil {
		t.Fatal("expected VerifySealedASTs to report a mutated permanent singleton root")
	}
	if !strings.Contains(err.Error(), "permanent singleton root Nil()") {
		t.Fatalf("error should name the permanent singleton root; got: %v", err)
	}

	// Restore and prove the report clears — the suite continues clean.
	singletonNil.Quoted = orig
	if err := VerifySealedASTs(); err != nil {
		t.Fatalf("VerifySealedASTs still failing after restore: %v", err)
	}
}

// TestPermanentSingletonRoots_CleanPass proves the permanent roots verify
// silently in the steady state at both hooks.
func TestPermanentSingletonRoots_CleanPass(t *testing.T) {
	verifySealedLoadRoots(nil)
	verifyPermanentSealRoots("clean-pass test")
	if err := VerifySealedASTs(); err != nil {
		t.Fatalf("VerifySealedASTs: %v", err)
	}
}
