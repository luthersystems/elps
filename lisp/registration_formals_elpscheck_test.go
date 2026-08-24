// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"testing"
)

// Red-proof for the shared sealed formals templates (registrationFormals,
// env.go): once registration aliases a definition table's sealed formals
// into an environment, a write reached THROUGH THE ENVIRONMENT's function
// value is a write to the process-wide template — and the checked-mode
// fingerprint verifier must catch it.  Under the pre-sharing design a write
// through an environment's private copy corrupted only that environment and
// no verifier looked; under sharing, the template IS what the environment
// holds, so the verifier's coverage extends to exactly the writes sharing
// makes dangerous.  This test proves that coverage is real, in the style of
// lisp/singleton_seal_elpscheck_test.go: mutate deliberately (under the
// paused -race seal watchdog), prove detection, restore, prove the report
// clears.

func TestSharedFormalsTemplate_TeardownVerifyCatchesMutation(t *testing.T) {
	defer pauseSealWatchdog()()

	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}

	// lisp:map — one of the functions issue #363 was reported against.
	fn := env.Get(Symbol("map"))
	if fn.Type != LFun {
		t.Fatalf("lisp:map did not resolve to a function: %v", fn)
	}
	formals := fn.Cells[0]

	// Anti-vacuity: the environment's formals must BE the sealed table
	// template — pointer identity, not a private copy.  Without this the
	// mutation below would corrupt private storage no verifier watches and
	// the red-proof would prove nothing.
	if !formals.IsSealed() {
		t.Fatal("lisp:map's formals are not sealed; the sharing design is not in effect")
	}
	var template *LVal
	for _, def := range DefaultBuiltins() {
		if def.Name() == "map" {
			template = def.Formals()
			break
		}
	}
	if template == nil {
		t.Fatal("builtin table has no `map` definition")
	}
	if formals != template {
		t.Fatalf("environment formals %p are not the table template %p; registration did not alias the sealed list", formals, template)
	}
	if len(formals.Cells) == 0 {
		t.Fatal("map's formals have no cells to mutate")
	}

	cell := formals.Cells[0]
	orig := cell.Str
	defer func() { cell.Str = orig }()

	// The #363 write shape: a formal parameter symbol edited in place,
	// reached through an environment's own function value.
	cell.Str = "corrupted-by-red-proof"

	err := VerifySealedASTs()
	if err == nil {
		t.Fatal("expected VerifySealedASTs to report the mutated shared formals template")
	}

	// Restore and prove the report clears — the suite continues clean.
	cell.Str = orig
	if err := VerifySealedASTs(); err != nil {
		t.Fatalf("VerifySealedASTs still failing after restore: %v", err)
	}
}
