// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Checked-mode half of the issue #394 red proof (default-build half:
// program_seal_gap_test.go).
//
// The ownership checker's allowlist admits exactly two kinds of value to
// cross a runtime boundary: singletons and SEALED nodes (see the Allowlist
// section of lisp/ownership_check_elpscheck.go).  On the unfixed tree a
// Program built through the format-preserving reader carried UNSEALED
// nodes, so this test's second LoadProgram was a cross-runtime sighting
// and the checker panicked with "ownership violation: LVal used by two
// Runtimes" — the detection the issue documents.  With the constructors
// establishing the seal, the same evaluation is the sanctioned sealed
// share: no panic, identical results, and the seal census that SealAST
// recorded at admission verifies clean afterwards.
func TestProgramSealGapCheckedCrossRuntime(t *testing.T) {
	p := formatPreservingProgram(t, sealGapSrc)

	for i := range 2 {
		// Each iteration is a fresh Runtime.  Pre-fix, iteration 2 panics
		// in the ownership checker before producing any value.
		got := programTestEnv(t).LoadProgram(p)
		if got.Type == lisp.LError {
			t.Fatalf("runtime %d: %v", i+1, got)
		}
		if got.String() != sealGapWant {
			t.Errorf("runtime %d = %v, want %s", i+1, got, sealGapWant)
		}
	}

	// The admission sealed (and therefore recorded) the Program's tree;
	// the census must agree nothing wrote it in place.
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("seal census after cross-runtime Program loads: %v", err)
	}
}
