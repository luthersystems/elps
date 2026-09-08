// Copyright © 2026 The ELPS authors

//go:build elpscheck

package elpstest

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// #625: the seal inspector retains every sealed fixture after its test ends.
// A deliberate mutation must be restored, or a later benchmark/test reports
// that fixture's corruption as its own failure. Running the real control as a
// subtest exercises its cleanup before asking the independent inspector.
func TestForkOracleSealedDiamondControlRestoresRecordedState(t *testing.T) {
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("sealed state already corrupt before the control: %v", err)
	}
	t.Run("mutation-is-visible", TestForkOracleSealedDiamondsAreMemoized)
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("sealed mutation control contaminated later verification: %v", err)
	}
}
