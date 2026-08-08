// Copyright © 2026 The ELPS authors

package fuzzseed

import (
	"os"
	"testing"
)

// selftestEnv arms FuzzGateSelfTest.  It is read, never written, by anything
// that ships: only scripts/ci-gates-test.sh sets it.
const selftestEnv = "ELPS_FUZZ_GATE_SELFTEST"

// FuzzGateSelfTest is a deliberately-failing fuzz target, disabled unless
// ELPS_FUZZ_GATE_SELFTEST is set.
//
// It exists because a CI gate that has only ever reported success is
// indistinguishable from a gate that cannot fail -- the failure mode that left
// this repository's benchmark gate dead for 473 runs.  scripts/ci-gates-test.sh
// arms this target and asserts that scripts/fuzz.sh exits non-zero, which is
// the only end-to-end proof that a crashing fuzz target actually reddens the
// board: discovery found it, the runner ran it, and the failure propagated
// through the per-package loop to the script's exit status.
//
// Skipped by default, so `make test`, `make race` and the real fuzz sweep are
// unaffected beyond the ~1s it takes to skip.
func FuzzGateSelfTest(f *testing.F) {
	if os.Getenv(selftestEnv) == "" {
		f.Skip("set " + selftestEnv + " to arm the fuzz-gate negative control")
	}
	f.Add([]byte("the gate must fail on this input"))
	f.Fuzz(func(t *testing.T, _ []byte) {
		t.Fatal("deliberate failure: scripts/fuzz.sh must report this as a failing target")
	})
}
