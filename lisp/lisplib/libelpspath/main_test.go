// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"os"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMain wires the checked-mode sealed-AST verifier into this package.
//
// ARM E.  The verifier already exists (lisp.VerifySealedASTs, armed by
// -tags elpscheck): it re-fingerprints every parse the checked-mode
// inspector recorded and fails the run if one drifted.  It was wired into
// exactly ONE package's TestMain — lisp's — and libelpspath, the package
// that shipped #392, had none.  That is the measurement this arm exists to
// make: a detector's reach is the set of test binaries it is installed in,
// and installing it here is one file.
func TestMain(m *testing.M) {
	code := m.Run()
	// Mint-time provenance faults: reported even when no test in this
	// package ever writes through the offending view.
	if faults := lisp.BorrowFaults(); len(faults) > 0 {
		fmt.Fprintf(os.Stderr, "FATAL: %d borrowed-backing provenance fault(s)\n%s\n",
			len(faults), faults[0])
		if code == 0 {
			code = 1
		}
	}
	if err := lisp.VerifySealedASTs(); err != nil {
		fmt.Fprintf(os.Stderr, "FATAL: sealed AST verification failed at end of test run\n%v\n", err)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}
