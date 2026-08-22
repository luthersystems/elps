// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"os"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMain arms the checked-mode sealed-AST oracle over this package's test
// binary.
//
// The oracle has two re-verify points (lisp/seal_check_elpscheck.go): a
// per-load check inside (*LEnv).load, which only re-fingerprints the roots
// of the load that just finished, and VerifySealedASTs at teardown, which
// re-fingerprints EVERY sealed parse the process recorded.  The teardown
// half is the only one that catches a corruption of an OLDER parse, or one
// reached through env.Eval rather than through a Load* entry point -- and it
// has no destructor to hook, so somebody has to call it.
//
// Before this file the only callers were the lisp package's own TestMain,
// elpstest.Runner and elpstest.RunBenchmark.  This package was therefore
// running checked mode with the teardown oracle switched off, which matters
// here specifically: the path engine reaches into a sequence's live cell
// backing, luthersystems/substrate#378 was that reach landing on a shared
// program literal, and this package's tests drive it through
// env.EvalContext (path_mutate_list_test.go) rather than through a load.
//
// Measured, with the errMutateList guard removed: the whole checked run
// stayed silent except for the assertions inside the one test written for
// that bug.  With this TestMain the same run additionally reports "6 of 261
// sealed roots were mutated in place" and names the parse.  A NOVEL
// corruption here -- one no bug-specific assertion is watching for -- needs
// that second signal.
//
// Free in untagged builds: VerifySealedASTs records nothing and returns nil
// without the elpscheck tag.
func TestMain(m *testing.M) {
	code := m.Run()
	if err := lisp.VerifySealedASTs(); err != nil {
		fmt.Fprintf(os.Stderr,
			"FATAL: sealed AST verification failed at end of test run\n%v\n", err)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}
