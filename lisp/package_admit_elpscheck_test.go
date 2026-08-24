// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Checked-mode half of the issue #524 red proof (default-build half:
// package_admit_test.go).
//
// Two verifiers cover the AddPackage hazard from opposite ends, and this file
// exercises both.
//
//   - The OWNERSHIP table (lisp/ownership_check_elpscheck.go) catches the
//     hazard while it is happening.  Its allowlist admits exactly two kinds
//     of value across a Runtime boundary: singletons and sealed nodes.  A
//     package added to two registries used to put one UNSEALED list into two
//     Runtimes, so the moment the second environment bound an element of it
//     the checker panicked with "ownership violation: LVal used by two
//     Runtimes".  With the admission in place each registry holds its own
//     sealed copy: no sharing, and sealed even if there were.
//
//     The probe for that arm deliberately does NOT mutate.  A mutating probe
//     hides the violation instead of provoking it — once the first Runtime
//     has reordered the shared list, the second one reads a DIFFERENT node
//     out of it, which the first never adopted, so the corruption arrives as
//     a wrong answer and the ownership table stays quiet.  The mutating probe
//     is the default-build proof's job (package_admit_test.go); this one
//     shares the node.
//
//   - The seal CENSUS (lisp/seal_check_elpscheck.go) catches it afterwards,
//     and its coverage is a consequence of the fix rather than a given:
//     SealAST is the census's record point, so the admitted copy is
//     fingerprinted precisely BECAUSE admission seals it.  An admission that
//     did not seal would leave the registry's binding invisible to the
//     verifier — no record, nothing to compare, silence.  The mutation test
//     below proves the coverage is real, in the style of
//     lisp/registration_formals_elpscheck_test.go and
//     lisp/singleton_seal_elpscheck_test.go: mutate deliberately under the
//     paused -race seal watchdog, prove detection, restore, prove the report
//     clears.

// addPkgSharingProbe binds an element of the admitted list into a lexical
// scope — LEnv.Put, one of the checker's three sighting points — and returns
// it.  It mutates nothing, so the node the second Runtime binds is the same
// node the first one adopted.
const addPkgSharingProbe = `(use-package 'addpkg-red)
(let ([head (car limits)]) head)
`

// TestAddPackageCheckedCrossRuntime is the ownership half: one hand-built
// package, two Runtimes, no violation, matching results.
func TestAddPackageCheckedCrossRuntime(t *testing.T) {
	pkg, _ := hostilePackage(t)

	for i := range 2 {
		// Each iteration is a fresh Runtime.  Pre-fix, iteration 2 panics in
		// the ownership checker before producing any value.
		env := programTestEnv(t)
		if !env.Runtime.Registry.AddPackage(pkg) {
			t.Fatalf("runtime %d: AddPackage refused the package", i+1)
		}
		got := env.LoadString("addpkg.lisp", addPkgSharingProbe)
		if got.Type == lisp.LError {
			t.Fatalf("runtime %d: %v", i+1, got)
		}
		if got.String() != addPkgWant {
			t.Errorf("runtime %d = %v, want %s", i+1, got, addPkgWant)
		}
	}

	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("seal census after cross-runtime package admission: %v", err)
	}
}

// TestAddPackageCheckedCensusCatchesMutation is the census half: an in-place
// write to the value AddPackage admitted must be reported by the fingerprint
// verifier.  The write shape is the hazard's own — a cell of the registered
// list edited underneath the Runtime serving it — reached here through the
// registry rather than through a builtin, because what is being tested is the
// verifier's coverage of the admitted copy, not any particular mutator.
func TestAddPackageCheckedCensusCatchesMutation(t *testing.T) {
	defer lisp.PauseSealWatchdogForTest()()

	pkg, limits := hostilePackage(t)
	env := programTestEnv(t)
	if !env.Runtime.Registry.AddPackage(pkg) {
		t.Fatal("AddPackage refused the package")
	}
	admitted, ok := env.Runtime.Registry.Package(addPkgRedName).Symbol("limits")
	if !ok {
		t.Fatal("the admitted package has no `limits` binding")
	}

	// Anti-vacuity.  The mutation below is only a red proof if the value it
	// writes is (a) the registry's own copy, not the caller's node, and (b)
	// sealed — which is what put it in the census in the first place.
	if admitted == limits {
		t.Fatal("the registry admitted the caller's retained node; the admission is not in effect")
	}
	if !admitted.IsSealed() {
		t.Fatal("the admitted binding is unsealed; the census never recorded it and cannot see a write")
	}
	if len(admitted.Cells) == 0 {
		t.Fatal("the admitted binding has no cells to mutate")
	}
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("census is already dirty before the deliberate mutation: %v", err)
	}

	cell := admitted.Cells[0]
	orig := cell.Int
	defer func() { cell.Int = orig }()

	cell.Int = 999

	err := lisp.VerifySealedASTs()
	if err == nil {
		t.Fatal("expected VerifySealedASTs to report the mutated admitted package value")
	}
	if !strings.Contains(err.Error(), "mutated in place") {
		t.Fatalf("error should use the seal-violation report; got: %v", err)
	}

	// Restore and prove the report clears — the suite continues clean.
	cell.Int = orig
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("VerifySealedASTs still failing after restore: %v", err)
	}
}
