// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// recordSealedRoot is a zero-cost no-op in production builds.  In the
// elpscheck build (//go:build elpscheck) it records the canonical
// fingerprint (lisp/sealfp.go) of every sealed parse at the SealAST choke
// point, so the checked-mode inspector can later prove the sealed bytes
// never changed.  See lisp/seal_check_elpscheck.go for the design.
func recordSealedRoot(_ *LVal) {}

// verifySealedLoadRoots is a zero-cost no-op in production builds.  In
// the elpscheck build it re-fingerprints the sealed roots of each
// completed top-level load and panics on a mismatch, naming the load
// that corrupted its own parse.
func verifySealedLoadRoots(_ []*LVal) {}

// VerifySealedASTs reports what the checked build observed about sealed
// program trees: sealed parses re-verified against their parse-time
// fingerprint (the corruption half), and LVals minted over sealed backing
// that never inherited the constraint (the provenance half, elps#392).
// Production builds (no elpscheck tag) record neither, so this always
// returns nil at zero cost; under `go test -tags elpscheck` it performs
// the real checks.  See lisp/seal_check_elpscheck.go and
// lisp/borrow_check_elpscheck.go.
func VerifySealedASTs() error { return nil }
