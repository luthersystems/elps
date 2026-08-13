// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// Production build: the borrowed-backing provenance detector is compiled
// out entirely.  Every hook below is an empty function with no arguments
// escaping, so a release binary carries no registry, no lock, no pointer
// arithmetic and no `unsafe` — see lisp/borrow_check_elpscheck.go for
// what the checked build does instead, and lisp/borrow_check.go for the
// class the detector exists to find.

// recordConstrainedCells is a no-op in production builds.
func recordConstrainedCells(_ []*LVal) {}

// recordConstrainedBytes is a no-op in production builds.
func recordConstrainedBytes(_ []byte) {}

// noteMintOverConstrainedCells is a no-op in production builds.
func noteMintOverConstrainedCells(_ *LVal, _ []*LVal) {}

// noteMintOverConstrainedBytes is a no-op in production builds.
func noteMintOverConstrainedBytes(_ *LVal, _ []byte) {}

// borrowProvenanceFaults reports the borrowed-backing provenance faults
// observed so far.  Production builds observe nothing, so the result is
// always empty.  It is folded into VerifySealedASTs' error rather than
// exported on its own: the detector adds no public API (see
// lisp/borrow_check.go).
func borrowProvenanceFaults() []string { return nil }

// dropBorrowNotes is a no-op in production builds.
func dropBorrowNotes() {}
