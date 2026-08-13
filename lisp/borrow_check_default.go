// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// Production build: the borrowed-backing provenance detector is compiled
// out entirely.  Every hook below is an empty function with no arguments
// escaping, so a release binary carries no registry, no lock, no pointer
// arithmetic and no `unsafe` — see lisp/borrow_check_elpscheck.go for what
// the checked build does instead, lisp/borrow.go for the in-kernel borrow
// helpers, and docs/borrowed-backing.md for the class itself.

// recordConstrainedCells is a no-op in production builds.
func recordConstrainedCells(_ []*LVal) {}

// noteMintOverConstrainedCells is a no-op in production builds.
func noteMintOverConstrainedCells(_ *LVal, _ []*LVal) {}

// dropBorrowNotes is a no-op in production builds.
func dropBorrowNotes() {}
