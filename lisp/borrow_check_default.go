// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// Production build: the borrowed-backing provenance detector is compiled
// out entirely.  Both hooks are empty and inlined, so a release binary
// carries no registry, no lock and no pointer arithmetic — see
// lisp/borrow_check_elpscheck.go for what the checked build does instead.

func recordConstrainedCells(cells []*LVal) {}

func checkMintedOverConstrained(v *LVal, cells []*LVal) {}

func recordConstrainedBytes(b []byte) {}

func checkMintedOverConstrainedBytes(v *LVal, b []byte) {}

// BorrowFaults reports the provenance faults observed so far.  Untagged
// builds record nothing, so the result is always empty; the checked build
// returns one line per fault.
func BorrowFaults() []string { return nil }
