// Copyright © 2026 The ELPS authors

package lisp

// DropBorrowNotesForTest discards the borrowed-backing detector's pending
// notes and recorded faults.  Test-only: this file is compiled into the
// lisp test binary and nothing else, so the detector still adds no public
// API.
//
// It exists for the same reason TestCheckSingleton_DetectsCorruption
// restores the singleton it corrupts: a test whose SUBJECT is the fault —
// TestInheritSealRefusesArrays mints a vector over a sealed list's cells
// to prove InheritSeal refuses to mark it — deliberately creates the thing
// the detector is built to report, and must not poison the package's
// TestMain with it.  That is test hygiene around a deliberate construction,
// not a whitelist entry for a production call site: no site in lisp/,
// lisp/lisplib/ or parser/ needs one.
//
// Untagged builds hold no state, so this is a no-op there.
func DropBorrowNotesForTest() { dropBorrowNotes() }
