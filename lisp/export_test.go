// Copyright © 2026 The ELPS authors

package lisp

// Test-only bridges to the unexported detach machinery (lisp/detach.go).
// detach has no production consumers and stays unexported until a real
// embedder consumer materializes; the external test battery in package
// lisp_test keeps exercising the full contract through these functions,
// which exist only in test builds.

// Detach exposes (*LVal).detach to package lisp_test.
func Detach(v *LVal) (*LVal, error) { return v.detach() }

// ProgramDetach exposes Program.detach to package lisp_test.
func ProgramDetach(p Program) ([]*LVal, error) { return p.detach() }

// SplicedFlag exposes the unexported spliced flag to package lisp_test.
// The field has no production accessor (issue #382): splicing is evaluator
// plumbing, but the copy-on-write fingerprint tests hash it to prove sealed
// trees survive evaluation bit-identically.
func SplicedFlag(v *LVal) bool { return v.spliced }

// MapBacking exposes MapData's unexported backing field to package
// lisp_test.  The field went unexported in issue #382 (the backing is fixed
// at construction); the detach tests still nil-probe it to walk degenerate
// MapData values.
func MapBacking(md *MapData) Map { return md.mapBacking }
