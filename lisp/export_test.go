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
