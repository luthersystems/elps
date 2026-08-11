// Copyright © 2026 The ELPS authors

package lisp

import "github.com/luthersystems/elps/parser/token"

// SourceLocForTest exposes the stored source-location pointer to the
// external test package.  The public accessor Source() deliberately returns
// a value copy (issue #362), but the detach tests need the pointer identity
// itself — both to prove a detached copy shares no location storage with its
// original and to mutate the stored location when probing for aliasing.
// Test-only; compiled only into the lisp test binary.
func SourceLocForTest(v *LVal) *token.Location {
	if v == nil {
		return nil
	}
	return v.source
}
