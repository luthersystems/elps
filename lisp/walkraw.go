// Copyright © 2026 The ELPS authors

package lisp

import (
	walkrawhook "github.com/luthersystems/elps/internal/walkraw/hook"
)

func init() {
	// Inject the detach walker for in-repo tooling (the class-level alias
	// guard in elpstest, which must drive every value-rebuilding walker
	// rather than only the ones that happen to be exported -- issue #598).
	// The typed surface lives in internal/walkraw; the untyped slot in
	// internal/walkraw/hook exists only to break the import cycle.  detach
	// stays unexported on the module's public API, and internal/ visibility
	// limits this to the module.
	walkrawhook.Detach = func(v *LVal) (*LVal, error) {
		return v.detach()
	}
}
