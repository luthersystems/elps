// Copyright © 2026 The ELPS authors

package lisp

import (
	funrawhook "github.com/luthersystems/elps/internal/funraw/hook"
)

func init() {
	// Inject the captured-environment accessor for in-repo tooling (the
	// debugger's user-defined-function classification, the profiler's name
	// resolution).  The typed surface lives in internal/funraw; the untyped
	// slot in internal/funraw/hook exists only to break the import cycle
	// (funraw needs lisp's types, so lisp cannot import funraw).  This is
	// deliberately the ONLY way to reach a function value's captured
	// environment (issue #382), and internal/ visibility limits it to this
	// module.
	funrawhook.Env = func(v *LVal) *LEnv {
		if v == nil || v.Type != LFun {
			return nil
		}
		return v.funEnv()
	}
}
