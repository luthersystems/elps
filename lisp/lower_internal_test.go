// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// Each lowering is keyed by a special operator of package lisp, so a
// misspelt or stale key fails here and not silently in a tool.
func TestLoweringsNameSpecialOps(t *testing.T) {
	ops := map[string]bool{}
	for _, op := range DefaultSpecialOps() {
		ops[op.Name()] = true
	}
	for name := range specialOpLowerings {
		if !ops[name] {
			t.Errorf("%s has a lowering but is not a special operator", name)
		}
	}
}
