// Copyright © 2025 The ELPS authors

package lisp

import (
	"fmt"
	"os"
	"testing"
)

// TestMain wraps the package test suite to detect inadvertent mutations
// of the shared singleton LVals (singletonNil, singletonTrue,
// singletonFalse). Any test that corrupts a singleton — directly or via
// a tree-walker that fails to guard against them — will fail the suite
// even if the offending test itself passed. See issue #274.
func TestMain(m *testing.M) {
	snap := TakeSingletonSnapshot()
	code := m.Run()
	if drift := snap.Verify(); drift != "" {
		fmt.Fprintf(os.Stderr,
			"FATAL: singleton %s was mutated during test run\n  Nil=%+v\n  True=%+v\n  False=%+v\n",
			drift, singletonNil, singletonTrue, singletonFalse)
		if code == 0 {
			code = 1
		}
	}
	os.Exit(code)
}
