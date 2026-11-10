// Copyright © 2025 The ELPS authors

//go:build !elpscheck

package lisp

// checkSingleton is a zero-cost no-op in production builds. In the
// elpscheck build (//go:build elpscheck) it verifies that the three
// singleton LVals are bit-identical to their init-time snapshot and
// panics if any has drifted.
//
// It compares values, so it cannot see a write that stores the value a
// field already holds — the shape of issue #333. See the "What this does
// NOT catch" section on the elpscheck implementation in
// singleton_check_elpscheck.go before relying on this guard.
func checkSingleton(_ *LVal) {}

// checkSmallInt is checkSingleton for an entry of the shared integer table.
// It is a no-op outside elpscheck builds.
func checkSmallInt(_ int) {}

// checkNotSharedInt is the elpscheck guard on the kernel's own location
// writers.  It is a no-op outside elpscheck builds.
func checkNotSharedInt(_ *LVal) {}
