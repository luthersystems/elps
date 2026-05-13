// Copyright © 2025 The ELPS authors

//go:build !elpscheck

package lisp

// checkSingleton is a zero-cost no-op in production builds. In the
// elpscheck build (//go:build elpscheck) it verifies that the three
// singleton LVals are bit-identical to their init-time snapshot and
// panics if any has drifted. See singleton_check_elpscheck.go.
//
//nolint:unused // build-tag variant
func checkSingleton(_ *LVal) {}
