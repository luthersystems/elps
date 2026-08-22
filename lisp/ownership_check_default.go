// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// checkOwnership is a zero-cost no-op in production builds.  In the
// elpscheck build (//go:build elpscheck) it enforces that no LVal is used
// by two Runtimes: the first sighting at LEnv.Put, LEnv.PutGlobal, or
// env.eval adopts the value for that Runtime, and any later sighting under
// a different Runtime panics.  See lisp/ownership_check_elpscheck.go for
// the honest list of what it does and does not catch.
func checkOwnership(_ *Runtime, _ *LVal) {}

// rethrowOwnershipViolation is a no-op in production builds.  In the
// elpscheck build it re-panics ownership violations inside env.eval's
// recover() so they stay hard panics instead of becoming catchable LError
// values.  It sits on the panic path only — never the hot path.
func rethrowOwnershipViolation(_ interface{}) {}
