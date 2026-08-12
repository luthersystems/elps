// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// recordCoWOnSealed is a zero-cost no-op in production builds.
//
// Under the elpscheck build tag (//go:build elpscheck) it records every
// copy-on-write-on-sealed event — the sites where a mutating builtin
// finds a sealed program literal and silently copies instead of writing
// (lisp/seal.go) — together with the lisp-level location that caused it.
// The recorded evidence decides whether those sites can be flipped from a
// silent copy to a catchable `cannot modify a program literal` condition
// (issue #378).
//
// The body is empty and the function is inlined away, so an untagged
// binary contains no counter, no table, no site strings, and no call: see
// lisp/cow_counter_test.go, which greps `go tool nm` output to prove it.
func recordCoWOnSealed(_ *LEnv, _ string) {}

// CoWOnSealedCounts returns nil in production builds, where nothing is
// recorded.  Under `-tags elpscheck` it returns the per-site event counts
// gathered so far.  See lisp/cow_check_elpscheck.go.
func CoWOnSealedCounts() map[string]uint64 { return nil }

// CoWOnSealedReport returns the empty string in production builds.  Under
// `-tags elpscheck` it renders the recorded events — per site, with the
// lisp-level locations that reached them — for an embedder to print at
// the end of a suite run.
func CoWOnSealedReport() string { return "" }
