// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp_test

// elpscheckActive reports whether the elpscheck build tag is on, for the
// one test in package lisp_test whose SUBJECT is the difference between
// the two builds: TestCoWCounterInactiveByDefault (cow_counter_test.go)
// asserts the copy-on-write census records nothing without the tag, which
// is only meaningful in an untagged build.
//
// An identically-named pair used to live in cow_seal_elpscheck_{on,off}_
// test.go, for tests that SKIPPED their cross-runtime half under
// elpscheck because the ownership checker forbade the pattern.  Sealed
// nodes are exempt from that checker now (#372, #379 item 2 — see the
// Allowlist in ownership_check_elpscheck.go), so those tests run their
// cross-runtime half in both builds and the old pair was deleted with the
// skips.  The census test's need is unrelated and outlived it, so the
// constant is reinstated here, named for its remaining consumer.
//
// Any NEW use is worth a second look: skipping a test under one build
// usually means the checked configuration is stepping around the thing it
// exists to check.
const elpscheckActive = false
