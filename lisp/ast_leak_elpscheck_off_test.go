// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp_test

// elpscheckActive reports whether the elpscheck runtime ownership checker
// is compiled in.  Some leak tests intentionally exercise the substrate
// cache pattern — one parsed tree evaluated by multiple Runtimes — which
// the ownership checker conservatively forbids by design; those portions
// are skipped when it is active.
const elpscheckActive = false
