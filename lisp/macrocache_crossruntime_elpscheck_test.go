// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

// Positive control for the cross-runtime macro-cache tests under
// `-tags elpscheck` (issue #381).
//
// The cross-runtime shared-cache tests (macrocache_crossruntime_test.go)
// pass under the ownership checker because sealed nodes are exempt from it
// by design.  "Passes under the tag" is only evidence if the tag can still
// fail on the same shape, so this file executes that failure: the SAME
// two-runtime program, shared UNSEALED, must panic with an ownership
// violation.  Without this, the tagged run of those tests would be
// indistinguishable from a checker that had quietly stopped watching.

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMacroCacheCrossRuntimeCheckerStillFires shares one program between two
// runtimes twice: once sealed (permitted — that is the parse-cache topology
// the cache targets) and once unsealed (a mutable value crossing runtimes,
// which must trip the checker).
func TestMacroCacheCrossRuntimeCheckerStillFires(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	const src = `
		(defmacro my-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defun probe (x) (my-when (> x 0) (* x 2)))
	`

	// Control arm: sealed sharing is exempt and must NOT panic.
	sealed := parseShared(t, src)
	sealedProbe := parseShared(t, `(probe 21)`)[0]
	if !lisp.SealedForTest(sealedProbe) {
		t.Fatalf("parser output was not sealed; the exempt arm proves nothing")
	}
	envA := newMacroCacheTestEnv(t)
	envB := newMacroCacheTestEnv(t)
	evalShared(t, envA, sealed)
	evalShared(t, envB, sealed)
	if got := envA.Eval(sealedProbe); got.Int != 42 {
		t.Fatalf("A: got %v want 42", got)
	}
	if got := envB.Eval(sealedProbe); got.Int != 42 {
		t.Fatalf("B: sealed cross-runtime sharing was rejected: %v", got)
	}

	// Test arm: the same handoff with the seal removed must panic.  Copy()
	// clears the sealed flag — that is precisely how a value stops being
	// immutable parse output and becomes runtime storage.
	unsealed := sealedProbe.Copy()
	if lisp.SealedForTest(unsealed) {
		t.Fatalf("Copy() kept the seal; this arm cannot distinguish the two cases")
	}
	envC := newMacroCacheTestEnv(t)
	envD := newMacroCacheTestEnv(t)
	evalShared(t, envC, sealed)
	evalShared(t, envD, sealed)
	if got := envC.Eval(unsealed); got.Int != 42 {
		t.Fatalf("C: got %v want 42", got)
	}
	defer func() {
		r := recover()
		if r == nil {
			t.Fatalf("ownership checker did NOT fire on unsealed cross-runtime sharing; " +
				"the tagged cross-runtime cache tests are not evidence of anything")
		}
		msg := fmt.Sprintf("%v", r)
		if !strings.Contains(msg, "ownership violation") {
			t.Fatalf("expected an ownership violation, got: %s", msg)
		}
	}()
	envD.Eval(unsealed)
}
