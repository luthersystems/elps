// Copyright © 2026 The ELPS authors

package elpstest

import (
	"regexp"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The oracles must be able to see what they exist to see.  A harness whose
// alias check cannot tell a de-aliased environment from its template
// would pass the very bug it was written for.

func mustEnv(t *testing.T, program string) *lisp.LEnv {
	t.Helper()
	env, err := NewForkCheckEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", program); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

// Two names over one map, and two names over two equal maps, render the
// same state and a different alias structure.
func TestAliasSignatureSeesDealiasing(t *testing.T) {
	aliased := mustEnv(t, `(set 'a (sorted-map "k" 1)) (set 'b (quasiquote (unquote a)))`)
	dealiased := mustEnv(t, `(set 'a (sorted-map "k" 1)) (set 'b (sorted-map "k" 1))`)
	if envState(aliased) != envState(dealiased) {
		t.Fatalf("premise: the two programs must render the same state\n%s", diffLines(envState(aliased), envState(dealiased)))
	}
	if aliasSignature(aliased) == aliasSignature(dealiased) {
		t.Fatalf("alias signature cannot tell two names over one map from two maps:\n%s", aliasSignature(aliased))
	}
}

// Nested aliasing shows too: a map reachable directly and through a list
// carries the same number in both places, and so does the vector inside
// it.
func TestAliasSignatureSeesNestedAlias(t *testing.T) {
	env := mustEnv(t, `(set 'a (sorted-map "k" (vector 1))) (set 'l (list a (get a "k")))`)
	sig := aliasSignature(env)
	lines := map[string]string{}
	for _, line := range strings.Split(sig, "\n") {
		if name, rest, ok := strings.Cut(line, " = "); ok {
			lines[name] = rest
		}
	}
	// user:a renders the map's number first and the vector's number
	// second (the vector's own cells follow, numbered too).
	nums := regexp.MustCompile(`#\d+`).FindAllString(lines["user:a"], -1)
	if len(nums) < 2 {
		t.Fatalf("user:a should carry a map number and a vector number: %q", lines["user:a"])
	}
	mapNum, vecNum := nums[0], nums[1]
	// user:l is a list (its own number) holding the same map and the same
	// vector, rendered by number only since both were rendered under a.
	if !strings.HasSuffix(lines["user:l"], "["+mapNum+" "+vecNum+"]") {
		t.Fatalf("user:l should hold the map and vector numbers seen under a (%s %s): %q", mapNum, vecNum, lines["user:l"])
	}
}

// A shared subtree is rendered once: a chain of lists each holding its
// predecessor twice is walked in linear time, not once per path in.
func TestAliasSignatureIsLinearOnDiamonds(t *testing.T) {
	base := mustEnv(t, `(set 'l0 (list 1))`)
	env := mustEnv(t, `
(set 'l0 (list 1))
(dotimes (i 40) (set 'l0 (list l0 l0)))
`)
	// Forty levels add forty short lines' worth of rendering, not 2^40.
	if grew := len(aliasSignature(env)) - len(aliasSignature(base)); grew > 40*40 {
		t.Fatalf("diamond chain grew the signature by %d bytes; the shared subtree is being re-walked", grew)
	}
	if ids := payloadIDs(env); len(ids) < 40 {
		t.Fatalf("payloadIDs found %d payloads, want at least 40", len(ids))
	}
}

// A closure's captured environment is part of every oracle: mutating it
// moves the state rendering, and the environment itself is a payload.
func TestOraclesSeeClosureState(t *testing.T) {
	env := mustEnv(t, `(let ([outer (vector 0)]) (defun bump! () (append! outer 1) ()))`)
	before := envState(env)
	if rc := env.LoadString("m.lisp", `(bump!)`); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if envState(env) == before {
		t.Fatal("state rendering did not move on a write through a closure")
	}
	found := false
	for _, path := range payloadIDs(env) {
		if strings.HasPrefix(path, "user:bump!/env") {
			found = true
		}
	}
	if !found {
		t.Fatal("payloadIDs did not reach the closure's captured environment")
	}
}

// An environment shares every payload with itself; a cold reload of the
// same program shares none.
func TestSharedPayloadsSeesSharing(t *testing.T) {
	env := mustEnv(t, `(set 'a (sorted-map "k" 1)) (set 'v (vector 1 2)) (set 'b (to-bytes "x"))`)
	if got := sharedPayloads(payloadIDs(env), payloadIDs(env)); len(got) == 0 {
		t.Fatal("an environment shares nothing with itself")
	}
	other := mustEnv(t, `(set 'a (sorted-map "k" 1)) (set 'v (vector 1 2)) (set 'b (to-bytes "x"))`)
	if got := sharedPayloads(payloadIDs(env), payloadIDs(other)); len(got) != 0 {
		t.Fatalf("two cold environments share payloads: %v", got)
	}
}

// A mutation moves the state rendering, so an unchanged rendering means an
// unchanged environment rather than a blind renderer.
func TestEnvStateSeesMutation(t *testing.T) {
	env := mustEnv(t, `(set 'a (sorted-map "k" 1))`)
	before := envState(env)
	if rc := env.LoadString("m.lisp", `(assoc! a "k" 2)`); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if envState(env) == before {
		t.Fatal("state rendering did not move on a sorted-map write")
	}
}
