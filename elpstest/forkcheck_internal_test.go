// Copyright © 2026 The ELPS authors

package elpstest

import (
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

// Nested aliasing shows too: a map reachable directly and through a list.
func TestAliasSignatureSeesNestedAlias(t *testing.T) {
	env := mustEnv(t, `(set 'a (sorted-map "k" (vector 1))) (set 'l (list a (get a "k")))`)
	sig := aliasSignature(env)
	// The map's storage and the vector's cells each carry one number, and
	// each number appears twice: once under a, once under l.
	for _, line := range strings.Split(sig, "\n") {
		if strings.HasPrefix(line, "user:l = ") && !strings.Contains(line, "#") {
			t.Fatalf("list of aliases rendered without payload numbers: %s", line)
		}
	}
	if strings.Count(sig, "user:a = ") != 1 {
		t.Fatalf("unexpected signature:\n%s", sig)
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
