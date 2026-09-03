// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This file covers issue #579: a schema validator lost its credential when
// the environment holding it was forked.
//
// libschema recognizes a constraint by a marker cell every validator LFun
// carries (isValidator, libschema.go).  LEnv.Fork shares a native payload by
// reference but gives every forked value a FRESH *LVal header, so a
// credential compared by HEADER identity is revoked in every fork: the
// template validates, the fork raises "Value is not a schema constraint".
// The credential therefore has to key off something the fork preserves --
// the marker's payload TYPE, which no code outside libschema can name.
//
// The security property the marker exists for is unchanged and is pinned
// separately by TestForgedValidatorCellIsRejected below: the fix widens the
// credential from one pointer to one unexported, uninstantiable-from-outside
// Go type, and the "the value must be a Go builtin" half of isValidator is
// untouched.

func mustLoad(t *testing.T, env *lisp.LEnv, name, src string) {
	t.Helper()
	if res := env.LoadString(name, src); res.Type == lisp.LError {
		t.Fatalf("%s: %v", name, res)
	}
}

func mustForkEnv(t *testing.T, env *lisp.LEnv) *lisp.LEnv {
	t.Helper()
	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	return fork
}

// assertValidates evaluates src and requires that it produce nil -- the
// libschema convention for "the value satisfies the constraint".
func assertValidates(t *testing.T, env *lisp.LEnv, name, src string) {
	t.Helper()
	res := env.LoadString(name, src)
	if res.Type == lisp.LError {
		t.Fatalf("%s: %v", name, res)
	}
	if !res.IsNil() {
		t.Fatalf("%s: expected nil (valid), got %v", name, res)
	}
}

// TestForkPreservesValidatorCredential is the catch for issue #579.
//
// A validator minted on the template -- by s:deftype, and by
// s:make-validator wrapping an s: constructor -- must still be a validator in
// a fork of that template, and a fork must still be able to mint its own.
func TestForkPreservesValidatorCredential(t *testing.T) {
	env := newSchemaEnv(t)
	mustLoad(t, env, "template.lisp", `(s:deftype "T" s:int)
(set 'anon (s:make-validator "Anon" s:int (s:gt 1)))`)
	// The template itself validates: this is the non-fork behaviour, and it
	// must be unchanged by the fix.
	assertValidates(t, env, "template-validate.lisp", `(s:validate T 3)`)

	fork := mustForkEnv(t, env)
	// A validator defined on the template, reached from the fork.
	assertValidates(t, fork, "fork-validate.lisp", `(s:validate T 3)`)
	// An anonymous validator (s:make-validator, wrapping an s: constructor)
	// minted on the template, reached from the fork.
	assertValidates(t, fork, "fork-anon.lisp", `(s:validate anon 3)`)
	// A validator minted inside the fork, from the fork's own s: package.
	assertValidates(t, fork, "fork-deftype.lisp", `(s:deftype "U" s:string) (s:validate U "x")`)
	assertValidates(t, fork, "fork-anon-new.lisp", `(s:validate (s:make-validator "Fresh" s:string) "x")`)
	// A failing validation must still FAIL as a constraint failure, not as a
	// "not a constraint" credential error.  Pinning the POSITIVE text, not
	// just the absence of the credential message: a credential regression
	// also produces an LError here, so "is an error and does not say
	// 'not a schema constraint'" would be satisfied by several wrong
	// answers.  This is the message the template produces for the same
	// input, verbatim.
	res := fork.LoadString("fork-invalid.lisp", `(s:validate T "nope")`)
	if res.Type != lisp.LError {
		t.Fatalf("expected a validation error, got %v", res)
	}
	const wantConstraintFailure = "wrong-type: Input was not an integer for type T"
	if !strings.Contains(res.String(), wantConstraintFailure) {
		t.Fatalf("expected %q, got %v", wantConstraintFailure, res)
	}

	// The template keeps working after being forked.
	assertValidates(t, env, "template-after-fork.lisp", `(s:validate T 3)`)
}

// TestForkOfForkPreservesValidatorCredential checks that the credential does
// not decay along a chain of forks (each fork re-walks the values it
// inherited, so a fix that only survived one hop would fail here).
func TestForkOfForkPreservesValidatorCredential(t *testing.T) {
	env := newSchemaEnv(t)
	mustLoad(t, env, "template.lisp", `(s:deftype "T" s:int)
(set 'anon (s:make-validator "Anon" s:int))`)

	fork := mustForkEnv(t, env)
	grandchild := mustForkEnv(t, fork)
	assertValidates(t, grandchild, "grandchild.lisp", `(s:validate T 3)`)
	assertValidates(t, grandchild, "grandchild-anon.lisp", `(s:validate anon 3)`)

	// And a fork taken from the grandchild, for good measure.
	greatGrandchild := mustForkEnv(t, grandchild)
	assertValidates(t, greatGrandchild, "great-grandchild.lisp", `(s:validate T 3)`)
}

// TestForkValidatorIsolation is the reverse direction: a type defined only in
// a fork must not appear in the template, and the two environments must not
// share the binding.
func TestForkValidatorIsolation(t *testing.T) {
	env := newSchemaEnv(t)
	fork := mustForkEnv(t, env)

	mustLoad(t, fork, "fork-only.lisp", `(s:deftype "ForkOnly" s:int)`)
	assertValidates(t, fork, "fork-only-validate.lisp", `(s:validate ForkOnly 3)`)

	// The error has to be an UNBOUND SYMBOL, not merely an error.  A
	// credential regression fails this expression too -- with "Value is not
	// a schema constraint" -- so a bare Type != LError check is vacuous
	// here: it passed on the parent commit, where the symbol resolved fine
	// and only the credential was gone.  Naming the message is what makes
	// this an isolation assertion.
	if res := env.LoadString("template-sees.lisp", `(s:validate ForkOnly 3)`); res.Type != lisp.LError {
		t.Fatalf("template saw a fork-only validator: %v", res)
	} else if !strings.Contains(res.String(), "unbound symbol") {
		t.Fatalf("expected an unbound-symbol error in the template, got %v", res)
	}

	// ... and the same in the other direction.
	mustLoad(t, env, "template-only.lisp", `(s:deftype "TemplateOnly" s:int)`)
	if res := fork.LoadString("fork-sees.lisp", `(s:validate TemplateOnly 3)`); res.Type != lisp.LError {
		t.Fatalf("fork saw a template-only validator defined after the fork: %v", res)
	} else if !strings.Contains(res.String(), "unbound symbol") {
		t.Fatalf("expected an unbound-symbol error in the fork, got %v", res)
	}
}

// TestCopyOfForkedValidatorKeepsCredential checks the sibling duplication
// paths, which are correct as they stand and need no change: the lisp `copy`
// builtin (deepCopy, lisp/copy.go) returns an LFun BY REFERENCE, so a copied
// validator is the very same value, and `detach` (lisp/detach.go) REFUSES an
// LFun outright rather than handing back a credential-less duplicate -- a
// loud refusal, not a silent revocation.  Fork was the one duplication
// primitive that rebuilt the value, which is why it was the one that lost the
// credential.
//
// Only the SECOND half is a regression test.  `copy` on the template
// (copy.lisp) passed on the parent commit and is a control -- it pins that
// this fix did not disturb the working path.  `copy` in a FORK
// (fork-copy.lisp) failed there, and for the fork's reason rather than for a
// reason of copy's own: the value copy hands back by reference is the fork's
// re-headered validator, so it inherited the revoked credential.
func TestCopyOfForkedValidatorKeepsCredential(t *testing.T) {
	env := newSchemaEnv(t)
	mustLoad(t, env, "template.lisp", `(s:deftype "T" s:int)`)
	// Control: unchanged behaviour on the template, passed before the fix.
	assertValidates(t, env, "copy.lisp", `(s:validate (copy T) 3)`)

	// The regression: this is the half that failed on the parent commit.
	fork := mustForkEnv(t, env)
	assertValidates(t, fork, "fork-copy.lisp", `(s:validate (copy T) 3)`)
}

// lookalikeValidatorTag is what an outside Go caller can build: a private
// zero-size type of its own, indistinguishable in shape from libschema's
// marker tag and equally cheap to allocate.
type lookalikeValidatorTag struct{}

// TestForgedValidatorCellIsRejected pins the security property the marker
// exists for (issue #325's crash class, restated in isValidator's comment):
// the credential must not be forgeable from outside libschema.
//
// The forgery attempts here are the strongest an outside caller has: a real
// Go builtin (so the Builtin() half of the check passes) carrying a third
// cell that is a native of a lookalike zero-size type, an empty struct, and
// a plain value.  None of them is a *libschema validator tag, and none may be
// accepted -- before or after a fork.
func TestForgedValidatorCellIsRejected(t *testing.T) {
	forgeries := map[string]*lisp.LVal{
		"lookalike-tag": lisp.Native(&lookalikeValidatorTag{}),
		"empty-struct":  lisp.Native(&struct{}{}),
		"nil-native":    lisp.Native(nil),
		"string-cell":   lisp.String("marker"),
	}
	for name, marker := range forgeries {
		t.Run(name, func(t *testing.T) {
			env := newSchemaEnv(t)
			forged := lisp.FunInPackage("user", "forged", lisp.Formals("input"),
				func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })
			if forged.Type == lisp.LError {
				t.Fatalf("build forged fun: %v", forged)
			}
			// Exactly the shape markValidator produces: three cells, the
			// third being the "credential".
			forged.Cells = append(forged.Cells, marker)
			env.PutGlobal(lisp.Symbol("forged"), forged)

			res := env.LoadString("forged.lisp", `(s:validate forged 3)`)
			if res.Type != lisp.LError {
				t.Fatalf("forged constraint accepted: %v", res)
			}
			if !strings.Contains(res.String(), "not a schema constraint") {
				t.Fatalf("expected a credential rejection, got %v", res)
			}

			// The same forgery must not become valid by riding through a fork.
			fork := mustForkEnv(t, env)
			res = fork.LoadString("forged-fork.lisp", `(s:validate forged 3)`)
			if res.Type != lisp.LError {
				t.Fatalf("forged constraint accepted in fork: %v", res)
			}
			if !strings.Contains(res.String(), "not a schema constraint") {
				t.Fatalf("expected a credential rejection in fork, got %v", res)
			}
		})
	}
}
