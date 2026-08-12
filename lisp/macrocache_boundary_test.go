// Copyright © 2026 The ELPS authors

package lisp_test

// Independent verification of the purity prover's REMAINING boundary
// (issue #381), written by attacking the narrowed prover rather than by
// re-testing the fixes.
//
// macrocache_shadow_test.go covers the operator spellings the prover records
// as obligations.  This file covers the three readings it makes WITHOUT
// recording an obligation, and pins the ground each one stands on:
//
//   - `true` / `false` are admitted as free symbol reads.  Sound only
//     because they are kernel CONSTANTS that no binding form can rebind.
//   - `unquote` / `unquote-splicing` are read as syntax.  Sound only because
//     quasiquote matches them by name and ignores any binding of them.
//   - operator obligations resolve in the macro's DEFINING environment,
//     which does not contain the macro's own formals.  A formal that shadows
//     an operator is therefore admitted on a reading that is wrong — but the
//     formal is bound to an unevaluated argument NODE, which is never
//     callable, so the expansion errors identically in every cache mode.
//
// If any of those grounds moves, the prover has a hole and these go red.

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestMacroCacheTrueFalseAreKernelConstants: the body grammar admits a bare
// `true`/`false` as a pure expression without recording any resolution
// obligation for it.  That is the ONLY free symbol read it admits, and it is
// sound only because neither name can be rebound.  Every binding route must
// refuse them; if one ever stops refusing, the prover needs an obligation
// exactly like the ones it records for `if` and friends.
func TestMacroCacheTrueFalseAreKernelConstants(t *testing.T) {
	for _, name := range []string{"true", "false"} {
		for _, form := range []string{
			`(set '` + name + ` 1)`,
			`(defun ` + name + ` () 1)`,
			`(defmacro ` + name + ` () 1)`,
			`(let ([` + name + ` 1]) ` + name + `)`,
			`(let* ([` + name + ` 1]) ` + name + `)`,
			`(labels ([` + name + ` () 1]) (` + name + `))`,
			`(flet ([` + name + ` () 1]) (` + name + `))`,
			`(in-package 'shadowpkg) (set '` + name + ` 1)`,
		} {
			env := newMacroCacheTestEnv(t)
			v := env.LoadString("constants.lisp", form)
			if v.Type != lisp.LError {
				t.Errorf("%s is rebindable via %s (got %v); the prover admits a bare "+
					"%s read with no resolution obligation and would now be unsound",
					name, form, v, name)
				continue
			}
			if msg := v.String(); !contains(msg, "cannot rebind constant") {
				t.Errorf("%s via %s: rejected for the wrong reason: %s", name, form, msg)
			}
		}
	}
}

func contains(s, sub string) bool {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

// TestMacroCacheUnquoteIsNotABinding: the prover reads `unquote` and
// `unquote-splicing` inside a template as syntax and records NO obligation
// for them, on the argument that quasiquote matches them by name on an
// LSymbol head (getUnquoteType) and never consults a binding.  This asserts
// that argument instead of assuming it: rebinding `unquote` to an impure
// function must leave quasiquote's behaviour and the counter untouched.
func TestMacroCacheUnquoteIsNotABinding(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheOff)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(set 'ctr 0)
		(defun unquote (x) (set 'ctr (+ ctr 1)) ctr)
		(defun unquote-splicing (x) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (quasiquote (unquote x)))
		(defun probe (v) (m v))
	`)
	if got := evalStr(t, env, `(list (probe 7) (probe 7) ctr)`).String(); got != `'(7 7 0)` {
		t.Fatalf("quasiquote honoured a binding of `unquote`: got %s want '(7 7 0); "+
			"the prover must then record an obligation for it", got)
	}
}

// TestMacroCacheQuotedUnquoteSplicingHeadNotCached is the second half of the
// quoted-head defeat.  TestMacroCacheQuotedUnquoteHeadNotCached covers
// the ('unquote (f)) shape; both names go through one switch arm, so the
// guard is shared — but "shared code path" is an argument, and the reviewer
// demonstrated TWO defeats, so the second one gets its own executed repro.
func TestMacroCacheQuotedUnquoteSplicingHeadNotCached(t *testing.T) {
	assertCacheModesAgree(t, "quoted-unquote-splicing-head", `
		(set 'ctr 0)
		(defun bump () (set 'ctr (+ ctr 1)) (list ctr))
		(defmacro impure (x)
		  (quasiquote (list ('unquote-splicing (bump)) (unquote x))))
		(defun probe (v) (impure v))
	`, `(list (probe 1) (probe 2) (probe 3) ctr)`)
}

// TestMacroCacheGensymShadowedByPureFunctionNotAdmitted separates the
// implemented fix from the one the review recommended.  The review asked for
// the `(gensym)` binding VALUE to be purity-checked; the implementation
// requires the head to resolve to the kernel gensym BUILTIN.  A deterministic
// user replacement for gensym is pure — a value-purity check would admit it —
// and it must still be refused, because the prover's `symGensym`
// classification (which licenses the local in template binding positions and
// rejects it under quote) is a claim about the kernel gensym specifically.
func TestMacroCacheGensymShadowedByPureFunctionNotAdmitted(t *testing.T) {
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(defun gensym () (quote fixed-sym))
		(defmacro m (x)
		  (let* ([g (gensym)])
		    (quasiquote (let* ([(unquote g) (unquote x)]) (unquote g)))))
	`)
	fun := env.Get(lisp.Symbol("m"))
	if fun.Type != lisp.LFun {
		t.Fatalf("m did not resolve to a function: %v", fun)
	}
	if lisp.MacroCacheIdentityForTest(env, fun) {
		t.Error("a pure but non-kernel `gensym` was admitted; the obligation is on the " +
			"binding's IDENTITY, not on its purity")
	}
}

// TestMacroCacheFormalShadowingAnOperatorAgrees pins the one reading that is
// demonstrably WRONG and is nonetheless harmless.
//
// Operator obligations resolve in the macro's defining environment
// (funData.env).  The body actually evaluates one frame below that, in a call
// environment carrying the macro's FORMALS — so a macro whose formal is named
// `if` or `gensym` is admitted on the strength of a binding that the body will
// never see.  It is harmless for a reason worth stating: a macro formal is
// bound to an UNEVALUATED argument node, which is never a function, so the
// body's `(if ...)` cannot do anything except fail to call it.  The failure is
// deterministic and identical in every mode, so the cache changes no answer.
//
// If macro formals ever start carrying callable values, this goes red and the
// obligations must move to the call environment.
func TestMacroCacheFormalShadowingAnOperatorAgrees(t *testing.T) {
	// These programs FAIL at expansion, so the comparison has to admit an
	// error rendering as the answer rather than fataling on it.
	agree := func(name, setup, program string) {
		t.Helper()
		var want string
		for _, m := range macroCacheModes {
			t.Run(name+"/"+m.name, func(t *testing.T) {
				withMacroCacheMode(t, m.mode)
				env := newMacroCacheTestEnv(t)
				env.LoadString("setup.lisp", setup)
				got := env.LoadString("probe.lisp", program).String()
				if m.mode == lisp.MacroCacheOff {
					want = got
					return
				}
				if got != want {
					t.Fatalf("cache changed the answer: %s gave %s, cache-off gave %s",
						m.name, got, want)
				}
			})
		}
	}

	// The formal `if` is bound to the unevaluated node `bump`, an LSymbol —
	// never callable.  Same rendering, cached or not.
	agree("formal-shadows-if", `
		(set 'ctr 0)
		(defun bump (a b c) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (if x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m bump v))
	`, `(list (probe 1) (probe 1) ctr)`)

	agree("formal-shadows-gensym", `
		(set 'ctr 0)
		(defun bump () (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (gensym x)
		  (let* ([g (gensym)])
		    (quasiquote (let* ([(unquote g) (unquote x)]) (unquote g)))))
		(defun probe (v) (m bump v))
	`, `(list (probe 1) (probe 1) ctr)`)

	// A formal named `true` is the same imprecision from the other side: the
	// prover thinks the argument node is spliced, while the constant wins at
	// expansion time.  Deterministic either way, so this one succeeds.
	assertCacheModesAgree(t, "formal-named-true", `
		(set 'ctr 0)
		(defun bump () (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (true) (quasiquote (unquote true)))
		(defun probe () (m (bump)))
	`, `(list (probe) (probe) ctr)`)
}

// TestMacroCacheCrossRuntimeShadowNotServed is the reviewer's memo-leak
// defeat carried into the SHARED TABLE rather than the purity memo: runtime A
// warms a process-wide entry from a shared parse, and runtime B evaluates the
// SAME callsite node with the operator shadowed.  B must not be served A's
// expansion — only the per-dispatch resolution can stop it, since the entry's
// identity (the shared formals node) matches exactly.
func TestMacroCacheCrossRuntimeShadowNotServed(t *testing.T) {
	t.Run("defining-env", func(t *testing.T) {
		withMacroCacheMode(t, lisp.MacroCacheShared)
		lib := parseShared(t, `
			(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
			(defun probe (v) (m v))
		`)
		call := parseShared(t, `(list (probe 1) (probe 1) (probe 1))`)[0]

		envA := newMacroCacheTestEnv(t)
		evalShared(t, envA, lib)
		if got := envA.Eval(call).String(); got != `'(1 1 1)` {
			t.Fatalf("A (kernel if): got %s want '(1 1 1)", got)
		}

		envB := newMacroCacheTestEnv(t)
		evalStr(t, envB, `(set 'ctr 0) (defun if (a b c) (set 'ctr (+ ctr 1)) ctr)`)
		evalShared(t, envB, lib)
		if got := envB.Eval(call).String(); got != `'(1 2 3)` {
			t.Fatalf("B was served A's shared-table entry despite shadowing `if`: "+
				"got %s want '(1 2 3)", got)
		}
	})

	t.Run("callsite-binder", func(t *testing.T) {
		const lib = `
			(in-package 'macrolib)
			(defmacro my-default (x d)
			  (let* ([g (gensym)])
			    (quasiquote
			      (let* ([(unquote g) (unquote x)])
			        (if (nil? (unquote g)) (unquote d) (unquote g))))))
			(export 'my-default)
			(in-package 'user)
			(use-package 'macrolib)
			(defun probe (v) (my-default v 1))
		`
		const shadow = `(defmacro let* (bindings &rest body)
			(quasiquote (quote (unquote (car (car bindings))))))`

		shared := parseShared(t, lib)
		call := parseShared(t, `(list (probe 1) (probe 1))`)[0]

		// Reference run with the cache off, same programs, same order.
		withMacroCacheMode(t, lisp.MacroCacheOff)
		envRef := newMacroCacheTestEnv(t)
		evalShared(t, envRef, shared)
		evalStr(t, envRef, shadow)
		want := envRef.Eval(call).String()

		withMacroCacheMode(t, lisp.MacroCacheShared)
		envA := newMacroCacheTestEnv(t)
		evalShared(t, envA, shared)
		if got := envA.Eval(call).String(); got != `'(1 1)` {
			t.Fatalf("A (kernel let*): got %s want '(1 1)", got)
		}
		envB := newMacroCacheTestEnv(t)
		evalShared(t, envB, shared)
		evalStr(t, envB, shadow)
		if got := envB.Eval(call).String(); got != want {
			t.Fatalf("B was served A's shared-table entry despite shadowing `let*` at "+
				"the callsite: got %s want %s", got, want)
		}
	})
}
