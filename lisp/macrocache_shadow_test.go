// Copyright © 2026 The ELPS authors

package lisp_test

// Prover-soundness regression tests for the per-callsite macro-expansion
// cache (issue #381).
//
// Every test here is a DEFEAT: a program whose observable answer differs
// between "cache off" and "cache on" because the purity prover matched a
// structural operator by NAME and the program rebound that name.  Each one
// was executed against the pre-fix prover and produced a wrong answer, not
// merely a misclassification — the shapes below return '(1 2 3) with the
// cache off and '(1 1 1) with it on, because the cache froze the first
// expansion of a macro whose body was never pure.
//
// The assertion is always the same and is deliberately BEHAVIOURAL: run the
// identical program under off / runtime / shared and require identical
// renderings.  Nothing here inspects the prover's classification, so the
// tests keep their meaning if the admission rules change again.
//
// Guard: macroPurity.defRefs / .callRefs (macrocache_purity.go) and
// opsResolveToKernel (macrocache.go).  Deleting either resolution check
// makes these fail.

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// assertCacheModesAgree evaluates program (after setup) under every cache
// mode and requires the renderings to match the cache-off rendering.
func assertCacheModesAgree(t *testing.T, name, setup, program string) {
	t.Helper()
	var want string
	for _, m := range macroCacheModes {
		t.Run(name+"/"+m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, setup)
			got := evalStr(t, env, program).String()
			if m.mode == lisp.MacroCacheOff {
				want = got
				return
			}
			if got != want {
				t.Fatalf("cache changed the answer: %s mode gave %s, cache-off gave %s",
					m.name, got, want)
			}
		})
	}
}

// TestMacroCacheShadowedGensymNotCached: the reviewer's first defeat.  The
// old prover short-circuited a let binding whose value was spelled
// `(gensym)` — the binding value was the ONE expression admitted without
// ever being proven — so rebinding `gensym` to an impure counter made the
// macro's expansion depend on runtime state while the prover still called it
// pure.  Cache off: 1,2,3.  Cache on: 1,1,1.
func TestMacroCacheShadowedGensymNotCached(t *testing.T) {
	assertCacheModesAgree(t, "gensym", `
		(set 'ctr 0)
		(defun gensym () (set 'ctr (+ ctr 1)) ctr)
		(defmacro m () (let* ([g (gensym)]) (quasiquote (unquote g))))
		(defun probe () (m))
	`, `(list (probe) (probe) (probe))`)
}

// TestMacroCacheShadowedStructuralOpsNotCached: the reviewer's second
// defeat, one subtest per structural operator in the body grammar.  The
// operator switch matched the head symbol by name, so an impure binding of
// that name was admitted with an impure body.  `if` and `progn` are shadowed
// by a function (arguments evaluate, which is what makes the shadow
// observable); `let*` and `quasiquote` by a macro (a function shadow would
// evaluate the binding list / the template and error out before the
// interesting part).
func TestMacroCacheShadowedStructuralOpsNotCached(t *testing.T) {
	assertCacheModesAgree(t, "if", `
		(set 'ctr 0)
		(defun if (a b c) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)

	assertCacheModesAgree(t, "progn", `
		(set 'ctr 0)
		(defun progn (&rest xs) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (progn (quasiquote (unquote x))))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)

	assertCacheModesAgree(t, "let-star", `
		(set 'ctr 0)
		(defmacro let* (bindings &rest body) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (let* ([a 1]) (quasiquote (unquote x))))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)

	assertCacheModesAgree(t, "quasiquote", `
		(set 'ctr 0)
		(defmacro quasiquote (x) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (quasiquote (unquote x)))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)
}

// TestMacroCacheShadowedViaSetNotCached: the same defeat reached through
// `set` on an existing binding rather than a defining form — and, more
// importantly, applied AFTER the macro is defined.  A verdict cached against
// an environment could not see this; the obligations are re-resolved on
// every dispatch, so it does.
func TestMacroCacheShadowedViaSetNotCached(t *testing.T) {
	assertCacheModesAgree(t, "set-shadow", `
		(set 'ctr 0)
		(defun bump-if (a b c) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m v))
		(set 'if bump-if)
	`, `(list (probe 1) (probe 1) (probe 1))`)
}

// TestMacroCacheShadowedInUsedPackageNotCached: the shadow does not have to
// be in the macro's own package.  `if` is rebound in a library package and
// inherited through use-package, so the name still resolves away from the
// kernel in the macro's defining environment.
func TestMacroCacheShadowedInUsedPackageNotCached(t *testing.T) {
	assertCacheModesAgree(t, "used-package-shadow", `
		(in-package 'shadowlib)
		(set 'ctr 0)
		(defun if (a b c) (set 'ctr (+ ctr 1)) ctr)
		(export 'if)
		(in-package 'user)
		(use-package 'shadowlib)
		(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)
}

// TestMacroCacheShadowedInDefiningPackageNotCached is a defeat found by
// attacking the fix rather than by the review, and it is the sharpest of
// the set: the shadow is INVISIBLE from the caller.
//
// funCall switches the runtime into the function's own package for the
// duration of a call (lisp/env.go), so a macro defined in package A resolves
// its unqualified free symbols against A no matter who called it.  A first
// cut at the resolution check looked names up through the caller's current
// package, which meant a `if` rebound inside A — where the body actually
// reads it — resolved to the kernel binding at the callsite and the macro
// was admitted.  Cache off: 1,2,3.  Cache on: 1,1,1.
//
// The check now falls through to the macro's own package, mirroring the
// swap the evaluator performs.
func TestMacroCacheShadowedInDefiningPackageNotCached(t *testing.T) {
	assertCacheModesAgree(t, "defining-package-shadow", `
		(in-package 'shadowpkg)
		(set 'ctr 0)
		(defun if (a b c) (set 'ctr (+ ctr 1)) ctr)
		(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(export 'm)
		(in-package 'user)
		(use-package 'shadowpkg)
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)
}

// TestMacroCacheLateShadowInDefiningPackageInvalidates is the same shadow
// installed AFTER the caller has warmed the cache, from a package the caller
// is not even in.  Nothing about the macro changes — same function object,
// same formals node, so identity-based invalidation cannot see it — only the
// per-dispatch resolution can.
func TestMacroCacheLateShadowInDefiningPackageInvalidates(t *testing.T) {
	for _, m := range macroCacheModes[1:] {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, `
				(in-package 'shadowpkg)
				(set 'ctr 0)
				(defun bump (a b c) (set 'ctr (+ ctr 1)) ctr)
				(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
				(export 'm)
				(in-package 'user)
				(use-package 'shadowpkg)
				(defun probe (v) (m v))
			`)
			if got := evalStr(t, env, `(list (probe 1) (probe 1))`).String(); got != `'(1 1)` {
				t.Fatalf("warm phase: got %s want '(1 1)", got)
			}
			evalStr(t, env, `(in-package 'shadowpkg) (set 'if bump) (in-package 'user)`)
			if got := evalStr(t, env, `(list (probe 1) (probe 1) (probe 1))`).String(); got != `'(1 2 3)` {
				t.Fatalf("late shadow in the defining package was not honoured: got %s want '(1 2 3)", got)
			}
		})
	}
}

// TestMacroCacheQualifiedSpellingResolvedAsWritten: a body that writes
// `lisp:if` names its package explicitly, so the obligation must be checked
// against THAT package rather than against whatever `if` means where the
// macro lives.  Rebinding the language package's own `if` must therefore
// stop the macro being cached — and conversely, an unqualified shadow
// elsewhere must not stop a `lisp:`-qualified body from being cached.
func TestMacroCacheQualifiedSpellingResolvedAsWritten(t *testing.T) {
	assertCacheModesAgree(t, "qualified-shadowed", `
		(in-package 'lisp)
		(set 'ctr 0)
		(defun bump (a b c) (set 'ctr (+ ctr 1)) ctr)
		(in-package 'user)
		(defmacro m (x) (lisp:if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m v))
		(in-package 'lisp)
		(set 'if bump)
		(in-package 'user)
	`, `(list (probe 1) (probe 1) (probe 1))`)

	// The positive half: a qualified body stays cacheable when an unrelated
	// unqualified `if` is rebound in the caller's package.
	withMacroCacheMode(t, lisp.MacroCacheRuntime)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(defun bump (a b c) 0)
		(defmacro m (x) (lisp:if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defun probe (v) (m v))
		(set 'if bump)
	`)
	before := lisp.SnapshotMacroCacheStats()
	for range 5 {
		evalStr(t, env, `(probe 1)`)
	}
	if hits := lisp.SnapshotMacroCacheStats().Hits - before.Hits; hits < 3 {
		t.Fatalf("a lisp:-qualified body should stay cacheable under an unqualified shadow: %d hits", hits)
	}
}

// TestMacroCacheLexicalShadowNotCached: the shadow does not need to be a
// package binding at all.  A defmacro evaluated inside a let that rebinds a
// structural operator captures that let as its defining environment.
func TestMacroCacheLexicalShadowNotCached(t *testing.T) {
	assertCacheModesAgree(t, "lexical-shadow", `
		(set 'ctr 0)
		(defun bump (a b c) (set 'ctr (+ ctr 1)) ctr)
		(let ([if bump])
		  (defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x)))))
		(defun probe (v) (m v))
	`, `(list (probe 1) (probe 1) (probe 1))`)
}

// TestMacroCacheNestedImpureMacroStillReExpands covers an interaction rather
// than a shadow.  Caching an admitted macro makes its expansion tree — and
// therefore every macro callsite INSIDE it — a stable node, where
// re-expansion mints a fresh subtree each time.  An impure inner macro must
// still re-expand at that now-stable callsite rather than being frozen by
// the outer macro's cache entry.
func TestMacroCacheNestedImpureMacroStillReExpands(t *testing.T) {
	assertCacheModesAgree(t, "nested-impure", `
		(set 'ctr 0)
		(defmacro impure (x)
		  (set 'ctr (+ ctr 1))
		  (quasiquote (list (unquote ctr) (unquote x))))
		(defmacro outer (x) (quasiquote (impure (unquote x))))
		(defun probe (v) (outer v))
	`, `(list (probe 1) (probe 2) ctr)`)
}

// TestMacroCacheShadowedBinderAtCallsiteNotCached is the defeat that lives
// in the CALLING environment rather than the defining one, and it was found
// by attacking the fix rather than by the review.
//
// The prover discharges one syntactic quote level on a let/let*/labels/flet/
// lambda binding list inside a template, because ELPS writes binding lists
// with brackets and the kernel binder consumes them as syntax.  That reading
// is what admits a gensym written into a binding position.  It is a claim
// about the OUTPUT code, which evaluates at the callsite — so a caller that
// rebinds `let*` to something that treats its binding list as data makes the
// gensym observable, and the cache then freezes it: two calls that must
// yield distinct generated symbols yield the same one.
//
// The macro is defined in a clean package and called from the shadowed one,
// so the defining-environment obligations are all satisfied; only the
// callsite obligation can catch this.
func TestMacroCacheShadowedBinderAtCallsiteNotCached(t *testing.T) {
	assertCacheModesAgree(t, "callsite-binder-shadow", `
		(in-package 'macrolib)
		(defmacro my-default (x d)
		  (let* ([g (gensym)])
		    (quasiquote
		      (let* ([(unquote g) (unquote x)])
		        (if (nil? (unquote g)) (unquote d) (unquote g))))))
		(export 'my-default)
		(in-package 'user)
		(use-package 'macrolib)
		(defmacro let* (bindings &rest body)
		  (quasiquote (quote (unquote (car (car bindings))))))
		(defun probe (v) (my-default v 1))
	`, `(list (probe 1) (probe 1))`)
}

// TestMacroCachePurityMemoIsEnvironmentIndependent is the reviewer's fourth
// defeat: the purity memo is keyed on the formals node, but the by-name
// operator checks were environment dependent, so a verdict computed in an
// unshadowed environment licensed caching in a shadowed one.
//
// Construction: ONE sealed parse (substrate's parse-cache topology),
// evaluated by two runtimes.  Runtime A has the kernel `if` and warms the
// memo.  Runtime B rebinds `if` to an impure counter before evaluating the
// same parse — the same formals node, hence the same memo key.  B must
// still see 1,2,3.
func TestMacroCachePurityMemoIsEnvironmentIndependent(t *testing.T) {
	for _, m := range macroCacheModes[1:] {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			p := parser.NewReader()
			read := func(name, src string) []*lisp.LVal {
				t.Helper()
				exprs, err := p.Read(name, strings.NewReader(src))
				if err != nil {
					t.Fatalf("parse %s: %v", name, err)
				}
				return exprs
			}
			shared := read("shared.lisp", `
				(defmacro m (x) (if () (quasiquote (unquote x)) (quasiquote (unquote x))))
				(defun probe (v) (m v))
			`)
			probe := read("probe.lisp", `(list (probe 1) (probe 1) (probe 1))`)[0]
			evalAll := func(env *lisp.LEnv, exprs []*lisp.LVal) {
				t.Helper()
				for _, e := range exprs {
					if v := env.Eval(e); v.Type == lisp.LError {
						t.Fatalf("eval: %v", v)
					}
				}
			}

			// A: kernel `if` — proves the macro pure and memoizes it.
			envA := newMacroCacheTestEnv(t)
			evalAll(envA, shared)
			if v := envA.Eval(probe); v.Type == lisp.LError {
				t.Fatalf("A probe: %v", v)
			}

			// B: same parse, same formals node, shadowed `if`.
			envB := newMacroCacheTestEnv(t)
			evalStr(t, envB, `
				(set 'ctr 0)
				(defun if (a b c) (set 'ctr (+ ctr 1)) ctr)
			`)
			evalAll(envB, shared)
			if got := envB.Eval(probe).String(); got != `'(1 2 3)` {
				t.Fatalf("memo leaked an unshadowed verdict into a shadowed environment: got %s want '(1 2 3)", got)
			}
		})
	}
}

// TestMacroCacheAliasedOperatorNotAdmitted covers the defeats that were
// tried and did NOT work, so a future change that starts admitting them is
// visible: renaming a structural operator hides it from the prover (the
// grammar rejects the unknown head), and reaching an impure operation
// through apply/funcall is likewise not in the grammar.  These macros must
// simply be inadmissible.
func TestMacroCacheAliasedOperatorNotAdmitted(t *testing.T) {
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(set 'my-if if)
		(defmacro aliased (x) (my-if () (quasiquote (unquote x)) (quasiquote (unquote x))))
		(defmacro applied (x) (apply (quote list) (list (quasiquote (unquote x)))))
		(defmacro funcalled (x) (funcall progn (quasiquote (unquote x))))
	`)
	for _, name := range []string{"aliased", "applied", "funcalled"} {
		fun := env.Get(lisp.Symbol(name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s did not resolve to a function: %v", name, fun)
		}
		if lisp.MacroCacheIdentityForTest(env, fun) {
			t.Errorf("%s was admitted for caching; the grammar must reject it", name)
		}
	}
}

// TestMacroCacheKernelObligationsRecorded pins WHICH obligations the prover
// records for the two representative admitted shapes, so a refactor that
// stops recording one is caught by more than the behavioural tests.
func TestMacroCacheKernelObligationsRecorded(t *testing.T) {
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(defmacro p-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defmacro p-default (x d)
		  (let* ([g (gensym)])
		    (quasiquote (let* ([(unquote g) (unquote x)])
		      (if (nil? (unquote g)) (unquote d) (unquote g))))))
	`)
	for _, tc := range []struct {
		name string
		def  []string
		call []string
	}{
		// The template's `if`/`progn` are output code, not interpreted by
		// the prover: no obligation.  Only the body's quasiquote is.
		{"p-when", []string{"quasiquote"}, nil},
		// let* appears twice with different meanings: in the body (defining
		// env) and as the template's binder syntax (calling env).
		{"p-default", []string{"gensym", "quasiquote", "let*"}, []string{"let*"}},
	} {
		fun := env.Get(lisp.Symbol(tc.name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s did not resolve to a function: %v", tc.name, fun)
		}
		def, call := lisp.MacroCacheKernelRefsForTest(fun)
		if !sameStrings(def, tc.def) {
			t.Errorf("%s defining-env obligations: got %v want %v", tc.name, def, tc.def)
		}
		if !sameStrings(call, tc.call) {
			t.Errorf("%s calling-env obligations: got %v want %v", tc.name, call, tc.call)
		}
	}
}

func sameStrings(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	seen := make(map[string]int, len(got))
	for _, s := range got {
		seen[s]++
	}
	for _, s := range want {
		seen[s]--
	}
	for _, n := range seen {
		if n != 0 {
			return false
		}
	}
	return true
}

// TestMacroCacheFIDFormatsPinned pins the three FID spellings the admission
// path builds by hand against what the registration code actually assigns.
// A drift here would not corrupt anything — every macro would simply stop
// being cacheable — but it would silently delete the feature, so it is
// worth an assertion.
func TestMacroCacheFIDFormatsPinned(t *testing.T) {
	env := newMacroCacheTestEnv(t)
	for _, tc := range []struct{ name, kind string }{
		{"get-default", "macro"},
		{"if", "special-op"},
		{"let*", "special-op"},
		{"quasiquote", "special-op"},
		{"gensym", "builtin"},
	} {
		fun := env.Get(lisp.Symbol(tc.name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s did not resolve to a function: %v", tc.name, fun)
		}
		want := lisp.MacroCacheFIDForTest(tc.kind, tc.name)
		if got := lisp.FunFIDForTest(fun); got != want {
			t.Errorf("%s FID: registration says %q, the cache builds %q", tc.name, got, want)
		}
		if pkg := lisp.FunPackageForTest(fun); pkg != lisp.DefaultLangPackage {
			t.Errorf("%s package: got %q want %q", tc.name, pkg, lisp.DefaultLangPackage)
		}
	}
}

// TestMacroCacheSharedFormalsTwoBodiesNotCached: a defeat of the memo KEY
// rather than of an operator name.  The purity verdict is memoized on the
// macro's sealed formals node, which assumes a formals node belongs to one
// defmacro form.  A macro-generating macro breaks that: splicing the SAME
// argument node into two defmacro forms gives both macros that one node,
// and the verdict proven from the first body licensed the second.  Here `p`
// (body `7`) is provably pure and `q` (body `(bump)`) is not pure by any
// reading — the only thing that could admit it is p's verdict.
//
// Cache off: (7 1) (7 2) (7 3).  Before the fix, cached: (7 1) (7 1) (7 1).
func TestMacroCacheSharedFormalsTwoBodiesNotCached(t *testing.T) {
	const setup = `
		(set 'ctr 0)
		(defun bump () (set 'ctr (+ ctr 1)) ctr)
		(defmacro two (fs)
		  (quasiquote (progn
		    (defmacro p (unquote fs) 7)
		    (defmacro q (unquote fs) (bump)))))
		(two (a))
		(defun probe () (list (p 0) (q 0)))
	`
	// The premise, asserted rather than assumed: p and q really do share
	// one sealed formals node.  Without this the test could pass for the
	// boring reason.
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, setup)
	fp := lisp.MacroFormalsForTest(env.GetGlobal(lisp.Symbol("p")))
	fq := lisp.MacroFormalsForTest(env.GetGlobal(lisp.Symbol("q")))
	if fp == nil || fp != fq {
		t.Fatalf("premise lost: p and q no longer share a formals node (%p vs %p)", fp, fq)
	}
	if !lisp.SealedForTest(fp) {
		t.Fatalf("premise lost: the shared formals node is not sealed")
	}

	assertCacheModesAgree(t, "shared-formals-two-bodies", setup,
		`(list (probe) (probe) (probe))`)
}

// TestMacroCacheSameFormalsDifferentDefiningEnv is the same key under the
// other kind of pressure: ONE source form (hence one formals node and one
// body) evaluated twice, in defining environments that resolve `if`
// differently.  The syntactic verdict is legitimately shared here — it is a
// function of the nodes — and what must not be shared is the resolution,
// which is why the obligations are re-checked per dispatch.  Attempted as a
// defeat, held: the second instance's `if` is a counter, and both cached
// modes report the counter advancing exactly as cache-off does.
func TestMacroCacheSameFormalsDifferentDefiningEnv(t *testing.T) {
	assertCacheModesAgree(t, "same-formals-two-defining-envs", `
		(set 'ctr 0)
		(defun impure-if (c a b) (set 'ctr (+ ctr 1)) ctr)
		(defun install (f)
		  (let ([if f])
		    (defmacro m () (if true (quasiquote 1) (quasiquote 2)))))
		(defun probe () (m))
	`, `
		(progn
		  (install lisp:if)
		  (let ([a (list (probe) (probe))])
		    (install impure-if)
		    (list a (probe) (probe))))`)
}
