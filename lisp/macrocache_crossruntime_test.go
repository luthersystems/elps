// Copyright © 2026 The ELPS authors

package lisp_test

// Cross-runtime tests for the process-shared macro-expansion cache
// (issue #381).  These reproduce the production parse-cache topology: one
// parse evaluated by many Runtimes.
//
// These used to skip under `-tags elpscheck` on the grounds that the
// ownership checker forbade the topology outright, and the skip was itself
// offered as the proof that a cross-runtime cache hit could not trip the
// checker.  That was an assumption stated as a result, and it is now stale:
// the checker exempts SEALED nodes (ownership_check_elpscheck.go allowlist
// entry 2 - sharing a sealed parse across runtimes is what sealing is FOR),
// so the topology is permitted and these tests RUN under the tag.
//
// A gate that has never been seen to fail proves nothing, so their passing
// under the tag is backed by an executed positive control rather than by
// assumption: macrocache_crossruntime_elpscheck_test.go shares the same
// program across two runtimes UNSEALED and requires the checker to panic.

import (
	"io"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func parseShared(t testing.TB, src string) []*lisp.LVal {
	t.Helper()
	p := parser.NewReader()
	exprs, err := p.Read("shared-parse.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return exprs
}

func evalShared(t testing.TB, env *lisp.LEnv, exprs []*lisp.LVal) {
	t.Helper()
	for _, e := range exprs {
		if v := env.Eval(e); v.Type == lisp.LError {
			t.Fatalf("eval shared parse: %v", v)
		}
	}
}

// TestMacroCacheCrossRuntimeSharedHit: a second runtime evaluating the same
// parsed program reuses the first runtime's cached expansions.
func TestMacroCacheCrossRuntimeSharedHit(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	shared := parseShared(t, `
		(set 'm (sorted-map "a" 1))
		(defun probe (k) (get-default m k 42))
	`)
	probe := parseShared(t, `(probe "miss")`)[0]

	envA := newMacroCacheTestEnv(t)
	evalShared(t, envA, shared)
	if got := envA.Eval(probe); got.Int != 42 {
		t.Fatalf("A: got %v want 42", got)
	}
	afterA := lisp.SnapshotMacroCacheStats()
	if afterA.Stores == 0 {
		t.Fatalf("A should have stored an expansion: %+v", afterA)
	}

	envB := newMacroCacheTestEnv(t)
	evalShared(t, envB, shared)
	if got := envB.Eval(probe); got.Int != 42 {
		t.Fatalf("B: got %v want 42", got)
	}
	afterB := lisp.SnapshotMacroCacheStats()
	if afterB.Hits <= afterA.Hits {
		t.Fatalf("B should have hit A's cached expansion: %+v -> %+v", afterA, afterB)
	}
	if afterB.Stores != afterA.Stores {
		t.Fatalf("B should not have re-stored the shared callsite: %+v -> %+v", afterA, afterB)
	}
}

// TestMacroCacheCrossRuntimeGensymCollision engineers the worst-case gensym
// shape for a process-shared cache: two runtimes each mint their FIRST
// gensym ("gen00000001") into cached expansions of two LEXICALLY NESTED
// callsites of the same user macro, so the reused trees bind the same
// generated name in nested scopes.  Correctness holds because admitted
// macros only ever reference gensyms they themselves bind — shadowing is
// then semantically inert — and this test is the executable form of that
// argument.
//
// Construction: `nested` contains an outer my-default whose DEFAULT branch
// holds the inner my-default callsite (lazily evaluated).  Runtime A calls
// (nested 5): only the outer callsite expands and is cached, carrying A's
// gen00000001.  Runtime B (fresh gensym counter) calls (nested ()): the
// outer callsite HITS A's cached tree, then the inner callsite misses and
// B expands it, minting B's gen00000001 into a tree evaluated INSIDE the
// scope where A's gen00000001 is bound.
func TestMacroCacheCrossRuntimeGensymCollision(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	shared := parseShared(t, `
		(defmacro my-default (x d)
		  (let* ([g (gensym)])
		    (quasiquote
		      (let* ([(unquote g) (unquote x)])
		        (if (nil? (unquote g)) (unquote d) (unquote g))))))
		(defun nested (a)
		  (my-default a (my-default () 42)))
	`)
	probeHit := parseShared(t, `(nested 5)`)[0]
	probeMiss := parseShared(t, `(nested ())`)[0]

	envA := newMacroCacheTestEnv(t)
	evalShared(t, envA, shared)
	// A expands ONLY the outer callsite (default branch not taken).
	if got := envA.Eval(probeHit); got.Int != 5 {
		t.Fatalf("A: got %v want 5", got)
	}

	envB := newMacroCacheTestEnv(t)
	evalShared(t, envB, shared)
	// B: outer hits A's tree (A's gensym), inner misses and is expanded
	// with B's first gensym — the same generated name, nested inside.
	if got := envB.Eval(probeMiss); got.Int != 42 {
		t.Fatalf("B: nested gensym shadowing broke evaluation: got %v want 42", got)
	}
	// And back on A, whose inner callsite now hits B's tree.
	if got := envA.Eval(probeMiss); got.Int != 42 {
		t.Fatalf("A: reuse of B's inner tree broke evaluation: got %v want 42", got)
	}
	if got := envA.Eval(probeHit); got.Int != 5 {
		t.Fatalf("A: outer callsite regressed: got %v want 5", got)
	}
}

// TestMacroCacheSharedConcurrent exercises the shared table under -race:
// two goroutines, each with a private Runtime, evaluating (a) the same
// shared parse — concurrent hits and competing stores on the same keys —
// and (b) private parses — concurrent inserts on distinct keys.
func TestMacroCacheSharedConcurrent(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	const src = `
		(defmacro my-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(set 'm (sorted-map "a" 1))
		(defun probe (k x)
		  (my-when (> x 0) (get-default m k (* x 2))))
	`
	shared := parseShared(t, src)
	probe := parseShared(t, `(probe "miss" 21)`)[0]

	var wg sync.WaitGroup
	errs := make(chan string, 2)
	for range 2 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			env.Runtime.Stderr = io.Discard
			if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
				errs <- rc.String()
				return
			}
			for _, e := range shared {
				if v := env.Eval(e); v.Type == lisp.LError {
					errs <- v.String()
					return
				}
			}
			for range 200 {
				if v := env.Eval(probe); v.Type == lisp.LError || v.Int != 42 {
					errs <- "probe: " + v.String()
					return
				}
				// Private parse per iteration: distinct unsealed-then-sealed
				// keys inserted concurrently.
				if v := env.LoadString("private.lisp", `(probe "miss" 21)`); v.Type == lisp.LError || v.Int != 42 {
					errs <- "private probe: " + v.String()
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for e := range errs {
		t.Fatal(e)
	}
}

// nativeMacroDef is a minimal embedder-style builtin macro definition: the
// public LBuiltinDef surface an embedder passes to LEnv.AddMacros.
type nativeMacroDef struct {
	name string
	eval func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
}

func (d nativeMacroDef) Name() string        { return d.name }
func (d nativeMacroDef) Formals() *lisp.LVal { return lisp.Formals() }
func (d nativeMacroDef) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return d.eval(env, args)
}

// TestMacroCacheNativeIdentityDistinguishesImplementations is a defeat of
// the NATIVE admission tier, found by attacking the identity rather than
// the prover.  A whitelisted native macro's cache identity was package +
// FID, and the FID is derived from the REGISTRATION NAME
// (AddMacros: "<builtin-macro “name”>").  Two runtimes that register
// different implementations under the same qualified name are therefore
// indistinguishable to the process-shared table, and a shared sealed
// callsite serves the first runtime's expansion to the second: runtime A's
// macro expands to 1, runtime B's to 2, and B evaluated to 1.
//
// Whitelisting asserts that a macro's expansion depends only on its
// argument nodes.  It cannot assert anything about a DIFFERENT macro that
// happens to share its name, which is why the identity — not the audit
// contract — has to carry this.  The identity now includes the
// implementation's function identity, so the two are distinct and B misses.
func TestMacroCacheNativeIdentityDistinguishesImplementations(t *testing.T) {
	lisp.RegisterPureNativeMacro("user:collide")
	callsite := parseShared(t, `(collide)`)[0]
	callsite.SealAST()

	var want string
	for _, m := range macroCacheModes {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			mk := func(out int) *lisp.LEnv {
				env := newMacroCacheTestEnv(t)
				env.AddMacros(true, nativeMacroDef{"collide", func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
					return lisp.Int(out)
				}})
				return env
			}
			envA, envB := mk(1), mk(2)
			// The mechanism, stated directly next to the behaviour: the two
			// environment-local definitions carry different registration
			// ids, while a macro bound from a process-global table (every
			// kernel macro, and everything RegisterDefaultMacro adds)
			// carries the SAME id in both — which is what keeps
			// TestMacroCacheCrossRuntimeSharedHit hitting across runtimes.
			idA := lisp.MacroRegistrationIDForTest(envA.GetGlobal(lisp.Symbol("collide")))
			idB := lisp.MacroRegistrationIDForTest(envB.GetGlobal(lisp.Symbol("collide")))
			if idA == idB || idA == 0 || idB == 0 {
				t.Fatalf("environment-local registrations share an id: %d, %d", idA, idB)
			}
			kernelA := lisp.MacroRegistrationIDForTest(envA.GetGlobal(lisp.Symbol("get-default")))
			kernelB := lisp.MacroRegistrationIDForTest(envB.GetGlobal(lisp.Symbol("get-default")))
			if kernelA != kernelB || kernelA == 0 {
				t.Fatalf("process-global registration lost its shared id: %d, %d", kernelA, kernelB)
			}
			got := envA.Eval(callsite).String() + "," + envB.Eval(callsite).String() +
				"," + envA.Eval(callsite).String()
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
