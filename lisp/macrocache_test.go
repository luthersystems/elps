// Copyright © 2026 The ELPS authors

package lisp_test

// Behavioral tests for the per-callsite macro-expansion cache (issue #381).
//
// The cache mode is process-global configuration, so these tests are NOT
// parallel and every test restores MacroCacheOff (and drops the shared
// table) on exit via withMacroCacheMode.

import (
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func withMacroCacheMode(t testing.TB, mode lisp.MacroCacheMode) {
	t.Helper()
	lisp.ResetMacroCache()
	lisp.SetMacroCacheMode(mode)
	t.Cleanup(func() {
		lisp.SetMacroCacheMode(lisp.MacroCacheOff)
		lisp.SetMacroCacheCap(0)
		lisp.ResetMacroCache()
	})
}

func newMacroCacheTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Stderr = io.Discard // the trace probe would spam test output
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}

func evalStr(t testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("macrocache_test.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("eval error: %v\nsource: %s", v, src)
	}
	return v
}

// macroCacheModes lists every cache mode for equivalence sweeps.
var macroCacheModes = []struct {
	name string
	mode lisp.MacroCacheMode
}{
	{"off", lisp.MacroCacheOff},
	{"runtime", lisp.MacroCacheRuntime},
	{"shared", lisp.MacroCacheShared},
}

// equivalencePrograms exercise cached macros in gensym-sensitive and
// laziness-sensitive shapes.  Each program is (setup, probe): the probe is
// evaluated twice (so cached expansions are actually REUSED at least once)
// and both renderings must match the cache-off run exactly.
var equivalencePrograms = []struct {
	name  string
	setup string
	probe string
}{
	{
		name: "kernel-get-default-hit-and-miss",
		setup: `
			(set 'm (sorted-map "a" 1))
			(defun probe (k) (get-default m k 42))
		`,
		probe: `(list (probe "a") (probe "b") (probe "a"))`,
	},
	{
		name: "kernel-get-default-laziness",
		setup: `
			(set 'counter 0)
			(set 'm (sorted-map "a" 1))
			(defun bump () (set 'counter (+ counter 1)) 99)
			(defun probe (k) (get-default m k (bump)))
		`,
		probe: `(list (probe "a") (probe "b") (probe "a") (probe "b") counter)`,
	},
	{
		name: "user-when-template",
		setup: `
			(defmacro my-when (p &rest body)
			  (quasiquote (if (unquote p)
			                  (progn (unquote-splicing body))
			                  ())))
			(defun probe (x) (my-when (> x 0) (+ x 1) (+ x 2)))
		`,
		probe: `(list (probe 1) (probe -1) (probe 5))`,
	},
	{
		name: "user-default-gensym-nested",
		setup: `
			(defmacro my-default (x d)
			  (let* ([g (gensym)])
			    (quasiquote
			      (let* ([(unquote g) (unquote x)])
			        (if (nil? (unquote g)) (unquote d) (unquote g))))))
			(defun probe (a b c) (my-default (my-default a b) c))
		`,
		probe: `(list (probe 1 2 3) (probe () 2 3) (probe () () 3) (probe () () ()))`,
	},
	{
		name: "user-while-labels-gensym",
		setup: `
			(defmacro my-when (p &rest body)
			  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
			(defmacro my-while (p &rest body)
			  (let* ([loop (gensym)])
			    (quasiquote
			      (labels ([(unquote loop) ()
			                (my-when (unquote p)
			                  (progn (unquote-splicing body))
			                  ((unquote loop)))])
			        ((unquote loop))))))
			(defun probe (n)
			  (let ([i 0] [acc 0])
			    (my-while (< i n)
			      (set! acc (+ acc i))
			      (set! i (+ i 1)))
			    acc))
		`,
		probe: `(list (probe 0) (probe 5) (probe 10))`,
	},
	{
		name: "kernel-curry-function-closure-freshness",
		setup: `
			(defun adder (n) (curry-function + n))
			(defun probe (a b) (list (funcall (adder a) 10) (funcall (adder b) 20)))
		`,
		probe: `(list (probe 1 2) (probe 3 4))`,
	},
	{
		name: "kernel-trace-eval-once",
		setup: `
			(set 'traced 0)
			(defun bump () (set 'traced (+ traced 1)) traced)
			(defun probe () (trace (bump) "probe"))
		`,
		probe: `(list (probe) (probe) traced)`,
	},
	{
		name: "recursive-macro-user",
		setup: `
			(defmacro my-when (p &rest body)
			  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
			(defun probe (n)
			  (my-when (>= n 0)
			    (if (= n 0) 0 (+ n (probe (- n 1))))))
		`,
		probe: `(list (probe 4) (probe 4))`,
	},
}

// TestMacroCacheModeEquivalence proves each program renders identical
// results under off / per-runtime / shared caching, including laziness
// counts (a cached macro must not change how many times expressions
// evaluate) and gensym-sensitive nesting.
func TestMacroCacheModeEquivalence(t *testing.T) {
	for _, prog := range equivalencePrograms {
		results := make(map[string]string)
		for _, m := range macroCacheModes {
			t.Run(prog.name+"/"+m.name, func(t *testing.T) {
				withMacroCacheMode(t, m.mode)
				env := newMacroCacheTestEnv(t)
				evalStr(t, env, prog.setup)
				// Evaluate the probe twice so cached expansions are REUSED
				// at least once; programs may be stateful, so the pair of
				// renderings (not each individually) must match across
				// modes.
				r1 := evalStr(t, env, prog.probe).String()
				r2 := evalStr(t, env, prog.probe).String()
				results[m.name] = r1 + " | " + r2
			})
		}
		if off, ok := results["off"]; ok {
			for name, got := range results {
				if got != off {
					t.Errorf("%s: mode %s diverged from cache-off: %s vs %s",
						prog.name, name, got, off)
				}
			}
		}
	}
}

// TestMacroCacheHits proves the cache actually hits: a whitelisted kernel
// macro inside a function body is expanded once and reused on later calls.
func TestMacroCacheHits(t *testing.T) {
	for _, m := range macroCacheModes[1:] {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, `
				(set 'm (sorted-map "a" 1))
				(defun probe (k) (get-default m k 42))
			`)
			for range 10 {
				evalStr(t, env, `(probe "a")`)
			}
			st := lisp.SnapshotMacroCacheStats()
			if st.Stores < 1 {
				t.Fatalf("expected at least one store, got %+v", st)
			}
			if st.Hits < 8 {
				t.Fatalf("expected >=8 hits from 10 calls, got %+v", st)
			}
		})
	}
}

// TestMacroCacheUserMacroHits proves the purity prover admits a
// substrate-style user template macro and that its callsites hit.
func TestMacroCacheUserMacroHits(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(defmacro my-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defun probe (x) (my-when (> x 0) (* x 2)))
	`)
	for range 10 {
		evalStr(t, env, `(probe 3)`)
	}
	st := lisp.SnapshotMacroCacheStats()
	if st.Hits < 8 {
		t.Fatalf("expected user-macro hits, got %+v", st)
	}
	if st.BypassImpure > 2 {
		// defmacro itself bypasses (it is not whitelisted); my-when must not.
		t.Fatalf("unexpected impure bypasses: %+v", st)
	}
}

// TestMacroCacheInvalidationOnRedefinition is the invalidation red-proof:
// redefining a macro MUST invalidate cached expansions at existing
// callsites.  Guard: macroCacheEntry identity comparison against the
// currently-resolved macro (macrocache.go).  Deleting that comparison makes
// this test fail with stale (+ ...) semantics.
func TestMacroCacheInvalidationOnRedefinition(t *testing.T) {
	for _, m := range macroCacheModes[1:] {
		t.Run("user/"+m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, `
				(defmacro op (a b) (quasiquote (+ (unquote a) (unquote b))))
				(defun probe (x y) (op x y))
			`)
			if got := evalStr(t, env, `(probe 3 4)`); got.Int != 7 {
				t.Fatalf("v1 semantics wrong: %v", got)
			}
			// Warm the cache thoroughly.
			for range 5 {
				evalStr(t, env, `(probe 3 4)`)
			}
			evalStr(t, env, `
				(defmacro op (a b) (quasiquote (* (unquote a) (unquote b))))
			`)
			if got := evalStr(t, env, `(probe 3 4)`); got.Int != 12 {
				t.Fatalf("REDEFINITION NOT HONORED: cached v1 expansion leaked through, got %v want 12", got)
			}
		})
		t.Run("builtin-shadowed/"+m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, `
				(set 'm (sorted-map "a" 1))
				(defun probe (k) (get-default m k 42))
			`)
			if got := evalStr(t, env, `(probe "b")`); got.Int != 42 {
				t.Fatalf("builtin semantics wrong: %v", got)
			}
			for range 5 {
				evalStr(t, env, `(probe "b")`)
			}
			// Shadow the kernel macro in the user package: callsites in
			// user code now resolve to the new macro and must re-expand.
			evalStr(t, env, `
				(defmacro get-default (m k d) (quasiquote (unquote d)))
			`)
			if got := evalStr(t, env, `(probe "a")`); got.Int != 42 {
				t.Fatalf("SHADOWING NOT HONORED: got %v want 42 (new macro ignores the map)", got)
			}
		})
	}
}

// TestMacroCacheGensymLeakNotCached proves the prover rejects the one
// gensym shape where caching would be observable: a gensym escaping as
// quoted data.  With any cache mode active, two calls must still yield
// DISTINCT symbols.
func TestMacroCacheGensymLeakNotCached(t *testing.T) {
	for _, m := range macroCacheModes {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, `
				(defmacro fresh-sym ()
				  (let* ([g (gensym)])
				    (quasiquote (quote (unquote g)))))
				(defun probe () (fresh-sym))
			`)
			a := evalStr(t, env, `(probe)`).String()
			b := evalStr(t, env, `(probe)`).String()
			if a == b {
				t.Fatalf("gensym-as-data was frozen by the cache: %s == %s", a, b)
			}
		})
	}
}

// TestMacroCacheUnsealedCallsiteBypass proves runtime-constructed macro
// calls (unsealed callsites) bypass the cache and stay correct.
func TestMacroCacheUnsealedCallsiteBypass(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `(set 'm (sorted-map "a" 1))`)
	// Build (get-default m "b" 7) from Go: unsealed nodes.
	expr := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("get-default"),
		lisp.Symbol("m"),
		lisp.String("b"),
		lisp.Int(7),
	})
	before := lisp.SnapshotMacroCacheStats()
	got := env.Eval(expr)
	if got.Type == lisp.LError {
		t.Fatalf("eval: %v", got)
	}
	if got.Int != 7 {
		t.Fatalf("got %v want 7", got)
	}
	after := lisp.SnapshotMacroCacheStats()
	if after.Stores != before.Stores {
		t.Fatalf("unsealed callsite was cached: %+v -> %+v", before, after)
	}
	if after.BypassUnsealed == before.BypassUnsealed {
		t.Fatalf("expected an unsealed bypass to be counted: %+v -> %+v", before, after)
	}
}

// TestMacroCacheLRUCap proves the shared cache evicts under a cap and stays
// correct while thrashing.
func TestMacroCacheLRUCap(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	lisp.SetMacroCacheCap(1)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(set 'm (sorted-map "a" 1))
		(defun probe1 (k) (get-default m k 41))
		(defun probe2 (k) (get-default m k 43))
	`)
	for range 6 {
		if got := evalStr(t, env, `(probe1 "x")`); got.Int != 41 {
			t.Fatalf("probe1: %v", got)
		}
		if got := evalStr(t, env, `(probe2 "x")`); got.Int != 43 {
			t.Fatalf("probe2: %v", got)
		}
	}
	st := lisp.SnapshotMacroCacheStats()
	if st.Evictions == 0 {
		t.Fatalf("expected evictions under cap=1: %+v", st)
	}
	if st.SharedEntries > 1 {
		t.Fatalf("cap=1 exceeded: %+v", st)
	}
}

// TestMacroCachePurityProver pins prover verdicts on representative macro
// shapes (admitted and rejected).
func TestMacroCachePurityProver(t *testing.T) {
	env := newMacroCacheTestEnv(t)
	defs := `
		(defmacro p-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defmacro p-default (x d)
		  (let* ([g (gensym)])
		    (quasiquote (let* ([(unquote g) (unquote x)])
		      (if (nil? (unquote g)) (unquote d) (unquote g))))))
		(defmacro p-memo (sym expr &optional doc)
		  (let* ([g (gensym)])
		    (if doc
		        (quasiquote (set (quote (unquote sym))
		                         (lambda () (let* ([(unquote g) (unquote expr)])
		                                      (unquote g)))
		                         (unquote doc)))
		        (quasiquote (set (quote (unquote sym))
		                         (lambda () (let* ([(unquote g) (unquote expr)])
		                                      (unquote g))))))))
		(defmacro r-free-read (x)
		  (quasiquote (if (unquote x) (unquote some-global) ())))
		(defmacro r-computed-unquote (x)
		  (quasiquote (list (unquote (car x)))))
		(defmacro r-side-effect (x)
		  (set 'leak (list x))
		  (quasiquote (quote (unquote x))))
		(defmacro r-gensym-quoted ()
		  (let* ([g (gensym)])
		    (quasiquote (quote (unquote g)))))
		(defmacro r-nested-qq (x)
		  (quasiquote (quasiquote (unquote (unquote x)))))
		(defmacro r-bare-gensym () (gensym))
		(defmacro r-quoted-unquote-head (x)
		  (quasiquote (list ('unquote (car x)) (unquote x))))
		(defmacro r-quoted-splice-head (x)
		  (quasiquote (list ('unquote-splicing (car x)) (unquote x))))
	`
	// r-free-read and r-computed-unquote reference symbols at expansion
	// time that do not exist; defining them is fine, they are only proven.
	if v := env.LoadString("prover.lisp", defs); v.Type == lisp.LError {
		t.Fatalf("defs: %v", v)
	}
	cases := map[string]bool{
		"p-when":             true,
		"p-default":          true,
		"p-memo":             true,
		"r-free-read":        false,
		"r-computed-unquote": false,
		"r-side-effect":      false,
		"r-gensym-quoted":    false,
		"r-nested-qq":        false,
		"r-bare-gensym":      false,
		// quasiquote's getUnquoteType matches the unquote operators on an
		// LSymbol head regardless of its quote flag, so ('unquote expr)
		// really does evaluate expr at expansion time.  The prover must
		// see it too or arbitrary computation passes as inert content.
		"r-quoted-unquote-head": false,
		"r-quoted-splice-head":  false,
	}
	for name, want := range cases {
		fun := env.Get(lisp.Symbol(name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s did not resolve to a function: %v", name, fun)
		}
		if got := lisp.ProveUserMacroPureForTest(fun); got != want {
			t.Errorf("prover verdict for %s: got %t want %t", name, got, want)
		}
	}
	// Kernel whitelist spot checks through the full admission path.
	for name, want := range map[string]bool{
		"get-default":    true,
		"trace":          true,
		"curry-function": true,
		"defun":          false,
		"defmacro":       false,
		"deftype":        false,
		"defconst":       false,
	} {
		fun := env.Get(lisp.Symbol(name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s did not resolve to a function: %v", name, fun)
		}
		if got := lisp.MacroCacheIdentityForTest(fun); got != want {
			t.Errorf("admission for builtin %s: got %t want %t", name, got, want)
		}
	}
}

// TestMacroCacheStatsWriter smoke-tests the instrumentation report.
func TestMacroCacheStatsWriter(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	lisp.EnableMacroExpansionStats(true)
	defer func() {
		lisp.EnableMacroExpansionStats(false)
		lisp.ResetMacroExpansionStats()
	}()
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(set 'm (sorted-map "a" 1))
		(defun probe (k) (get-default m k 42))
	`)
	for range 5 {
		evalStr(t, env, `(probe "a")`)
	}
	var sb strings.Builder
	if err := lisp.WriteMacroExpansionStats(&sb); err != nil {
		t.Fatal(err)
	}
	out := sb.String()
	if !strings.Contains(out, "get-default") {
		t.Fatalf("report missing get-default row:\n%s", out)
	}
	if !strings.Contains(out, "# macro-expansion callsite stats") {
		t.Fatalf("report missing header:\n%s", out)
	}
}

// TestMacroCacheFootprint sanity-checks the memory accounting: entries and
// nonzero byte estimates after caching, zero after reset.
func TestMacroCacheFootprint(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(set 'm (sorted-map "a" 1))
		(defun probe (k) (get-default m k 42))
	`)
	evalStr(t, env, `(probe "a")`)
	st := lisp.SnapshotMacroCacheStats()
	if st.SharedEntries == 0 || st.SharedBytes == 0 {
		t.Fatalf("expected nonzero footprint: %+v", st)
	}
	lisp.ResetMacroCache()
	st = lisp.SnapshotMacroCacheStats()
	if st.SharedEntries != 0 {
		t.Fatalf("reset did not drop entries: %+v", st)
	}
}

// TestMacroCacheRuntimeModeIsolation: with per-runtime mode, one runtime's
// cache does not serve another (each runtime re-expands once).
func TestMacroCacheRuntimeModeIsolation(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheRuntime)
	src := `
		(set 'm (sorted-map "a" 1))
		(defun probe (k) (get-default m k 42))
	`
	envA := newMacroCacheTestEnv(t)
	evalStr(t, envA, src)
	evalStr(t, envA, `(probe "a")`)
	evalStr(t, envA, `(probe "a")`)
	baseline := lisp.SnapshotMacroCacheStats()
	envB := newMacroCacheTestEnv(t)
	evalStr(t, envB, src)
	evalStr(t, envB, `(probe "a")`)
	st := lisp.SnapshotMacroCacheStats()
	if st.Stores <= baseline.Stores {
		t.Fatalf("second runtime should have stored its own entry: %+v -> %+v", baseline, st)
	}
	ea, _ := lisp.RuntimeMacroCacheFootprint(envA.Runtime)
	eb, _ := lisp.RuntimeMacroCacheFootprint(envB.Runtime)
	if ea == 0 || eb == 0 {
		t.Fatalf("both runtimes should hold private entries: A=%d B=%d", ea, eb)
	}
}

// TestMacroCacheExpansionSealed: cached expansions are sealed before reuse.
func TestMacroCacheExpansionSealed(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `
		(defmacro my-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defun probe (x) (my-when (> x 0) (* x 2)))
	`)
	evalStr(t, env, `(probe 3)`)
	st := lisp.SnapshotMacroCacheStats()
	if st.Stores == 0 {
		t.Fatalf("no expansion cached: %+v", st)
	}
	if st.Unsealable != 0 {
		t.Fatalf("expansion refused as unsealable: %+v", st)
	}
	// The published entries must actually BE sealed trees, not just
	// counted as stored: walk the shared table and verify every node.
	entries, allSealed := lisp.SharedMacroCacheAllSealedForTest()
	if entries == 0 {
		t.Fatalf("no entries in the shared table")
	}
	if !allSealed {
		t.Fatalf("shared cache published an unsealed expansion tree")
	}
}

// TestMacroCacheStableAcrossManyCallsites stresses distinct callsites to
// catch key collisions: N distinct probes, each with a distinct default.
func TestMacroCacheStableAcrossManyCallsites(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	var b strings.Builder
	b.WriteString(`(set 'm (sorted-map "a" 1))` + "\n")
	for i := range 40 {
		fmt.Fprintf(&b, "(defun probe%d (k) (get-default m k %d))\n", i, 100+i)
	}
	evalStr(t, env, b.String())
	for round := range 3 {
		for i := range 40 {
			got := evalStr(t, env, fmt.Sprintf(`(probe%d "miss")`, i))
			if got.Int != 100+i {
				t.Fatalf("round %d probe%d: got %v want %d", round, i, got, 100+i)
			}
		}
	}
	st := lisp.SnapshotMacroCacheStats()
	if st.SharedEntries < 40 {
		t.Fatalf("expected >=40 distinct entries: %+v", st)
	}
}

// TestMacroCacheTextLoaderSealed pins the TextLoader seam: code loaded
// through a lisp.Loader must evaluate as sealed AST, so a defmacro defined
// by a loader binds a sealed formals node and stays eligible for expansion
// caching (this is how substrate loads its shirocore layer — before the fix
// every macro defined there was silently disqualified in every environment).
func TestMacroCacheTextLoaderSealed(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheRuntime)
	const src = `
		(defmacro l-when (p &rest body)
		  (quasiquote (if (unquote p) (progn (unquote-splicing body)) ())))
		(defun use-l-when (x) (l-when x 'yes))
	`
	loader, err := lisp.TextLoader(parser.NewReader(), "loader_test.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("TextLoader: %v", err)
	}
	env := newMacroCacheTestEnv(t)
	if v := loader(env); v.Type == lisp.LError {
		t.Fatalf("loader: %v", v)
	}
	fun := env.Get(lisp.Symbol("l-when"))
	if fun.Type != lisp.LFun {
		t.Fatalf("l-when did not resolve to a function: %v", fun)
	}
	if !lisp.SealedForTest(fun.Cells[0]) {
		t.Fatalf("loader-defined macro has unsealed formals; loader output was not sealed")
	}
	if !lisp.MacroCacheIdentityForTest(fun) {
		t.Fatalf("loader-defined pure macro not admitted for caching")
	}
	// The loader-installed function's macro callsite lives in the sealed
	// loader AST: calling it repeatedly must populate and then hit the cache.
	before := lisp.SnapshotMacroCacheStats()
	for range 5 {
		if v := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("use-l-when"), lisp.Bool(true)})); v.Type == lisp.LError {
			t.Fatalf("call: %v", v)
		}
	}
	after := lisp.SnapshotMacroCacheStats()
	if hits := after.Hits - before.Hits; hits < 4 {
		t.Fatalf("expected >=4 cache hits through the loader-loaded callsite, got %d (stores=%d bypassImpure=%d bypassUnsealed=%d)",
			hits, after.Stores-before.Stores, after.BypassImpure-before.BypassImpure, after.BypassUnsealed-before.BypassUnsealed)
	}
}

// TestMacroCacheQuotedUnquoteHeadNotCached is the red-proof for the
// quoted-head unquote hole: quasiquote's getUnquoteType matches "unquote"
// on an LSymbol head and IGNORES that symbol's quote flag, so
// ('unquote (f)) evaluates f at every expansion.  The prover used to skip
// the unquote branch for a quoted head and scanned the form as inert
// template content, admitting an arbitrarily impure macro; caching it froze
// the first expansion's side effect and returned a different answer from
// the same program with the cache off.  Guard: the unquote recognition in
// pureMacroTemplateQ must not depend on head.quoted.
func TestMacroCacheQuotedUnquoteHeadNotCached(t *testing.T) {
	const setup = `
		(set 'ctr 0)
		(defun bump () (set 'ctr (+ ctr 1)) ctr)
		(defmacro impure (x)
		  (quasiquote (list ('unquote (bump)) (unquote x))))
		(defun probe (v) (impure v))
	`
	const program = `(list (probe 1) (probe 2) ctr)`
	var want string
	for _, m := range macroCacheModes {
		t.Run(m.name, func(t *testing.T) {
			withMacroCacheMode(t, m.mode)
			env := newMacroCacheTestEnv(t)
			evalStr(t, env, setup)
			// Run the probe repeatedly: with the cache active every
			// evaluation must still re-expand and re-run (bump).
			var got string
			for range 3 {
				got = evalStr(t, env, program).String()
			}
			if m.mode == lisp.MacroCacheOff {
				want = got
				return
			}
			if got != want {
				t.Fatalf("impure macro was cached: %s mode gave %s, cache-off gave %s", m.name, got, want)
			}
		})
	}
}

// TestMacroCacheCapAfterFill proves SetMacroCacheCap bounds a table that is
// ALREADY over capacity.  Entries published while the table was unbounded
// carry no LRU bookkeeping; without adopting them the eviction loop can only
// reach the entry it just pushed, so every new store evicts itself while the
// untracked backlog stays pinned — the cap is honoured by neither the entry
// count nor the hit rate.
func TestMacroCacheCapAfterFill(t *testing.T) {
	withMacroCacheMode(t, lisp.MacroCacheShared)
	env := newMacroCacheTestEnv(t)
	evalStr(t, env, `(defmacro op (a b) (quasiquote (+ (unquote a) (unquote b))))`)
	warm := func(lo, hi int) {
		for i := lo; i < hi; i++ {
			evalStr(t, env, fmt.Sprintf(`(defun p%d () (op %d 1))`, i, i))
			for range 3 {
				if got := evalStr(t, env, fmt.Sprintf(`(p%d)`, i)); got.Int != i+1 {
					t.Fatalf("p%d: %v", i, got)
				}
			}
		}
	}
	warm(0, 40)
	if st := lisp.SnapshotMacroCacheStats(); st.SharedEntries < 40 {
		t.Fatalf("expected an unbounded table of >=40 entries, got %+v", st)
	}
	const limit = 8
	lisp.SetMacroCacheCap(limit)
	before := lisp.SnapshotMacroCacheStats()
	warm(40, 60)
	st := lisp.SnapshotMacroCacheStats()
	if st.SharedEntries > limit {
		t.Fatalf("cap=%d not enforced on a pre-filled table: %+v", limit, st)
	}
	if st.Hits-before.Hits == 0 {
		t.Fatalf("no post-cap hits: new entries are evicting themselves: %+v -> %+v", before, st)
	}
}
