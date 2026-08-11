// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// Two independently constructed environments must not share any MUTABLE
// *LVal pointers.  Before issue #363 was addressed, two fresh environments
// shared 620 of 1,189 reachable LVal pointers — every one of them a formals
// list (220) or a parameter-name symbol inside one (400), all of them
// writable — because nine lisplib packages and the lisp core register
// builtins out of package-level tables whose Formals() lists are built once
// at Go program init.  libjson was the control: it builds its builtin table
// inside a function, so its formals were never shared.
//
// The fix is sealing, not copying: the definition tables' formals are
// sealed at construction (sealDefaultFormals in builtins.go, the libutil
// constructors) and registration aliases the sealed list into each
// environment (registrationFormals in env.go).  A sealed value is
// immutable by contract — the copy-on-write guards (lisp/seal.go) and, in
// checked builds, the fingerprint verifier (VerifySealedASTs) police that —
// so sharing it across environments is exactly as safe as the sealed parser
// output that lisp-defined functions already alias as their formals, and it
// spares every environment a deep copy per builtin at construction time
// (the eager copy cost ~90KiB and >1000 allocations per LoadLibrary env;
// the CI benchmark gate flagged it at +9.3% B/op on libjson's $load).
//
// This test makes the whole class regression-proof: it walks the complete
// registry symbol graphs of two fully loaded environments and asserts that
// every pointer in the intersection is either a process-wide singleton or a
// SEALED value.  An unsealed shared pointer — the mutable aliasing that was
// issue #363 — still fails the test.

// walkStats records what a pointer walk saw, so assertions can prove the walk
// actually traversed the graph it claims to cover (anti-vacuity).
type walkStats struct {
	funs int // LFun values encountered
}

// maxWalkDepth bounds the recursive pointer walk.  Registry graphs are
// shallow (package -> symbol -> formals -> parameter symbols); 64 is far
// deeper than anything real while still guaranteeing termination on
// adversarial cyclic structures beyond what the seen-set already handles.
const maxWalkDepth = 64

// collectLVals walks v to bounded depth, recording every reachable *LVal in
// seen.  It descends through Cells, through LFun lexical environment scopes
// (closure bindings), and through sorted-map entries.
func collectLVals(v *lisp.LVal, depth int, seen map[*lisp.LVal]bool, stats *walkStats) {
	if v == nil || depth <= 0 || seen[v] {
		return
	}
	seen[v] = true
	switch v.Type {
	case lisp.LFun:
		stats.funs++
		for _, c := range v.Cells {
			collectLVals(c, depth-1, seen, stats)
		}
		if fenv := v.Env(); fenv != nil {
			for _, sv := range fenv.Scope {
				collectLVals(sv, depth-1, seen, stats)
			}
		}
	case lisp.LSortMap:
		md := v.Map()
		if md == nil {
			return
		}
		buf := make([]*lisp.LVal, md.Len())
		if lerr := md.Entries(buf); lerr.Type == lisp.LError {
			return
		}
		for _, pair := range buf {
			// The pair wrapper (and possibly the key) is freshly allocated by
			// Entries; walking it is harmless because fresh values can never
			// intersect across environments.
			collectLVals(pair, depth-1, seen, stats)
		}
	default:
		for _, c := range v.Cells {
			collectLVals(c, depth-1, seen, stats)
		}
	}
}

// collectRegistry walks every symbol bound in every package of env's registry.
func collectRegistry(env *lisp.LEnv) (map[*lisp.LVal]bool, *walkStats) {
	seen := make(map[*lisp.LVal]bool)
	stats := &walkStats{}
	for _, name := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(name)
		for _, sym := range pkg.SymbolNames() {
			v, _ := pkg.Symbol(sym)
			collectLVals(v, maxWalkDepth, seen, stats)
		}
	}
	return seen, stats
}

// newFullEnv builds a complete environment the way embedders do: core
// language plus the entire standard library.
func newFullEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		t.Fatalf("lisplib.LoadLibrary: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("InPackage: %v", rc)
	}
	return env
}

// describeLVal renders enough about a shared value to debug a failure.
func describeLVal(v *lisp.LVal) string {
	return fmt.Sprintf("%p type=%v str=%q cells=%d quoted=%v (%s)",
		v, v.Type, v.Str, len(v.Cells), v.Quoted, v.String())
}

// assertNoMutableSharing asserts that every pointer common to the two sets
// is either explicitly allowed (the boolean/nil singletons) or sealed.
// Sealed values are immutable by contract and safe to share; an unsealed
// shared pointer is exactly the issue-#363 aliasing this test polices.
func assertNoMutableSharing(t *testing.T, phase string, seen1, seen2, allowed map[*lisp.LVal]bool) {
	t.Helper()
	shared := 0
	sealedShared := 0
	for p := range seen1 {
		if !seen2[p] || allowed[p] {
			continue
		}
		if p.IsSealed() {
			sealedShared++
			continue
		}
		shared++
		if shared <= 20 { // don't drown the log; the count is reported below
			t.Errorf("%s: shared mutable LVal: %s", phase, describeLVal(p))
		}
	}
	if shared > 0 {
		t.Fatalf("%s: %d mutable LVal pointers shared between independently built environments (of %d and %d reachable; %d sealed pointers legitimately shared)",
			phase, shared, len(seen1), len(seen2), sealedShared)
	}
	// Anti-vacuity for the seal path: builtin formals ARE shared by design
	// now.  If nothing sealed is shared, the walk stopped reaching the
	// formals graph and the assertion above proves nothing about it.  The
	// measured baseline shares 620+ sealed pointers; 100 leaves slack for
	// library churn.
	if sealedShared < 100 {
		t.Fatalf("%s: only %d sealed pointers shared (want >= 100); the walk no longer reaches the shared formals graph and this test is vacuous", phase, sealedShared)
	}
}

// requireNonVacuous fails loudly when a walk did not actually traverse the
// graph this test exists to police.
func requireNonVacuous(t *testing.T, phase string, seen map[*lisp.LVal]bool, stats *walkStats) {
	t.Helper()
	if len(seen) == 0 {
		t.Fatalf("%s: pointer walk collected zero LVals; the walk is broken and the test is vacuous", phase)
	}
	// The measured baseline had 220 builtin formals lists across the registry.
	// Requiring at least 100 LFun values proves the walk reaches the function
	// graph where the formals-sharing bug lived, with slack for library churn.
	if stats.funs < 100 {
		t.Fatalf("%s: pointer walk saw only %d LFun values (want >= 100); the walk no longer covers builtin registrations and the test is vacuous", phase, stats.funs)
	}
}

func TestNoCrossEnvironmentLValSharing(t *testing.T) {
	env1 := newFullEnv(t)
	env2 := newFullEnv(t)

	// Allowlist: the three process-wide singletons (lisp/singleton.go).
	// LEnv.get and Package.get answer true/false lookups with shared
	// singleton values by design, and lisp code that stores the result of
	// such a lookup (e.g. (set 'x true)) legitimately puts the same pointer
	// in both registries.  Nil() likewise returns the shared singletonNil,
	// so a global binding whose value evaluates to nil (e.g. an else-less
	// if) stores the same pointer in every registry; without this entry a
	// harmless nil-valued global in lisplib would trip the assertion with a
	// baffling failure.  All three are immutable by decree and guarded by
	// checkSingleton, so sharing them is safe.
	allowed := map[*lisp.LVal]bool{
		lisp.Nil():                              true,
		env1.Get(lisp.Symbol(lisp.TrueSymbol)):  true,
		env1.Get(lisp.Symbol(lisp.FalseSymbol)): true,
		env2.Get(lisp.Symbol(lisp.TrueSymbol)):  true,
		env2.Get(lisp.Symbol(lisp.FalseSymbol)): true,
	}

	seen1, stats1 := collectRegistry(env1)
	seen2, stats2 := collectRegistry(env2)
	requireNonVacuous(t, "post-init env1", seen1, stats1)
	requireNonVacuous(t, "post-init env2", seen2, stats2)
	assertNoMutableSharing(t, "post-init", seen1, seen2, allowed)

	// Extend the property past construction: run a program that exercises
	// defun/defmacro/quoted literals (and stores a boolean singleton, which
	// must trip the allowlist rather than the assertion) and re-check.
	const program = `
	(defun shared-walk-fn (a b &optional c &rest xs) (list a b c xs))
	(defmacro shared-walk-mac (x) (quasiquote (+ 1 (unquote x))))
	(set 'shared-walk-lit '(quoted list (nested 1 2) "str" 3.5 :kw))
	(set 'shared-walk-bool true)
	`
	if rc := env1.LoadString("shared-walk-test", program); rc.Type == lisp.LError {
		t.Fatalf("env1 LoadString: %v", rc)
	}
	if rc := env2.LoadString("shared-walk-test", program); rc.Type == lisp.LError {
		t.Fatalf("env2 LoadString: %v", rc)
	}

	seen1, stats1 = collectRegistry(env1)
	seen2, stats2 = collectRegistry(env2)
	requireNonVacuous(t, "post-load env1", seen1, stats1)
	requireNonVacuous(t, "post-load env2", seen2, stats2)
	assertNoMutableSharing(t, "post-load", seen1, seen2, allowed)
}

// BenchmarkEnvInit measures full environment construction — NewEnv +
// InitializeUserEnv + lisplib.LoadLibrary — which is where builtin
// registration lands.  Registration aliases the sealed table formals
// (registrationFormals in env.go) instead of deep-copying them per
// environment, and this benchmark is the regression tripwire for that.
func BenchmarkEnvInit(b *testing.B) {
	b.ReportAllocs()
	for range b.N {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
			b.Fatalf("InitializeUserEnv: %v", rc)
		}
		if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
			b.Fatalf("LoadLibrary: %v", rc)
		}
	}
}
