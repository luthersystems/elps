// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
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
// TWO MECHANISMS ANSWER IT, and this test polices the composition.
//
// COPYING (issue #513) is what removes the sharing: formalsCopier
// (lisp/defformals.go) gives every registration its own formals list, carved
// out of one block per Add* call, so no two environments hold one.  The
// intersection this test measures is therefore EMPTY of formals now, not
// merely free of writable ones.
//
// SEALING is what protects the templates the copies are made FROM.  The
// definition tables' own lists are sealed at construction (sealDefaultFormals
// in builtins.go, the libutil constructors), which puts the process-wide
// originals under the copy-on-write guards (lisp/seal.go) and, in checked
// builds, under the fingerprint verifier (VerifySealedASTs).  Nothing in this
// test reaches those templates -- they are not in any environment's registry;
// builtin_formals_test.go's end-of-run snapshot is what watches them.
//
// THE ASSERTION IS STILL "NO SHARED MUTABLE POINTER", unchanged, and it is
// still the thing worth asserting: it holds whichever mechanism is doing the
// work, so it does not have to be rewritten when the balance between them
// moves.  Sealed sharing is TOLERATED rather than required -- a sealed value
// is immutable by contract, so sharing one is safe -- which matters because
// sharing sealed formals instead of copying them is a live optimization
// (issues #379, #514) that this test must not stand in the way of.
//
// What that costs is the anti-vacuity floor this test used to carry on the
// sealed-sharing COUNT; see assertNoMutableSharing.

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
		if fenv := funraw.Env(v); fenv != nil {
			for _, sv := range fenv.Bindings() {
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
		v, v.Type, v.Str, len(v.Cells), v.IsQuoted(), v.String())
}

// countSharing partitions the pointers common to the two sets (minus the
// allowlist) into mutable and sealed shares.  Factored out of
// assertNoMutableSharing so TestCensusDetectsUnsealedFormalsSharing can run
// the same census expecting a nonzero mutable count — the red-proof that
// this walk actually fails when the sharing it polices occurs.
func countSharing(seen1, seen2, allowed map[*lisp.LVal]bool) (mutableShared []*lisp.LVal, sealedShared int) {
	for p := range seen1 {
		if !seen2[p] || allowed[p] {
			continue
		}
		if p.IsSealed() {
			sealedShared++
			continue
		}
		mutableShared = append(mutableShared, p)
	}
	return mutableShared, sealedShared
}

// assertNoMutableSharing asserts that every pointer common to the two sets
// is either explicitly allowed (the boolean/nil singletons) or sealed.
// Sealed values are immutable by contract and safe to share — since the
// sharing work (issues #379, #514) the sealed count includes every builtin
// formals template aliased by registrationFormals (lisp/env.go); an
// unsealed shared pointer is exactly the issue-#363 aliasing this test
// polices.
func assertNoMutableSharing(t *testing.T, phase string, seen1, seen2, allowed map[*lisp.LVal]bool) {
	t.Helper()
	mutableShared, sealedShared := countSharing(seen1, seen2, allowed)
	shared := len(mutableShared)
	for i, p := range mutableShared {
		if i >= 20 { // don't drown the log; the count is reported below
			break
		}
		t.Errorf("%s: shared mutable LVal: %s", phase, describeLVal(p))
	}
	if shared > 0 {
		t.Fatalf("%s: %d mutable LVal pointers shared between independently built environments (of %d and %d reachable; %d sealed pointers legitimately shared)",
			phase, shared, len(seen1), len(seen2), sealedShared)
	}
	// NO FLOOR ON sealedShared, deliberately, and it is worth saying why
	// rather than leaving a removed check to look like an oversight.
	//
	// This used to require at least 100 shared SEALED pointers, on the
	// reasoning that builtin formals were shared by design, so a zero meant
	// the walk had stopped reaching the formals graph and the assertion above
	// was proving nothing.  Issue #513 then gave every environment its own
	// formals (formalsCopier), and the honest count became zero -- not
	// because the walk went blind but because the sharing genuinely stopped.
	// A floor that fires on the STRONGER outcome is worse than no floor.
	//
	// Anti-vacuity is not lost with it: requireNonVacuous fails unless the
	// walk saw >= 100 LFun values in each environment, which is the property
	// the floor was standing in for -- it proves the walk reaches the function
	// graph where the aliasing lived, and it keeps proving it whether the
	// formals there are shared, sealed, copied, or all three.
	_ = sealedShared
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

// TestCensusDetectsUnsealedFormalsSharing is the red-proof for the census
// above: with registrationFormals' seal guard disabled — templates aliased
// into every environment WITHOUT being sealed — the walk must FAIL, not
// pass vacuously.  The guard cannot be disabled from outside the kernel, so
// the test replicates the guard-disabled outcome directly (the
// path_view_alias_test.go replica style): two function values sharing one
// unsealed formals list, one installed in each registry, exactly what
// AddBuiltins would produce if registrationFormals aliased without checking
// IsSealed.  countSharing must report the shared list and its parameter
// symbol as mutable shares.
func TestCensusDetectsUnsealedFormalsSharing(t *testing.T) {
	env1 := newFullEnv(t)
	env2 := newFullEnv(t)
	allowed := map[*lisp.LVal]bool{
		lisp.Nil():                              true,
		env1.Get(lisp.Symbol(lisp.TrueSymbol)):  true,
		env1.Get(lisp.Symbol(lisp.FalseSymbol)): true,
		env2.Get(lisp.Symbol(lisp.TrueSymbol)):  true,
		env2.Get(lisp.Symbol(lisp.FalseSymbol)): true,
	}

	// The clean baseline: without the injected sharing the census must pass,
	// or the nonzero count below proves nothing about the injection.
	seen1, stats1 := collectRegistry(env1)
	seen2, stats2 := collectRegistry(env2)
	requireNonVacuous(t, "red-proof baseline env1", seen1, stats1)
	requireNonVacuous(t, "red-proof baseline env2", seen2, stats2)
	if pre, _ := countSharing(seen1, seen2, allowed); len(pre) != 0 {
		t.Fatalf("red-proof baseline already has %d mutable shares; cannot attribute detection to the injection", len(pre))
	}

	// The injection: one UNSEALED formals list behind a function value in
	// each environment — the #363 topology the seal guard prevents.
	sharedFormals := lisp.Formals("leaked-param")
	nop := func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() }
	fn1 := lisp.FunInPackage(lisp.DefaultUserPackage, "<red-proof-shared-formals>", sharedFormals, nop)
	fn2 := lisp.FunInPackage(lisp.DefaultUserPackage, "<red-proof-shared-formals>", sharedFormals, nop)
	if rc := env1.PutGlobal(lisp.Symbol("census-red-proof"), fn1); rc.Type == lisp.LError {
		t.Fatalf("env1 PutGlobal: %v", rc)
	}
	if rc := env2.PutGlobal(lisp.Symbol("census-red-proof"), fn2); rc.Type == lisp.LError {
		t.Fatalf("env2 PutGlobal: %v", rc)
	}

	seen1, _ = collectRegistry(env1)
	seen2, _ = collectRegistry(env2)
	mutableShared, _ := countSharing(seen1, seen2, allowed)
	if len(mutableShared) == 0 {
		t.Fatal("census failed to detect an unsealed formals list shared between two registries; " +
			"TestNoCrossEnvironmentLValSharing would not catch registrationFormals aliasing without the seal guard")
	}
	found := false
	for _, p := range mutableShared {
		if p == sharedFormals {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("census flagged %d mutable shares but not the injected formals list itself", len(mutableShared))
	}
}

// BenchmarkEnvInit measures full environment construction — NewEnv +
// InitializeUserEnv + lisplib.LoadLibrary — which is where builtin
// registration lands.  Registration aliases each definition's sealed formals
// template into the environment and deep-copies only unsealed third-party
// formals (registrationFormals in env.go; issues #379, #513, #514); this
// benchmark is the regression tripwire for that cost — it is what measured
// the per-env copies this design removed, and it is what catches any future
// change that quietly reintroduces per-definition work.
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
