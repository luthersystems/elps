// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"context"
	"os"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
)

// libschema invokes constraints through a PRIVATE calling convention: a
// validator's Go closure takes the value under test directly, not an argument
// list, so constraints are called by reaching into the raw Builtin closure rather
// than through LEnv.FunCall. Nothing in the type system enforced that the
// value in a constraint slot was actually built by this package, so any LFun
// landed there and was invoked with a bare value:
//
//	(s:validate identity 1)        -> index out of range [0] with length 0
//	(s:validate (lambda (x) x) 1)  -> nil pointer dereference (Builtin is nil)
//
// Both are reachable from phylum source, which in substrate is
// customer-supplied. These tests cover the CLASS -- every constraint slot in
// the package -- not the two reported instances.

// TestForeignFunctionInConstraintSlot drives an ordinary function and an ELPS
// lambda into every constraint slot the s package exposes.
//
// Each case must produce an ordinary lisp error. Specifically NOT:
//
//   - an internal-panic condition (a recovered Go panic, i.e. a crash), which
//     is what every one of these did before; and
//   - a successful validation, which is worse -- see the s:when case.
func TestForeignFunctionInConstraintSlot(t *testing.T) {
	// Each source is run twice, once with `identity` (a Go builtin, whose
	// Builtin is non-nil so the old code called it with an empty arg list)
	// and once with an ELPS lambda (whose Builtin IS nil, so the old code
	// dereferenced nil). Different crashes, same class.
	foreigners := map[string]string{
		"builtin": "identity",
		"lambda":  "(lambda (x) x)",
	}
	cases := []struct{ name, src string }{
		{"validate", `(s:validate FOREIGN 1)`},
		{"deftype-constraint", `(s:deftype "T" s:int FOREIGN) (s:validate T 1)`},
		{"deftype-type", `(s:deftype "T" FOREIGN) (s:validate T 1)`},
		{"make-validator", `(s:validate (s:make-validator "T" s:int FOREIGN) 1)`},
		{"check-bool", `(s:deftype "T" s:bool FOREIGN) (s:validate T true)`},
		{"check-fun", `(s:deftype "T" s:fun FOREIGN) (s:validate T identity)`},
		{"check-string", `(s:deftype "T" s:string FOREIGN) (s:validate T "x")`},
		{"check-float", `(s:deftype "T" "float" FOREIGN) (s:validate T 1.5)`},
		{"check-number", `(s:deftype "T" s:number FOREIGN) (s:validate T 1)`},
		{"check-array", `(s:deftype "T" s:array FOREIGN) (s:validate T (vector 1))`},
		{"check-map", `(s:deftype "T" s:sorted-map FOREIGN) (s:validate T (sorted-map "a" 1))`},
		{"check-any", `(s:deftype "T" "any" FOREIGN) (s:validate T 1)`},
		{"check-tagged", `(s:deftype "T" s:tagged-value FOREIGN) (s:validate T 1)`},
		{"has-key", `(s:deftype "T" s:sorted-map (s:has-key "a" FOREIGN)) (s:validate T (sorted-map "a" 1))`},
		{"may-have-key", `(s:deftype "T" s:sorted-map (s:may-have-key "a" FOREIGN)) (s:validate T (sorted-map "a" 1))`},
		{"of", `(s:deftype "T" s:array (s:of FOREIGN)) (s:validate T (vector 1))`},
		{"no-other-keys", `(s:deftype "T" s:sorted-map (s:no-other-keys FOREIGN)) (s:validate T (sorted-map "a" 1))`},
		{"not", `(s:deftype "T" s:int (s:not FOREIGN)) (s:validate T 1)`},
		{"when-guard", `(s:deftype "T" s:sorted-map (s:when "a" FOREIGN "b" (s:gt 100))) (s:validate T (sorted-map "a" 1 "b" 1))`},
		{"when-check", `(s:deftype "T" s:sorted-map (s:when "a" (s:gt 0) "b" FOREIGN)) (s:validate T (sorted-map "a" 1 "b" 1))`},
	}
	for _, c := range cases {
		for kind, foreign := range foreigners {
			t.Run(c.name+"/"+kind, func(t *testing.T) {
				env := newSchemaEnv(t)
				src := strings.ReplaceAll(c.src, "FOREIGN", foreign)
				res := env.LoadStringContext(context.Background(), c.name, src)
				if res.Type != lisp.LError {
					t.Fatalf("a foreign %s in the %s constraint slot was ACCEPTED (result %v).\n"+
						"A validator that silently approves a value it cannot check is worse "+
						"than one that crashes.\n--- source ---\n%s", kind, c.name, res, src)
				}
				if lisp.IsInternalPanic(res) {
					t.Fatalf("a foreign %s in the %s constraint slot panicked: %v\n--- source ---\n%s",
						kind, c.name, res, src)
				}
			})
		}
	}
}

// TestWhenGuardRejectsForeignAtConstruction is the specific regression for
// the inversion hazard in s:when, and it deliberately pins the ANSWER, not
// just the absence of a panic.
//
// s:when treats an error from its guard constraint as "the condition is not
// met, skip these checks". A "not a schema constraint" error IS an error, so
// routing the guard through the marker check at APPLICATION time -- the
// obvious fix -- makes the whole clause silently disappear:
//
//	before this work:  [INTERNAL-PANIC]  (loud, obviously broken)
//	naive fix:         [list] ()          VALIDATION PASSES with b = 1
//	                                      against a constraint demanding > 100
//
// The panic became a silent wrong answer in a validator. The guard is
// therefore rejected at CONSTRUCTION, where no inversion can reach it.
func TestWhenGuardRejectsForeignAtConstruction(t *testing.T) {
	const src = `(s:deftype "M" s:sorted-map (s:has-key "a" s:int) (s:has-key "b" s:int)
	                                (s:when "a" identity "b" (s:gt 100)))`
	env := newSchemaEnv(t)
	res := env.LoadStringContext(context.Background(), "when-guard", src)
	if res.Type != lisp.LError {
		t.Fatalf("s:when accepted a foreign guard at construction: %v", res)
	}
	if lisp.IsInternalPanic(res) {
		t.Fatalf("s:when panicked on a foreign guard: %v", res)
	}

	// And the belt-and-braces check: even if a future refactor moves the
	// rejection later, the clause must never be skipped. b = 1 fails (s:gt
	// 100), so a successful validation here means the clause vanished.
	env2 := newSchemaEnv(t)
	full := src + `
	(s:validate M (sorted-map "a" 1 "b" 1))`
	res2 := env2.LoadStringContext(context.Background(), "when-guard-validate", full)
	if res2.Type != lisp.LError {
		t.Fatalf("validation PASSED with b=1 against (s:gt 100): the s:when clause was "+
			"silently skipped because the guard rejection was read as "+
			"'condition not met'. Got %v", res2)
	}
}

// TestWhenSemanticsPreserved pins that the fix did not break s:when itself:
// the guard still selects whether the clause applies.
func TestWhenSemanticsPreserved(t *testing.T) {
	cases := []struct {
		name  string
		src   string
		valid bool
	}{
		{
			"guard met, check fails",
			`(s:deftype "M" s:sorted-map (s:when "a" (s:gt 0) "b" (s:gt 100)))
			 (s:validate M (sorted-map "a" 1 "b" 1))`,
			false,
		},
		{
			"guard met, check passes",
			`(s:deftype "M" s:sorted-map (s:when "a" (s:gt 0) "b" (s:gt 100)))
			 (s:validate M (sorted-map "a" 1 "b" 101))`,
			true,
		},
		{
			"guard not met, check skipped",
			`(s:deftype "M" s:sorted-map (s:when "a" (s:gt 5) "b" (s:gt 100)))
			 (s:validate M (sorted-map "a" 1 "b" 1))`,
			true,
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			env := newSchemaEnv(t)
			res := env.LoadStringContext(context.Background(), c.name, c.src)
			if got := res.Type != lisp.LError; got != c.valid {
				t.Fatalf("expected valid=%v, got %v (%v)", c.valid, got, res)
			}
		})
	}
}

// TestWhenRejectsNonMapInput covers the other panic reachable through s:when:
// it called input.Map() with no type check, which panics ("not sorted-map:
// int") for any other type. Reachable because s:when composes under s:any,
// where nothing has checked the type first.
func TestWhenRejectsNonMapInput(t *testing.T) {
	env := newSchemaEnv(t)
	res := env.LoadStringContext(context.Background(), "when-nonmap",
		`(s:deftype "T" "any" (s:when "a" s:int "b" s:int)) (s:validate T 1)`)
	if res.Type != lisp.LError {
		t.Fatalf("expected a wrong-type error, got %v", res)
	}
	if lisp.IsInternalPanic(res) {
		t.Fatalf("s:when panicked on a non-map input: %v", res)
	}
}

// TestNewValidatorIsAcceptedAsAConstraint covers the extension point the
// marker would otherwise have closed.
//
// Before the marker, a Go embedder could pass any lisp.LFun where libschema
// expected a constraint and it worked by accident. The marker ends that, so
// libschema.NewValidator restores the capability deliberately. This test is
// the proof that the exported constructor really produces something the
// package accepts -- otherwise "we kept the extension point" would be a claim
// with nothing behind it.
func TestNewValidatorIsAcceptedAsAConstraint(t *testing.T) {
	// One validator per environment, NOT one validator shared by all of
	// them: each newSchemaEnv is a separate Runtime, and an LVal must not
	// be used by two Runtimes — the validator LFun's Cells and FunData are
	// unguarded shared mutable state, the same hazard class as the shared
	// builtin formals of issue #363.  The elpscheck ownership checker
	// (lisp/ownership_check_elpscheck.go) caught the original sharing here
	// on its first full-suite run.
	newEven := func() *lisp.LVal {
		return libschema.NewValidator(lisp.Formals("input"),
			func(_ *lisp.LEnv, input *lisp.LVal) *lisp.LVal {
				if input.Type == lisp.LInt && input.Int%2 == 0 {
					return lisp.Nil()
				}
				return lisp.ErrorConditionf(libschema.FailedConstraint, "not even: %v", input)
			})
	}

	env := newSchemaEnv(t)
	if rc := env.PutGlobal(lisp.Symbol("even?"), newEven()); rc.Type == lisp.LError {
		t.Fatalf("bind: %v", rc)
	}

	for _, c := range []struct {
		src   string
		valid bool
	}{
		{`(s:deftype "T" s:int even?) (s:validate T 4)`, true},
		{`(s:deftype "T" s:int even?) (s:validate T 5)`, false},
		{`(s:deftype "T" s:int (s:not even?)) (s:validate T 5)`, true},
		{`(s:deftype "T" s:sorted-map (s:has-key "a" even?)) (s:validate T (sorted-map "a" 2))`, true},
		{`(s:deftype "T" s:sorted-map (s:when "a" even? "b" (s:gt 100))) (s:validate T (sorted-map "a" 2 "b" 1))`, false},
	} {
		e := newSchemaEnv(t)
		if rc := e.PutGlobal(lisp.Symbol("even?"), newEven()); rc.Type == lisp.LError {
			t.Fatalf("bind: %v", rc)
		}
		res := e.LoadStringContext(context.Background(), "newvalidator", c.src)
		if got := res.Type != lisp.LError; got != c.valid {
			t.Errorf("%s: expected valid=%v, got %v (%v)", c.src, c.valid, got, res)
		}
	}
}

// builtinInvocationRe matches an invocation of a raw LBuiltin closure, in
// BOTH spellings this file has historically used:
//
//	rest.FunData().Builtin(env, input)      (historical, pre-#382)
//	builtinIsTruthy(env, nil).Builtin()(env, input)
//
// A drift guard that only grepped the first spelling would have declared
// libschema clean while the second was sitting in builtinIsFalsy.
var builtinInvocationRe = regexp.MustCompile(`\.Builtin\s*\(env|\.Builtin\(\)\s*\(`)

// TestNoUnroutedConstraintInvocation is the drift guard: it fails if a
// SIXTEENTH raw constraint invocation is added.
//
// The original defect was not one bad line, it was fifteen call sites each
// independently trusting that whatever sat in a constraint slot obeyed a
// private calling convention. Fixing fifteen sites without a guard leaves the
// class open: the next person to add a constraint has no reason to know the
// convention exists. Everything must go through applyConstraint, which is the
// only function permitted to hold the raw invocation.
func TestNoUnroutedConstraintInvocation(t *testing.T) {
	src, err := os.ReadFile("libschema.go")
	if err != nil {
		t.Fatalf("read libschema.go: %v", err)
	}
	var offenders []string
	for i, line := range strings.Split(string(src), "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), "//") {
			continue // a doc comment quoting the pattern is not a call site
		}
		if builtinInvocationRe.MatchString(line) {
			offenders = append(offenders, "libschema.go:"+strconv.Itoa(i+1)+": "+strings.TrimSpace(line))
		}
	}
	if len(offenders) != 1 {
		t.Fatalf("expected exactly ONE raw constraint invocation (the one inside "+
			"applyConstraint), found %d:\n  %s\n\n"+
			"Constraints are invoked through a private calling convention that "+
			"cannot be enforced by the type system: the closure takes the value "+
			"under test directly. Any new call site must go through "+
			"applyConstraint, which verifies the unforgeable validator marker "+
			"first. See isValidator.",
			len(offenders), strings.Join(offenders, "\n  "))
	}
	if !strings.Contains(offenders[0], "constraint.Builtin()(env, input)") {
		t.Fatalf("the single raw invocation is not the one in applyConstraint: %s", offenders[0])
	}
}

// TestValidatorMarkerIsUnforgeableFromLisp checks the credential itself: a
// validator is identified by an LVal POINTER that no lisp-visible path can
// produce, not by a name, a package or a structural shape any of which a
// phylum could imitate.
func TestValidatorMarkerIsUnforgeableFromLisp(t *testing.T) {
	// A user function defined INSIDE a package called "s" carries the same
	// package label and the same shape as a validator. It must still be
	// refused: the marker is identity, not metadata.
	env := newSchemaEnv(t)
	res := env.LoadStringContext(context.Background(), "forge", `
		(in-package 's)
		(defun impostor (x) x)
		(in-package 'user)
		(s:validate s:impostor 1)`)
	if res.Type != lisp.LError {
		t.Fatalf("a user function defined in package 's' was accepted as a validator: %v", res)
	}
	if lisp.IsInternalPanic(res) {
		t.Fatalf("panicked instead of rejecting: %v", res)
	}
}
