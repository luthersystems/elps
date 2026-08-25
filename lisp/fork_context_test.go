// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"testing"
)

// Tests pinning the fork/context contract that pooled embedders depend on
// (issue #380, "Context" in docs/fork.md): a request-scoped value carried by
// the context.Context bound to a fork — at fork time via ForkWithContext, or
// after the fact via WithContext — is observable from inside a builtin
// running on that fork through env.Context().Value, including through
// intervening lisp call frames.  This is how an embedder rebinds per-request
// state (a storage handle, a transaction context) on forks of a shared
// template whose builtins were registered once, at template-load time: the
// builtin closure is shared by every fork, so the per-fork half of its state
// has to travel on the evaluation context instead.

// forkCtxKey carries the per-fork probe value in these tests.
type forkCtxKey struct{}

// bindProbe registers a builtin `ctx-probe` in env's current package that
// reports the context value for forkCtxKey, and a lisp-level wrapper
// `probe-via-call` so the value is also read through a user function's call
// frame rather than only at a top-level builtin call.
func bindProbe(t *testing.T, env *LEnv) {
	t.Helper()
	probe := Fun(DefaultUserPackage+":ctx-probe", Formals(), func(env *LEnv, args *LVal) *LVal {
		s, _ := env.Context().Value(forkCtxKey{}).(string)
		return String(s)
	})
	if lerr := env.PutGlobal(Symbol("ctx-probe"), probe); lerr.Type == LError {
		t.Fatalf("PutGlobal failed: %v", lerr)
	}
	defun := SExpr([]*LVal{
		Symbol("defun"),
		Symbol("probe-via-call"),
		SExpr(nil),
		SExpr([]*LVal{Symbol("ctx-probe")}),
	})
	if lerr := env.Eval(defun); lerr.Type == LError {
		t.Fatalf("defun probe-via-call failed: %v", lerr)
	}
}

// evalProbe evaluates (probe-via-call) on env and returns the string result.
func evalProbe(t *testing.T, env *LEnv, label string) string {
	t.Helper()
	v := env.Eval(SExpr([]*LVal{Symbol("probe-via-call")}))
	if v.Type != LString {
		t.Fatalf("%s: (probe-via-call) = %v, want a string", label, v)
	}
	return v.Str
}

func TestForkContextValueReachesBuiltins(t *testing.T) {
	tmpl := newForkTestEnv(t)
	bindProbe(t, tmpl)

	// Fork-time binding: the pool-checkout pattern.
	fork1, err := tmpl.Fork(ForkWithContext(
		context.WithValue(context.Background(), forkCtxKey{}, "fork-1")))
	if err != nil {
		t.Fatalf("fork1: %v", err)
	}

	// Post-fork binding: the fork-ahead / finalize-later pattern.  The fork
	// is taken with no context and a request context is bound to it later,
	// before its first evaluation, with the WithContext config.
	fork2, err := tmpl.Fork()
	if err != nil {
		t.Fatalf("fork2: %v", err)
	}
	if lerr := WithContext(
		context.WithValue(context.Background(), forkCtxKey{}, "fork-2"))(fork2); lerr.Type == LError {
		t.Fatalf("WithContext(fork2): %v", lerr)
	}

	if got := evalProbe(t, fork1, "fork1"); got != "fork-1" {
		t.Errorf("fork1 sees %q, want %q", got, "fork-1")
	}
	if got := evalProbe(t, fork2, "fork2"); got != "fork-2" {
		t.Errorf("fork2 sees %q, want %q", got, "fork-2")
	}

	// Each binding is per-fork: fork1's value is undisturbed by fork2's
	// bind and evaluation, and re-evaluation is stable.
	if got := evalProbe(t, fork1, "fork1 (again)"); got != "fork-1" {
		t.Errorf("fork1 re-eval sees %q, want %q", got, "fork-1")
	}

	// The template never inherits a fork's context: with no binding of its
	// own it observes no value.
	if got := evalProbe(t, tmpl, "template"); got != "" {
		t.Errorf("template sees %q, want empty (no binding)", got)
	}

	// And the template can bind its own context without touching the forks.
	if lerr := WithContext(
		context.WithValue(context.Background(), forkCtxKey{}, "tmpl"))(tmpl); lerr.Type == LError {
		t.Fatalf("WithContext(tmpl): %v", lerr)
	}
	if got := evalProbe(t, tmpl, "template (bound)"); got != "tmpl" {
		t.Errorf("template sees %q, want %q", got, "tmpl")
	}
	if got := evalProbe(t, fork1, "fork1 (after template bind)"); got != "fork-1" {
		t.Errorf("fork1 sees %q after template bind, want %q", got, "fork-1")
	}
}

// TestForkContextValueThreadsThroughEvalContext pins the same contract for
// the explicit EvalContext entry point: a per-call context passed to
// EvalContext overrides the fork's bound context for that evaluation only,
// which is what lets a pooled fork serve sequential requests by rebinding
// per call instead of per fork.
func TestForkContextValueThreadsThroughEvalContext(t *testing.T) {
	tmpl := newForkTestEnv(t)
	bindProbe(t, tmpl)
	fork, err := tmpl.Fork(ForkWithContext(
		context.WithValue(context.Background(), forkCtxKey{}, "bound")))
	if err != nil {
		t.Fatalf("fork: %v", err)
	}

	call := SExpr([]*LVal{Symbol("probe-via-call")})
	perCall := context.WithValue(context.Background(), forkCtxKey{}, "per-call")
	if v := fork.EvalContext(perCall, call); v.Type != LString || v.Str != "per-call" {
		t.Errorf("EvalContext sees %v, want \"per-call\"", v)
	}
	// The per-call override does not stick: the fork's bound context is
	// restored for subsequent Eval calls.
	if got := evalProbe(t, fork, "fork (after per-call)"); got != "bound" {
		t.Errorf("fork sees %q after per-call override, want %q", got, "bound")
	}
}
