// Copyright © 2026 The ELPS authors

// Exact allocation pins exclude race and ownership instrumentation, as in
// env_hotpath_alloc_test.go.  Behavioral coverage is in bind_native_test.go.
//go:build !race && !elpscheck

package lisp

import "testing"

// TestBindNativePositionalAllocations pins what binding a host function's
// arguments costs, and so that bind takes its fast path: the argument list
// the builtin owns is two allocations, its backing array and its header.
// The general binder also wraps what a &rest formal collects in a transient
// list, a third allocation per call, which is what the fast path exists to
// drop.  Calls the fast path hands to the general binder cost what they did.
func TestBindNativePositionalAllocations(t *testing.T) {
	env := initSafetyTestEnv(t)
	builtin := func(*LEnv, *LVal) *LVal { return Nil() }
	three := []*LVal{Int(1), Int(2), Int(3)}
	for _, tc := range []struct {
		name        string
		formals     *LVal
		args        []*LVal
		bind        float64
		bindGeneral float64
	}{
		{"Required", Formals("a", "b", "c"), three, 2, 2},
		{"Rest", Formals(VarArgSymbol, "r"), three, 2, 3},
		{"RequiredAndRest", Formals("a", VarArgSymbol, "r"), three, 2, 3},
		{"EmptyRest", Formals("a", VarArgSymbol, "r"), three[:1], 2, 3},
		// Fallbacks: the general binder's cost, unchanged.
		{"Optional", Formals("a", OptArgSymbol, "b"), three[:1], 2, 2},
		{"KeyNoKeywords", Formals("a", KeyArgSymbol, "k"), three[:1], 2, 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			fun := Fun("bind-alloc", tc.formals, builtin)
			args := QExpr(tc.args)
			if _, list := env.bind(fun, args); list.Type == LError {
				t.Fatal(list)
			}
			if got := testing.AllocsPerRun(200, func() { _, hotpathValue = env.bind(fun, args) }); got != tc.bind {
				t.Errorf("bind allocated %v times per call, want %v", got, tc.bind)
			}
			if got := testing.AllocsPerRun(200, func() { _, hotpathValue = env.bindGeneral(fun, args) }); got != tc.bindGeneral {
				t.Errorf("bindGeneral allocated %v times per call, want %v", got, tc.bindGeneral)
			}
		})
	}
}
