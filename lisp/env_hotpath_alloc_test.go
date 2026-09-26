// Copyright © 2026 The ELPS authors

// Exact allocation pins exclude race and ownership instrumentation, as in
// sort_alloc_pin_test.go. Behavioral coverage lives in env_hotpath_test.go.
//go:build !race && !elpscheck

package lisp

import "testing"

var hotpathValue *LVal
var hotpathString string

func TestEvaluatorHotPathAllocations(t *testing.T) {
	env := initSafetyTestEnv(t)
	sym, value := Symbol("allocation-value"), Int(42)
	if got := env.PutGlobal(sym, value); got.Type == LError {
		t.Fatal(got)
	}
	qualified := QExpr([]*LVal{Quote(Symbol("user:allocation-value"))})
	formals := Formals()
	// fmt boxes counters above 255; keep the measurement beyond that range.
	env.Runtime.numenv = 1000
	env.Runtime.numsym = 1000
	for _, tc := range []struct {
		call func()
		name string
		want float64
	}{
		{func() { hotpathValue = SplitSymbol(sym) }, "SplitSymbol", 2},
		{func() { hotpathValue = opQualifiedSymbol(env, qualified) }, "QualifiedSymbol", 0},
		{func() { hotpathValue = env.GetGlobal(sym) }, "GetGlobal", 0},
		{func() { hotpathValue = env.PutGlobal(sym, value) }, "PutGlobal", 0},
		{func() { hotpathValue = env.PutGlobalFromLisp(sym, value) }, "PutGlobalFromLisp", 0},
		{func() { hotpathValue = env.Lambda(formals, []*LVal{value}) }, "Lambda", 4},
		{func() { hotpathString = env.Runtime.GenSym() }, "GenSym", 1},
		{func() { hotpathValue = env.Terminal(value) }, "Terminal", 1},
		{func() { hotpathValue = markTailRec(3, sym, value) }, "TailRec", 1},
		{func() { hotpathValue = markMacExpand(value) }, "MacExpand", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := testing.AllocsPerRun(200, tc.call); got != tc.want {
				t.Errorf("allocated %v times per call, want %v", got, tc.want)
			}
			if hotpathValue != nil && hotpathValue.Type == LError {
				t.Fatal(hotpathValue)
			}
		})
	}
}

func TestSplitSymbolPartsAllocations(t *testing.T) {
	for _, s := range []string{"name", "pkg:name", ":keyword", "", "a:b:c"} {
		t.Run(s, func(t *testing.T) {
			if got := testing.AllocsPerRun(200, func() {
				_, hotpathString, _ = splitSymbolParts(s)
			}); got != 0 {
				t.Errorf("allocated %v times, want zero", got)
			}
		})
	}
}

func TestEmptyScopeAllocations(t *testing.T) {
	env := initSafetyTestEnv(t)
	value := Int(42)
	fun := env.Lambda(Formals(), []*LVal{value})
	args := Nil()
	let := SExpr([]*LVal{Symbol("let"), Nil(), value})
	// Before lazy scopes these allocated 3 and 8 objects respectively.  The
	// lambda call fell from 2 to 1 when bind stopped wrapping the body in a
	// fresh list header on every call; the one left is the call env.
	for _, tc := range []struct {
		name string
		call func() *LVal
		want float64
	}{
		{"ZeroArgumentLambda", func() *LVal { return env.FunCall(fun, args) }, 1},
		{"EmptyLet", func() *LVal { return env.Eval(let) }, 6},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := testing.AllocsPerRun(200, func() { hotpathValue = tc.call() })
			if hotpathValue.Type != LInt || hotpathValue.Int != 42 {
				t.Fatalf("unexpected result: %v", hotpathValue)
			}
			if got != tc.want {
				t.Errorf("allocated %v times per call, want %v", got, tc.want)
			}
		})
	}
}

// TestCallFormAllocations pins the call value evalSExprCells builds for a
// builtin call: its header and its cells (the function plus its arguments)
// are one allocation (newSExprCap) up to eight cells, and two past that, as
// every call form was before.  The remaining four allocations are the
// builtin's argument binding and its bookkeeping, which newSExprCap does not
// touch.
func TestCallFormAllocations(t *testing.T) {
	env := initSafetyTestEnv(t)
	x := Symbol("call-form-x")
	if got := env.PutGlobal(x, Int(1)); got.Type == LError {
		t.Fatal(got)
	}
	form := func(nargs int) *LVal {
		cells := []*LVal{Symbol("max")}
		for range nargs {
			cells = append(cells, x)
		}
		return SExpr(cells)
	}
	for _, tc := range []struct {
		name  string
		nargs int
		want  float64
	}{
		{"2-cells", 1, 4},
		{"8-cells", 7, 4},
		{"9-cells-not-coallocated", 8, 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			call := form(tc.nargs)
			got := testing.AllocsPerRun(200, func() { hotpathValue = env.Eval(call) })
			if hotpathValue.Type != LInt || hotpathValue.Int != 1 {
				t.Fatalf("unexpected result: %v", hotpathValue)
			}
			if got != tc.want {
				t.Errorf("allocated %v times per call, want %v", got, tc.want)
			}
		})
	}
}

func TestLazyScopePresizing(t *testing.T) {
	env := initSafetyTestEnv(t)
	keys := make([]*LVal, 32)
	for i := range keys {
		keys[i] = Symbol(string(rune('a' + i)))
	}
	value := Int(42)
	measure := func(eager bool) float64 {
		return testing.AllocsPerRun(200, func() {
			child := newEnvN(env, len(keys))
			if eager {
				child.scope = make(map[string]*LVal, len(keys))
			}
			for _, key := range keys {
				hotpathValue = child.Put(key, value)
			}
		})
	}
	if lazy, eager := measure(false), measure(true); lazy != eager {
		t.Errorf("lazy scope allocated %v times, eager presized scope allocated %v", lazy, eager)
	}
}
