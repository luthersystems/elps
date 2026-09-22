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
