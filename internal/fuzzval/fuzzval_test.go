// Copyright © 2026 The ELPS authors

package fuzzval_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func genEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestGeneratorIsDeterministic is the load-bearing property of the whole
// generator: a saved crasher is a byte string, and it is only a reproduction
// if those bytes always build the same value. A map iteration, a clock read or
// a rand call anywhere in fuzzval would silently turn every committed
// regression case into noise.
func TestGeneratorIsDeterministic(t *testing.T) {
	env := genEnv(t)
	for _, seed := range fuzzval.Seeds() {
		a := renderN(fuzzval.New(seed, env), 12)
		b := renderN(fuzzval.New(seed, env), 12)
		if a != b {
			t.Fatalf("seed %q produced different values on two runs:\n%s\nvs\n%s", seed, a, b)
		}
	}
}

func renderN(g *fuzzval.Gen, n int) string {
	out := ""
	for range n {
		out += g.Value().String() + "\n"
	}
	return out
}

// TestGeneratorCoversEveryLType asserts the generator actually reaches the
// shapes it claims to. A generator that quietly stopped producing, say,
// LNative would leave the targets green while covering strictly less -- the
// silent-shrinking failure mode a fuzz gate is most vulnerable to.
func TestGeneratorCoversEveryLType(t *testing.T) {
	env := genEnv(t)
	seen := map[lisp.LType]int{}
	for _, seed := range fuzzval.Seeds() {
		g := fuzzval.New(seed, env)
		for range 40 {
			seen[g.Value().Type]++
		}
	}
	want := []lisp.LType{
		lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol, lisp.LQSymbol,
		lisp.LBytes, lisp.LError, lisp.LSExpr, lisp.LArray, lisp.LSortMap,
		lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LQuote,
	}
	for _, ty := range want {
		if seen[ty] == 0 {
			t.Errorf("the seed corpus never produced an %v", ty)
		}
	}
}

// TestGeneratorProducesSingletons pins that the shared Nil()/Bool() values
// really are handed to builtins. That is the only way the elpscheck singleton
// guard and the per-iteration SingletonSnapshot check have anything to catch;
// a generator that always allocated fresh values would make both vacuous.
func TestGeneratorProducesSingletons(t *testing.T) {
	env := genEnv(t)
	nilV, trueV, falseV := lisp.Nil(), lisp.Bool(true), lisp.Bool(false)
	var sawNil, sawTrue, sawFalse bool
	for _, seed := range fuzzval.Seeds() {
		g := fuzzval.New(seed, env)
		for range 40 {
			switch v := g.Value(); v {
			case nilV:
				sawNil = true
			case trueV:
				sawTrue = true
			case falseV:
				sawFalse = true
			}
		}
	}
	if !sawNil || !sawTrue || !sawFalse {
		t.Errorf("singletons missing from the corpus: nil=%v true=%v false=%v",
			sawNil, sawTrue, sawFalse)
	}
}

// TestGeneratorIsBounded pins the allocation cap. Without it a handful of
// bytes can request an exponentially large tree and the target spends its
// budget in the allocator instead of in the code under test.
func TestGeneratorIsBounded(t *testing.T) {
	env := genEnv(t)
	// The most expansion-hungry input available: every byte selects the
	// widest compound kind with the largest child count.
	data := make([]byte, 4096)
	for i := range data {
		data[i] = 0xff
	}
	g := fuzzval.New(data, env)
	total := 0
	for range 8 {
		total += countCells(g.Value(), 0)
	}
	if total > 4*fuzzval.Budget {
		t.Errorf("generated %d cells from one Gen; Budget is %d", total, fuzzval.Budget)
	}
}

func countCells(v *lisp.LVal, depth int) int {
	if v == nil || depth > 32 {
		return 1
	}
	n := 1
	if v.Type == lisp.LFun {
		// A validator/lambda's cells are its formals and body, not generated
		// payload; counting them would measure the interpreter, not the
		// generator.
		return n
	}
	for _, c := range v.Cells {
		n += countCells(c, depth+1)
	}
	return n
}

// TestSeedsAreDistinct keeps the corpus from carrying duplicates, which cost
// a baseline-coverage iteration each and buy nothing.
func TestSeedsAreDistinct(t *testing.T) {
	seen := make(map[string]int)
	for i, s := range fuzzval.Seeds() {
		if prev, ok := seen[string(s)]; ok {
			t.Errorf("seed %d duplicates seed %d (%v)", i, prev, s)
		}
		seen[string(s)] = i
	}
}
