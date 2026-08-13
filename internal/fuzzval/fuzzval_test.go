// Copyright © 2026 The ELPS authors

package fuzzval_test

import (
	"fmt"
	"strings"
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
	var out strings.Builder
	for range n {
		out.WriteString(g.Value().String())
		out.WriteByte('\n')
	}
	return out.String()
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

// TestGeneratorProducesRealSourceLocations pins the other half of the corpus
// the Source rule in internal/fuzzfp depends on.
//
// stampMacroExpansion is guarded by `Source == nil || Source.Pos < 0`, so a
// corpus in which EVERY value carries the shared synthetic location presents
// only the case where stamping is legal, and a guard on Source has nothing to
// catch a corrupting builtin with. Both populations must be present.
func TestGeneratorProducesRealSourceLocations(t *testing.T) {
	env := genEnv(t)
	var located, synthetic int
	for _, seed := range fuzzval.Seeds() {
		g := fuzzval.New(seed, env)
		for range 40 {
			v := g.Value()
			if v.Type == lisp.LFun || v == lisp.Nil() || v == lisp.Bool(true) || v == lisp.Bool(false) {
				continue
			}
			if loc, ok := v.Source(); ok && loc.Pos >= 0 {
				located++
			} else {
				synthetic++
			}
		}
	}
	if located == 0 {
		t.Errorf("no generated value carried a REAL source location; internal/fuzzfp's rule that a"+
			" real location is never replaced has nothing to bite on (%d synthetic)", synthetic)
	}
	if synthetic == 0 {
		t.Errorf("every generated value carried a real source location; the legitimate"+
			" stampMacroExpansion transition is no longer covered (%d located)", located)
	}
}

// TestGeneratorNeverRelocatesSharedValues is the safety half of the same
// change. Writing Source onto a singleton is issue #274, and writing it onto
// an LFun drawn from the environment corrupts the environment's own function
// object -- either would make the generator, not the builtin, the defect.
func TestGeneratorNeverRelocatesSharedValues(t *testing.T) {
	env := genEnv(t)
	// Source() returns a value copy plus a "carries a location" bool (#362),
	// so the singletons are compared BY VALUE here rather than by pointer.
	// That is strictly stronger: the old pointer comparison could not see an
	// edit made through the shared Location it pointed at.
	nilSrc, nilOK := lisp.Nil().Source()
	trueSrc, trueOK := lisp.Bool(true).Source()
	falseSrc, falseOK := lisp.Bool(false).Source()
	for _, seed := range fuzzval.Seeds() {
		g := fuzzval.New(seed, env)
		for range 40 {
			v := g.Value()
			if v.Type != lisp.LFun {
				continue
			}
			if loc, ok := v.Source(); ok && loc.Pos >= 0 {
				t.Fatalf("the generator wrote a real location onto a function value (%s)", v.Str)
			}
		}
	}
	nilNow, nilNowOK := lisp.Nil().Source()
	trueNow, trueNowOK := lisp.Bool(true).Source()
	falseNow, falseNowOK := lisp.Bool(false).Source()
	if nilNow != nilSrc || nilNowOK != nilOK ||
		trueNow != trueSrc || trueNowOK != trueOK ||
		falseNow != falseSrc || falseNowOK != falseOK {
		t.Fatalf("the generator relocated a shared singleton")
	}
}

// TestGeneratorProducesStdlibNativeTypes pins that the natives the standard
// library itself hands back to lisp are in the corpus. Without them the only
// LNative shapes generated are ones no stdlib function has a branch for, so
// every one stops at a type check and the whole LNative surface is covered
// only in its rejection path.
func TestGeneratorProducesStdlibNativeTypes(t *testing.T) {
	env := genEnv(t)
	seen := map[string]bool{}
	for _, seed := range fuzzval.Seeds() {
		g := fuzzval.New(seed, env)
		for range 200 {
			if v := g.Value(); v.Type == lisp.LNative {
				seen[fmt.Sprintf("%T", v.Native)] = true
			}
		}
	}
	for _, want := range []string{
		"time.Time", "time.Duration", "*regexp.Regexp", "json.RawMessage",
		"string", "int", "[]uint8", "map[string]int", "struct {}", "<nil>",
	} {
		if !seen[want] {
			t.Errorf("the corpus never produced an LNative wrapping %s (saw %v)", want, seen)
		}
	}
}

// TestKindSeedsCoverEveryKind pins KindSeeds' contract. Callers use it because
// it GUARANTEES every value shape is present -- lisp/lisplib's seed cross
// applies every callable to one value of every kind on the strength of it --
// so a KindSeeds that quietly stopped reaching a kind would shrink that cross
// without shrinking any assertion.
func TestKindSeedsCoverEveryKind(t *testing.T) {
	env := genEnv(t)
	seen := map[lisp.LType]bool{}
	for _, seed := range fuzzval.KindSeeds() {
		seen[fuzzval.New(seed, env).Value().Type] = true
	}
	want := []lisp.LType{
		lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol, lisp.LQSymbol,
		lisp.LBytes, lisp.LError, lisp.LSExpr, lisp.LArray, lisp.LSortMap,
		lisp.LTaggedVal, lisp.LNative, lisp.LFun, lisp.LQuote,
	}
	for _, ty := range want {
		if !seen[ty] {
			t.Errorf("KindSeeds' FIRST value never has type %v; the per-kind guarantee it exists"+
				" to make does not hold", ty)
		}
	}
}
