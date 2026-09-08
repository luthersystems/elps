// Copyright © 2026 The ELPS authors

package elpstest

import (
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// #625: these controls reproduce the relevant oracle blind spots from #617.
// Their premises do not use Template or its storage inventory.
func TestForkOraclePreservesUserIdentifiers(t *testing.T) {
	for _, makeValue := range []func(string) *lisp.LVal{lisp.String, lisp.Symbol} {
		if renderResult(makeValue("_fun12")) == renderResult(makeValue("_fun34")) {
			t.Fatal("user data was erased by function-ID normalization")
		}
	}
	for _, prefix := range []string{"_fun", "_validation_fun_"} {
		if renderResult(lisp.Errorf("%s12", prefix)) == renderResult(lisp.Errorf("%s34", prefix)) {
			t.Fatal("user error message was erased by function-ID normalization")
		}
	}
}

func visibilityEnv(t *testing.T, values ...*lisp.LVal) *lisp.LEnv {
	t.Helper()
	env := mustEnv(t, "")
	for i, value := range values {
		// These are deliberately broken ownership fixtures. The package setter
		// avoids PutGlobal's checked-runtime guard rejecting the fixture before
		// the independent oracle gets to demonstrate that it sees the leak.
		if rc := env.Runtime.Package.Put(lisp.Symbol(string(rune('a'+i))), value); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
	}
	return env
}

func TestForkOracleSeesMutableScalarHeaders(t *testing.T) {
	shared := lisp.Int(42)
	first, second := visibilityEnv(t, shared), visibilityEnv(t, shared)
	if len(sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil)) == 0 {
		t.Fatal("two VMs share a writable scalar header but the oracle reports isolation")
	}
	if got := sharedOracleCensuses(newOracleCensus(first), newOracleCensus(visibilityEnv(t, lisp.Int(42))), nil); len(got) != 0 {
		t.Fatalf("independent scalar headers reported shared: %v", got)
	}
}

func TestForkOracleSeesActualCellSlots(t *testing.T) {
	for _, empty := range []bool{false, true} {
		t.Run(map[bool]string{false: "overlap", true: "empty-capacity"}[empty], func(t *testing.T) {
			cells := []*lisp.LVal{lisp.Int(11), lisp.Int(22), lisp.Int(33)}
			view := cells[1:]
			if empty {
				view = view[:0]
			}
			// No declared CellView link: physical Go backing is the independent oracle.
			aliased := visibilityEnv(t, lisp.QExpr(cells), lisp.QExpr(view))
			copied := append([]*lisp.LVal(nil), view[:cap(view)]...)
			dealiased := visibilityEnv(t, lisp.QExpr(cells), lisp.QExpr(copied[:len(view)]))
			if envState(aliased) != envState(dealiased) {
				t.Fatal("premise: contents differ before the alias check")
			}
			if aliasSignature(aliased) == aliasSignature(dealiased) {
				t.Fatal("cell-slot de-aliasing is invisible")
			}
			left, right := visibilityEnv(t, lisp.QExpr(cells)), visibilityEnv(t, lisp.QExpr(view))
			// Use immutable children to ensure the witness is a slot, not a child header.
			for _, value := range cells {
				value.SealAST()
			}
			if len(sharedOracleCensuses(newOracleCensus(left), newOracleCensus(right), nil)) == 0 {
				t.Fatal("overlapping cell storage is invisible across VMs")
			}
		})
	}
}

func TestForkOracleSeesByteOverlapAndCapacity(t *testing.T) {
	backing := []byte("abcd")
	left := visibilityEnv(t, lisp.Bytes(backing[:1]))
	right := visibilityEnv(t, lisp.Bytes(backing[3:]))
	if len(sharedOracleCensuses(newOracleCensus(left), newOracleCensus(right), nil)) == 0 {
		t.Fatal("distinct byte headers hide overlapping backing")
	}
	before := envState(left)
	backing[3] = 'x'
	if envState(left) == before {
		t.Fatal("a capacity-tail write is invisible to state comparison")
	}
}

func TestForkOraclePreservesQuoteState(t *testing.T) {
	cells := []*lisp.LVal{lisp.Int(7)}
	if renderResult(lisp.QExpr(cells)) == renderResult(lisp.SExpr(cells)) {
		t.Fatal("quoted data and executable expression render identically")
	}
}

func TestForkOracleSeesNativeReferenceKinds(t *testing.T) {
	for name, payload := range map[string]any{
		"map": map[string]int{"n": 1}, "slice": []int{1, 2}, "channel": make(chan int, 1),
	} {
		t.Run(name, func(t *testing.T) {
			first, second := visibilityEnv(t, lisp.Native(payload)), visibilityEnv(t, lisp.Native(payload))
			if len(sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil)) == 0 {
				t.Fatal("shared reference-kind native is invisible")
			}
		})
	}
}

func TestForkOracleNativeIdentityIgnoresNamedWrappers(t *testing.T) {
	type namedMap map[string]int
	backing := map[string]int{"n": 1}
	first := visibilityEnv(t, lisp.Native(backing))
	second := visibilityEnv(t, lisp.Native(namedMap(backing)))
	if len(sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil)) == 0 {
		t.Fatal("a named Go map conversion concealed shared mutable storage")
	}
	if len(nativeReferenceIDs(new(struct{}))) != 0 {
		t.Fatal("a zero-size pointer was assigned mutable storage identity")
	}
}

func TestForkOracleNativeObserverCoversBothChannels(t *testing.T) {
	first, second := 1, 2
	one, two := lisp.Native(&first), lisp.Native(&second)
	render := func(value any) string { return strconv.Itoa(*value.(*int)) }
	if renderResultWithNative(one, render) == renderResultWithNative(two, render) {
		t.Fatal("result channel ignores native contents")
	}
	if envStateWithNative(visibilityEnv(t, one), render) == envStateWithNative(visibilityEnv(t, two), render) {
		t.Fatal("state channel ignores native contents")
	}
}

func TestForkOracleDoesNotInventFunctionIdentity(t *testing.T) {
	first, second := 1, 2
	one, two := func() int { return first }, func() int { return second }
	if len(nativeReferenceIDs(one)) != 0 || len(nativeReferenceIDs(two)) != 0 {
		t.Fatal("a Go code address was treated as a captured function's storage identity")
	}
	render := func(value any) string { return strconv.Itoa(value.(func() int)()) }
	if renderResultWithNative(lisp.Native(one), render) == renderResultWithNative(lisp.Native(two), render) {
		t.Fatal("explicit native observations lost captured function state")
	}
}

func TestForkOracleSealedContentIgnoresInterning(t *testing.T) {
	makeLiteral := func(n int) *lisp.LVal {
		value := lisp.QExpr([]*lisp.LVal{lisp.Int(n)})
		value.SealAST()
		return value
	}
	shared := makeLiteral(7)
	aliased := visibilityEnv(t, lisp.QExpr([]*lisp.LVal{shared, shared}))
	duplicated := visibilityEnv(t, lisp.QExpr([]*lisp.LVal{makeLiteral(7), makeLiteral(7)}))
	if envState(aliased) != envState(duplicated) || aliasSignature(aliased) != aliasSignature(duplicated) {
		t.Fatal("sealed interning changed the program-state or mutable-alias observation")
	}
	changed := visibilityEnv(t, lisp.QExpr([]*lisp.LVal{makeLiteral(7), makeLiteral(8)}))
	if envState(aliased) == envState(changed) {
		t.Fatal("ignoring sealed identity also erased sealed content")
	}
}

func TestForkOracleSeesNilVersusEmptyStorage(t *testing.T) {
	for _, pair := range [][2]*lisp.LVal{
		{lisp.QExpr(nil), lisp.QExpr([]*lisp.LVal{})},
		{lisp.Bytes(nil), lisp.Bytes([]byte{})},
	} {
		if renderResult(pair[0]) == renderResult(pair[1]) {
			t.Fatalf("nil and empty %s storage were conflated", pair[0].Type)
		}
	}
}

func TestForkOracleSealedDiamondsAreMemoized(t *testing.T) {
	leaf := lisp.Int(7)
	value := leaf
	for range 40 {
		value = lisp.QExpr([]*lisp.LVal{value, value})
	}
	value.SealAST()
	var state sealedOracleState
	before := state.digest(value)
	if len(state.memo) != 41 {
		t.Fatalf("40 diamond levels must digest exactly 41 distinct nodes, got %d", len(state.memo))
	}
	// Deliberately violate the seal without evaluating it: the observer must
	// see a content write even at the bottom of a heavily shared code DAG.
	// The checked-mode inspector retains this root after the test, so restore
	// the mutation even if the oracle assertion below fails (#625).
	original := leaf.Int
	t.Cleanup(func() { leaf.Int = original })
	leaf.Int = 8
	var after sealedOracleState
	if before == after.digest(value) {
		t.Fatal("sealed diamond memoization hid a leaf mutation")
	}
}

func TestForkOracleReadonlyFunctionCellsBoundary(t *testing.T) {
	cells := []*lisp.LVal{lisp.Int(1), lisp.Int(2)}
	for _, value := range cells {
		value.SealAST()
	}
	for _, length := range []int{len(cells), len(cells) - 1} {
		first, second := &lisp.LVal{Type: lisp.LFun, Cells: cells[:length]}, &lisp.LVal{Type: lisp.LFun, Cells: cells[:length]}
		sharedSlot := false
		for _, left := range oracleValueIDs(first) {
			// Headers differ and children are sealed, so only a writable
			// cell slot can have a shared physical identity in this fixture.
			for _, right := range oracleValueIDs(second) {
				sharedSlot = sharedSlot || left == right
			}
		}
		if sharedSlot != (length < cap(cells)) {
			t.Fatalf("len=%d cap=%d: shared mutable slots=%t", length, cap(cells), sharedSlot)
		}
	}
}

func TestForkOracleSeesMapBackingBehindDistinctWrappers(t *testing.T) {
	wrap := oracleJSONValue
	backing := make(map[string]any)
	one, two := wrap(backing), wrap(backing)
	first, second := visibilityEnv(t, one), visibilityEnv(t, two)
	if one.Map() == two.Map() {
		t.Fatal("premise: wrappers must differ")
	}
	if len(sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil)) == 0 {
		t.Fatal("empty shared map backing escaped isolation census")
	}
	aliased := visibilityEnv(t, wrap(backing), wrap(backing))
	independent := visibilityEnv(t, wrap(make(map[string]any)), wrap(make(map[string]any)))
	if aliasSignature(aliased) == aliasSignature(independent) {
		t.Fatal("distinct wrappers concealed shared map backing from alias parity")
	}
	if rc := one.MapSet("witness", lisp.Int(17)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if got := two.MapGet("witness"); got.Type != lisp.LInt || got.Int != 17 {
		t.Fatal("premise: wrappers did not share actual map storage")
	}
}

func TestForkOracleSeesNativeAnnotationsInsideSealedCode(t *testing.T) {
	shared := map[string]int{"n": 1}
	makeValue := func() *lisp.LVal {
		value := lisp.String("unchanged")
		value.Native = shared
		outer := lisp.QExpr([]*lisp.LVal{value})
		outer.SealAST()
		return outer
	}
	first, second := visibilityEnv(t, makeValue()), visibilityEnv(t, makeValue())
	if len(sharedOracleCensuses(newOracleCensus(first), newOracleCensus(second), nil)) == 0 {
		t.Fatal("a native annotation below sealed code escaped identity census")
	}
	render := func(value any) string { return strconv.Itoa(value.(map[string]int)["n"]) }
	before := envStateWithNative(first, render)
	shared["n"] = 2
	if envStateWithNative(first, render) == before {
		t.Fatal("sealed content memo hid an observed native annotation write")
	}
}
