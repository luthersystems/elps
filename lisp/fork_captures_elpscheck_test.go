//go:build elpscheck

package lisp

import "testing"

func TestOwnershipCheckBuiltinCapturesArePrivate(t *testing.T) {
	first, second := newForkTestEnv(t), newForkTestEnv(t)
	fun := newCapturedBuiltin(capturedBuiltin{
		Package: DefaultUserPackage, FID: "capture", Formals: Formals(), Captures: SortedMap(),
		Eval: func(_ *LEnv, _ *LVal, captures *LVal) *LVal { return captures },
	})
	if got := first.PutGlobal(Symbol("capture"), fun); got.Type == LError {
		t.Fatal(got)
	}
	expectOwnershipPanic(t, func() { second.PutGlobal(Symbol("capture"), fun) })
}
