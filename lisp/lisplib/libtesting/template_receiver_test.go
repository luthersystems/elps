// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
)

func TestTemplateTestingOperationsRequireCallingVMSuite(t *testing.T) {
	source := forkRuntimeEnv(t)
	receiver := libtesting.NewTestSuite()
	if rc := source.DefinePackage(lisp.Symbol("testing")); !rc.IsNil() {
		t.Fatal(rc)
	}
	if rc := source.InPackage(lisp.Symbol("testing")); !rc.IsNil() {
		t.Fatal(rc)
	}
	for _, op := range receiver.Ops() {
		source.AddSpecialOps(true, op)
	}
	if rc := source.InPackage(lisp.Symbol("user")); !rc.IsNil() {
		t.Fatal(rc)
	}
	plan := testingTemplate(t, source)
	first, second := testingFork(t, plan), testingFork(t, plan)
	for _, vm := range []*lisp.LEnv{source, first, second} {
		for _, form := range []string{`(testing:test "private" 42)`, `(testing:benchmark "private" (n) n)`} {
			got := vm.LoadString("missing-suite.lisp", form)
			if got.Type != lisp.LError || !strings.Contains(got.String(), "testing:test-suite is not installed") {
				t.Fatalf("operation used hidden receiver or wrong error: %s => %v", form, got)
			}
		}
	}
	if receiver.Len() != 0 || len(receiver.Benchmarks()) != 0 {
		t.Fatal("operations registered into the hidden source receiver")
	}
	// Install only the binding, keeping the inherited operations. They must
	// resolve the calling VM's suite, not require freshly registered callbacks.
	for _, vm := range []*lisp.LEnv{first, second} {
		suite := libtesting.NewTestSuite()
		if rc := vm.InPackage(lisp.Symbol("testing")); !rc.IsNil() {
			t.Fatal(rc)
		}
		if rc := vm.PutGlobal(lisp.Symbol(libtesting.DefaultSuiteSymbol), lisp.Native(suite)); !rc.IsNil() {
			t.Fatal(rc)
		}
		if rc := vm.InPackage(lisp.Symbol("user")); !rc.IsNil() {
			t.Fatal(rc)
		}
		mustLoadTesting(t, vm, "private.lisp", `(testing:test "private" 42) (testing:benchmark "private" (n) n)`)
		if !reflect.DeepEqual(suite.Tests(), []string{"private"}) || !reflect.DeepEqual(suite.Benchmarks(), []string{"private"}) {
			t.Fatalf("wrong calling-VM registrations: tests=%v benchmarks=%v", suite.Tests(), suite.Benchmarks())
		}
		if got := vm.FunCall(suite.Test(0).Fun, lisp.SExpr(nil)); got.Type != lisp.LInt || got.Int != 42 {
			t.Fatalf("registered test is not executable: %v", got)
		}
		if got := vm.FunCall(suite.Benchmark(0).Fun, lisp.SExpr([]*lisp.LVal{lisp.Int(9)})); got.Type != lisp.LInt || got.Int != 9 {
			t.Fatalf("registered benchmark is not executable: %v", got)
		}
	}
	if libtesting.EnvTestSuite(first) == libtesting.EnvTestSuite(second) || receiver.Len() != 0 || len(receiver.Benchmarks()) != 0 {
		t.Fatal("private registration leaked into sibling or hidden receiver")
	}
}
