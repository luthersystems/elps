// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

// Mutable test registries and their captured Go receivers are request-local.
// Templates reject loaded suites; each VM loads its own testing package and
// definitions after construction. This preserves issue #420's isolation and
// runnable-registration controls without sharing inherited closures.
func forkRuntimeEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	return env
}

func loadForkTestingPackage(t *testing.T, env *lisp.LEnv) {
	t.Helper()
	if rc := libtesting.LoadPackage(env); !rc.IsNil() {
		t.Fatalf("load-package: %v", rc)
	}
}

func testingTemplate(t *testing.T, env *lisp.LEnv) *lisp.Template {
	t.Helper()
	// These fixed fixtures publish only audited, stateless core builtins.
	template, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Builtin() != nil }))
	if err != nil {
		t.Fatalf("template: %v", err)
	}
	return template
}

func testingFork(t *testing.T, template *lisp.Template) *lisp.LEnv {
	t.Helper()
	env, err := template.NewVM()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if suite := libtesting.EnvTestSuite(env); suite != nil {
		t.Fatal("fresh VM inherited a test registry")
	}
	return env
}

func mustLoadTesting(t *testing.T, env *lisp.LEnv, name, src string) {
	t.Helper()
	if res := env.LoadString(name, src); res.Type == lisp.LError {
		t.Fatalf("%s: %v", name, res)
	}
}

func TestForkGetsItsOwnSuite(t *testing.T) {
	source := forkRuntimeEnv(t)
	template := testingTemplate(t, source)
	first, sibling := testingFork(t, template), testingFork(t, template)
	for _, env := range []*lisp.LEnv{source, first, sibling} {
		loadForkTestingPackage(t, env)
	}
	sourceSuite, firstSuite, siblingSuite := libtesting.EnvTestSuite(source), libtesting.EnvTestSuite(first), libtesting.EnvTestSuite(sibling)
	if sourceSuite == nil || firstSuite == nil || siblingSuite == nil {
		t.Fatal("per-VM registration did not install suites")
	}
	if sourceSuite == firstSuite || sourceSuite == siblingSuite || firstSuite == siblingSuite {
		t.Fatal("per-VM suites share mutable storage")
	}

	mustLoadTesting(t, first, "fork-only.lisp", `(use-package 'testing) (test "fork-only" (assert-equal 1 1))`)
	if names := sourceSuite.Tests(); len(names) != 0 {
		t.Errorf("source holds fork test: %v", names)
	}
	if names := siblingSuite.Tests(); len(names) != 0 {
		t.Errorf("sibling holds fork test: %v", names)
	}
	if names := firstSuite.Tests(); len(names) != 1 || names[0] != "fork-only" {
		t.Errorf("fork holds %v, want [fork-only]", names)
	}

	mustLoadTesting(t, source, "source-later.lisp", `(use-package 'testing) (test "source-later" (assert-equal 1 1))`)
	if names := firstSuite.Tests(); len(names) != 1 || names[0] != "fork-only" {
		t.Errorf("fork saw source registration: %v", names)
	}
	if names := siblingSuite.Tests(); len(names) != 0 {
		t.Errorf("sibling saw source registration: %v", names)
	}
}

func TestTemplateRejectsInheritedSuiteDefinitions(t *testing.T) {
	const definitions = `(use-package 'testing)
(test "first" (assert-equal 1 1))
(test "second" (assert-equal 2 2))
(benchmark "bench" (n) (dotimes (_ n) ()))`
	source := forkRuntimeEnv(t)
	loadForkTestingPackage(t, source)
	for _, state := range []string{"empty", "populated"} {
		t.Run("reject-"+state, func(t *testing.T) {
			if state == "populated" {
				mustLoadTesting(t, source, "source.lisp", definitions)
			}
			template, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Builtin() != nil }))
			if template != nil || err == nil || !strings.Contains(err.Error(), "*libtesting.TestSuite") {
				t.Fatalf("mutable suite admitted or wrong rejection: template=%v error=%v", template, err)
			}
		})
	}

	template := testingTemplate(t, forkRuntimeEnv(t))
	first, sibling := testingFork(t, template), testingFork(t, template)
	// A new template can be published from a still-clean child; testing state
	// is introduced only afterwards, independently in every runtime.
	grandchild := testingFork(t, testingTemplate(t, first))
	for _, env := range []*lisp.LEnv{first, sibling, grandchild} {
		loadForkTestingPackage(t, env)
		mustLoadTesting(t, env, "per-vm.lisp", definitions)
	}
	sourceSuite := libtesting.EnvTestSuite(source)
	for _, env := range []*lisp.LEnv{source, first, sibling, grandchild} {
		suite := libtesting.EnvTestSuite(env)
		if names := suite.Tests(); len(names) != 2 || names[0] != "first" || names[1] != "second" {
			t.Fatalf("test order: %v, want [first second]", names)
		}
		if got := suite.Len(); got != 2 {
			t.Errorf("suite Len=%d, want 2", got)
		}
		if names := suite.Benchmarks(); len(names) != 1 || names[0] != "bench" {
			t.Fatalf("benchmarks: %v, want [bench]", names)
		}
		bench := suite.Benchmark(0)
		if bench == nil || bench.Name != "bench" {
			t.Fatalf("Benchmark(0)=%v, want bench", bench)
		}
		for i := range suite.Len() {
			test := suite.Test(i)
			if env != source && (test == sourceSuite.Test(i) || test.Fun == sourceSuite.Test(i).Fun) {
				t.Fatal("per-VM test inherited a source closure")
			}
			if got := env.FunCall(test.Fun, lisp.SExpr(nil)); got.Type == lisp.LError {
				t.Errorf("execute %s: %v", test.Name, got)
			}
		}
		if got := env.FunCall(bench.Fun, lisp.SExpr([]*lisp.LVal{lisp.Int(2)})); got.Type == lisp.LError {
			t.Errorf("execute benchmark: %v", got)
		}
		if got := env.LoadString("duplicate.lisp", `(test "first" ())`); got.Type != lisp.LError || !strings.Contains(got.String(), "test with the same name already defined: first") {
			t.Errorf("duplicate registration: %v", got)
		}
		if suite.Len() != 2 {
			t.Error("duplicate registration changed suite length")
		}
	}
}

func TestForkedSuiteRunsItsOwnTest(t *testing.T) {
	source := forkRuntimeEnv(t)
	mustLoadTesting(t, source, "source.lisp", `(set 'shared-value 41)`)
	template := testingTemplate(t, source)
	first, sibling := testingFork(t, template), testingFork(t, template)
	loadForkTestingPackage(t, first)
	mustLoadTesting(t, first, "fork.lisp", `(use-package 'testing)
(set 'shared-value 42)
(test "reads-fork-state" (assert-equal 42 shared-value))`)
	suite := libtesting.EnvTestSuite(first)
	if suite.Len() != 1 || suite.Test(0).Name != "reads-fork-state" {
		t.Fatalf("fork registry: %v", suite.Tests())
	}
	if got := first.FunCall(suite.Test(0).Fun, lisp.SExpr(nil)); got.Type == lisp.LError {
		t.Errorf("running fork test: %v", got)
	}
	for _, env := range []*lisp.LEnv{source, sibling} {
		if got := env.LoadString("check.lisp", `shared-value`); got.Type != lisp.LInt || got.Int != 41 {
			t.Errorf("untouched VM changed: %v", got)
		}
		if libtesting.EnvTestSuite(env) != nil {
			t.Error("test registration escaped into another VM")
		}
	}
}
