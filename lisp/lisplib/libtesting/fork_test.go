// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

// This file covers the fork half of the suite-sharing question issue #420
// opened. That issue asked whether one suite reached from several runtimes is
// SAFE (it is now: the suite's own bookkeeping is locked). This one asks
// whether a fork should be reaching the template's suite at all. It should
// not: a `(test ...)` evaluated in a fork used to land in the TEMPLATE's
// registry, carrying a lambda closed over the FORK's environment -- so the
// template ended up owning a definition it never made and could not correctly
// run, and a fork-served test runner accumulated every fork's definitions in
// one place.
//
// Two mechanisms had to meet for that:
//
//   - The suite is an LVal.Native payload, and the fork walk's default policy
//     for a native payload is share-by-reference (docs/fork.md). TestSuite now
//     implements lisp.NativeCloner, so the fork gets its own.
//
//   - OpTest and OpBenchmark are METHODS, and the receiver is captured in the
//     op closure registered with AddSpecialOps. A fork copies that function
//     value without being able to rewrite the *TestSuite inside it, so the
//     clone alone changes nothing: the ops keep writing to the template's
//     suite. They now resolve the suite from the calling environment first.
//
// Neither elpsvet nor the checked-mode ownership gate sees this: the suite
// crosses runtimes by a direct field read out of a Go closure, never through
// Put or eval, and a *TestSuite is not an *LVal so the static rule about
// package-level LVals does not apply either.

// forkTestEnv builds a template environment with the testing package loaded,
// which is the configuration under test: docs/fork.md's standing advice is to
// keep the suite OUT of the template and load it per fork, and that advice
// exists precisely because of the defect below.
func forkTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := libtesting.LoadPackage(env); !rc.IsNil() {
		t.Fatalf("load-package: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

func mustLoadTesting(t *testing.T, env *lisp.LEnv, name, src string) {
	t.Helper()
	if res := env.LoadString(name, src); res.Type == lisp.LError {
		t.Fatalf("%s: %v", name, res)
	}
}

// TestForkGetsItsOwnSuite is the catch.
//
// On the pre-fix tree the two environments report the SAME *TestSuite pointer
// and the template holds the fork's test.
func TestForkGetsItsOwnSuite(t *testing.T) {
	env := forkTestEnv(t)
	templateSuite := libtesting.EnvTestSuite(env)
	if templateSuite == nil {
		t.Fatal("premise: template has no suite")
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	forkSuite := libtesting.EnvTestSuite(fork)
	if forkSuite == nil {
		t.Fatal("fork has no suite")
	}
	// Errorf, not Fatalf: on the pre-fix tree this is the first symptom, and
	// letting the test continue past it is what shows the second one -- the
	// fork's definition landing in the template's registry below.
	if forkSuite == templateSuite {
		t.Errorf("shared=true: fork and template hold the same suite %p", forkSuite)
	}

	mustLoadTesting(t, fork, "fork-only.lisp",
		`(use-package 'testing) (test "fork-only" (assert-equal 1 1))`)

	if names := templateSuite.Tests(); len(names) != 0 {
		t.Errorf("template suite holds the fork's test: %v", names)
	}
	if names := forkSuite.Tests(); len(names) != 1 || names[0] != "fork-only" {
		t.Errorf("fork suite holds %v, want [fork-only]", names)
	}

	// The reverse direction: a definition made on the template AFTER the
	// fork must not appear in the fork.
	mustLoadTesting(t, env, "template-later.lisp",
		`(use-package 'testing) (test "template-later" (assert-equal 1 1))`)
	if names := forkSuite.Tests(); len(names) != 1 {
		t.Errorf("fork suite saw a later template definition: %v", names)
	}
}

// TestForkInheritsTemplateDefinitions is the other half of the clone's
// contract: separating the registries must not LOSE the definitions the
// template had already made. A fork-served runner that loads its test file
// into the template and then forks per case depends on this.
//
// This one PASSES on the pre-fix tree, trivially -- sharing one suite gives
// inheritance for free. It is the control: it fails a "fix" that hands the
// fork an empty suite instead of a seeded clone.
func TestForkInheritsTemplateDefinitions(t *testing.T) {
	env := forkTestEnv(t)
	mustLoadTesting(t, env, "template.lisp", `(use-package 'testing)
(test "first" (assert-equal 1 1))
(test "second" (assert-equal 2 2))
(benchmark "bench" (n) (dotimes (_ n) ()))`)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	forkSuite := libtesting.EnvTestSuite(fork)
	if forkSuite == nil {
		t.Fatal("fork has no suite")
	}

	names := forkSuite.Tests()
	if len(names) != 2 || names[0] != "first" || names[1] != "second" {
		t.Errorf("fork suite holds %v, want [first second] in template order", names)
	}
	if got := forkSuite.Len(); got != 2 {
		t.Errorf("fork suite Len = %d, want 2", got)
	}
	if benches := forkSuite.Benchmarks(); len(benches) != 1 || benches[0] != "bench" {
		t.Errorf("fork suite benchmarks = %v, want [bench]", benches)
	}
	if bench := forkSuite.Benchmark(0); bench == nil || bench.Name != "bench" {
		t.Errorf("fork suite Benchmark(0) = %v, want the inherited benchmark", bench)
	}

	// Inherited names are real registrations, so redefining one in the fork
	// must still be the duplicate-name error -- the clone copied the
	// bookkeeping, not just the name list.
	if res := fork.LoadString("dup.lisp", `(use-package 'testing) (test "first" ())`); res.Type != lisp.LError {
		t.Errorf("redefining an inherited test in the fork was accepted: %v", res)
	}

	// And a fork of the fork keeps them.
	grandchild, err := fork.Fork()
	if err != nil {
		t.Fatalf("fork of fork: %v", err)
	}
	if names := libtesting.EnvTestSuite(grandchild).Tests(); len(names) != 2 {
		t.Errorf("grandchild suite holds %v, want the two inherited tests", names)
	}
}

// TestForkedSuiteRunsItsOwnTest checks that the test a fork registers is
// runnable through the fork's suite -- separating the registries would be a
// poor trade if the entry it files were unusable. Another control: it passes
// on the pre-fix tree too, because there the lambda is equally runnable; it
// was just filed in the wrong registry.
func TestForkedSuiteRunsItsOwnTest(t *testing.T) {
	env := forkTestEnv(t)
	mustLoadTesting(t, env, "template.lisp", `(use-package 'testing)
(set 'shared-value 41)`)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	mustLoadTesting(t, fork, "fork.lisp", `(use-package 'testing)
(set 'shared-value 42)
(test "reads-fork-state" (assert-equal 42 shared-value))`)

	suite := libtesting.EnvTestSuite(fork)
	if suite.Len() != 1 {
		t.Fatalf("fork suite holds %d tests, want 1", suite.Len())
	}
	test := suite.Test(0)
	if res := fork.FunCall(test.Fun, lisp.SExpr(nil)); res.Type == lisp.LError {
		t.Errorf("running the fork's own test failed: %v", res)
	}
	// The template's binding is untouched, which is the point of forking.
	if res := env.LoadString("check.lisp", `shared-value`); res.Type == lisp.LError || res.Int != 41 {
		t.Errorf("template state changed: %v", res)
	}
}
