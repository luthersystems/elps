// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

// TestForkServedRunner exercises the fork-served test-runner pattern
// (issue #380): a template environment is loaded once, and Runner.NewEnvFn
// hands every runner-constructed environment a fork of it instead of
// paying a fresh full load.
//
// The NewEnvFn below is also the reference implementation of the
// stateful-native contract documented in docs/fork.md: the lisp testing
// suite is an accumulator whose ops and macros are Go closures over the
// suite instance, so the TEMPLATE IS BUILT WITHOUT IT — stop the template
// before the stateful hooks — and each fork loads the testing package
// itself (fresh suite, fresh closures), exactly the way an embedder runs
// its per-environment hooks on a fork instead of baking their state into
// the template.
func TestForkServedRunner(t *testing.T) {
	template := lisp.NewEnv(nil)
	template.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(template); !rc.IsNil() {
		t.Fatalf("init: %v", rc)
	}
	if rc := template.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		t.Fatalf("in-package: %v", rc)
	}
	// Program state the tests rely on: proves forks serve loaded-template
	// state, not just stdlib.
	if res := template.LoadString("template.lisp", `
(set 'base (vector 10))
(defun base-sum () (foldl '+ 0 base))
`); res.Type == lisp.LError {
		t.Fatalf("template program: %v", res)
	}

	forks := 0
	r := &elpstest.Runner{
		NewEnvFn: func(tb testing.TB) (*lisp.LEnv, error) {
			fork, err := template.Fork(lisp.ForkWithStderr(elpstest.NewLogger(tb)))
			if err != nil {
				return nil, err
			}
			// Per-fork stateful hook: the testing package registers a fresh
			// suite (an accumulator native) and op/macro closures over it.
			if rc := libtesting.LoadPackage(fork); !rc.IsNil() {
				return nil, lisp.GoError(rc)
			}
			forks++
			return fork, nil
		},
	}

	// Each test case mutates the shared-looking vector; every case sees
	// the pristine template state because every case runs on its own fork.
	source := `
(use-package 'testing)
(test "first-mutates" (append! base 99) (assert-equal 2 (length base)))
(test "second-sees-pristine" (assert-equal 1 (length base)) (assert-equal 10 (base-sum)))
(test "third-sees-pristine" (assert-equal 10 (base-sum)) (append! base 1) (assert-equal 11 (base-sum)))
`
	names := r.LoadTests(t, "fork_served_test.lisp", strings.NewReader(source))
	if len(names) != 3 {
		t.Fatalf("expected 3 tests, got %v", names)
	}
	for i, name := range names {
		t.Run(name, func(t *testing.T) {
			r.RunTest(t, i, "fork_served_test.lisp", strings.NewReader(source))
		})
	}
	// LoadTests + one env per test case.
	if forks != 4 {
		t.Errorf("expected 4 forks (1 load + 3 tests), got %d", forks)
	}
	// The template itself never saw any test's mutation.
	if res := template.LoadString("check.lisp", `(length base)`); res.Type == lisp.LError || res.Int != 1 {
		t.Errorf("template mutated by fork-served tests: %v", res)
	}
}

// TestForkServedRunnerStderrContract: a fork-served runner that forgets
// ForkWithStderr is the easy mistake — a plain Fork shares the TEMPLATE's
// Stderr, which for a template built outside a test is os.Stderr, not an
// *elpstest.Logger.  The runner must name the contract violation instead of
// dying in an interface-conversion panic inside a deferred flush.
func TestForkServedRunnerStderrContract(t *testing.T) {
	template := lisp.NewEnv(nil)
	template.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(template); !rc.IsNil() {
		t.Fatalf("init: %v", rc)
	}
	if rc := template.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		t.Fatalf("in-package: %v", rc)
	}
	r := &elpstest.Runner{
		NewEnvFn: func(testing.TB) (*lisp.LEnv, error) {
			return template.Fork() // no ForkWithStderr: the mistake
		},
	}
	_, err := r.NewEnv(t)
	if err == nil {
		t.Fatalf("NewEnv accepted an environment with a non-Logger Stderr")
	}
	if !strings.Contains(err.Error(), "ForkWithStderr") {
		t.Errorf("error does not name the fix: %v", err)
	}
}
