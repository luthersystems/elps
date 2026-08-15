// Copyright © 2026 The ELPS authors

package libtesting_test

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
)

// This file covers issue #420: *TestSuite is handed out by an exported
// constructor (NewTestSuite) and an exported accessor (EnvTestSuite), with no
// stated scope, and its four fields -- tests, benchmarks, torder, border --
// were unsynchronised. Evaluating a `test` or `benchmark` form calls Add /
// AddBenchmark, so ordinary ELPS source is a write to those fields.
//
// LoadPackage is not affected: it calls NewTestSuite() itself, so every
// environment loaded the normal way owns its suite. The hazard needs an
// embedder that builds one suite and installs it into several runtimes, which
// is reachable through exported API alone -- that is what sharedSuiteEnv below
// does, using nothing LoadPackage does not use.
//
// # Why every test here runs in a child process
//
// The failure mode is not a data race the runtime tolerates. Two goroutines
// writing one Go map is
//
//	fatal error: concurrent map writes
//
// which is thrown by the runtime, is not a panic, and CANNOT be recovered. A
// test that triggers it in-process takes the whole test binary down with it,
// so every other test in the package stops reporting and the failure looks
// like an infrastructure problem rather than a defect.
//
// So the concurrency lives in a child process: the parent re-executes this
// same test binary with ELPS_ISSUE_420_CHILD set to a scenario name, the child
// runs the scenario, and the parent asserts the child exited cleanly. On main
// the child dies and the parent reports its output; after the fix the child
// exits 0. The parent survives either way, which is what makes this test
// re-runnable rather than a one-shot demonstration.
//
// Running under -race adds a second, independent signal for free: the child
// inherits the parent's instrumentation, so the torder/border slice appends
// are reported as DATA RACE even in the runs where the map write happens to
// interleave harmlessly.

const (
	childEnvVar   = "ELPS_ISSUE_420_CHILD"
	childTestName = "TestSharedSuiteConcurrentDefinition"
)

// sharedSuiteEnv installs an externally-owned suite into a fresh runtime the
// same way LoadPackage installs a private one. Every call it makes is exported:
// DefinePackage, InPackage, PutGlobal, AddSpecialOps, AddMacros, and the suite
// itself came from NewTestSuite. This is the embedder shape issue #420
// describes, not a reach into package internals.
func sharedSuiteEnv(tb testing.TB, suite *libtesting.TestSuite) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	name := lisp.Symbol(libtesting.DefaultPackageName)
	if rc := env.DefinePackage(name); !rc.IsNil() {
		tb.Fatalf("define-package: %v", rc)
	}
	if rc := env.InPackage(name); !rc.IsNil() {
		tb.Fatalf("in-package: %v", rc)
	}
	if rc := env.PutGlobal(lisp.Symbol(libtesting.DefaultSuiteSymbol), lisp.Native(suite)); rc.Type == lisp.LError {
		tb.Fatalf("put-global: %v", rc)
	}
	for _, fn := range suite.Ops() {
		env.AddSpecialOps(true, fn)
	}
	for _, fn := range suite.Macros() {
		env.AddMacros(true, fn)
	}
	return env
}

// TestEnvTestSuiteHandsOutTheSharedSuite pins the premise the rest of the file
// rests on: the suite an embedder installed is the suite the package hands back
// out, so "two runtimes, one suite" is a real configuration and not a fiction
// the test constructed. This is a GUARD -- it passes on main.
func TestEnvTestSuiteHandsOutTheSharedSuite(t *testing.T) {
	suite := libtesting.NewTestSuite()
	a := sharedSuiteEnv(t, suite)
	b := sharedSuiteEnv(t, suite)
	if got := libtesting.EnvTestSuite(a); got != suite {
		t.Errorf("runtime A: EnvTestSuite returned %p, want the shared suite %p", got, suite)
	}
	if got := libtesting.EnvTestSuite(b); got != suite {
		t.Errorf("runtime B: EnvTestSuite returned %p, want the shared suite %p", got, suite)
	}

	// A definition made through one runtime is visible through the other's
	// view of the suite, which is what makes concurrent definition a shared
	// write rather than two independent ones.
	if rc := a.LoadStringContext(context.Background(), "premise", `(test "from-a" ())`); rc.Type == lisp.LError {
		t.Fatalf("define via A: %v", rc)
	}
	if names := libtesting.EnvTestSuite(b).Tests(); len(names) != 1 || names[0] != "from-a" {
		t.Errorf("suite seen through B is %v, want [from-a]", names)
	}
}

// TestLoadPackageSuitesAreNotShared is a GUARD, not a catch: it passes on main
// and records the scope limit the issue claims. Environments built the ordinary
// way must NOT share a suite, or this defect would reach every embedder rather
// than only the ones that deliberately share.
func TestLoadPackageSuitesAreNotShared(t *testing.T) {
	newEnv := func() *lisp.LEnv {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
			t.Fatalf("initialize-user-env: %v", rc)
		}
		if rc := libtesting.LoadPackage(env); !rc.IsNil() {
			t.Fatalf("load-package: %v", rc)
		}
		return env
	}
	a, b := libtesting.EnvTestSuite(newEnv()), libtesting.EnvTestSuite(newEnv())
	if a == nil || b == nil {
		t.Fatalf("LoadPackage did not install a suite: a=%p b=%p", a, b)
	}
	if a == b {
		t.Errorf("LoadPackage shared one suite between two runtimes (%p)", a)
	}
}

// TestSharedSuiteConcurrentDefinition is the catch.
//
// Each subtest re-executes this binary with ELPS_ISSUE_420_CHILD set and
// asserts the child exits 0. On main all three fail, every run, under -race,
// because the child dies in the runtime rather than returning. Verbatim from
// the run against 751e61e:
//
//	fatal error: concurrent map read and map write
//
//	goroutine 25 [running]:
//	internal/runtime/maps.fatal(...)
//	    /usr/local/go/src/runtime/panic.go:1046
//	.../libtesting.(*TestSuite).AddBenchmark(0xc000110940, 0xc0000ac0d8)
//	    lisp/lisplib/libtesting/libtesting.go:88
//	.../libtesting.(*TestSuite).OpBenchmark(...)
//	    lisp/lisplib/libtesting/libtesting.go:392
//	.../lisp.(*LEnv).LoadStringContext(...)
//	    lisp/env.go:1276
//
// and, for the read scenario, the same fault raised from a plain accessor:
//
//	fatal error: concurrent map read and map write
//	.../libtesting.(*TestSuite).Test(...)
//	    lisp/lisplib/libtesting/libtesting.go:84
//
// Under -race the child additionally reports the unsynchronised access as a
// DATA RACE, which is what makes the failure deterministic rather than
// timing-dependent:
//
//	WARNING: DATA RACE
//	Read at 0x00c00010a6f0 by goroutine 9:
//	  runtime.mapaccess1_faststr()
//	  .../libtesting.(*TestSuite).Add()   libtesting.go:59
//	Previous write at 0x00c00010a6f0 by goroutine 16:
//	  runtime.mapassign_faststr()
//	  .../libtesting.(*TestSuite).Add()   libtesting.go:63
//
// Without -race the fault is probabilistic per round, which is why each
// scenario runs nRounds of the workload before reporting success.
func TestSharedSuiteConcurrentDefinition(t *testing.T) {
	if scenario := os.Getenv(childEnvVar); scenario != "" {
		runChildScenario(t, scenario)
		return
	}
	for _, scenario := range childScenarioNames {
		t.Run(scenario, func(t *testing.T) {
			t.Parallel()
			runParent(t, scenario)
		})
	}
}

func runParent(t *testing.T, scenario string) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	// #nosec G204 -- os.Args[0] is this test binary; the only variable part
	// is a scenario name from childScenarioNames, passed via the environment.
	cmd := exec.CommandContext(ctx, os.Args[0],
		"-test.run=^"+childTestName+"$",
		"-test.count=1",
		"-test.timeout=90s",
		"-test.v",
	)
	cmd.Env = append(os.Environ(), childEnvVar+"="+scenario)
	out, err := cmd.CombinedOutput()
	if err == nil {
		return
	}
	text := string(out)
	switch {
	case strings.Contains(text, "concurrent map writes"),
		strings.Contains(text, "concurrent map read and map write"),
		strings.Contains(text, "concurrent map iteration and map write"):
		t.Errorf("shared TestSuite killed the process with an unrecoverable map fault (%v)\n%s",
			err, indent(text))
	case strings.Contains(text, "DATA RACE"):
		t.Errorf("shared TestSuite raced (%v)\n%s", err, indent(text))
	default:
		t.Errorf("child scenario %q failed: %v\n%s", scenario, err, indent(text))
	}
}

func indent(s string) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	if len(lines) > 60 {
		lines = append(lines[:60], fmt.Sprintf("... (%d more lines)", len(lines)-60))
	}
	return "\t" + strings.Join(lines, "\n\t")
}

var childScenarioNames = []string{"test-forms", "benchmark-forms", "define-while-reading"}

// runChildScenario is the half that runs in the child process. It does the
// concurrent work directly, with no recover() anywhere, because the fault it
// provokes is not recoverable.
func runChildScenario(t *testing.T, scenario string) {
	for range nRounds {
		if t.Failed() {
			return
		}
		switch scenario {
		case "test-forms":
			concurrentDefine(t, `(test "%s" ())`)
		case "benchmark-forms":
			concurrentDefine(t, `(benchmark "%s" (n) ())`)
		case "define-while-reading":
			defineWhileReading(t)
		default:
			t.Fatalf("unknown child scenario %q", scenario)
		}
	}
}

const (
	nRuntimes = 8
	nDefines  = 40
	// nRounds repeats the workload on a fresh suite. One round is enough
	// under -race; without it the interleaving that makes the map fault
	// fatal is probabilistic, and repeating drives the miss rate down.
	nRounds = 8
)

// concurrentDefine is the shape issue #420 reports: one suite, several
// runtimes, each evaluating definition forms at the same time. Names are
// unique per goroutine so nothing fails for the boring reason of a duplicate.
func concurrentDefine(t *testing.T, form string) {
	suite := libtesting.NewTestSuite()
	envs := make([]*lisp.LEnv, nRuntimes)
	for i := range envs {
		envs[i] = sharedSuiteEnv(t, suite)
	}

	var wg sync.WaitGroup
	wg.Add(nRuntimes)
	for i := range nRuntimes {
		go func() {
			defer wg.Done()
			for j := range nDefines {
				src := fmt.Sprintf(form, fmt.Sprintf("t-%d-%d", i, j))
				if rc := envs[i].LoadStringContext(context.Background(), "issue420", src); rc.Type == lisp.LError {
					t.Errorf("runtime %d: %v", i, rc)
					return
				}
			}
		}()
	}
	wg.Wait()

	if got, want := suite.Len()+len(suite.Benchmarks()), nRuntimes*nDefines; got != want {
		t.Errorf("suite holds %d definitions, want %d: a write was lost", got, want)
	}
}

// defineWhileReading covers the READ side. A map read concurrent with a map
// write is `fatal error: concurrent map read and map write` -- exactly as fatal
// as two writes -- so Len, Tests, Benchmarks, Test(i) and Benchmark(i) have to
// be synchronised too, not just Add and AddBenchmark. An embedder polling the
// suite it installed (a progress report, an MCP tool listing the registered
// tests) reaches this without evaluating anything.
func defineWhileReading(t *testing.T) {
	suite := libtesting.NewTestSuite()
	writer := sharedSuiteEnv(t, suite)

	done := make(chan struct{})
	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		defer wg.Done()
		defer close(done)
		for j := range nRuntimes * nDefines {
			src := fmt.Sprintf(`(test "t-%d" ()) (benchmark "b-%d" (n) ())`, j, j)
			if rc := writer.LoadStringContext(context.Background(), "issue420", src); rc.Type == lisp.LError {
				t.Errorf("writer: %v", rc)
				return
			}
		}
	}()

	for range nRuntimes {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for {
				select {
				case <-done:
					return
				default:
				}
				if n := suite.Len(); n > 0 {
					_ = suite.Test(n - 1)
				}
				_ = suite.Tests()
				if b := suite.Benchmarks(); len(b) > 0 {
					_ = suite.Benchmark(len(b) - 1)
				}
			}
		}()
	}
	wg.Wait()
}
