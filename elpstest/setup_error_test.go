// Copyright © 2026 The ELPS authors

package elpstest

import (
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
)

// An error from SetupFn or TeardownFn fails the test
// (luthersystems/elps#941).  The Runner dropped both results, so a setup
// that failed left the test to run in an environment that was not set up,
// and the report did not show the cause.
//
// A failed test is the result to check, so each case runs in a child
// process that is this test binary again.
func TestRunnerReportsSetupAndTeardownErrors(t *testing.T) {
	const childFlag = "ELPS_RUNNER_HOOK_ERROR_CHILD"
	const source = `(use-package 'testing) (test "ok" (debug-print "body ran") (assert true))`
	fail := func(msg string) func(*lisp.LEnv) *lisp.LVal {
		return func(env *lisp.LEnv) *lisp.LVal { return env.Errorf("%s", msg) }
	}
	cases := map[string]struct {
		runner  *Runner
		want    string
		bodyRan bool
	}{
		// The body does not run in an environment that was not set up.
		"setup":    {&Runner{SetupFn: fail("setup went wrong")}, "setup went wrong", false},
		"teardown": {&Runner{TeardownFn: fail("teardown went wrong")}, "teardown went wrong", true},
	}
	if name := os.Getenv(childFlag); name != "" {
		cases[name].runner.RunTest(t, 0, "hook_test.lisp", strings.NewReader(source))
		return
	}
	executable, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			// #nosec G204 -- this test binary and fixed selector contain no user input.
			command := exec.CommandContext(t.Context(), executable, "-test.run=^TestRunnerReportsSetupAndTeardownErrors$")
			command.Env = append(os.Environ(), childFlag+"="+name)
			output, err := testdeadline.RunChild(command, 30*time.Second)
			if err == nil {
				t.Fatalf("the test passed though %s failed\n%s", name, output)
			}
			if !strings.Contains(string(output), tc.want) {
				t.Errorf("the report does not show %q\n%s", tc.want, output)
			}
			if got := strings.Contains(string(output), "body ran"); got != tc.bodyRan {
				t.Errorf("body ran: %v, want %v\n%s", got, tc.bodyRan, output)
			}
		})
	}
}
