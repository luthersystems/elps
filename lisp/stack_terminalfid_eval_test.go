// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

const cpsChainProgram = `
(defun build-cps (n k)
  (if (= n 0)
    (k 0)
    (build-cps (- n 1) (lambda (v) (k (+ v 1))))))
`

func newCPSTestEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env, cfg...); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.LoadString("cps.lisp", cpsChainProgram); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

func evalCPS(t *testing.T, env *lisp.LEnv, n int) *lisp.LVal {
	t.Helper()
	exprs, err := env.Runtime.Reader.Read("call", strings.NewReader(fmt.Sprintf("(build-cps %d (lambda (x) x))", n)))
	if err != nil {
		t.Fatal(err)
	}
	return env.Eval(exprs[0])
}

// TestTerminalFIDCPSChainScaling checks that calling n distinct continuations
// in tail position costs linear work rather than quadratic work
// (luthersystems/elps#1009).  It counts the frames that the terminal index
// paths visit, so it does not depend on timing, -race or machine load.
func TestTerminalFIDCPSChainScaling(t *testing.T) {
	const (
		smallN = 1000
		largeN = 10 * smallN
	)
	work := func(n int) int {
		env := newCPSTestEnv(t)
		if v := evalCPS(t, env, n); v.Type != lisp.LInt || v.Int != n {
			t.Fatalf("build-cps %d returned %v, want %d", n, v, n)
		}
		return lisp.TerminalWork(env.Runtime.Stack)
	}
	wSmall := work(smallN)
	wLarge := work(largeN)
	t.Logf("CPS work n=%d: %d, n=%d: %d", smallN, wSmall, largeN, wLarge)
	// A linear cost gives a ratio near 10.  A quadratic cost gives one near
	// 100.
	if wLarge > 15*wSmall {
		t.Fatalf("CPS work grew super-linearly: n=%d visited %d frames, n=%d visited %d",
			smallN, wSmall, largeN, wLarge)
	}
}

// TestTerminalFIDCPSChainExactSteps verifies that indexing long terminal chains
// preserves the exact result and ELPS step count (1065 for build-cps 50,
// measured against the unmodified linear scan).
func TestTerminalFIDCPSChainExactSteps(t *testing.T) {
	const (
		n         = 50
		wantSteps = int64(1065)
	)
	envOK := newCPSTestEnv(t, lisp.WithMaxSteps(wantSteps))
	if v := evalCPS(t, envOK, n); v.Type != lisp.LInt || v.Int != n {
		t.Fatalf("at exact step limit %d: got %v, want %d", wantSteps, v, n)
	}
	envShort := newCPSTestEnv(t, lisp.WithMaxSteps(wantSteps-1))
	if v := evalCPS(t, envShort, n); !v.IsError() {
		t.Fatalf("at limit %d (one below exact): expected error, got %v", wantSteps-1, v)
	}
}

// TestTerminalFIDLongMutualTailRecursion checks that tail-recursion
// optimization still collapses a mutual tail-call cycle longer than the
// terminal-index threshold without growing the physical stack.
func TestTerminalFIDLongMutualTailRecursion(t *testing.T) {
	// Define a 24-function tail-call ring f0 -> f1 -> ... -> f23 -> f0 so the
	// terminal chain exceeds terminalIndexThreshold (16) before looping back.
	const ringSize = 24
	var sb strings.Builder
	for i := range ringSize {
		next := (i + 1) % ringSize
		fmt.Fprintf(&sb, "(defun f%d (n acc) (if (= n 0) acc (f%d (- n 1) (+ acc 1))))\n", i, next)
	}
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env, lisp.WithMaximumPhysicalStackHeight(200)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.LoadString("ring.lisp", sb.String()); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	// 5000 steps would overflow physical stack height 200 without TRO.
	v := env.LoadString("call.lisp", "(f0 5000 0)")
	if v.Type != lisp.LInt || v.Int != 5000 {
		t.Fatalf("(f0 5000 0) = %v, want 5000", v)
	}
}
