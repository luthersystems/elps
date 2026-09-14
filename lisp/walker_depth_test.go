// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
)

// TestRuntimeWalkerDepthAt500K pins ordinary depth errors for recursive
// walkers and preserves the existing rendering, quoting and conversion cases.
// Each operation runs in its own process with an external deadline.
func TestRuntimeWalkerDepthAt500K(t *testing.T) {
	const childEnv = "ELPS_RUNTIME_WALKER_500K"
	if operation := os.Getenv(childEnv); operation != "" {
		runRuntimeWalkerAt500K(t, operation)
		return
	}
	executable, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		operation, outcome string
	}{
		{"equal", "ordinary LError: value nesting depth exceeds maximum: 1024"},
		{"go-copy", "ordinary LError: value nesting depth exceeds maximum: 1024"},
		{"lisp-copy", "ordinary LError: value nesting depth exceeds maximum: 1024"},
		{"json-dump", "ordinary LError: value nesting depth exceeds maximum: 1024"},
		{"format-string", "success: 1024 balanced wrappers around #<depth-limit>"},
		{"quote", "success: shallow quoted header; source unchanged"},
		{"to-string", "ordinary LError: cannot convert type to string"},
	} {
		t.Run(tc.operation, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 30*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, executable, "-test.run=^TestRuntimeWalkerDepthAt500K$", "-test.count=1") //nolint:gosec // executable is this test binary from os.Executable, not user input
			cmd.Env = append(os.Environ(), childEnv+"="+tc.operation)
			output, err := cmd.CombinedOutput()
			if ctx.Err() != nil {
				t.Fatalf("walker exceeded external 30s deadline: %v\n%s", ctx.Err(), output)
			}
			if err != nil {
				t.Fatalf("walker subprocess failed: %v\n%s", err, output)
			}
			t.Log(tc.outcome)
		})
	}
}

func runRuntimeWalkerAt500K(t *testing.T, operation string) {
	t.Helper()
	const depth = 500_000
	chain := func(leaf int) *lisp.LVal {
		v := lisp.Int(leaf)
		for range depth {
			v = lisp.SExpr([]*lisp.LVal{v})
		}
		return v
	}
	source := chain(7)
	env := newLimitTestEnv(t)
	if err := lisp.GoError(env.PutGlobal(lisp.Symbol("source"), source)); err != nil {
		t.Fatal(err)
	}
	eval := func(expression string) *lisp.LVal {
		t.Helper()
		v := env.LoadString("walker-depth.lisp", expression)
		if v.Type == lisp.LError {
			t.Fatalf("%s failed: %s", expression, v)
		}
		return v
	}
	checkDepthError := func(v *lisp.LVal) {
		t.Helper()
		if v.Type != lisp.LError || lisp.IsInternalPanic(v) || !strings.Contains(v.String(), "value nesting depth exceeds maximum: 1024") {
			t.Fatalf("expected ordinary depth error, got %s", v)
		}
	}
	switch operation {
	case "equal":
		other := chain(7)
		if err := lisp.GoError(env.PutGlobal(lisp.Symbol("other"), other)); err != nil {
			t.Fatal(err)
		}
		checkDepthError(env.LoadString("walker-depth.lisp", "(equal? source other)"))
		leaf := other
		for range depth {
			leaf = leaf.Cells[0]
		}
		leaf.Int = 8
		checkDepthError(env.LoadString("walker-depth.lisp", "(equal? source other)"))
	case "go-copy":
		checkDepthError(source.Copy())
	case "lisp-copy":
		checkDepthError(env.LoadString("walker-depth.lisp", "(copy source)"))
	case "json-dump":
		checkDepthError(env.LoadString("walker-depth.lisp", "(json:dump-string source)"))
	case "format-string":
		v := eval(`(format-string "{}" source)`)
		want := strings.Repeat("(", 1024) + "#<depth-limit>" + strings.Repeat(")", 1024)
		if v.Type != lisp.LString || v.Str != want {
			t.Fatal("format-string must truncate after 1024 containers with balanced delimiters")
		}
	case "quote":
		v := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), source}))
		if v.Type != lisp.LSExpr || !v.IsQuoted() || len(v.Cells) != 1 || v.Cells[0] != source.Cells[0] {
			t.Fatal("quote must return a shallow quoted list retaining its deep child")
		}
		if source.IsQuoted() {
			t.Fatal("quote must leave the source header unquoted")
		}
	case "to-string":
		v := env.LoadString("walker-depth.lisp", "(to-string source)")
		if v.Type != lisp.LError || lisp.IsInternalPanic(v) || !strings.Contains(v.String(), "cannot convert type to string") {
			t.Fatal("to-string must reject a list with an ordinary conversion error")
		}
	default:
		t.Fatalf("unknown walker operation %q", operation)
	}
}
