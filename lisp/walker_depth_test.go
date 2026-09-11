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

// TestRuntimeWalkerDepthAt500K pins the observed behavior at 500,000 acyclic
// containers. Survival at this depth does not establish arbitrary-depth
// safety for walkers that still use recursion. Each operation runs in its
// own process so a fatal stack overflow or hang becomes a test failure,
// enforced by an external deadline rather than an in-process timer.
// Successes assert their full result; the expected to-string LError must be
// an ordinary conversion error, never a recovered internal panic.
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
		{"equal", "success: equal chains true; different deepest leaves false"},
		{"go-copy", "success: independent copy of all 500,000 lists and the leaf"},
		{"lisp-copy", "success: independent copy of all 500,000 lists and the leaf"},
		{"json-dump", "success: all 500,000 lists serialized exactly"},
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
	checkCopy := func(copied *lisp.LVal) {
		t.Helper()
		original := source
		for i := range depth {
			if copied == nil || copied.Type != lisp.LSExpr || len(copied.Cells) != 1 {
				t.Fatalf("copy changed list shape at depth %d", i)
			}
			if copied == original || &copied.Cells[0] == &original.Cells[0] {
				t.Fatalf("copy shares header or cell storage at depth %d", i)
			}
			original, copied = original.Cells[0], copied.Cells[0]
		}
		if copied == nil || copied.Type != lisp.LInt || copied.Int != 7 || copied == original {
			t.Fatal("copy must retain an independent integer leaf at exactly 500,000 lists")
		}
		copied.Int = 8
		if original.Int != 7 {
			t.Fatal("mutating copied leaf changed source")
		}
	}
	switch operation {
	case "equal":
		other := chain(7)
		if err := lisp.GoError(env.PutGlobal(lisp.Symbol("other"), other)); err != nil {
			t.Fatal(err)
		}
		if v := eval("(equal? source other)"); v.Type != lisp.LSymbol || v.Str != "true" {
			t.Fatal("separately allocated equal chains must compare equal")
		}
		leaf := other
		for range depth {
			leaf = leaf.Cells[0]
		}
		leaf.Int = 8
		if v := eval("(equal? source other)"); v.Type != lisp.LSymbol || v.Str != "false" {
			t.Fatal("chains differing only at deepest leaf must compare unequal")
		}
	case "go-copy":
		checkCopy(source.Copy())
	case "lisp-copy":
		checkCopy(eval("(copy source)"))
	case "json-dump":
		v := eval("(json:dump-string source)")
		want := strings.Repeat("[", depth) + "7" + strings.Repeat("]", depth)
		if v.Type != lisp.LString || v.Str != want {
			t.Fatal("JSON must serialize all 500,000 lists and the integer leaf exactly")
		}
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
