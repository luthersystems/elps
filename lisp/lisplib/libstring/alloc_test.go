// Copyright © 2018 The ELPS authors

package libstring_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func TestRepeatAllocLimit(t *testing.T) {
	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()),
	)
	lisplib.LoadLibrary(env)
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	// Normal usage should succeed and return the correct value.
	exprs, err := env.Runtime.Reader.Read("test", strings.NewReader(`(string:repeat "xy" 3)`))
	if err != nil {
		t.Fatalf("parse normal repeat: %v", err)
	}
	result := env.Eval(exprs[0])
	if result.Type == lisp.LError {
		t.Fatalf("expected success, got error: %v", result)
	}
	if result.Str != "xyxyxy" {
		t.Errorf("expected %q, got %q", "xyxyxy", result.Str)
	}

	// Exceeding the limit should return a lisp error, not panic or OOM.
	exprs, err = env.Runtime.Reader.Read("test", strings.NewReader(`(string:repeat "AAAA" 5000000)`))
	if err != nil {
		t.Fatalf("parse oversized repeat: %v", err)
	}
	result = env.Eval(exprs[0])
	if result.Type != lisp.LError {
		t.Fatalf("expected LError for oversized repeat, got: %v", result)
	}
	errStr := result.String()
	if !strings.Contains(errStr, "exceed maximum allocation size") {
		t.Errorf("error message should mention allocation size, got: %v", errStr)
	}
}
