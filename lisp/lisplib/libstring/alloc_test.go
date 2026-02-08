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

	// Normal usage should succeed.
	exprs, _ := env.Runtime.Reader.Read("test", strings.NewReader(`(string:repeat "x" 100)`))
	result := env.Eval(exprs[0])
	if result.Type == lisp.LError {
		t.Errorf("expected success, got error: %v", result)
	}

	// Exceeding the limit should return a lisp error, not panic or OOM.
	exprs, _ = env.Runtime.Reader.Read("test", strings.NewReader(`(string:repeat "AAAA" 5000000)`))
	result = env.Eval(exprs[0])
	if result.Type != lisp.LError {
		t.Errorf("expected LError for oversized repeat, got: %v", result)
	}
	errStr := result.String()
	if !strings.Contains(errStr, "exceed maximum allocation size") {
		t.Errorf("error message should mention allocation size, got: %v", errStr)
	}
}
