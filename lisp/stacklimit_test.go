// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func TestDeepRecursionHitsStackLimit(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Stack.MaxHeightLogical = 100
	env.Runtime.Stack.MaxHeightPhysical = 50
	lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	// Define a function that recurses without bound.
	exprs, err := env.Runtime.Reader.Read("test", strings.NewReader(`(defun boom (n) (+ 1 (boom (+ n 1))))`))
	if err != nil {
		t.Fatalf("parse defun: %v", err)
	}
	result := env.Eval(exprs[0])
	if result.Type == lisp.LError {
		t.Fatalf("defun failed: %v", result)
	}

	// Calling boom should hit the stack limit, not exhaust memory.
	exprs, err = env.Runtime.Reader.Read("test", strings.NewReader(`(boom 0)`))
	if err != nil {
		t.Fatalf("parse call: %v", err)
	}
	result = env.Eval(exprs[0])
	if result.Type != lisp.LError {
		t.Fatalf("expected stack overflow error, got: %v", result)
	}
	errStr := result.String()
	if !strings.Contains(errStr, "stack height exceeded") {
		t.Errorf("error should mention stack height exceeded, got: %v", errStr)
	}
}
