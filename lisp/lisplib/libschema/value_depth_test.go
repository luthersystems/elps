package libschema

import (
	"github.com/luthersystems/elps/lisp"
	"strings"
	"testing"
)

func TestAllowedValuesDepthError(t *testing.T) {
	env := lisp.NewEnv(nil)
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := LoadPackage(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	lisp.WithMaxValueDepth(1024)(env)
	chain := func(leaf int) *lisp.LVal {
		v := lisp.Int(leaf)
		for range 1100 {
			v = lisp.SExpr([]*lisp.LVal{v})
		}
		return v
	}
	validator := builtinAllowedValues(env, lisp.SExpr([]*lisp.LVal{chain(7)}))
	got := env.FunCall(validator, lisp.SExpr([]*lisp.LVal{chain(8)}))
	if got.Type != lisp.LError || lisp.IsInternalPanic(got) || !strings.Contains(got.String(), "value nesting depth exceeds maximum") {
		t.Fatalf("expected ordinary depth error, got %s", got)
	}
}
