package funraw_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
)

func TestCapturedBuiltinConstructorPreservesCallContract(t *testing.T) {
	env := lisp.NewEnv(nil)
	args := lisp.QExpr([]*lisp.LVal{lisp.Int(17)})
	formals := lisp.Formals("value")
	state := lisp.QExpr([]*lisp.LVal{lisp.Int(23)})
	fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package: "capture-test", FID: "identity", Formals: formals, Captures: state,
		Eval: func(gotEnv *lisp.LEnv, gotArgs, captures *lisp.LVal) *lisp.LVal {
			if gotEnv != env || gotArgs != args {
				t.Fatal("constructor changed the callback environment or arguments")
			}
			return captures
		},
	})
	if fn.Type != lisp.LFun || fn.Package() != "capture-test" || fn.FID() != "identity" || fn.Cells[0] != formals {
		t.Fatalf("constructor changed function metadata: %v", fn)
	}
	if got := fn.Builtin()(env, args); got != state {
		t.Fatalf("callback did not receive the exact capture graph: got %v", got)
	}
}

func TestCapturedBuiltinConstructorHandlesNilInputs(t *testing.T) {
	missing := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{Package: "user", FID: "missing", Formals: lisp.Formals()})
	if missing.Type != lisp.LError || missing.String() != "<native code>: captured builtin requires Eval" {
		t.Fatalf("missing callback did not preserve the constructor error: %v", missing)
	}
	fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package: "user", FID: "no-captures", Formals: lisp.Formals(),
		Eval: func(_ *lisp.LEnv, _ *lisp.LVal, captures *lisp.LVal) *lisp.LVal {
			if captures != nil {
				t.Fatal("nil captures changed at the constructor boundary")
			}
			return lisp.Int(29)
		},
	})
	if got := fn.Builtin()(lisp.NewEnv(nil), lisp.Nil()); got.Type != lisp.LInt || got.Int != 29 {
		t.Fatalf("callback result changed: %v", got)
	}
}

func TestCapturesReturnsDeclaredGraphWithoutExposingOtherState(t *testing.T) {
	state := lisp.SortedMap()
	if rc := state.MapSet("n", lisp.Int(7)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package: "user", FID: "captured", Formals: lisp.Formals(), Captures: state,
		Eval: func(_ *lisp.LEnv, _ *lisp.LVal, captures *lisp.LVal) *lisp.LVal { return captures },
	})
	if got := funraw.Captures(fn); got != state || got.MapGet("n").Int != 7 {
		t.Fatalf("capture graph: got %v want exact declared graph", got)
	}
	plain := lisp.Fun("plain", lisp.Formals(), func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })
	for _, value := range []*lisp.LVal{nil, lisp.Int(1), lisp.Nil(), {Type: lisp.LFun}, plain} {
		if got := funraw.Captures(value); got != nil {
			t.Fatalf("value %v unexpectedly exposes captures: %v", value, got)
		}
	}
}
