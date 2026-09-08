package lisp

import (
	"context"
	"testing"
)

func TestCapturedBuiltinRequiresImplementation(t *testing.T) {
	got := newCapturedBuiltin(capturedBuiltin{Package: DefaultUserPackage, FID: "missing", Formals: Formals()})
	if got.Type != LError || got.String() != "<native code>: captured builtin requires Eval" {
		t.Fatalf("missing implementation: got %v", got)
	}
}

func TestForkBuiltinCapturesPreserveAliasesAndCycles(t *testing.T) {
	template := newForkTestEnv(t)
	state := SortedMap()
	callback := func(_ *LEnv, _ *LVal, captures *LVal) *LVal { return captures }
	first := newCapturedBuiltin(capturedBuiltin{
		Package: DefaultUserPackage, FID: "first", Formals: Formals(),
		Captures: state, Eval: callback,
	})
	second := newCapturedBuiltin(capturedBuiltin{
		Package: DefaultUserPackage, FID: "second", Formals: Formals(),
		Captures: state, Eval: callback,
	})
	for name, value := range map[string]*LVal{"self": state, "first": first, "second": second, "value": Int(1)} {
		if got := state.Map().Set(String(name), value); got.Type == LError {
			t.Fatal(got)
		}
	}
	for name, value := range map[string]*LVal{"state": state, "first": first, "second": second} {
		if got := template.PutGlobal(Symbol(name), value); got.Type == LError {
			t.Fatal(got)
		}
	}
	plans := make(map[*LEnv]*Template)
	fork := func(env *LEnv) *LEnv {
		plan := plans[env]
		if plan == nil {
			var err error
			plan, err = NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
			if err != nil {
				t.Fatal(err)
			}
			plans[env] = plan
		}
		child, err := plan.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		return child
	}
	child, sibling := fork(template), fork(template)
	audit := newForkAuditor(t)
	for name, value := range map[string]*LVal{"state": state, "first": first, "second": second} {
		audit.val(name, value, child.Runtime.Registry.packages[DefaultUserPackage].symbols[name])
	}
	if got := child.Get(Symbol("state")).Map().Set(String("value"), Int(5)); got.Type == LError {
		t.Fatal(got)
	}
	grandchild := fork(child)
	if got, ok := grandchild.Get(Symbol("state")).Map().Get(String("value")); !ok || got.Type != LInt || got.Int != 5 {
		t.Fatalf("grandchild did not inherit its parent's current state: %v", got)
	}
	arms := []*LEnv{template, child, sibling, grandchild}
	for i, env := range arms {
		owned := env.Get(Symbol("state"))
		self, ok := owned.Map().Get(String("self"))
		if !ok || self != owned {
			t.Fatalf("VM %d lost its capture cycle", i)
		}
		for _, name := range []string{"first", "second"} {
			fun := env.Get(Symbol(name))
			got := env.FunCallContext(context.Background(), fun, Nil())
			if got != owned {
				t.Fatalf("VM %d callback %s lost its alias to the VM's state", i, name)
			}
			throughCycle, ok := owned.Map().Get(String(name))
			if !ok || throughCycle.Builtin()(env, Nil()) != owned {
				t.Fatalf("VM %d callback %s lost its function/state cycle", i, name)
			}
		}
		if got := owned.Map().Set(String("value"), Int(i+10)); got.Type == LError {
			t.Fatal(got)
		}
	}
	for i, env := range arms {
		got, ok := env.Get(Symbol("state")).Map().Get(String("value"))
		if !ok || got.Type != LInt || got.Int != i+10 {
			t.Fatalf("VM %d observed another VM's mutation: %v", i, got)
		}
	}
}
