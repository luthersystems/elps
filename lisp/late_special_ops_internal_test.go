package lisp

import (
	"testing"
)

// TestLateSpecialOpHostRegistrationReplaces pins that a host which registered
// its own when/unless/while/default before lisp had them keeps initializing:
// the host registration replaces lisp's operator rather than panicking.
func TestLateSpecialOpHostRegistrationReplaces(t *testing.T) {
	saved := userSpecialOps
	defer func() { userSpecialOps = saved }()
	userSpecialOps = append(userSpecialOps[:len(userSpecialOps):len(userSpecialOps)],
		&langBuiltin{"when", Formals(VarArgSymbol, "args"), func(*LEnv, *LVal) *LVal { return String("host") }, "host when"})
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatal(rc)
	}
	got := env.Eval(SExpr([]*LVal{Symbol("when"), Nil()}))
	if got.Type != LString || got.Str != "host" {
		t.Fatalf("host when not installed: %v", got)
	}
	// the others are untouched
	got = env.Eval(SExpr([]*LVal{Symbol("unless"), Nil(), Int(3)}))
	if got.Type != LInt || got.Int != 3 {
		t.Fatalf("lisp unless lost: %v", got)
	}
}

func TestLateSpecialOpAddIntoLispReplaces(t *testing.T) {
	for _, add := range []func(env *LEnv){
		func(env *LEnv) {
			env.AddBuiltins(true, &langBuiltin{"default", Formals("x"), func(*LEnv, *LVal) *LVal { return String("host") }, "d"})
		},
		func(env *LEnv) {
			env.AddMacros(true, &langBuiltin{"default", Formals("x"), func(*LEnv, *LVal) *LVal { return Quote(String("host")) }, "d"})
		},
		func(env *LEnv) {
			env.AddSpecialOps(true, &langBuiltin{"default", Formals("x"), func(*LEnv, *LVal) *LVal { return String("host") }, "d"})
		},
	} {
		env := NewEnv(nil)
		if rc := InitializeUserEnv(env); rc.Type == LError {
			t.Fatal(rc)
		}
		if rc := env.InPackage(Symbol(DefaultLangPackage)); rc.Type == LError {
			t.Fatal(rc)
		}
		add(env)
		// a name lisp had all along still collides
		func() {
			defer func() {
				if recover() == nil {
					t.Error("re-registering if did not panic")
				}
			}()
			env.AddSpecialOps(true, &langBuiltin{"if", Formals("x"), func(*LEnv, *LVal) *LVal { return Nil() }, "d"})
		}()
		got := env.Eval(SExpr([]*LVal{Symbol("default"), Int(1)}))
		if got.Type != LString || got.Str != "host" {
			t.Fatalf("host default not installed: %v", got)
		}
	}
}
