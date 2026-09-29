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

// TestLateNamesHostRegistrationAnyKind pins the #736 late names against
// every host registration kind: a host that registered help, test,
// benchmark, test-let, test-let* or benchmark-simple -- as a special
// operator, a macro or a builtin -- keeps initializing, and its definition
// wins.  InitializeUserEnv adds macros, then special operators, then
// builtins, so a host macro named like a lisp operator used to collide.
func TestLateNamesHostRegistrationAnyKind(t *testing.T) {
	names := []string{"help", "test", "benchmark", "test-let", "test-let*", "benchmark-simple", "when"}
	kinds := []struct {
		name  string
		table *[]*langBuiltin
		ret   func() *LVal
	}{
		{"special-op", &userSpecialOps, func() *LVal { return String("host") }},
		{"macro", &userMacros, func() *LVal { return Quote(String("host")) }},
		{"builtin", &userBuiltins, func() *LVal { return String("host") }},
	}
	for _, kind := range kinds {
		for _, name := range names {
			t.Run(kind.name+"/"+name, func(t *testing.T) {
				saved := *kind.table
				defer func() { *kind.table = saved }()
				ret := kind.ret
				*kind.table = append(saved[:len(saved):len(saved)],
					&langBuiltin{name, Formals(VarArgSymbol, "args"), func(*LEnv, *LVal) *LVal { return ret() }, "host " + name})
				env := NewEnv(nil)
				var rc *LVal
				func() {
					defer func() {
						if r := recover(); r != nil {
							t.Fatalf("InitializeUserEnv panicked: %v", r)
						}
					}()
					rc = InitializeUserEnv(env)
				}()
				if rc.Type == LError {
					t.Fatal(rc)
				}
				got := env.Eval(SExpr([]*LVal{Symbol(name), String("x")}))
				if got.Type != LString || got.Str != "host" {
					t.Fatalf("host %s %s not installed: %v", kind.name, name, got)
				}
			})
		}
	}
}
