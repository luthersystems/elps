// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strings"
	"testing"
)

// bindNativeShapes are the formal lists the fast-path tests bind against.
// fast marks the shapes bindNativePositional may take; every other shape,
// including the malformed ones a low-level constructor (Fun) accepts without
// validation, must fall back to bindGeneral.
var bindNativeShapes = []struct {
	formals []string
	fast    bool
}{
	{nil, true},
	{[]string{"a"}, true},
	{[]string{"a", "b"}, true},
	{[]string{VarArgSymbol, "r"}, true},
	{[]string{"a", VarArgSymbol, "r"}, true},
	{[]string{"a", "b", VarArgSymbol, "r"}, true},
	{[]string{OptArgSymbol, "o"}, false},
	{[]string{"a", OptArgSymbol, "o"}, false},
	{[]string{"a", OptArgSymbol, "o", VarArgSymbol, "r"}, false},
	{[]string{KeyArgSymbol, "k"}, false},
	{[]string{"a", KeyArgSymbol, "k"}, false},
	{[]string{"a", KeyArgSymbol, "k", "j"}, false},
	{[]string{VarArgSymbol, "r", KeyArgSymbol, "k"}, false},
	{[]string{"a", VarArgSymbol}, false},
	{[]string{"a", VarArgSymbol, "r", "b"}, false},
	{[]string{VarArgSymbol, KeyArgSymbol}, false},
	{[]string{VarArgSymbol, VarArgSymbol, "r"}, false},
	{[]string{"&bogus"}, false},
	{[]string{"a", "&whole", "w"}, false},
	{[]string{OptArgSymbol}, false},
	{[]string{KeyArgSymbol}, false},
}

// bindNativeArgLists are the argument lists bound against every shape:
// every positional count up to four, and keyword-shaped lists so the &key
// shapes reach their success and error branches.
func bindNativeArgLists() [][]*LVal {
	lists := [][]*LVal{}
	for n := range 5 {
		args := make([]*LVal, n)
		for i := range args {
			args[i] = Int(i + 1)
		}
		lists = append(lists, args)
	}
	return append(lists,
		[]*LVal{Symbol(":k"), Int(1)},
		[]*LVal{Int(1), Symbol(":k"), Int(2)},
		[]*LVal{Int(1), Symbol(":x"), Int(2)},
		[]*LVal{Int(1), Symbol(":k")},
		[]*LVal{Int(1), Int(2), Int(3)},
		[]*LVal{Int(1), String("k"), Int(2)},
	)
}

// rawFormals builds a formal list without Formals' misplaced-&rest check,
// the way a low-level constructor may be handed one, so the malformed shapes
// reach the binder as written.
func rawFormals(names ...string) *LVal {
	cells := make([]*LVal, len(names))
	for i, name := range names {
		cells[i] = Symbol(name)
	}
	return QExpr(cells)
}

func describeBind(v *LVal) string {
	if v == nil {
		return "<nil>"
	}
	if v.Type == LError {
		return fmt.Sprintf("error condition=%q %s", v.Str, v.String())
	}
	return fmt.Sprintf("%v len=%d cap=%d quoted=%v %s", v.Type, len(v.Cells), cap(v.Cells), v.quoted, v.String())
}

// TestBindNativePositionalMatchesGeneral checks, for every shape and
// argument list, that bind returns exactly what the general binder returns
// (same env, same argument list or the same error), that the fast path is
// taken whenever the shape and arity allow it and never otherwise, and that
// the list it builds is a copy the builtin owns.
func TestBindNativePositionalMatchesGeneral(t *testing.T) {
	env := initSafetyTestEnv(t)
	builtin := func(*LEnv, *LVal) *LVal { return Nil() }
	for _, shape := range bindNativeShapes {
		for _, list := range bindNativeArgLists() {
			name := fmt.Sprintf("(%s)/%d-args", strings.Join(shape.formals, " "), len(list))
			t.Run(name, func(t *testing.T) {
				fun := Fun("bind-native-test", rawFormals(shape.formals...), builtin)
				args := QExpr(list)
				before := append([]*LVal(nil), list...)

				genEnv, gen := env.bindGeneral(fun, args)
				gotEnv, got := env.bind(fun, args)
				fast := bindNativePositional(fun.Cells[0].Cells, args.Cells)

				if gotEnv != genEnv {
					t.Errorf("bind env %p, general env %p", gotEnv, genEnv)
				}
				if describeBind(got) != describeBind(gen) {
					t.Fatalf("bind and general binder disagree:\n  bind:    %s\n  general: %s", describeBind(got), describeBind(gen))
				}
				if got.Type != LError {
					for i := range got.Cells {
						if got.Cells[i] != gen.Cells[i] {
							t.Errorf("argument %d is a different value from the general binder's", i)
						}
					}
				}

				ok := gen.Type != LError
				switch {
				case fast != nil && !shape.fast:
					t.Fatalf("fast path taken for a shape it must not handle: %s", describeBind(fast))
				case fast != nil && !ok:
					t.Fatalf("fast path accepted a call the general binder rejects (%s)", describeBind(gen))
				case fast == nil && shape.fast && ok:
					t.Fatalf("fast path declined a positional call it can bind")
				case fast != nil && describeBind(fast) != describeBind(gen):
					t.Fatalf("fast path %s, general %s", describeBind(fast), describeBind(gen))
				}

				for i := range list {
					if list[i] != before[i] {
						t.Fatalf("bind modified the caller's argument %d", i)
					}
				}
				if ok && len(got.Cells) > 0 {
					got.Cells[0] = Symbol("clobbered")
					if len(list) > 0 && list[0] != before[0] {
						t.Fatal("bound argument list aliases the caller's backing array")
					}
				}
			})
		}
	}
}

// TestBindNativePositionalArityErrors pins the exact error text bind gives a
// host function called with the wrong number of arguments, on each shape the
// fast path handles.  Those calls fall back to the general binder, so these
// messages must stay exactly as they were.
func TestBindNativePositionalArityErrors(t *testing.T) {
	env := initSafetyTestEnv(t)
	builtin := func(*LEnv, *LVal) *LVal { return Nil() }
	for _, tc := range []struct {
		formals []string
		narg    int
		want    string
	}{
		{nil, 1, "invalid number of arguments: 1"},
		{[]string{"a"}, 0, "invalid number of arguments: 0"},
		{[]string{"a"}, 2, "invalid number of arguments: 2"},
		{[]string{"a", "b"}, 1, "invalid number of arguments: 1"},
		{[]string{"a", "b"}, 3, "invalid number of arguments: 3"},
		{[]string{"a", VarArgSymbol, "r"}, 0, "invalid number of arguments: 0"},
		{[]string{"a", "b", VarArgSymbol, "r"}, 1, "invalid number of arguments: 1"},
	} {
		args := make([]*LVal, tc.narg)
		for i := range args {
			args[i] = Int(i)
		}
		_, got := env.bind(Fun("arity", rawFormals(tc.formals...), builtin), QExpr(args))
		if got.Type != LError || got.Str != "error" || got.Cells[0].Str != tc.want {
			t.Errorf("formals %v with %d args: got %s, want error %q", tc.formals, tc.narg, describeBind(got), tc.want)
		}
	}
}

// TestBindLambdaUnaffected checks the fast path is confined to host
// functions: a lambda with purely positional formals still gets a fresh
// lexical environment with its parameters bound, and no argument list.
func TestBindLambdaUnaffected(t *testing.T) {
	env := initSafetyTestEnv(t)
	fun := env.Lambda(Formals("a", VarArgSymbol, "r"), []*LVal{Symbol("a")})
	if fun.Type == LError {
		t.Fatal(fun)
	}
	fenv, list := env.bind(fun, QExpr([]*LVal{Int(1), Int(2), Int(3)}))
	if list != nil {
		t.Fatalf("lambda bind returned an argument list: %s", describeBind(list))
	}
	if fenv == env || fenv == nil {
		t.Fatal("lambda bind did not build a call environment")
	}
	if a := fenv.Get(Symbol("a")); a.Type != LInt || a.Int != 1 {
		t.Errorf("a = %v", a)
	}
	if r := fenv.Get(Symbol("r")); r.String() != "'(2 3)" {
		t.Errorf("r = %v", r)
	}
}

// TestBindMalformedFunctionFailsLikeGeneral checks that bind reads a function
// value in the same order as the general binder, so a malformed value an
// embedder builds by hand (no formals cell, no function data) fails with the
// same panic whichever path would have handled it.
func TestBindMalformedFunctionFailsLikeGeneral(t *testing.T) {
	env := initSafetyTestEnv(t)
	try := func(bind func(fun, args *LVal) (*LEnv, *LVal), fun, args *LVal) (msg string) {
		defer func() {
			if r := recover(); r != nil {
				msg = fmt.Sprint(r)
			}
		}()
		_, list := bind(fun, args)
		return describeBind(list)
	}
	one := QExpr([]*LVal{Int(1)})
	for name, fun := range map[string]*LVal{
		"NoCells":         {Type: LFun},
		"NilFormalsCell":  {Type: LFun, Cells: []*LVal{nil}},
		"NoFunctionData":  {Type: LFun, Cells: []*LVal{Formals("a")}},
		"NilFormalSymbol": {Type: LFun, Cells: []*LVal{QExpr([]*LVal{nil})}},
	} {
		t.Run(name, func(t *testing.T) {
			got := try(env.bind, fun, one)
			want := try(env.bindGeneral, fun, one)
			if got != want {
				t.Errorf("bind: %s\ngeneral: %s", got, want)
			}
		})
	}
}
