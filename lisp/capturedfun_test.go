// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// counterObj returns a CapturedFun over a fresh state map: (obj 'put k v)
// stores v and (obj 'get k) reads it, as a substrate storage object does.
func counterObj(name string) *lisp.LVal {
	return lisp.NewCapturedFun(lisp.CapturedFun{
		Package: lisp.DefaultUserPackage, FID: "test-obj", Name: name,
		Formals:  lisp.Formals("op", lisp.VarArgSymbol, "args"),
		Captures: lisp.SortedMap(),
		Eval: func(env *lisp.LEnv, args, state *lisp.LVal) *lisp.LVal {
			switch args.Cells[0].Str {
			case "put":
				if _, err := env.MapPut(state, args.Cells[1], args.Cells[2]); err != nil {
					return env.Errorf("%v", err)
				}
				return lisp.Nil()
			case "get":
				v, err := env.MapLookup(state, args.Cells[1])
				if err != nil {
					return lisp.Nil()
				}
				return v
			}
			return env.Errorf("unknown op %v", args.Cells[0])
		},
	})
}

func newCapturedFunEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

func TestCapturedFunPrintsName(t *testing.T) {
	for _, tc := range []struct{ name, want string }{
		{"utils:storage-counter", "#<utils:storage-counter>"},
		{"", "#<builtin>"},
	} {
		obj := counterObj(tc.name)
		if got := obj.String(); got != tc.want {
			t.Errorf("Name %q: prints %q, want %q", tc.name, got, tc.want)
		}
		if got, want := lisp.Quote(obj).String(), "'"+tc.want; got != want {
			t.Errorf("Name %q quoted: prints %q, want %q", tc.name, got, want)
		}
	}
}

func TestCapturedFunRequiresEval(t *testing.T) {
	got := lisp.NewCapturedFun(lisp.CapturedFun{Package: lisp.DefaultUserPackage, FID: "missing", Formals: lisp.Formals()})
	if got.Type != lisp.LError {
		t.Fatalf("nil Eval: got %v, want an error", got)
	}
}

// TestCapturedFunForks checks that each VM forked from a template, eager or
// lazy, gets its own copy of the object's state, and keeps its name.
func TestCapturedFunForks(t *testing.T) {
	for _, eager := range []bool{false, true} {
		env := newCapturedFunEnv(t)
		obj := counterObj("test:obj")
		if rc := env.PutGlobal(lisp.Symbol("obj"), obj); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		if rc := env.LoadString("t", `(obj 'put "k" 1)`); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		// The policy approves the core builtins only: a CapturedFun needs no
		// approval, because its state is declared.
		opts := []lisp.TemplateOption{lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.FID() != "test-obj" })}
		if eager {
			opts = append(opts, lisp.TemplateWithEagerInstantiation())
		}
		tpl, err := lisp.NewTemplate(env, opts...)
		if err != nil {
			t.Fatal(err)
		}
		a, err := tpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		b, err := tpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		if rc := a.LoadString("t", `(obj 'put "k" 2)`); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		for _, tc := range []struct {
			env  *lisp.LEnv
			want string
		}{{a, "2"}, {b, "1"}, {env, "1"}} {
			if got := tc.env.LoadString("t", `(obj 'get "k")`); got.String() != tc.want {
				t.Errorf("eager=%v: got %v, want %v", eager, got, tc.want)
			}
			if got := tc.env.LoadString("t", `(format-string "{}" obj)`); got.Str != "#<test:obj>" {
				t.Errorf("eager=%v: fork prints %v", eager, got)
			}
		}
	}
}

func TestCapturedFunRefusesBadName(t *testing.T) {
	for _, name := range []string{"a>b", "x> (lambda ()", "two words", "tab\tname", "\xff"} {
		got := lisp.NewCapturedFun(lisp.CapturedFun{Package: lisp.DefaultUserPackage, FID: "f", Name: name, Eval: func(*lisp.LEnv, *lisp.LVal, *lisp.LVal) *lisp.LVal { return lisp.Nil() }})
		if got.Type != lisp.LError {
			t.Errorf("Name %q: got %v, want an error", name, got)
		}
	}
}
