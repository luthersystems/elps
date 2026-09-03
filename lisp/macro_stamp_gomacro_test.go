// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// goMacroDef is a Go macro registered for the tests below.  Its expansion
// is captured on the way out so the test can inspect the very nodes the
// macro constructed.
type goMacroDef struct {
	name    string
	formals *lisp.LVal
	fun     lisp.LBuiltin
}

func (d *goMacroDef) Name() string                                    { return d.name }
func (d *goMacroDef) Formals() *lisp.LVal                             { return d.formals }
func (d *goMacroDef) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return d.fun(env, args) }

func newGoMacroEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestGoMacroExpansionIsLocatedInPlace pins the "who pays" half of the
// copy-on-write stamp (see the warning above stampMacroExpansion): a Go
// macro's expansion is fresh nodes and its arguments, by contract, so
// macroCall locates it IN PLACE and the stamp shares it instead of copying
// two allocations per node on every expansion -- the cost that regressed
// libjson's get-nested-baseline benchmark by 23% allocs/op on the first
// draft, and that substrate's logging macros would pay on every log line.
//
// Under a debugger the hand-off is skipped: the stamp copies the expansion
// and attaches the expansion metadata to the copies, and the macro's own
// nodes stay unlocated.
func TestGoMacroExpansionIsLocatedInPlace(t *testing.T) {
	run := func(t *testing.T, debugger lisp.Debugger) (fresh []*lisp.LVal, arg *lisp.LVal, result *lisp.LVal) {
		env := newGoMacroEnv(t)
		env.Runtime.Debugger = debugger
		// (m X) expands to (lisp:+ X (lisp:* 2 3)): four fresh nodes and
		// two fresh leaves around the caller's argument.
		def := &goMacroDef{name: "m", formals: lisp.Formals("x"), fun: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			arg = args.Cells[0]
			plus, times := lisp.Symbol("lisp:+"), lisp.Symbol("lisp:*")
			two, three := lisp.Int(2), lisp.Int(3)
			inner := lisp.SExpr([]*lisp.LVal{times, two, three})
			outer := lisp.SExpr([]*lisp.LVal{plus, arg, inner})
			fresh = []*lisp.LVal{outer, plus, inner, times, two, three}
			return outer
		}}
		env.AddMacros(true, def)
		result = env.LoadString("go-macro.lisp", "(m 10)")
		return fresh, arg, result
	}

	t.Run("no debugger: fresh nodes located in place, the argument untouched", func(t *testing.T) {
		fresh, arg, result := run(t, nil)
		if result.Type != lisp.LInt || result.Int != 16 {
			t.Fatalf("(m 10) = %v, want 16", result)
		}
		for i, n := range fresh {
			loc, ok := n.Source()
			if !ok {
				t.Errorf("fresh node %d (%v) was not located", i, n)
				continue
			}
			if loc.File != "go-macro.lisp" || loc.Line != 1 || loc.Col != 1 {
				t.Errorf("fresh node %d (%v) located at %s:%d:%d, want the macro call site go-macro.lisp:1:1", i, n, loc.File, loc.Line, loc.Col)
			}
		}
		// The argument is the reader's own node: sealed, located at its
		// real position, and shared with the expansion as it is.
		loc, ok := arg.Source()
		if !ok || loc.Col != 4 {
			t.Errorf("the argument's location was rewritten: %v", loc)
		}
	})

	t.Run("an unlocated runtime argument is a binding and is not located", func(t *testing.T) {
		// The call form is built at runtime, so the argument the macro
		// splices in is the raw binding l, not a sealed reader node.  That
		// is the #582 shape reaching a Go macro through its ARGUMENT --
		// from lisp via macroexpand-1, from Go via Eval of a runtime form
		// -- and the in-place locate must stop at it: the stamp copies it
		// instead, as it does for a binding a lisp macro returns.
		env := newGoMacroEnv(t)
		var fresh []*lisp.LVal
		def := &goMacroDef{name: "m", formals: lisp.Formals("x"), fun: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			plus := lisp.Symbol("lisp:+")
			outer := lisp.SExpr([]*lisp.LVal{plus, args.Cells[0], lisp.Int(1)})
			fresh = []*lisp.LVal{outer, plus, outer.Cells[2]}
			return outer
		}}
		env.AddMacros(true, def)
		if rc := env.LoadString("binding.lisp", "(set 'l (list 1 2))"); rc.Type == lisp.LError {
			t.Fatalf("fixture: %v", rc)
		}
		l := env.Runtime.Package.Get(lisp.Symbol("l"))
		if _, ok := l.Source(); ok || l.Type != lisp.LSExpr {
			t.Fatalf("fixture: want an unlocated runtime list, got %v", l)
		}
		got := env.LoadString("expand.lisp", "(macroexpand-1 (list 'm l))")
		if got.Type == lisp.LError {
			t.Fatalf("macroexpand-1: %v", got)
		}
		if _, ok := l.Source(); ok {
			t.Errorf("the binding passed as the macro's argument was located in place (issue #582): %v", l)
		}
		for i, n := range fresh {
			if _, ok := n.Source(); !ok {
				t.Errorf("fresh node %d (%v) was not located", i, n)
			}
		}
		// And the expansion the caller gets still carries the call site on
		// the binding's stand-in, so an error raised there is attributed.
		if got.Type != lisp.LSExpr || len(got.Cells) != 3 || got.Cells[1] == l {
			t.Fatalf("expansion %v: want (lisp:+ <copy of l> 1) with a private copy in the binding's place", got)
		}
		if loc, ok := got.Cells[1].Source(); !ok || loc.File != "expand.lisp" {
			t.Errorf("the binding's stand-in in the expansion is not stamped with the call site: %v", got.Cells[1])
		}
	})

	t.Run("debugger attached: the macro's nodes stay unlocated", func(t *testing.T) {
		fresh, _, result := run(t, dormantDebugger{})
		if result.Type != lisp.LInt || result.Int != 16 {
			t.Fatalf("(m 10) = %v, want 16", result)
		}
		for i, n := range fresh {
			if _, ok := n.Source(); ok {
				t.Errorf("fresh node %d (%v) was located in place under a debugger; the stamp's copy is where the debugger's metadata goes", i, n)
			}
		}
	})
}

// TestGoMacroExpansionValueIsNotLocatedInPlace pins that the in-place
// locate stops at a VALUE the Go macro spliced in: a function reached
// through the environment is a binding, and the stamp gives the expansion
// a private header copy of it instead (the value fix).
func TestGoMacroExpansionValueIsNotLocatedInPlace(t *testing.T) {
	env := newGoMacroEnv(t)
	if rc := env.LoadString("def.lisp", "(defun f (x) (lisp:* x 2))"); rc.Type == lisp.LError {
		t.Fatalf("fixture: %v", rc)
	}
	f := env.Runtime.Package.Get(lisp.Symbol("f"))
	f.SetSource(nil)
	def := &goMacroDef{name: "m", formals: lisp.Formals("x"), fun: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.SExpr([]*lisp.LVal{f, args.Cells[0]})
	}}
	env.AddMacros(true, def)
	if result := env.LoadString("go-macro.lisp", "(m 21)"); result.Type != lisp.LInt || result.Int != 42 {
		t.Fatalf("(m 21) = %v, want 42", result)
	}
	if _, ok := f.Source(); ok {
		t.Errorf("the function value the Go macro spliced in was located in place")
	}
}
