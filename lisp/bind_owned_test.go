// Copyright © 2026 The ELPS authors

package lisp

import (
	"runtime"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// TestGoMacroMayRewriteItsArgumentList: macroCall reads a macro's argument
// list after the call.  The arguments bound the in-place locate of a Go
// macro's expansion, and a debugger records them.  So a Go macro gets a copy
// of the list, not the list the evaluator built (bindOwned), and a macro
// that consumes one argument and overwrites another does not move that
// boundary: the binding it splices in stays unlocated (the issue #582 shape).
func TestGoMacroMayRewriteItsArgumentList(t *testing.T) {
	env := initSafetyTestEnv(t)
	env.AddMacros(true, &langBuiltin{
		name:    "take-first",
		formals: Formals("first", VarArgSymbol, "rest"),
		fun: func(env *LEnv, args *LVal) *LVal {
			first := args.Cells[0]
			args.Cells[1] = Int(0)
			args.Cells = args.Cells[1:]
			return SExpr([]*LVal{Symbol("lisp:quote"), first})
		},
		docs: "Test macro: consumes its argument list.",
	})
	// data is a binding: a runtime list with no location.  The call form is
	// located, so the expansion is located in place up to the arguments.
	data := QExpr([]*LVal{Int(1), Int(2)})
	form := SExpr([]*LVal{Symbol("take-first"), data, Int(3)})
	form.source = &token.Location{File: "rewrite.lisp", Line: 7, Col: 1, Pos: 10}
	if got := env.Eval(form); got.IsError() {
		t.Fatalf("(take-first data 3): %v", got)
	}
	if data.source != nil {
		t.Errorf("the binding the macro spliced in was located in place at %v (issue #582)", data.source)
	}
	for i, c := range data.Cells {
		if c.source != nil {
			t.Errorf("cell %d of the binding was located in place at %v (issue #582)", i, c.source)
		}
	}
}

// retainedBytes reports the heap that n values made by build keep alive,
// per value.
func retainedBytes(n int, build func() *LVal) float64 {
	keep := make([]*LVal, n)
	var before, after runtime.MemStats
	runtime.GC()
	runtime.GC()
	runtime.ReadMemStats(&before)
	for i := range keep {
		keep[i] = build()
	}
	runtime.GC()
	runtime.GC()
	runtime.ReadMemStats(&after)
	runtime.KeepAlive(keep)
	if after.HeapAlloc < before.HeapAlloc {
		return 0
	}
	return float64(after.HeapAlloc-before.HeapAlloc) / float64(n)
}

// TestListAndVectorKeepNoCallStorage: a program often stores what list and
// vector return.  The evaluator builds a call form's argument list in one
// allocation with the call header and the function slot (newSExprCap) and
// hands it to the builtin as it is (bindOwned).  A result built over those
// cells would keep that whole allocation alive, about twice the memory of a
// list built directly.  A stored result keeps at most the function slot and
// the allocation's spare cells beyond what the same value built directly
// keeps.
func TestListAndVectorKeepNoCallStorage(t *testing.T) {
	env := initSafetyTestEnv(t)
	const n = 20000
	// The elements are shared, so only the list storage counts.
	one, two := Int(1), Int(2)
	for _, tc := range []struct {
		name   string
		form   *LVal
		direct func() *LVal
	}{
		{"list", SExpr([]*LVal{Symbol("list"), one, two}), func() *LVal { return QExpr([]*LVal{one, two}) }},
		{"vector", SExpr([]*LVal{Symbol("vector"), one, two}), func() *LVal { return Vector([]*LVal{one, two}) }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if v := env.Eval(tc.form); v.IsError() {
				t.Fatal(v)
			}
			got := retainedBytes(n, func() *LVal { return env.Eval(tc.form) })
			want := retainedBytes(n, tc.direct)
			if got > want+32 {
				t.Errorf("a stored result keeps %.1f bytes, a value built directly %.1f", got, want)
			}
		})
	}
}

// TestListCalledFromGo: a Go caller that calls list's builtin directly may
// pass an s-expression it built.  list returns a new header over its cells,
// marked as data, so the result evaluates to itself.
func TestListCalledFromGo(t *testing.T) {
	env := initSafetyTestEnv(t)
	var def LBuiltinDef
	for _, d := range DefaultBuiltins() {
		if d.Name() == "list" {
			def = d
		}
	}
	in := SExpr([]*LVal{Symbol("+"), Int(1), Int(2)})
	out := def.Eval(env, in)
	if out == in {
		t.Fatal("list returned the caller's header")
	}
	if got := env.Eval(out); got != out {
		t.Errorf("list's result evaluates to %v, not to itself", got)
	}
	if got := out.String(); got != "'(+ 1 2)" {
		t.Errorf("list's result is %s, want '(+ 1 2)", got)
	}
}
