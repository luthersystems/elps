// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"testing"
)

// Tests for the function-cell sharing branch in forker.val.  A function's
// Cells are [formals, body...], written once by the constructor that
// allocated them and never grown or rewritten afterwards, so a fork whose
// children all map to themselves — the shape of every defun and lambda
// parsed from source, whose formals and body are sealed program nodes —
// can share the template's slice instead of allocating a fresh one to hold
// the same pointers.
//
// Package lisp cannot import the parser (import cycle), so these tests
// apply SealAST by hand exactly where the parser would, the same technique
// TestForkOwnership_SealedSharedAcrossRuntimes uses.

// sealedDefun evaluates (defun <name> (x) (+ x 1)) against a sealed
// expression tree, the shape the reader hands the evaluator, and returns
// the resulting function value as the package holds it.  Reading the symbol
// out of the package (rather than through env.Get) matters: Get hands back
// a FunRef header, and these tests are about the stored value's cells.
func sealedDefun(t testing.TB, env *LEnv, name string) *LVal {
	t.Helper()
	expr := SExpr([]*LVal{
		Symbol("defun"),
		Symbol(name),
		SExpr([]*LVal{Symbol("x")}),
		SExpr([]*LVal{Symbol("+"), Symbol("x"), Int(1)}),
	})
	expr.SealAST()
	if res := env.Eval(expr); res.Type == LError {
		t.Fatalf("defun %s: %v", name, res)
	}
	fun := packageSymbol(t, env, name)
	if fun.Type != LFun {
		t.Fatalf("%s is not a function: %v", name, fun.Type)
	}
	return fun
}

// packageSymbol returns the value the environment's current package stores
// under name.
func packageSymbol(t testing.TB, env *LEnv, name string) *LVal {
	t.Helper()
	pkg := env.Runtime.Registry.packages[env.Runtime.Package.Name]
	if pkg == nil {
		t.Fatalf("package %q not in registry", env.Runtime.Package.Name)
	}
	v, ok := pkg.Symbol(name)
	if !ok || v == nil {
		t.Fatalf("symbol %q not bound in package %q", name, pkg.Name)
	}
	return v
}

// TestForkSharesSealedFunctionCells pins the sharing branch: a function
// whose cells are all sealed program nodes forks with the template's cell
// slice, not a copy of it.  The value and its funData are still fresh —
// sharing the cells must not leak the closure environment — and the fork
// must still be callable.
func TestForkSharesSealedFunctionCells(t *testing.T) {
	env := newForkTestEnv(t)
	tmpl := sealedDefun(t, env, "f")

	// Anti-vacuity: the branch only fires when every cell maps to itself,
	// so the test means nothing unless the template's cells really are
	// sealed program nodes.
	if len(tmpl.Cells) != 2 {
		t.Fatalf("template function has %d cells, want 2 (formals, body)", len(tmpl.Cells))
	}
	for i, c := range tmpl.Cells {
		if !c.IsSealed() {
			t.Fatalf("template cell %d is not sealed (type %v); sharing branch cannot fire", i, c.Type)
		}
	}

	fork, err := forkTestSnapshot(env)
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	ffun := packageSymbol(t, fork, "f")

	// The function value itself is a fresh header, and so is its funData:
	// the closure environment is remapped onto the fork's tree.
	if ffun == tmpl {
		t.Fatalf("fork shares the template's function value")
	}
	if ffun.funData() == tmpl.funData() {
		t.Errorf("fork shares the template's funData")
	}
	if fd := ffun.funData(); fd != nil && fd.env != nil && fd.env.Runtime != fork.Runtime {
		t.Errorf("forked closure environment is not on the fork's runtime")
	}

	// The point of the change: one slice, not two.  This assertion fails on
	// a build without the sharing branch, where forker.val allocates a
	// fresh slice per function to hold the very same pointers.
	if len(ffun.Cells) != len(tmpl.Cells) {
		t.Fatalf("fork has %d cells, template %d", len(ffun.Cells), len(tmpl.Cells))
	}
	if &ffun.Cells[0] != &tmpl.Cells[0] {
		t.Errorf("fork allocated a new cell slice for an all-sealed function")
	}
	for i := range tmpl.Cells {
		if ffun.Cells[i] != tmpl.Cells[i] {
			t.Errorf("cell %d not pointer-shared with the template", i)
		}
	}
	// Shared backing, but no shared spare capacity: an append on either
	// side must reallocate rather than write the other side's array.
	if cap(ffun.Cells) != len(ffun.Cells) {
		t.Errorf("fork's shared cell slice has cap %d, len %d: spare capacity is appendable-through to the template", cap(ffun.Cells), len(ffun.Cells))
	}

	// Behavior: the shared cells are the program the fork actually runs.
	call := SExpr([]*LVal{Symbol("f"), Int(1)})
	call.SealAST()
	if res := fork.Eval(call); res.Type == LError || res.Int != 2 {
		t.Errorf("(f 1) in the fork = %v, want 2", res)
	}
	if res := env.Eval(call); res.Type == LError || res.Int != 2 {
		t.Errorf("(f 1) in the template after forking = %v, want 2", res)
	}
}

// TestForkCopiesUnsealedFunctionCells is the other half of the branch: a
// function built at runtime out of unsealed cells still gets a private
// slice, so neither side can write through into the other's cells.  It also
// runs the full fork audit, which asserts the copy policy over the whole
// reachable graph — builtins included, whose docstring cell is an unsealed
// String and therefore keeps them on the copy path.
func TestForkCopiesUnsealedFunctionCells(t *testing.T) {
	env := newForkTestEnv(t)
	// lambdaExpr builds (lambda (x) x) with constructors and never seals
	// it, so the resulting function's formals and body are ordinary mutable
	// values.
	fun := env.Eval(lambdaExpr())
	if fun.Type == LError {
		t.Fatalf("lambda: %v", fun)
	}
	env.PutGlobal(Symbol("g"), fun)
	tmpl := packageSymbol(t, env, "g")
	if tmpl.Type != LFun {
		t.Fatalf("g is not a function: %v", tmpl.Type)
	}
	for i, c := range tmpl.Cells {
		if c.IsSealed() {
			t.Fatalf("cell %d is sealed; this test needs an unsealed function", i)
		}
	}

	fork, err := forkTestSnapshot(env)
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	ffun := packageSymbol(t, fork, "g")
	if ffun == tmpl {
		t.Fatalf("fork shares the template's function value")
	}
	if len(ffun.Cells) != len(tmpl.Cells) {
		t.Fatalf("fork has %d cells, template %d", len(ffun.Cells), len(tmpl.Cells))
	}
	if sameCellsBacking(tmpl, ffun) {
		t.Errorf("fork shares the template's cell backing array for an unsealed function")
	}
	for i := range tmpl.Cells {
		if ffun.Cells[i] == tmpl.Cells[i] {
			t.Errorf("cell %d pointer-shared with the template", i)
		}
		if ffun.Cells[i].Type != tmpl.Cells[i].Type || ffun.Cells[i].Str != tmpl.Cells[i].Str {
			t.Errorf("cell %d differs: %v vs %v", i, ffun.Cells[i], tmpl.Cells[i])
		}
	}

	// The whole graph satisfies the fork sharing contract.  This runs
	// before the mutation below: the auditor compares copied values
	// field-by-field, so it has to see the fork as Fork left it.
	a := newForkAuditor(t)
	a.env("root", env, fork)
	oldReg, newReg := env.Runtime.Registry, fork.Runtime.Registry
	for name, opkg := range oldReg.packages {
		npkg, ok := newReg.packages[name]
		if !ok {
			t.Fatalf("package %q missing in fork", name)
		}
		for sym, ov := range opkg.symbols {
			a.val("pkg:"+name+":"+sym, ov, npkg.symbols[sym])
		}
	}
	if a.copied < 10 {
		t.Errorf("only %d mutable values copied; audit is vacuous", a.copied)
	}
	if t.Failed() {
		t.FailNow()
	}

	// Isolation, stated as a write: mutating the fork's body symbol must be
	// invisible to the template.
	body := ffun.Cells[len(ffun.Cells)-1]
	body.Str = "mutated-in-fork"
	if got := tmpl.Cells[len(tmpl.Cells)-1].Str; got != "x" {
		t.Errorf("fork's mutation reached the template: body symbol is now %q", got)
	}
}

// BenchmarkForkManyFunctions measures Fork on a template whose bulk is
// lisp-defined functions — the shape of a loaded production phylum, which
// is thousands of defuns and lambdas over sealed parser output.  It exists
// so the benchmark gate sees the per-function cell-slice cost directly
// rather than diluted across an env-wide fork.
func BenchmarkForkManyFunctions(b *testing.B) {
	env := newForkTestEnv(b)
	for i := range 500 {
		sealedDefun(b, env, fmt.Sprintf("fn-%03d", i))
	}
	tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
	if err != nil {
		b.Fatal(err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		if _, err := tmpl.NewVM(); err != nil {
			b.Fatalf("fork: %v", err)
		}
	}
}

// Spare capacity is observable mutable storage, even on a function whose
// visible children are sealed. Its exact view survives, but its backing is
// private; appends on either VM cannot overwrite the other's slots.
func TestForkFunctionCellsPreservePrivateCapacity(t *testing.T) {
	env := newForkTestEnv(t)
	source := sealedDefun(t, env, "f")
	source.Cells = append(make([]*LVal, 0, 5), source.Cells...)
	if cap(source.Cells) != 5 {
		t.Fatal("fixture lost explicit capacity")
	}
	fork, err := forkTestSnapshot(env)
	if err != nil {
		t.Fatal(err)
	}
	fun := packageSymbol(t, fork, "f")
	if &fun.Cells[0] == &source.Cells[0] || cap(fun.Cells) != cap(source.Cells) || len(fun.Cells) != len(source.Cells) {
		t.Fatal("function storage was shared or its view changed")
	}
	for i := range source.Cells {
		if fun.Cells[i] != source.Cells[i] {
			t.Fatal("sealed function code was copied")
		}
	}
	first, second := Symbol("fork-tag"), Symbol("source-tag")
	fun.Cells = append(fun.Cells, first)
	source.Cells = append(source.Cells, second)
	if fun.Cells[len(fun.Cells)-1] != first || source.Cells[len(source.Cells)-1] != second {
		t.Fatal("independent appends crossed VM boundary")
	}
}

// TestForkSharedFunctionCellsConcurrentCalls runs many forks of one
// template in parallel, each calling the shared-cell functions (positional,
// optional, keyword and rest formals, a docstring, a macro and a closure)
// under the race detector.  Sharing the cell slice must introduce no
// read-write race between forks or against the template.
func TestForkSharedFunctionCellsConcurrentCalls(t *testing.T) {
	env := newForkTestEnv(t)
	for _, src := range []string{
		"(defun pos (a b) (+ a b))",
		"(defun opt (a &optional b) (+ a (if b b 10)))",
		"(defun key (a &key k) (* a (if k k 3)))",
		"(defun rest (a &rest xs) (foldl + a xs))",
		`(defun doc (x) "adds one" (+ x 1))`,
		"(defmacro twice (e) (quasiquote (+ (unquote e) (unquote e))))",
		"(defun mk (n) (lambda (x) (+ x n)))",
	} {
		expr, err := parseTestSource(src)
		if err != nil {
			t.Fatalf("parse %q: %v", src, err)
		}
		if res := env.Eval(expr); res.Type == LError {
			t.Fatalf("eval %q: %v", src, res)
		}
	}
	probe, err := parseTestSource("(list (pos 1 2) (opt 1) (opt 1 2) (key 2) (key 2 :k 5) (rest 1 2 3) (doc 1) (twice 4) (funcall (mk 3) 4))")
	if err != nil {
		t.Fatalf("parse probe: %v", err)
	}
	want := env.Eval(probe)
	if want.Type == LError {
		t.Fatalf("probe in template: %v", want)
	}

	const forks, iters = 8, 25
	template, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
	if err != nil {
		t.Fatal(err)
	}
	var wg sync.WaitGroup
	errs := make(chan string, forks)
	for range forks {
		wg.Add(1)
		go func() {
			defer wg.Done()
			fork, err := template.NewVM()
			if err != nil {
				errs <- fmt.Sprintf("fork: %v", err)
				return
			}
			for range iters {
				got := fork.Eval(probe)
				if got.Type == LError {
					errs <- fmt.Sprintf("probe in fork: %v", got)
					return
				}
				if eq := got.Equal(want); eq.Type != LSymbol || eq.Str != TrueSymbol {
					errs <- fmt.Sprintf("fork result %v, want %v", got, want)
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for e := range errs {
		t.Error(e)
	}
	if got := env.Eval(probe); got.Type == LError {
		t.Fatalf("probe in template after forks: %v", got)
	} else if eq := got.Equal(want); eq.Type != LSymbol || eq.Str != TrueSymbol {
		t.Errorf("template result changed after forks: %v, want %v", got, want)
	}
}

// parseTestSource reads one form of a small lisp subset (lists, symbols,
// keywords, integers, double-quoted strings and 'quote) into a sealed
// expression tree, the shape the real reader hands the evaluator.  Package
// lisp cannot import the parser, so the tests here build their programs
// with this instead.
func parseTestSource(src string) (*LVal, error) {
	toks := strings.Fields(strings.NewReplacer("(", " ( ", ")", " ) ", "'", " ' ").Replace(src))
	// Re-join string literals the whitespace split broke apart.
	var joined []string
	for i := 0; i < len(toks); i++ {
		t := toks[i]
		if strings.HasPrefix(t, "\"") && (!strings.HasSuffix(t, "\"") || len(t) == 1) {
			parts := []string{t}
			for i+1 < len(toks) {
				i++
				parts = append(parts, toks[i])
				if strings.HasSuffix(toks[i], "\"") {
					break
				}
			}
			t = strings.Join(parts, " ")
		}
		joined = append(joined, t)
	}
	pos := 0
	var read func() (*LVal, error)
	read = func() (*LVal, error) {
		if pos >= len(joined) {
			return nil, errors.New("unexpected end of input")
		}
		t := joined[pos]
		pos++
		switch {
		case t == "(":
			var cells []*LVal
			for {
				if pos >= len(joined) {
					return nil, errors.New("unterminated list")
				}
				if joined[pos] == ")" {
					pos++
					return SExpr(cells), nil
				}
				c, err := read()
				if err != nil {
					return nil, err
				}
				cells = append(cells, c)
			}
		case t == ")":
			return nil, errors.New("unexpected )")
		case t == "'":
			q, err := read()
			if err != nil {
				return nil, err
			}
			return SExpr([]*LVal{Symbol("quote"), q}), nil
		case strings.HasPrefix(t, "\""):
			return String(strings.Trim(t, "\"")), nil
		default:
			if n, err := strconv.Atoi(t); err == nil {
				return Int(n), nil
			}
			return Symbol(t), nil
		}
	}
	v, err := read()
	if err != nil {
		return nil, err
	}
	if pos != len(joined) {
		return nil, fmt.Errorf("trailing input after form: %q", joined[pos:])
	}
	v.SealAST()
	return v, nil
}
