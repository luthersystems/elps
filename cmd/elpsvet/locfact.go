// Copyright © 2026 The ELPS authors

// Cross-package freshness summaries for *token.Location results.
//
// The escape rule (escape.go) is intraprocedural: it cannot see inside a
// callee, so its first approximation treated EVERY *token.Location-returning
// method on a receiver the caller did not construct as a taint source.  That
// is right for an accessor handing out the receiver's own pointer
// (rdparser.Parser.Location) and wrong — a false positive — for an accessor
// that allocates: lisp.LEnv.Source returns copyLocation(env.loc), and
// token.Scanner.LocStart mints a &Location{...} per token.  Both are the
// SANCTIONED way to read a location, so an embedder calling the correct API
// must not have to annotate; the mitigation cannot be an //elps:aliases at
// every call site.
//
// go/analysis solves exactly this with Facts.  This file computes, for every
// function and method in the package under analysis whose sole result is a
// *token.Location, the boolean summary "every value this can return is
// freshly allocated", and exports it as a freshLocation fact.  callLocTaint
// consults the fact at the call site — for callees in the package under
// analysis and, because facts are serialized along the import graph, for
// callees anywhere in the dependency tree.  lisp.LEnv.Source is therefore
// clean in lsp/, in mcpserver/, and in any embedder, with no annotation.
//
// The summary itself is intraprocedural, which keeps the design honest: the
// analysis WITHIN a body stays the same shape as the rest of elpsvet
// (source-order walk, last-assignment-wins, path-insensitive), and only the
// one-bit summary crosses the package boundary.
//
// A result is provably fresh when it is:
//
//   - nil;
//   - &token.Location{...} or new(token.Location) — memory this call allocated;
//   - &local where local is a value-typed token.Location variable or
//     parameter of this function (the deref-copy idiom, cp := *loc;
//     return &cp — Go value semantics already gave the function its own
//     struct); a package-level var is deliberately excluded, its address is
//     shared;
//   - the result of calling another function already proven fresh
//     (copyLocation, and anything built only out of it).
//
// Everything else — a field read, a parameter handed straight back, a call
// with no fact — leaves the summary FALSE, and the call site falls back to
// the conservative treatment.  The property is proven, never assumed: a
// function with a single leaking return keeps no fact at all, and one that
// bare-returns a named result is not analysed (the assignment to the result
// is not tracked).
//
// Mutual and self recursion are handled by a least fixpoint seeded with
// nothing proven: a function is marked fresh only once every one of its
// returns is fresh given what is ALREADY proven, so knowledge grows
// monotonically and a cycle can never bootstrap itself into a fact.
package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/types/typeutil"
)

// freshLocation is the exported fact: the object it is attached to is a
// function or method whose sole *token.Location result is always freshly
// allocated, so storing that result into an escaping value is safe.
type freshLocation struct{}

func (*freshLocation) AFact() {}

func (*freshLocation) String() string { return "freshLocation" }

// exportFreshLocationFacts computes and exports the freshLocation fact for
// every eligible function in the package under analysis.  It must run before
// the escape checks so that in-package call sites can consult it.
func exportFreshLocationFacts(pass *analysis.Pass) {
	type candidate struct {
		fn   *types.Func
		body *ast.BlockStmt
	}
	var candidates []candidate
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			fd, ok := decl.(*ast.FuncDecl)
			if !ok || fd.Body == nil {
				continue
			}
			fn, _ := pass.TypesInfo.Defs[fd.Name].(*types.Func)
			if fn == nil || !returnsLocationPtr(fn) {
				continue
			}
			candidates = append(candidates, candidate{fn, fd.Body})
		}
	}
	if len(candidates) == 0 {
		return
	}
	// Least fixpoint from "nothing proven".  Each round can only ADD to
	// proven, so the loop terminates in at most len(candidates) rounds and a
	// recursive cycle never proves itself.
	proven := make(map[*types.Func]bool)
	for changed := true; changed; {
		changed = false
		for _, c := range candidates {
			if proven[c.fn] {
				continue
			}
			t := &locFresher{pass: pass, proven: proven, fresh: make(map[types.Object]bool)}
			if t.bodyReturnsFresh(c.body) {
				proven[c.fn] = true
				changed = true
			}
		}
	}
	for fn := range proven {
		pass.ExportObjectFact(fn, new(freshLocation))
	}
}

// freshLocationCallee reports whether fn carries the freshLocation fact —
// in-package (exported by this pass) or imported from a dependency.
func freshLocationCallee(pass *analysis.Pass, fn *types.Func) bool {
	if fn == nil {
		return false
	}
	var fact freshLocation
	return pass.ImportObjectFact(fn, &fact)
}

// returnsLocationPtr reports whether fn's signature has exactly one result
// and that result is *token.Location.  Multi-result accessors are out of
// scope: (*lisp.LVal).Source returns (token.Location, bool), a VALUE copy
// that cannot alias anything, so the escape rule never treats it as a
// location source in the first place.
func returnsLocationPtr(fn *types.Func) bool {
	sig, ok := fn.Type().(*types.Signature)
	if !ok {
		return false
	}
	return sig.Results().Len() == 1 && isLocationPtr(sig.Results().At(0).Type())
}

// locFresher proves the "every return is freshly allocated" property for one
// function body.
//
// Local *token.Location variables are handled FLOW-INSENSITIVELY: a local is
// fresh only when EVERY expression ever assigned to it is fresh.  The rest
// of elpsvet uses a cheaper source-order, last-assignment-wins walk, which
// is fine for deciding whether to raise a warning but would be unsound here,
// where the answer is a claim of proof exported to other packages: in
//
//	for _, tok := range toks {
//	    if tok.stop { return loc }   // reached on iteration 2 with the
//	    loc = tok.Source             // PREVIOUS iteration's value
//	}
//
// a source-order walk sees the return before the assignment and would call
// loc fresh.  Requiring every assignment to be fresh removes the ordering
// question entirely.  It costs nothing real — a location local that is first
// aliased and then overwritten with a copy is not a shape any accessor uses,
// and refusing to prove it merely leaves the call site conservatively
// flagged.
type locFresher struct {
	pass   *analysis.Pass
	proven map[*types.Func]bool  // in-package functions proven so far this fixpoint
	fresh  map[types.Object]bool // *token.Location locals holding fresh memory
}

// bodyReturnsFresh reports whether every return statement in body yields
// freshly allocated memory.
func (t *locFresher) bodyReturnsFresh(body *ast.BlockStmt) bool {
	t.resolveLocals(body)
	ok := true
	ast.Inspect(body, func(n ast.Node) bool {
		if !ok {
			return false
		}
		switch stmt := n.(type) {
		case *ast.FuncLit:
			// A nested literal's returns belong to the literal, not to the
			// function being summarised.
			return false
		case *ast.ReturnStmt:
			// A bare return in a function with a named result returns
			// whatever was assigned to that result, which this analysis does
			// not track.  Refuse to prove anything.
			if len(stmt.Results) != 1 || !t.isFresh(stmt.Results[0]) {
				ok = false
				return false
			}
		}
		return true
	})
	return ok
}

// resolveLocals fills t.fresh with the *token.Location locals every one of
// whose assignments is fresh, by least fixpoint over the whole body —
// including inside nested function literals, which can assign to a captured
// local and must not be able to launder taint past this pass.
func (t *locFresher) resolveLocals(body *ast.BlockStmt) {
	assigns := make(map[types.Object][]ast.Expr) // a nil element == untrackable
	declared := make(map[types.Object]bool)
	// declare marks a location local as seen without recording an
	// assignment; record adds one (rhs nil when the analysis cannot see the
	// assigned expression, which permanently sinks the local).
	declare := func(id *ast.Ident) types.Object {
		if id == nil || id.Name == "_" {
			return nil
		}
		obj := t.objectOf(id)
		if obj == nil || !isLocationPtr(obj.Type()) {
			return nil
		}
		declared[obj] = true
		return obj
	}
	record := func(id *ast.Ident, rhs ast.Expr) {
		if obj := declare(id); obj != nil {
			assigns[obj] = append(assigns[obj], rhs)
		}
	}
	ident := func(e ast.Expr) *ast.Ident {
		id, _ := ast.Unparen(e).(*ast.Ident)
		return id
	}
	ast.Inspect(body, func(n ast.Node) bool {
		switch stmt := n.(type) {
		case *ast.AssignStmt:
			// x, y := f(): the per-value expressions are unavailable.
			tuple := len(stmt.Rhs) == 1 && len(stmt.Lhs) > 1
			for i, lhs := range stmt.Lhs {
				if tuple || i >= len(stmt.Rhs) {
					record(ident(lhs), nil)
					continue
				}
				record(ident(lhs), stmt.Rhs[i])
			}
		case *ast.ValueSpec:
			switch {
			case len(stmt.Values) == 0:
				// `var loc *token.Location`: the zero value is nil, which
				// aliases nothing.  Declare it, record no assignment.
				for _, name := range stmt.Names {
					declare(name)
				}
			case len(stmt.Values) == len(stmt.Names):
				for i, name := range stmt.Names {
					record(name, stmt.Values[i])
				}
			default: // var a, b = f()
				for _, name := range stmt.Names {
					record(name, nil)
				}
			}
		case *ast.RangeStmt:
			// for loc = range ... rebinds from storage this pass cannot see.
			for _, e := range []ast.Expr{stmt.Key, stmt.Value} {
				if e != nil {
					record(ident(e), nil)
				}
			}
		case *ast.UnaryExpr:
			// &loc hands a **token.Location to something that may rebind loc
			// out of sight.  Refuse to prove such a local.
			if stmt.Op == token.AND {
				record(ident(stmt.X), nil)
			}
		}
		return true
	})
	// Least fixpoint from "nothing proven": a local becomes fresh only once
	// every expression assigned to it is fresh given what is already known,
	// so an assignment cycle (a = b; b = a) never proves itself.
	for changed := true; changed; {
		changed = false
		for obj := range declared {
			if t.fresh[obj] {
				continue
			}
			allFresh := true
			for _, rhs := range assigns[obj] {
				if rhs == nil || !t.isFresh(rhs) {
					allFresh = false
					break
				}
			}
			if allFresh {
				t.fresh[obj] = true
				changed = true
			}
		}
	}
}

// isFresh reports whether e evaluates to a *token.Location this call
// allocated.
func (t *locFresher) isFresh(e ast.Expr) bool {
	e = ast.Unparen(e)
	switch x := e.(type) {
	case *ast.Ident:
		obj := t.objectOf(x)
		if _, isNil := obj.(*types.Nil); isNil {
			return true // nil aliases nothing
		}
		return obj != nil && t.fresh[obj]
	case *ast.UnaryExpr:
		if x.Op != token.AND {
			return false
		}
		switch inner := ast.Unparen(x.X).(type) {
		case *ast.CompositeLit:
			return isLocationValue(t.pass.TypesInfo.TypeOf(inner))
		case *ast.Ident:
			// &local / &param: Go value semantics already gave this call its
			// own token.Location struct.  A package-level var is excluded —
			// its address is process-wide shared storage.
			v, _ := t.objectOf(inner).(*types.Var)
			return v != nil && isLocationValue(v.Type()) && !isPackageScoped(v)
		}
		return false
	case *ast.CallExpr:
		return t.callIsFresh(x)
	}
	return false
}

func (t *locFresher) callIsFresh(call *ast.CallExpr) bool {
	// new(token.Location)
	if id, ok := ast.Unparen(call.Fun).(*ast.Ident); ok && id.Name == "new" {
		if _, ok := t.pass.TypesInfo.Uses[id].(*types.Builtin); ok && len(call.Args) == 1 {
			return isLocationValue(t.pass.TypesInfo.TypeOf(call.Args[0]))
		}
	}
	fn, ok := typeutil.Callee(t.pass.TypesInfo, call).(*types.Func)
	if !ok {
		return false
	}
	if t.proven[fn] {
		return true
	}
	return freshLocationCallee(t.pass, fn)
}

func (t *locFresher) objectOf(id *ast.Ident) types.Object {
	if obj := t.pass.TypesInfo.Defs[id]; obj != nil {
		return obj
	}
	return t.pass.TypesInfo.Uses[id]
}

func isPackageScoped(v *types.Var) bool {
	return v.Pkg() != nil && v.Parent() == v.Pkg().Scope()
}
