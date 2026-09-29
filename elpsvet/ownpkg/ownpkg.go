// Copyright © 2026 The ELPS authors

// Package ownpkg is the elpsownpkg analyzer: a library builtin must not
// depend on which package is current (issue #736).
//
// THE RULE IT KEEPS TRUE.  A name resolves in the package of the code doing
// the lookup.  Core lisp (package github.com/luthersystems/elps/lisp) acts in
// the caller's package: set, defun, in-package, funcall with a quoted name and
// the rest are the language.  Every other Go builtin or Go macro runs in its
// OWN package while it runs, like a Lisp function defined there
// (lisp/env.go, the builtin branch of LEnv.call).  So inside a library
// builtin, env.Runtime.Package is the library's package, which a library
// package usually defines without importing lisp.  An operation that depends
// on the current package is therefore either a bug (it was written for the
// caller's package, which a library builtin cannot see) or at best a lookup
// in the library's own package that a qualified name or a Go value states
// more plainly.
//
// WHAT IS A LIBRARY BUILTIN.  In any package other than lisp itself, a
// function declaration or literal whose signature is exactly lisp.LBuiltin's,
// func(*lisp.LEnv, *lisp.LVal) *lisp.LVal (a method matches on its explicit
// parameters), or the captured-builtin shape func(*lisp.LEnv, *lisp.LVal,
// *lisp.LVal) *lisp.LVal.  The match is by type, not by registration, like
// elpsvet's elpsbuiltinstate.  From a builtin's body, a direct call to a
// function or concrete method declared in the same package is followed into
// that callee's body, transitively.  Registration code (a package's
// LoadPackage, which runs with its package current on purpose) has a
// different signature and is never read.
//
// WHAT IS REPORTED, as calls on a *lisp.LEnv:
//
//   - evaluating or loading code: Eval*, Load*, MacroCall, SpecialOpCall;
//   - building a closure: Lambda, which stamps the current package into the
//     function it returns (and closes over the caller's lexical scope,
//     which a builtin still receives: only the package switches);
//   - handing back an expression: Terminal, which the caller evaluates in
//     the caller's package after the switch is undone, like a macro
//     expansion;
//   - changing or writing the current package: InPackage, UsePackage,
//     SetPackageDoc, SetSymbolDoc, AddBuiltins, AddMacros, AddSpecialOps;
//   - resolving or binding a symbol: Get, GetGlobal, GetFun, GetFunGlobal,
//     Put, PutGlobal, PutGlobalFromLisp, Update -- unless the symbol is a
//     literal qualified name, lisp.Symbol("pkg:name"), or a keyword -- and
//     CallGlobal, unless its name is a literal "pkg:name" string.  A
//     symbol that arrives as an argument is exactly the hazard: a quoted
//     name the caller passed resolves in the library, not in the caller.
//
// and any read of the Package field of a lisp.Runtime.
//
// WHAT IS NOT SEEN, so a clean run is evidence, not proof: calls through a
// variable, an interface, a function-typed field or another package (a
// helper in another package, or another builtin's Go code invoked directly
// through LBuiltinDef.Eval, which also bypasses the package switch);
// reflection; and anything a builtin reaches through a Lisp value it calls
// back (env.FunCall of a caller's function is fine: that function runs in its
// own package).
//
// THE FIX, in order of preference: take a value instead of a name (a
// function value rather than a quoted symbol; the caller evaluates it in its
// own package), qualify a fixed name ("pkg:name"), or turn the builtin into a
// Go macro whose expansion uses core forms, which are evaluated in the
// caller.  When the operation is intended -- it deliberately works in the
// library's own package, or in a package named explicitly -- annotate it:
//
//	//elpsvet:allow-ownpkg <justification of at least three words>
//
// trailing on the reported line, alone on the line above, or in the doc
// comment of the builtin or helper whose body is being read.
package ownpkg

import (
	"go/ast"
	"go/token"
	"go/types"
	"strconv"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// LispPkgPath is the import path of the core language package, the one
// package whose builtins act in the caller's package.
const LispPkgPath = "github.com/luthersystems/elps/lisp"

const (
	allowMarker   = "elpsvet:allow-ownpkg"
	allowMinWords = 3
)

// Analyzer reports package-sensitive operations inside library builtins.
var Analyzer = &analysis.Analyzer{
	Name: "elpsownpkg",
	Doc: "flag operations inside a library builtin (a Go function with lisp.LBuiltin's signature outside package lisp) " +
		"that depend on the current package: evaluating or loading code, Lambda, InPackage, reading Runtime.Package, " +
		"or resolving a symbol that is not a literal qualified name.  A library builtin runs in its own package " +
		"(issue #736), so these act there, never in the caller's; take a value instead of a name, qualify the name, " +
		"or annotate //elpsvet:allow-ownpkg <justification>",
	Run: run,
}

// callMethods are the *lisp.LEnv methods reported whatever their arguments.
var callMethods = map[string]string{
	"Lambda":        "builds a closure stamped with the current package",
	"Terminal":      "hands back an expression the caller evaluates in the caller's package",
	"InPackage":     "changes the current package",
	"UsePackage":    "imports into the current package",
	"SetPackageDoc": "writes the current package",
	"SetSymbolDoc":  "writes the current package",
	"AddBuiltins":   "registers into the current package",
	"AddMacros":     "registers into the current package",
	"AddSpecialOps": "registers into the current package",
	"MacroCall":     "expands a macro in the current package",
	"SpecialOpCall": "evaluates code in the current package",
}

// symbolMethods are the *lisp.LEnv methods whose first argument is a symbol
// resolved or bound in the current package unless it is qualified.
var symbolMethods = map[string]bool{
	"Get":               true,
	"GetGlobal":         true,
	"GetFun":            true,
	"GetFunGlobal":      true,
	"Put":               true,
	"PutGlobal":         true,
	"PutGlobalFromLisp": true,
	"Update":            true,
}

// nameMethods are the *lisp.LEnv methods whose first argument is a symbol
// NAME, a string, resolved in the current package unless it is qualified.
var nameMethods = map[string]bool{
	"CallGlobal": true,
}

// qualifiedString reports whether expr is a string literal naming a
// qualified symbol ("pkg:name") or a keyword.
func qualifiedString(expr ast.Expr) bool {
	lit, ok := ast.Unparen(expr).(*ast.BasicLit)
	if !ok || lit.Kind != token.STRING {
		return false
	}
	value, err := strconv.Unquote(lit.Value)
	return err == nil && strings.Contains(value, ":")
}

// Justified reports whether a comment's text is a suppression carrying a
// justification of at least three words.
func Justified(text string) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	text = strings.TrimSuffix(text, "*/")
	text = strings.TrimSpace(text)
	rest, ok := strings.CutPrefix(text, allowMarker)
	if !ok || rest == "" || (rest[0] != ' ' && rest[0] != '\t') {
		return false
	}
	return len(strings.Fields(rest)) >= allowMinWords
}

type lineKey struct {
	file string
	line int
}

type runState struct {
	pass    *analysis.Pass
	allow   map[lineKey]bool
	decls   map[*types.Func]*ast.FuncDecl
	visited map[*types.Func]bool
}

func run(pass *analysis.Pass) (any, error) {
	if pass.Pkg.Path() == LispPkgPath {
		return nil, nil // core: acts in the caller's package by design
	}
	r := &runState{
		pass:    pass,
		allow:   make(map[lineKey]bool),
		decls:   make(map[*types.Func]*ast.FuncDecl),
		visited: make(map[*types.Func]bool),
	}
	for _, file := range pass.Files {
		name := pass.Fset.Position(file.Pos()).Filename
		for line := range markerLines(pass.Fset, file) {
			r.allow[lineKey{name, line}] = true
		}
		for _, decl := range file.Decls {
			if fd, ok := decl.(*ast.FuncDecl); ok {
				if fn, ok := pass.TypesInfo.Defs[fd.Name].(*types.Func); ok {
					r.decls[fn] = fd
				}
			}
		}
	}
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.FuncDecl:
				if x.Body == nil {
					return true
				}
				if fn, ok := pass.TypesInfo.Defs[x.Name].(*types.Func); ok && isBuiltinSignature(fn.Signature()) {
					r.visited[fn] = true
					r.checkBody(x.Body, x.Name.Name, justifiedDoc(x.Doc))
				}
			case *ast.FuncLit:
				if tv, ok := pass.TypesInfo.Types[x]; ok {
					if sig, ok := types.Unalias(tv.Type).(*types.Signature); ok && isBuiltinSignature(sig) {
						r.checkBody(x.Body, "builtin", false)
					}
				}
			}
			return true
		})
	}
	return nil, nil
}

func justifiedDoc(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if Justified(c.Text) {
			return true
		}
	}
	return false
}

// isBuiltinSignature reports whether sig is func(*lisp.LEnv, *lisp.LVal)
// *lisp.LVal or func(*lisp.LEnv, *lisp.LVal, *lisp.LVal) *lisp.LVal.
func isBuiltinSignature(sig *types.Signature) bool {
	if sig == nil || sig.Variadic() {
		return false
	}
	params, results := sig.Params(), sig.Results()
	if (params.Len() != 2 && params.Len() != 3) || results.Len() != 1 {
		return false
	}
	if !isLispPtr(params.At(0).Type(), "LEnv") || !isLispPtr(results.At(0).Type(), "LVal") {
		return false
	}
	for i := 1; i < params.Len(); i++ {
		if !isLispPtr(params.At(i).Type(), "LVal") {
			return false
		}
	}
	return true
}

func isLispPtr(t types.Type, name string) bool {
	ptr, ok := types.Unalias(t).(*types.Pointer)
	if !ok {
		return false
	}
	return isLispNamed(ptr.Elem(), name)
}

func isLispNamed(t types.Type, name string) bool {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == name && obj.Pkg() != nil && obj.Pkg().Path() == LispPkgPath
}

func (r *runState) checkBody(body *ast.BlockStmt, root string, rootAllowed bool) {
	ast.Inspect(body, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncLit:
			// A nested literal of builtin shape is checked on its own
			// (run visits it); any other closure is part of this body.
			if tv, ok := r.pass.TypesInfo.Types[x]; ok {
				if sig, ok := types.Unalias(tv.Type).(*types.Signature); ok && isBuiltinSignature(sig) {
					return false
				}
			}
		case *ast.SelectorExpr:
			if sel := r.pass.TypesInfo.Selections[x]; sel != nil && sel.Kind() == types.FieldVal {
				if v, ok := sel.Obj().(*types.Var); ok && v.Name() == "Package" && isLispRecv(sel.Recv(), "Runtime") {
					r.report(x.Sel.Pos(), rootAllowed, "%s reads Runtime.Package", root)
				}
			}
		case *ast.CallExpr:
			r.checkCall(x, root, rootAllowed)
		}
		return true
	})
}

func isLispRecv(t types.Type, name string) bool {
	if ptr, ok := types.Unalias(t).(*types.Pointer); ok {
		t = ptr.Elem()
	}
	return isLispNamed(t, name)
}

func (r *runState) checkCall(call *ast.CallExpr, root string, rootAllowed bool) {
	sel, ok := ast.Unparen(call.Fun).(*ast.SelectorExpr)
	if ok {
		if s := r.pass.TypesInfo.Selections[sel]; s != nil && s.Kind() == types.MethodVal && isLispRecv(s.Recv(), "LEnv") {
			name := sel.Sel.Name
			switch {
			case strings.HasPrefix(name, "Eval") || strings.HasPrefix(name, "Load"):
				r.report(sel.Sel.Pos(), rootAllowed, "%s calls env.%s, which evaluates code in the current package", root, name)
			case callMethods[name] != "":
				r.report(sel.Sel.Pos(), rootAllowed, "%s calls env.%s, which %s", root, name, callMethods[name])
			case nameMethods[name]:
				if len(call.Args) > 0 && !qualifiedString(call.Args[0]) {
					r.report(sel.Sel.Pos(), rootAllowed,
						"%s calls env.%s on a name that is not a literal qualified name, so it resolves in the current package", root, name)
				}
			case symbolMethods[name]:
				if len(call.Args) > 0 && !r.qualifiedLiteral(call.Args[0]) {
					r.report(sel.Sel.Pos(), rootAllowed,
						"%s calls env.%s on a symbol that is not a literal qualified name, so it resolves in the current package", root, name)
				}
			}
			return
		}
	}
	r.follow(call, root)
}

// qualifiedLiteral reports whether expr is lisp.Symbol("pkg:name") or a
// keyword, lisp.Symbol(":name"): a name that does not depend on the current
// package.
func (r *runState) qualifiedLiteral(expr ast.Expr) bool {
	call, ok := ast.Unparen(expr).(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return false
	}
	var fn *types.Func
	switch f := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		fn, _ = r.pass.TypesInfo.Uses[f].(*types.Func)
	case *ast.SelectorExpr:
		fn, _ = r.pass.TypesInfo.Uses[f.Sel].(*types.Func)
	}
	if fn == nil || fn.Name() != "Symbol" || fn.Pkg() == nil || fn.Pkg().Path() != LispPkgPath {
		return false
	}
	lit, ok := ast.Unparen(call.Args[0]).(*ast.BasicLit)
	if !ok || lit.Kind != token.STRING {
		return false
	}
	value, err := strconv.Unquote(lit.Value)
	if err != nil {
		return false
	}
	return strings.Contains(value, ":")
}

// follow reads a direct call to a function or concrete method declared in
// the package under analysis, transitively.
func (r *runState) follow(call *ast.CallExpr, root string) {
	var fn *types.Func
	switch f := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		fn, _ = r.pass.TypesInfo.Uses[f].(*types.Func)
	case *ast.SelectorExpr:
		if sel := r.pass.TypesInfo.Selections[f]; sel != nil {
			if sel.Kind() == types.MethodVal {
				fn, _ = sel.Obj().(*types.Func)
			}
		} else {
			fn, _ = r.pass.TypesInfo.Uses[f.Sel].(*types.Func)
		}
	}
	if fn == nil || fn.Pkg() != r.pass.Pkg {
		return
	}
	fn = fn.Origin()
	if r.visited[fn] {
		return
	}
	fd := r.decls[fn]
	if fd == nil || fd.Body == nil {
		return
	}
	r.visited[fn] = true
	r.checkBody(fd.Body, root, justifiedDoc(fd.Doc))
}

func (r *runState) report(pos token.Pos, allowed bool, format string, args ...any) {
	if allowed {
		return
	}
	p := r.pass.Fset.Position(pos)
	if r.allow[lineKey{p.Filename, p.Line}] {
		return
	}
	r.pass.Reportf(pos, format+" (a library builtin runs in its own package, issue #736): take a value instead of a name,"+
		" qualify the name, or annotate //elpsvet:allow-ownpkg <justification>", args...)
}

// markerLines returns the lines a justified marker covers: its own line, and
// the next when the marker stands alone.
func markerLines(fset *token.FileSet, file *ast.File) map[int]bool {
	code := make(map[int]bool)
	ast.Inspect(file, func(n ast.Node) bool {
		switch n.(type) {
		case nil, *ast.CommentGroup, *ast.Comment:
			return false
		}
		code[fset.Position(n.Pos()).Line] = true
		code[fset.Position(n.End()).Line] = true
		return true
	})
	lines := make(map[int]bool)
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if !Justified(c.Text) {
				continue
			}
			line := fset.Position(c.Pos()).Line
			lines[line] = true
			if !code[line] {
				lines[line+1] = true
			}
		}
	}
	return lines
}
