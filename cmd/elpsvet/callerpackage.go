// Copyright © 2026 The ELPS authors

package main

// elpscallerpackage: a builtin must not resolve a hardcoded, unqualified
// symbol name against the AMBIENT current package (issue #736).
//
// WHY.  Calling a Lisp function (defun) switches env.Runtime.Package to that
// function's own package for the call, then restores the caller's on return
// (funCall, lisp/env.go).  A builtin -- any Go function with the LBuiltin
// signature, func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal -- gets NO such
// swap: it runs with *package* left exactly as the calling code set it. This
// is deliberate and load-bearing (set/defun/defmacro/s:deftype must bind into
// the CALLER's package, not the builtin's home package -- see "Symbols,
// packages, and the caller" in docs/lang.md), but it means a builtin that
// resolves a symbol against env.Runtime.Package as though it were its own
// package is silently reading whatever package the caller happened to have
// current, not a fixed home package.  A symbol whose text is chosen
// dynamically (from args, a caller-supplied name, or any other runtime
// value) is exactly the resolve-by-name behavior ELPS gives Lisp code
// (funcall on a quoted symbol, get, etc.) and is not what this rule is
// about. What IS a bug-shaped pattern is a call site that hardcodes a
// specific unqualified symbol NAME as a Go string literal and then looks it
// up through the ambient package (env.Get, env.GetGlobal, env.GetFunGlobal)
// or binds it there (env.PutGlobal, env.PutGlobalFromLisp) -- the literal
// reads as "the symbol named X", but which package X resolves in depends on
// the caller, which the literal's author is unlikely to have intended.
//
// WHAT IS A BUILTIN.  A *ast.FuncDecl or *ast.FuncLit whose Go signature is
// IDENTICAL to LBuiltin's underlying func type: exactly two parameters,
// *lisp.LEnv then *lisp.LVal, and one result, *lisp.LVal (a method's
// receiver is not part of this shape, so a builtin registered as a method
// value -- e.g. (*Serializer).DumpBytesBuiltin -- still matches on its two
// explicit parameters). This mirrors elpsbuiltinstate's registration-free
// approach: no constructor-name list to drift as constructors are added, at
// the cost of also matching a same-shaped helper that is never registered as
// a builtin. That's an intentional false-positive-shaped trade: LBuiltin's
// shape is distinctive enough in this codebase that nothing exercises it by
// accident.
//
// WHAT COUNTS AS A LOOKUP: a call whose callee resolves (via
// go/types.Info.Selections) to one of *lisp.LEnv's own methods Get,
// GetGlobal, GetFunGlobal, PutGlobal or PutGlobalFromLisp, whose first
// argument is itself a call to lisp.Symbol with a single, LITERAL string
// argument containing no ":" (a qualified name, "pkg:name", already names a
// fixed package and is exempt). Package.Get (a *Package method, not
// *LEnv's) is deliberately NOT matched: it looks a name up in an already
// resolved, specific package value, which does not depend on the caller's
// ambient *package* at all.
//
// HELPERS.  From each builtin's body, a direct call to a plain function or a
// concrete (non-interface) method declared in the SAME PACKAGE is followed
// into that callee's body too, transitively, guarding cycles with a
// per-analysis visited set. A call reached only through a variable,
// interface value, function-typed field or another package is invisible to
// this rule, as is any argument built by concatenation, a const or a var
// initialized elsewhere -- ONLY a literal string argument to lisp.Symbol is
// seen. That is a deliberate precision trade over completeness: the rule
// exists to catch a hardcoded name typed into a lookup call, and a fully
// dynamic-flow-sensitive version would both cost much more to write here and
// still miss a name built from a format string or table.
//
// SUPPRESSION: `//elpsvet:allow-callerpkg <justification>`, trailing on the
// reported line, standalone on the line above, or in the doc comment of the
// builtin or helper function whose body is being read when the call is
// found. As with allow-native and allow-shared, the justification must be at
// least three words.

import (
	"go/ast"
	"go/token"
	"go/types"
	"strconv"
	"strings"

	"golang.org/x/tools/go/analysis"
)

const (
	callerPkgAllowMarker   = "elpsvet:allow-callerpkg"
	callerPkgAllowMinWords = 3
	lEnvTypeName           = "LEnv"
	symbolFuncName         = "Symbol"
)

// callerPackageLookupMethods are the *lisp.LEnv methods that resolve or bind
// a symbol against env.Runtime.Package -- the package the CALLER left
// current, for a builtin.
var callerPackageLookupMethods = map[string]bool{
	"Get":               true,
	"GetGlobal":         true,
	"GetFunGlobal":      true,
	"PutGlobal":         true,
	"PutGlobalFromLisp": true,
}

var callerPackageAnalyzer = &analysis.Analyzer{
	Name: "elpscallerpackage",
	Doc: "flag a builtin (func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal) that resolves a hardcoded," +
		" unqualified symbol literal (lisp.Symbol(\"name\"), no \":\") through env.Get/GetGlobal/GetFunGlobal/" +
		"PutGlobal/PutGlobalFromLisp -- a builtin runs in its CALLER's package, not its own (issue #736)," +
		" so the literal silently resolves wherever the caller happens to be; qualify it (\"pkg:name\")," +
		" resolve it from a caller-supplied symbol instead, or annotate //elpsvet:allow-callerpkg <justification>",
	Run: runCallerPackage,
}

func justifiedCallerPkgAllow(text string) bool {
	return justifiedAllow(text, callerPkgAllowMarker, callerPkgAllowMinWords)
}

func hasJustifiedCallerPkgAllow(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if justifiedCallerPkgAllow(c.Text) {
			return true
		}
	}
	return false
}

type callerPackageRun struct {
	pass    *analysis.Pass
	allow   map[lineKey]bool
	decls   map[*types.Func]*ast.FuncDecl
	visited map[*types.Func]bool
}

func runCallerPackage(pass *analysis.Pass) (any, error) {
	r := &callerPackageRun{
		pass:    pass,
		allow:   make(map[lineKey]bool),
		decls:   make(map[*types.Func]*ast.FuncDecl),
		visited: make(map[*types.Func]bool),
	}
	for _, file := range pass.Files {
		name := pass.Fset.Position(file.Pos()).Filename
		for line := range markerLinesMatching(pass.Fset, file, justifiedCallerPkgAllow) {
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
					r.checkBody(x.Body, x.Name.Name, x.Doc)
				}
			case *ast.FuncLit:
				if tv, ok := pass.TypesInfo.Types[x]; ok {
					if sig, ok := types.Unalias(tv.Type).(*types.Signature); ok && isBuiltinSignature(sig) {
						r.checkBody(x.Body, "builtin", nil)
					}
				}
			}
			return true
		})
	}
	return nil, nil
}

// isBuiltinSignature reports whether sig has exactly LBuiltin's shape:
// func(*lisp.LEnv, *lisp.LVal) *lisp.LVal. The receiver, if any, is not part
// of sig's Params in go/types, so a method matches on its two explicit
// parameters alone.
func isBuiltinSignature(sig *types.Signature) bool {
	if sig == nil || sig.Variadic() {
		return false
	}
	params := sig.Params()
	results := sig.Results()
	if params.Len() != 2 || results.Len() != 1 {
		return false
	}
	return isNamedPtr(params.At(0).Type(), lEnvTypeName) &&
		isNamedPtr(params.At(1).Type(), "LVal") &&
		isNamedPtr(results.At(0).Type(), "LVal")
}

func isNamedPtr(t types.Type, name string) bool {
	ptr, ok := types.Unalias(t).(*types.Pointer)
	if !ok {
		return false
	}
	named, ok := types.Unalias(ptr.Elem()).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == name && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

// checkBody walks body reporting caller-package lookups, and follows a
// direct call to a same-package plain function or concrete method into its
// own body, transitively.
func (r *callerPackageRun) checkBody(body *ast.BlockStmt, rootName string, doc *ast.CommentGroup) {
	rootAllowed := hasJustifiedCallerPkgAllow(doc)
	ast.Inspect(body, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok {
			r.followHelper(call, rootName)
			return true
		}
		selInfo := r.pass.TypesInfo.Selections[sel]
		if selInfo == nil {
			r.followHelper(call, rootName)
			return true
		}
		fn, ok := selInfo.Obj().(*types.Func)
		if !ok {
			return true
		}
		if callerPackageLookupMethods[fn.Name()] && isLEnvMethod(fn) {
			r.checkLookup(call, fn.Name(), rootName, rootAllowed)
			return true
		}
		r.followHelper(call, rootName)
		return true
	})
}

func isLEnvMethod(fn *types.Func) bool {
	sig, ok := fn.Type().(*types.Signature)
	if !ok || sig.Recv() == nil {
		return false
	}
	return isNamedPtr(sig.Recv().Type(), lEnvTypeName) ||
		isNamedType(sig.Recv().Type(), lEnvTypeName)
}

func isNamedType(t types.Type, name string) bool {
	named, ok := types.Unalias(t).(*types.Named)
	if !ok {
		return false
	}
	obj := named.Obj()
	return obj.Name() == name && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath
}

// followHelper recurses into a direct, statically resolvable call to a
// function or concrete method declared in the package under analysis.
func (r *callerPackageRun) followHelper(call *ast.CallExpr, rootName string) {
	var fn *types.Func
	switch f := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		fn, _ = r.pass.TypesInfo.Uses[f].(*types.Func)
	case *ast.SelectorExpr:
		if sel := r.pass.TypesInfo.Selections[f]; sel != nil && sel.Kind() == types.MethodVal {
			fn, _ = sel.Obj().(*types.Func)
		} else if fn2, ok := r.pass.TypesInfo.Uses[f.Sel].(*types.Func); ok {
			fn = fn2 // package-qualified plain function
		}
	}
	if fn == nil || fn.Pkg() != r.pass.Pkg || r.visited[fn] {
		return
	}
	fd := r.decls[fn.Origin()]
	if fd == nil || fd.Body == nil {
		return
	}
	r.visited[fn] = true
	r.checkBody(fd.Body, rootName, fd.Doc)
}

func (r *callerPackageRun) checkLookup(call *ast.CallExpr, method, rootName string, rootAllowed bool) {
	if len(call.Args) == 0 {
		return
	}
	name, ok := literalSymbolName(r.pass, call.Args[0])
	if !ok || name == "" || strings.Contains(name, ":") {
		return
	}
	if rootAllowed {
		return
	}
	pos := r.pass.Fset.Position(call.Args[0].Pos())
	if r.allow[lineKey{pos.Filename, pos.Line}] {
		return
	}
	r.pass.Reportf(call.Args[0].Pos(),
		"%s resolves the literal, unqualified symbol %q via env.%s in the CALLER's current package,"+
			" not its own (issue #736: a builtin gets no package swap); qualify it (%q) if a fixed package"+
			" is meant, resolve it from a caller-supplied symbol instead, or annotate"+
			" //elpsvet:allow-callerpkg <justification>",
		rootName, name, method, name+":"+name)
}

// literalSymbolName reports the string literal argument of a lisp.Symbol(...)
// call, so ONLY a name typed directly into the call site is seen -- a
// variable, constant or computed string is invisible to this rule by design
// (see the header comment).
func literalSymbolName(pass *analysis.Pass, expr ast.Expr) (string, bool) {
	call, ok := ast.Unparen(expr).(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return "", false
	}
	var fn *types.Func
	switch f := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		fn, _ = pass.TypesInfo.Uses[f].(*types.Func)
	case *ast.SelectorExpr:
		fn, _ = pass.TypesInfo.Uses[f.Sel].(*types.Func)
	}
	if fn == nil || fn.Name() != symbolFuncName || fn.Pkg() == nil || fn.Pkg().Path() != lispPkgPath {
		return "", false
	}
	lit, ok := ast.Unparen(call.Args[0]).(*ast.BasicLit)
	if !ok || lit.Kind != token.STRING {
		return "", false
	}
	value, err := strconv.Unquote(lit.Value)
	if err != nil {
		return "", false
	}
	return value, true
}
