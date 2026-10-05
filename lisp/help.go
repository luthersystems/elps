// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"

	"github.com/luthersystems/elps/internal/helpdoc"
)

// opHelp is lisp:help.  It is core, so like set or function it acts in the
// caller's package (issue #736): (help my-fn) documents the my-fn the caller
// sees, lexical bindings included, and an unqualified name's symbol doc comes
// from the caller's current package.  It used to be help:help, which only
// worked because builtins had no package of their own; once a library builtin
// runs in its own package, a help defined in package help would look names up
// there.
//
// The format is internal/helpdoc's, shared with the help package's
// help-package, so the two cannot drift.
func opHelp(env *LEnv, args *LVal) *LVal {
	name := args.Cells[0]
	if name.Type != LSymbol {
		return env.Errorf("argument is not a symbol: %v", GetType(name))
	}
	v := env.Get(Symbol(name.Str))
	if v.Type == LError {
		return env.Error(GoError(v))
	}
	doc := helpSymbolDoc(env, name.Str)
	var err error
	if v.Type != LFun {
		err = helpdoc.WriteVal(env.Runtime.getStderr(), helpdoc.ValueDoc{TypeName: GetType(v).Str, Name: name.Str, Rendered: env.Render(v), Doc: doc})
	} else {
		sig := SExpr(make([]*LVal, 1+v.Cells[0].Len()))
		sig.Cells[0] = Symbol(name.Str)
		copy(sig.Cells[1:], v.Cells[0].Cells)
		err = helpdoc.WriteFun(env.Runtime.getStderr(), helpdoc.FunctionDoc{FunType: v.FunType.String(), Signature: env.Render(sig), Docstring: v.Docstring(), SymbolDoc: doc})
	}
	if err != nil {
		return env.Error(err)
	}
	return Nil()
}

// helpSymbolDoc is the symbol documentation help prints for sym: a qualified
// name's from the package it names, an unqualified one's from the current
// package.  It is libhelp.LookupSymbolDoc, which predates lisp:help and stays
// exported there for embedders.
func helpSymbolDoc(env *LEnv, sym string) string {
	if pkgName, symName, ok := strings.Cut(sym, ":"); ok {
		if env.Runtime.Registry == nil {
			return ""
		}
		if pkg := env.Runtime.Registry.Package(pkgName); pkg != nil {
			return pkg.SymbolDoc(symName)
		}
		return ""
	}
	if env.Runtime.Package == nil {
		return ""
	}
	return env.Runtime.Package.SymbolDoc(sym)
}
