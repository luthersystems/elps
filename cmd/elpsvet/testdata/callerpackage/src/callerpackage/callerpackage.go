// Fixture for elpscallerpackage (issue #736).
package callerpackage

import "github.com/luthersystems/elps/lisp"

// builtinGetLiteral is a genuine builtin (LBuiltin's exact shape) that
// hardcodes an unqualified symbol name and looks it up through env.Get,
// which resolves in the CALLER's current package.
func builtinGetLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol("helper")) // want `builtinGetLiteral resolves the literal, unqualified symbol "helper" via env\.Get in the CALLER's current package`
}

func builtinGetGlobalLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.GetGlobal(lisp.Symbol("helper")) // want `builtinGetGlobalLiteral resolves the literal, unqualified symbol "helper" via env\.GetGlobal`
}

func builtinGetFunGlobalLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.GetFunGlobal(lisp.Symbol("helper")) // want `builtinGetFunGlobalLiteral resolves the literal, unqualified symbol "helper" via env\.GetFunGlobal`
}

func builtinPutGlobalLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	env.PutGlobal(lisp.Symbol("counter"), args) // want `builtinPutGlobalLiteral resolves the literal, unqualified symbol "counter" via env\.PutGlobal`
	return args
}

func builtinPutGlobalFromLispLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	env.PutGlobalFromLisp(lisp.Symbol("counter"), args) // want `builtinPutGlobalFromLispLiteral resolves the literal, unqualified symbol "counter" via env\.PutGlobalFromLisp`
	return args
}

// builtinQualifiedLiteral is clean: the literal already names a fixed
// package, so it does not depend on the caller's current package.
func builtinQualifiedLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol("otherpkg:helper"))
}

// builtinDynamicName is clean: the symbol name comes from args at run time,
// which is ordinary resolve-by-name behavior, not a hardcoded literal.
func builtinDynamicName(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol(args.Str))
}

// notABuiltin has an extra parameter, so its shape does not match LBuiltin
// and it is invisible to this rule even though it does the same lookup.
func notABuiltin(env *lisp.LEnv, args *lisp.LVal, extra string) *lisp.LVal {
	return env.Get(lisp.Symbol("helper"))
}

// packageGet is clean: Package.Get resolves within an already-specific
// package value, not the caller's ambient current package.
func packageGet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	pkg := env.Runtime.Registry.Package("otherpkg")
	return pkg.Get(lisp.Symbol("helper"))
}

// helperLookup is a plain helper (not itself builtin-shaped) that a builtin
// calls directly; its own hardcoded lookup is followed and attributed to the
// calling builtin.
func helperLookup(env *lisp.LEnv) *lisp.LVal {
	return env.Get(lisp.Symbol("helper")) // want `builtinCallsHelper resolves the literal, unqualified symbol "helper" via env\.Get`
}

func builtinCallsHelper(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return helperLookup(env)
}

// builtinAllowedTrailing is suppressed by a trailing marker on the flagged
// line.
func builtinAllowedTrailing(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol("helper")) //elpsvet:allow-callerpkg intentionally shared lisp namespace lookup
}

// builtinAllowedAbove is suppressed by a standalone marker on the line
// above.
func builtinAllowedAbove(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	//elpsvet:allow-callerpkg intentionally shared lisp namespace lookup
	return env.Get(lisp.Symbol("helper"))
}

// builtinAllowedDoc is suppressed by a justified marker in the function's
// own doc comment.
//
//elpsvet:allow-callerpkg intentionally shared lisp namespace lookup
func builtinAllowedDoc(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol("helper"))
}

// builtinAllowedTooShort carries a marker but no real justification (fewer
// than 3 words), so it still reports.
func builtinAllowedTooShort(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	//elpsvet:allow-callerpkg short
	return env.Get(lisp.Symbol("helper")) // want `builtinAllowedTooShort resolves the literal, unqualified symbol "helper" via env\.Get`
}

// registerAnonymous registers an anonymous builtin literal; the FuncLit
// shape is matched the same as a declaration.
func registerAnonymous() lisp.LBuiltin {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.Get(lisp.Symbol("helper")) // want `builtin resolves the literal, unqualified symbol "helper" via env\.Get`
	}
}
