package libcase

import "github.com/luthersystems/elps/lisp"

func builtinEval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Eval(args) // want `builtinEval calls env.Eval, which evaluates code in the current package`
}

func builtinLoad(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.LoadString("x", "(+ 1 2)") // want `calls env.LoadString`
}

func builtinLambda(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Lambda(lisp.Nil(), nil) // want `calls env.Lambda, which builds a closure stamped with the current package`
}

func builtinInPackage(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.InPackage(args) // want `calls env.InPackage, which changes the current package`
}

func builtinTerminal(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Terminal(args) // want `calls env.Terminal, which hands back an expression the caller evaluates in the caller's package`
}

func builtinPackageRead(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lisp.Symbol(env.Runtime.Package.Name) // want `builtinPackageRead reads Runtime.Package`
}

func builtinArgLookup(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.GetFun(args) // want `calls env.GetFun on a symbol that is not a literal qualified name`
}

func builtinUnqualifiedLiteral(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(lisp.Symbol("thing")) // want `calls env.Get on a symbol`
}

// Qualified names and keywords do not depend on the current package.
func builtinQualified(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	env.Get(lisp.Symbol(":keyword"))
	return env.Get(lisp.Symbol("json:null"))
}

// CallGlobal resolves its name like env.Get: only a literal qualified name
// is independent of the current package.
func builtinCallGlobalUnqualified(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.CallGlobal("helper", args) // want `calls env.CallGlobal on a name that is not a literal qualified name`
}

func builtinCallGlobalComputed(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.CallGlobal(args.Str) // want `calls env.CallGlobal on a name that is not a literal qualified name`
}

func builtinCallGlobalQualified(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.CallGlobal("utils:helper", args)
}

// Calling back a function value is fine: it runs in its own package.
func builtinCallback(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.FunCall(args, lisp.Nil())
}

// A same-package helper a builtin calls is read too.
func builtinViaHelper(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return helper(env, args, 1)
}

func helper(env *lisp.LEnv, v *lisp.LVal, _ int) *lisp.LVal {
	return env.Eval(v) // want `builtinViaHelper calls env.Eval`
}

// Registration code has another signature and runs in its package on
// purpose: not read.
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	env.InPackage(lisp.Symbol("libcase"))
	return env.PutGlobal(lisp.Symbol("x"), lisp.Nil())
}

// Literals of builtin shape, and the captured-builtin shape, are builtins.
var table = []lisp.LBuiltin{
	func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.Eval(args) // want `builtin calls env.Eval`
	},
}

func captured(env *lisp.LEnv, args, captures *lisp.LVal) *lisp.LVal {
	return env.Eval(captures) // want `captured calls env.Eval`
}

// Methods match on their explicit parameters.
type suite struct{}

func (s *suite) Builtin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Lambda(args, nil) // want `Builtin calls env.Lambda`
}

// Suppressions: trailing, on the line above, and in the doc comment; a
// marker with a short justification does not count.
func builtinAllowed(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	env.Eval(args) //elpsvet:allow-ownpkg evaluates in its own package deliberately
	//elpsvet:allow-ownpkg the library's own package is intended here
	env.Eval(args)
	//elpsvet:allow-ownpkg too short
	env.Eval(args) // want `builtinAllowed calls env.Eval`
	return nil
}

// builtinDocAllowed reads its own package on purpose.
//
//elpsvet:allow-ownpkg documented own-package lookup for this builtin
func builtinDocAllowed(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return env.Get(args)
}

// An ordinary function is not a builtin.
func notBuiltin(env *lisp.LEnv) {
	env.Eval(lisp.Nil())
}

// The body passed to a typed binding is a builtin body, whatever its
// signature.
var builtinFuncE = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	return env.Eval(args), nil // want `builtin calls env.Eval`
})

var builtinFunc1 = lisp.Func1(lisp.StringArg("argument"), func(env *lisp.LEnv, s string) *lisp.LVal {
	return env.CallGlobal(s) // want `builtin calls env.CallGlobal on a name`
})

var builtinFunc1Value = lisp.Func1(lisp.ValueArg(), func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
	return env.GetFun(v) // want `builtin calls env.GetFun on a symbol`
})

var builtinFunc2Decl = lisp.Func2(lisp.StringArg("first argument"), lisp.StringArg("second argument"), typedBody)

func typedBody(env *lisp.LEnv, a, b string) *lisp.LVal {
	return env.Get(lisp.Symbol(a)) // want `typedBody calls env.Get on a symbol`
}

var builtinFuncEDecl = lisp.FuncE(errorBody)

func errorBody(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	return env.InPackage(args), nil // want `errorBody calls env.InPackage`
}

// A clean typed body reports nothing.
var builtinFuncEClean = lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
	return env.Get(lisp.Symbol("json:null")), nil
})

// A FuncE body nested in a builtin is reported once.
func builtinNested(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	inner := lisp.FuncE(func(env *lisp.LEnv, args *lisp.LVal) (*lisp.LVal, error) {
		return env.Eval(args), nil // want `builtin calls env.Eval`
	})
	return inner(env, args)
}

var builtinFunc1E = lisp.Func1E(func(env *lisp.LEnv, s string) (string, error) {
	return env.CallGlobal(s).Str, nil // want `builtin calls env.CallGlobal on a name`
})

var builtinFunc2E = lisp.Func2E(twoArgs)

func twoArgs(env *lisp.LEnv, a string, n int) (*lisp.LVal, error) {
	return env.Lambda(lisp.Nil(), nil), nil // want `twoArgs calls env.Lambda`
}
