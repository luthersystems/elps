package idiomcase

import "github.com/luthersystems/elps/lisp"

func builtinUpper(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lisp.String(args.Cells[0].Str + args.Cells[1].Str)
}

func builtinCount(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lisp.Int(len(args.Cells))
}

func builtinPass(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return helperOf(args)
}

func helperOf(v *lisp.LVal) *lisp.LVal { return v }

func builtinBeyond(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return args.Cells[2]
}

func fixedArity() []*lisp.LVal {
	return []*lisp.LVal{
		lisp.FunInPackage("p", "upper", lisp.Formals("s", "t"), builtinUpper), // want `builtinUpper takes 2 required arguments; lisp.Func2E with Go types`
		lisp.FunInPackage("p", "one", lisp.Formals("v"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { // want `the builtin takes 1 required arguments; lisp.Func1E`
			return args.Cells[0]
		}),
		// Not reported: optional formals, len(args.Cells), args passed on,
		// an index past the count, and four formals.
		lisp.FunInPackage("p", "upper2", lisp.Formals("s", lisp.OptArgSymbol, "t"), builtinUpper),
		lisp.FunInPackage("p", "count", lisp.Formals("a"), builtinCount),
		lisp.FunInPackage("p", "pass", lisp.Formals("a"), builtinPass),
		lisp.FunInPackage("p", "beyond", lisp.Formals("a", "b"), builtinBeyond),
		lisp.FunInPackage("p", "four", lisp.Formals("a", "b", "c", "d"), builtinUpper),
	}
}
