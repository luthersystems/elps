package libx

import "github.com/luthersystems/elps/lisp"

type Builtin struct {
	name string
	fun  lisp.LBuiltin
}

func (b *Builtin) Name() string { return b.name }

func (b *Builtin) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return b.fun(env, args) }

func Function(name string, fun lisp.LBuiltin) *Builtin { return &Builtin{name, fun} }

var closure lisp.LBuiltin = func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return nil }

var plain = Function("plain", okCells)

var defs = []lisp.LBuiltinDef{
	lisp.DropsArgList(Function("ok-cells", okCells)),
	lisp.DropsArgList(Function("ok-methods", okMethods)),
	lisp.DropsArgList(lisp.FreeKeywords(Function("ok-wrapped", okCells))),
	lisp.DropsArgList(Function("bad-reader", badReader)),
	lisp.DropsArgList(Function("bad-method", badMethod)),
	lisp.DropsArgList(Function("closure", closure)),             // want "not a named top-level function"
	lisp.DropsArgList(plain),                                    // want "needs the call that builds the definition"
	lisp.DropsArgList(Function("lisp-func", lisp.ReadArgsLike)), // want "marks github.com/luthersystems/elps/lisp.ReadArgsLike, which may keep its argument list"
	lisp.DropsArgList(Function("lisp-ok", lisp.CellBuiltin)),
}

func okCells(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { // want okCells:"callListSafe\\[1\\]"
	return lisp.CellOf(args, 0)
}

// Methods of package lisp are read through their facts.
func okMethods(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { // want okMethods:"callListSafe\\[1\\]"
	if args.Len() == 0 {
		return nil
	}
	return args.Arg(0)
}

func badReader(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lisp.ReadArgs(env, args) // want "passed to github.com/luthersystems/elps/lisp.ReadArgs, which this rule cannot read or which may keep it .reached from badReader, which DropsArgList marks"
	return nil
}

func badMethod(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	args.Keep() // want "Keep, which this rule cannot read or which may keep it"
	return nil
}
