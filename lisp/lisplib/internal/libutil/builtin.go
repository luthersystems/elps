// Copyright © 2018 The ELPS authors

package libutil

import "github.com/luthersystems/elps/lisp"

func Function(name string, formals *lisp.LVal, fun lisp.LBuiltin) *Builtin {
	return &Builtin{name, formals, fun}
}

type Builtin struct {
	name    string
	formals *lisp.LVal
	fun     lisp.LBuiltin
}

func (fun *Builtin) Name() string {
	return fun.name
}

func (fun *Builtin) Formals() *lisp.LVal {
	return fun.formals
}

func (fun *Builtin) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return fun.fun(env, args)
}
