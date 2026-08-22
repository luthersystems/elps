// Copyright © 2018 The ELPS authors

package libutil

import "github.com/luthersystems/elps/lisp"

// Function constructs a Builtin definition.  The formals list is sealed
// (lisp.LVal.SealAST) as part of construction: lisplib definition tables are
// package-level values shared by every Runtime in the process, and sealing
// lets lisp.LEnv.AddBuiltins alias the sealed list into each environment
// under copy-on-write protection instead of deep-copying it per environment
// (see registrationFormals in lisp/env.go).  Callers must therefore pass a
// freshly constructed formals list (every table in this repository builds
// one inline with lisp.Formals) and must not mutate it after construction.
func Function(name string, formals *lisp.LVal, fun lisp.LBuiltin) *Builtin {
	formals.SealAST()
	return &Builtin{name, formals, fun, ""}
}

// FunctionDoc constructs a documented Builtin definition.  The formals list
// is sealed as part of construction — see Function.
func FunctionDoc(name string, formals *lisp.LVal, fun lisp.LBuiltin, docs string) *Builtin {
	formals.SealAST()
	return &Builtin{name, formals, fun, docs}
}

type Builtin struct {
	name    string
	formals *lisp.LVal
	fun     lisp.LBuiltin
	docs    string
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

func (fun *Builtin) Docstring() string {
	return fun.docs
}
