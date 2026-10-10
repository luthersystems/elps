// Package lisp is a stub of the core package for the elpsbuiltinstate
// fixtures.
package lisp

type LVal struct{ Str string }

type Runtime struct{}

type LEnv struct{ Runtime *Runtime }

type LBuiltin func(env *LEnv, args *LVal) *LVal

func Fun(fid string, formals *LVal, fn LBuiltin) *LVal { return &LVal{Str: fid} }
