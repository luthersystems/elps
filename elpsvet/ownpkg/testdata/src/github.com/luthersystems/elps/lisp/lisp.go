// Package lisp is a stub of the core package for the elpsownpkg fixtures.
package lisp

type LVal struct{ Str string }

type Package struct{ Name string }

type Runtime struct{ Package *Package }

type LEnv struct{ Runtime *Runtime }

type LBuiltin func(env *LEnv, args *LVal) *LVal

func Symbol(s string) *LVal { return &LVal{Str: s} }
func Nil() *LVal            { return &LVal{} }

func (env *LEnv) Eval(v *LVal) *LVal                    { return v }
func (env *LEnv) LoadString(name, src string) *LVal     { return nil }
func (env *LEnv) Lambda(formals *LVal, b []*LVal) *LVal { return nil }
func (env *LEnv) InPackage(name *LVal) *LVal            { return nil }
func (env *LEnv) Get(k *LVal) *LVal                     { return k }
func (env *LEnv) GetFun(k *LVal) *LVal                  { return k }
func (env *LEnv) PutGlobal(k, v *LVal) *LVal            { return v }
func (env *LEnv) FunCall(f, args *LVal) *LVal           { return f }
func (env *LEnv) Errorf(format string, a ...any) *LVal  { return nil }

// Core builtins act in the caller's package: nothing here is reported.
func builtinEval(env *LEnv, args *LVal) *LVal {
	_ = env.Runtime.Package.Name
	return env.Eval(args)
}
