// Package lisp is a stub of the core package for the elpsownpkg fixtures.
package lisp

type LVal struct{ Str string }

type Package struct{ Name string }

type Runtime struct{ Package *Package }

type LEnv struct{ Runtime *Runtime }

type LBuiltin func(env *LEnv, args *LVal) *LVal

func Symbol(s string) *LVal { return &LVal{Str: s} }
func Nil() *LVal            { return &LVal{} }

func (env *LEnv) Eval(v *LVal) *LVal                         { return v }
func (env *LEnv) LoadString(name, src string) *LVal          { return nil }
func (env *LEnv) Lambda(formals *LVal, b []*LVal) *LVal      { return nil }
func (env *LEnv) InPackage(name *LVal) *LVal                 { return nil }
func (env *LEnv) Get(k *LVal) *LVal                          { return k }
func (env *LEnv) GetFun(k *LVal) *LVal                       { return k }
func (env *LEnv) PutGlobal(k, v *LVal) *LVal                 { return v }
func (env *LEnv) FunCall(f, args *LVal) *LVal                { return f }
func (env *LEnv) CallGlobal(sym string, args ...*LVal) *LVal { return nil }
func (env *LEnv) Terminal(expr *LVal) *LVal                  { return expr }
func (env *LEnv) Errorf(format string, a ...any) *LVal       { return nil }

// Core builtins act in the caller's package: nothing here is reported.
func builtinEval(env *LEnv, args *LVal) *LVal {
	_ = env.Runtime.Package.Name
	return env.Eval(args)
}

type ArgDecoder[T any] struct{}

func StringArg(what string) ArgDecoder[string] { return ArgDecoder[string]{} }
func ValueArg() ArgDecoder[*LVal]              { return ArgDecoder[*LVal]{} }

func FuncE(f func(env *LEnv, args *LVal) (*LVal, error)) LBuiltin { return nil }
func Func1[A any](da ArgDecoder[A], f func(env *LEnv, a A) *LVal) LBuiltin {
	return nil
}
func Func2[A, B any](da ArgDecoder[A], db ArgDecoder[B], f func(env *LEnv, a A, b B) *LVal) LBuiltin {
	return nil
}

func Func1E[A any, R any](f func(env *LEnv, a A) (R, error)) LBuiltin { return nil }
func Func2E[A, B any, R any](f func(env *LEnv, a A, b B) (R, error)) LBuiltin {
	return nil
}
