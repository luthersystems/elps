// Package lisp is a stub of the core package for the elpsbuiltinstate
// fixtures.
package lisp

type LVal struct{ Str string }

type Runtime struct{}

type LEnv struct{ Runtime *Runtime }

type LBuiltin func(env *LEnv, args *LVal) *LVal

func Fun(fid string, formals *LVal, fn LBuiltin) *LVal { return &LVal{Str: fid} }

type ArgDecoder[T any] struct{}

func StringArg(what string) ArgDecoder[string] { return ArgDecoder[string]{} }

func FuncE(f func(env *LEnv, args *LVal) (*LVal, error)) LBuiltin { return nil }
func Func1[A any](da ArgDecoder[A], f func(env *LEnv, a A) *LVal) LBuiltin {
	return nil
}
func Func2[A, B any](da ArgDecoder[A], db ArgDecoder[B], f func(env *LEnv, a A, b B) *LVal) LBuiltin {
	return nil
}

func Func1E[A any, R any](f func(env *LEnv, a A) (R, error)) LBuiltin { return nil }
