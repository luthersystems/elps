// Package lisp is a stub of the core package for the elpsidiom fixtures.
package lisp

type LType uint

const (
	LInvalid LType = iota
	LInt
	LString
	LSortMap
	LError
	LSExpr
	LArray
	LNative
	LSymbol
)

const (
	TrueSymbol  = "true"
	FalseSymbol = "false"
)

const OptArgSymbol = "&optional"

type LVal struct {
	Type   LType
	Str    string
	Int    int
	Cells  []*LVal
	Native any
}

type ErrorVal LVal

func (e *ErrorVal) Error() string { return e.Str }

type Runtime struct{}

func (r *Runtime) CheckAlloc(n int) string { return "" }

type LEnv struct{ Runtime *Runtime }

type LBuiltin func(env *LEnv, args *LVal) *LVal

type BuiltinRef struct{ idx int }

type Cells []*LVal

func (c Cells) List() *LVal   { return QExpr(c) }
func (c Cells) SExpr() *LVal  { return SExpr(c) }
func (c Cells) Vector() *LVal { return Vector(c) }

func SExpr(cells []*LVal) *LVal             { return &LVal{Cells: cells} }
func Vector(cells []*LVal) *LVal            { return Array(nil, cells) }
func Array(dims *LVal, cells []*LVal) *LVal { return &LVal{Cells: cells} }

func BuiltinFunc(name string) BuiltinRef                             { return BuiltinRef{} }
func QExpr(cells []*LVal) *LVal                                      { return &LVal{Cells: cells} }
func String(s string) *LVal                                          { return &LVal{Str: s} }
func Nil() *LVal                                                     { return &LVal{} }
func Formals(names ...string) *LVal                                  { return &LVal{} }
func FunInPackage(pkg, fid string, formals *LVal, fn LBuiltin) *LVal { return nil }
func FunInPackageDoc(pkg, fid string, formals *LVal, fn LBuiltin, doc string) *LVal {
	return nil
}
func GoError(v *LVal) error              { return nil }
func Result(v *LVal) (*LVal, error)      { return v, nil }
func ResultAs[T any](v *LVal) (T, error) { var z T; return z, nil }

func FuncE(f func(env *LEnv, args *LVal) (*LVal, error)) LBuiltin { return nil }
func Func1E[A any, R any](f func(env *LEnv, a A) (R, error)) LBuiltin {
	return nil
}
func Func2E[A, B any, R any](f func(env *LEnv, a A, b B) (R, error)) LBuiltin {
	return nil
}

func (v *LVal) MapKeys() *LVal              { return v }
func (v *LVal) MapGetString(k string) *LVal { return v }
func (v *LVal) IsError() bool               { return v.Type == LError }
func (v *LVal) IsSymbol(name string) bool   { return v.Type == LSymbol && v.Str == name }
func (v *LVal) Len() int                    { return len(v.Cells) }
func (v *LVal) ArrayIndex(i ...*LVal) *LVal { return v }
func (v *LVal) SeqCells() ([]*LVal, bool)   { return v.Cells, true }

func SeqOf[T any](v *LVal) ([]T, bool)     { return nil, false }
func NativeValue[T any](v *LVal) (T, bool) { var z T; return z, false }

func (env *LEnv) Errorf(format string, a ...any) *LVal          { return nil }
func (env *LEnv) Error(msg ...any) *LVal                        { return nil }
func (env *LEnv) CheckAlloc(n int) *LVal                        { return nil }
func (env *LEnv) CallBuiltin(b BuiltinRef, args ...*LVal) *LVal { return nil }
func (env *LEnv) MapLookup(m, k *LVal) *LVal                    { return nil }
func (env *LEnv) MapPut(m, k, v *LVal) *LVal                    { return nil }
func (env *LEnv) ToString(v *LVal) *LVal                        { return nil }
func (env *LEnv) FormatString(f string, fvals ...*LVal) *LVal   { return nil }

func (env *LEnv) MapOf(kv ...any) *LVal         { return nil }
func (env *LEnv) SortedMapOf(kv ...*LVal) *LVal { return nil }
func Int(n int) *LVal                           { return &LVal{Int: n} }
