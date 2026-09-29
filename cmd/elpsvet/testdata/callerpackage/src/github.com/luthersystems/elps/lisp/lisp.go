// Package lisp is a minimal stub of github.com/luthersystems/elps/lisp for
// the elpscallerpackage fixtures. Only the shapes that rule inspects matter:
// LEnv's own Get/GetGlobal/GetFunGlobal/PutGlobal/PutGlobalFromLisp methods
// (the ambient, caller-package lookups), Package.Get (a different receiver,
// deliberately NOT one of them), the Registry lookup chain used to reach a
// specific package, LBuiltin's exact shape, and the Symbol constructor whose
// literal string argument the rule reads.
package lisp

type LVal struct {
	Str   string
	Cells []*LVal
}

type LBuiltin func(env *LEnv, args *LVal) *LVal

type Package struct{ name string }

func (pkg *Package) Get(k *LVal) *LVal { return &LVal{} }

type PackageRegistry struct{}

func (r *PackageRegistry) Package(name string) *Package { return &Package{name: name} }

type Runtime struct {
	Registry *PackageRegistry
}

type LEnv struct {
	Runtime *Runtime
}

// Get, GetGlobal and GetFunGlobal all resolve k in env.Runtime's CURRENT
// package -- the package the calling code left current, for a builtin.
func (env *LEnv) Get(k *LVal) *LVal { return &LVal{} }

func (env *LEnv) GetGlobal(k *LVal) *LVal { return &LVal{} }

func (env *LEnv) GetFunGlobal(k *LVal) *LVal { return &LVal{} }

func (env *LEnv) PutGlobal(k, v *LVal) *LVal { return v }

func (env *LEnv) PutGlobalFromLisp(k, v *LVal) *LVal { return v }

func Symbol(s string) *LVal { return &LVal{Str: s} }
