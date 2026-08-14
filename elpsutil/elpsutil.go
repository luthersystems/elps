package elpsutil

import (
	"github.com/luthersystems/elps/lisp"
)

// Function is a helper to construct builtins.
func Function(name string, formals *lisp.LVal, fun lisp.LBuiltin) *Builtin {
	return &Builtin{formals, fun, name}
}

// Builtin captures Go functions that are callable from elps.
type Builtin struct {
	formals *lisp.LVal
	fun     lisp.LBuiltin
	name    string
}

// Name returns the name of a function.
func (fun *Builtin) Name() string {
	return fun.name
}

// Formals returns the formal arguments of a function.
func (fun *Builtin) Formals() *lisp.LVal {
	return fun.formals
}

// Eval evaluates a function on an environment.
func (fun *Builtin) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return fun.fun(env, args)
}

// Loader is a generic function to initialize/load an LEnv.  A Loader should
// first define and switch into the package(s) it loads.  The helpers in this
// package (Load, LoadAll, LibraryLoader and PackageLoader) restore the package
// that was active when they were called, so that symbols defined after a load
// end up where the caller expects them.  A load performed from the user
// package -- the conventional top-level usage -- therefore ends in the user
// package, as it always has.
//
// A chain of loaders may be formed to load a library.
type Loader = lisp.Loader

func nopLoader(env *lisp.LEnv) *lisp.LVal {
	return lisp.Nil()
}

// Package is an elps package implemented in Go.
type Package interface {
	PackageName() string
}

// PackageDocumented allows an elps package to provide a documentation string.
type PackageDocumented interface {
	Package
	PackageDoc() string
}

// PackageInit allows initialization of an elps package implemented in Go.
type PackageInit interface {
	Package
	PackageInit(env *lisp.LEnv) *lisp.LVal
}

func packageInit(p Package) Loader {
	_p, ok := p.(PackageInit)
	if !ok {
		return nopLoader
	}
	return _p.PackageInit
}

// PackageBuiltins retrieves the exposed builtins for an elps package implemented
// in Go.
type PackageBuiltins interface {
	Package
	Builtins() []lisp.LBuiltinDef
}

func packageBuiltins(p Package) []lisp.LBuiltinDef {
	_p, ok := p.(PackageBuiltins)
	if !ok {
		return nil
	}
	return _p.Builtins()
}

// PackageSpecialOps returns the special operators for an elps package implemented
// in Go.
type PackageSpecialOps interface {
	Package
	SpecialOps() []lisp.LBuiltinDef
}

func packageSpecialOps(p Package) []lisp.LBuiltinDef {
	_p, ok := p.(PackageSpecialOps)
	if !ok {
		return nil
	}
	return _p.SpecialOps()
}

// PackageMacros returns the macros for an elps package implemented in Go.
type PackageMacros interface {
	Package
	Macros() []lisp.LBuiltinDef
}

func packageMacros(p Package) []lisp.LBuiltinDef {
	_p, ok := p.(PackageMacros)
	if !ok {
		return nil
	}
	return _p.Macros()
}

// Load loads an elps package implemented in Go.  Load restores the package
// that was active when it was called so that further defined symbols end up in
// that package by default.
func Load(env *lisp.LEnv, fn Loader) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	lerr := fn(env)
	if lerr.Type == lisp.LError {
		return lerr
	}
	lerr = env.InPackage(lisp.String(prevPkg))
	if lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.Nil()
}

// LoadAll loads multiple elps files implemented in Go.  Like Load, LoadAll
// restores the package that was active when it was called, and runs each
// loader from that package.
func LoadAll(fn ...Loader) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		for _, fn := range fn {
			lerr := Load(env, fn)
			if lerr.Type == lisp.LError {
				return lerr
			}
		}
		return lisp.Nil()
	}
}

// LibraryLoader loads multiple elps packages implemented in Go.
func LibraryLoader(ps ...Package) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		for _, p := range ps {
			lerr := Load(env, PackageLoader(p))
			if lerr.Type == lisp.LError {
				return lerr
			}
		}
		return lisp.Nil()
	}
}

// PackageLoader loads an elps package implemented in Go.
//
// PackageLoader restores the package that was active when it was called, the
// same invariant the stdlib LoadPackage functions honor
// (https://github.com/luthersystems/elps/issues/99).  It also re-establishes
// the loaded package after PackageInit returns, so that a PackageInit which
// switches packages -- directly, or indirectly through a nested Load -- does
// not cause the package's builtins, special operators and macros to be
// registered and exported somewhere else
// (https://github.com/luthersystems/elps/issues/352).
func PackageLoader(p Package) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		prevPkg := env.Runtime.Package.Name
		defer env.InPackage(lisp.Symbol(prevPkg))
		name := lisp.Symbol(p.PackageName())
		e := env.DefinePackage(name)
		if !e.IsNil() {
			return e
		}
		e = env.InPackage(name)
		if !e.IsNil() {
			return e
		}
		if dp, ok := p.(PackageDocumented); ok {
			env.SetPackageDoc(dp.PackageDoc())
		}
		initLoader := packageInit(p)
		e = initLoader(env)
		if e.Type == lisp.LError {
			return e
		}
		// PackageInit may have switched packages -- the Loader convention
		// documented above tells embedders to do exactly that.  Re-establish
		// the package being loaded so the definitions below land in it.
		e = env.InPackage(name)
		if !e.IsNil() {
			return e
		}
		for _, fn := range packageBuiltins(p) {
			env.AddBuiltins(true, fn)
		}
		for _, fn := range packageSpecialOps(p) {
			env.AddSpecialOps(true, fn)
		}
		for _, fn := range packageMacros(p) {
			env.AddMacros(true, fn)
		}
		return lisp.Nil()
	}
}
