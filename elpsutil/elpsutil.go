package elpsutil

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// Function is a helper to construct builtins.
//
// formals must be a list of symbols, as returned by lisp.Formals.  A function
// that takes no arguments is declared with lisp.Formals(), the empty list; a
// nil formals list is not a valid spelling of that and is rejected by
// PackageLoader and Validate.
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
// that was active when they were called, whether the load succeeds or fails,
// so that symbols defined after a load end up where the caller expects them.
// A load performed from the user package -- the conventional top-level usage
// -- therefore ends in the user package, as it always has.
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

// currentPackageName returns the name of env's current package, or an error
// when there is no current package to restore into.  An LEnv that has not been
// through lisp.InitializeUserEnv has a nil Runtime.Package (and a
// hand-constructed one may have a nil Runtime); dereferencing either panics,
// which is the class of defect issue #351 is about.
func currentPackageName(env *lisp.LEnv) (string, *lisp.LVal) {
	if env == nil || env.Runtime == nil || env.Runtime.Package == nil {
		return "", lisp.Errorf("environment has no current package -- initialize it with lisp.InitializeUserEnv before loading")
	}
	return env.Runtime.Package.Name, nil
}

// runLoader runs a non-nil fn and restores the package that was active on
// entry, whether or not fn succeeds.  When fn returns an error that error is
// returned, even if the restore itself also fails.  what identifies fn in any
// diagnostic.
func runLoader(env *lisp.LEnv, fn Loader, what string) *lisp.LVal {
	prevPkg, cerr := currentPackageName(env)
	if cerr != nil {
		return cerr
	}
	lerr := loaderResult(fn(env), what)
	rerr := env.InPackage(lisp.String(prevPkg))
	if lerr.Type == lisp.LError {
		return lerr
	}
	if rerr.Type == lisp.LError {
		return rerr
	}
	return lisp.Nil()
}

// Load loads an elps package implemented in Go.  Load restores the package
// that was active when it was called, whether or not fn succeeds, so that
// further defined symbols end up in that package by default.
//
// A loader that returns a Go nil *LVal, rather than lisp.Nil(), is reported as
// an error naming the loader instead of panicking.
func Load(env *lisp.LEnv, fn Loader) *lisp.LVal {
	if fn == nil {
		return lisp.Errorf("loader is nil")
	}
	return runLoader(env, fn, "loader "+loaderName(fn))
}

// LoadAll loads multiple elps files implemented in Go.  Like Load, LoadAll
// restores the package that was active when it was called, whether or not the
// loaders succeed, and runs each loader from that package.
//
// A loader that returns a Go nil *LVal, rather than lisp.Nil(), is reported as
// an error naming the loader and its position in the chain instead of
// panicking.
func LoadAll(fns ...Loader) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		for i, fn := range fns {
			what := fmt.Sprintf("loader %d of %d, %s", i+1, len(fns), loaderName(fn))
			if fn == nil {
				return lisp.Errorf("%s is nil", what)
			}
			if lerr := runLoader(env, fn, what); lerr.Type == lisp.LError {
				return lerr
			}
		}
		return lisp.Nil()
	}
}

// LibraryLoader loads multiple elps packages implemented in Go.
func LibraryLoader(ps ...Package) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		for i, p := range ps {
			if p == nil {
				return lisp.Errorf("package %d of %d is nil", i+1, len(ps))
			}
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
// PackageLoader restores the package that was active when it was called,
// whether or not the load succeeds, the same invariant the stdlib LoadPackage
// functions honor (https://github.com/luthersystems/elps/issues/99).  It also
// re-establishes the loaded package after PackageInit returns, so that a
// PackageInit which switches packages -- directly, or indirectly through a
// nested Load -- does not cause the package's builtins, special operators and
// macros to be registered and exported somewhere else
// (https://github.com/luthersystems/elps/issues/352).
//
// PackageLoader validates the package definition before registering anything,
// and returns an *LVal error identifying the package and the offending
// definition rather than letting lisp panic or deferring a nil dereference to
// call time.  A definition lisp accepts today is still accepted.
func PackageLoader(p Package) Loader {
	return func(env *lisp.LEnv) *lisp.LVal {
		if p == nil {
			return lisp.Errorf("package is nil")
		}
		prevPkg, cerr := currentPackageName(env)
		if cerr != nil {
			return cerr
		}
		defer env.InPackage(lisp.Symbol(prevPkg))
		pkgName := p.PackageName()
		// A method meant to implement one of the optional Package interfaces
		// but declared with the wrong signature registers nothing at all, with
		// no diagnostic on any path.  Report it before the silent load.
		if problems := checkPackageMethods(p); len(problems) > 0 {
			return lisp.Errorf("package %q: %s", pkgName, strings.Join(problems, "; "))
		}
		name := lisp.Symbol(pkgName)
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
		e = loaderResult(initLoader(env), fmt.Sprintf("package %q: PackageInit", pkgName))
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
		e = addDefs(env, pkgName, kindBuiltin, packageBuiltins(p), env.AddBuiltins)
		if e.Type == lisp.LError {
			return e
		}
		e = addDefs(env, pkgName, kindSpecialOp, packageSpecialOps(p), env.AddSpecialOps)
		if e.Type == lisp.LError {
			return e
		}
		e = addDefs(env, pkgName, kindMacro, packageMacros(p), env.AddMacros)
		if e.Type == lisp.LError {
			return e
		}
		return lisp.Nil()
	}
}
