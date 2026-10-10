package elpsutil

import "github.com/luthersystems/elps/lisp"

// inPackageBuiltin is resolved once; a BuiltinRef is a table position and
// keeps no LVal reachable.
var inPackageBuiltin = lisp.BuiltinFunc("in-package")

// ExtendPackage makes the package named name env's current package, exactly
// as the Lisp form (in-package 'name) does: the package is created if it does
// not exist, and a package this call creates uses the language package (when
// the registry has one), while an existing package is entered unchanged.  The
// name is validated as in-package validates it, with the same error.
//
// It is the first step of a loader that adds Go builtins to a package that
// Lisp code may also define or extend (luthersystems/elps#745); follow it
// with LEnv.BindBuiltins.  Like in-package it does not restore the previous
// package -- the Load helpers in this package do that for a loader.
//
// It charges no evaluation step.  A PackageLoader package does not use the
// language package; use ExtendPackage only where in-package semantics are
// what the package had before.
func ExtendPackage(env *lisp.LEnv, name string) *lisp.LVal {
	if _, cerr := currentPackageName(env); cerr != nil {
		return cerr
	}
	v := env.CallBuiltin(inPackageBuiltin, lisp.Symbol(name))
	if v.IsError() {
		return v
	}
	return lisp.Nil()
}
