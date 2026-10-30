package lisp

import "errors"

// A sealed package refuses changes to its bindings from Lisp code: set, set!,
// defun and defmacro of one of its names (qualified, or unqualified while it
// is the current package), a new export, and a use-package into it.  Trusted
// Go APIs (PutGlobal, Update, Package.Put, the AddBuiltins family) still
// write it, because the host owns the package and Lisp does not.
//
// elps seals the lisp package itself (InitializeUserEnv) so that no program
// can change what if or lambda means for the code loaded after it.  An
// embedder has the same need for the packages it defines: once a Go host
// reads a configuration global or calls a helper in Go, a Lisp rebind of
// that name changes what Lisp sees but not what Go does, and the two halves
// of one package disagree.  Package.Seal gives the embedder the seal elps
// uses, with a policy that decides, write by write, whether to refuse it or
// only record it.  A policy that records lets a checker report every rebind
// of a program that still has to load.

// SealWrite names the kind of Lisp write that reached a sealed package.
type SealWrite int

const (
	// SealBind is set, set!, defun or defmacro of a name in the package.
	SealBind SealWrite = iota
	// SealExport is an export of a name the package does not export yet.
	// A new export changes what every later use-package of the package
	// binds, even before the name has a value.
	SealExport
	// SealUse is a use-package into the package: it binds each export of
	// the used package in the sealed one.  Symbol is that export.
	SealUse
)

// String returns the Lisp operation the write kind stands for.
func (w SealWrite) String() string {
	switch w {
	case SealBind:
		return "bind"
	case SealExport:
		return "export"
	case SealUse:
		return "use-package"
	}
	return "unknown"
}

// SealViolation describes one Lisp write to a sealed package.
type SealViolation struct {
	// Package is the sealed package's name.
	Package string
	// Symbol is the unqualified name the write changes.
	Symbol string
	// Write is the kind of write.
	Write SealWrite
}

// SealPolicy decides a Lisp write to a sealed package.  elps calls it before
// the write, so the package still holds its old binding (a host can tell an
// added name from a rebound one by looking it up).  A nil result, or any
// result that is not an error, lets the write go ahead; an error result is
// returned to the Lisp caller in place of the write, which does not happen.
//
// One policy serves every VM a template makes from the sealed package, and
// may run on several goroutines at once, so it must not write state of its
// own: per-VM state belongs in env.Runtime (SetVMValue, for example).  It
// runs inside the write and charges no step.
type SealPolicy func(env *LEnv, v SealViolation) *LVal

// packageSeal is a package's seal.  It is immutable once set: a template, an
// admitted copy and every VM share one by pointer.
type packageSeal struct {
	policy SealPolicy
	lang   bool // the lisp package's own seal, with its fixed error text
}

// langSeal seals the lisp package.  Its error text predates Package.Seal
// and stays as it was, since programs and their tests match on it.
var langSeal = &packageSeal{lang: true}

// ErrPackageSealed is returned by Package.Seal for a package that is already
// sealed.  A seal cannot be changed: a policy that a later call could swap
// would let any Go code that reaches the package unseal it, and the lisp
// package's seal must never be relaxed.
var ErrPackageSealed = errors.New("package is already sealed")

// Seal seals pkg against Lisp writes from now on.  With a nil policy every
// Lisp write to pkg fails with "cannot rebind sealed package binding:
// pkg:name"; otherwise policy decides each write (see SealPolicy).  Writes
// before the call, and Go writes at any time, are not affected.
//
// A host seals its packages after it has defined them and before it loads
// code it does not own.  The seal applies to every Lisp write, the package's
// own Lisp functions included: a function defined in the package runs with
// it as the current package, so its (set 'counter ...) is refused too.  A
// host package that keeps state that way must move the state to Go, or keep
// it in an unsealed package.  The seal is part of the package: NewTemplate keeps
// it in every VM, and so does package admission.  Seal returns
// ErrPackageSealed if pkg is already sealed, and so for the lisp package.
func (pkg *Package) Seal(policy SealPolicy) error {
	if pkg.seal != nil {
		return ErrPackageSealed
	}
	pkg.seal = &packageSeal{policy: policy}
	return nil
}

// Sealed reports whether pkg is sealed against Lisp writes.
func (pkg *Package) Sealed() bool {
	return pkg.seal != nil
}

// checkSealedWrite applies pkg's seal to a Lisp write of name.  It returns
// nil when pkg is not sealed or its policy lets the write go ahead, and else
// the error to return in place of the write.
func (env *LEnv) checkSealedWrite(pkg *Package, name string, write SealWrite) *LVal {
	s := pkg.seal
	if s == nil {
		return nil
	}
	if s.lang {
		return env.Errorf("cannot rebind lisp package binding: %s", name)
	}
	if s.policy == nil {
		return env.Errorf("cannot rebind sealed package binding: %s:%s", pkg.Name, name)
	}
	if r := s.policy(env, SealViolation{Package: pkg.Name, Symbol: name, Write: write}); r.IsError() {
		// A policy may build its error without env (lisp.Errorf): give it
		// the caller's stack, as other errors from package writes get.
		if err := env.ErrorAssociate(r); err != nil {
			return err
		}
		return r
	}
	return nil
}
