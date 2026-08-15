// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"errors"
	"fmt"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// The checks in this file exist because elpsutil sits on the boundary between
// an embedder's Go code and the interpreter.  Inside lisp a panic means "this
// is a bug in the interpreter, or in code that had no business calling this".
// A mistake in an embedder's package definition is neither -- it is ordinary,
// recoverable, and belongs to a registration path where every other failure
// already returns an *LVal error.  So the mistakes are caught here, at the
// boundary, and reported as errors naming the offending package, definition,
// or loader.  The panics inside lisp are left exactly as they are.

// Definition kinds.  These are the names used in error messages, and they
// double as the switch used to reproduce the (differing) collision rules of
// LEnv.AddBuiltins, LEnv.AddSpecialOps and LEnv.AddMacros.
const (
	kindBuiltin   = "builtin"
	kindSpecialOp = "special operator"
	kindMacro     = "macro"
)

// Validate reports mistakes in an elps package implemented in Go that the Go
// type system cannot catch.  It is intended to be called from an embedder's
// test:
//
//	func TestPackage(t *testing.T) {
//		if err := elpsutil.Validate(&MyPackage{}); err != nil {
//			t.Fatal(err)
//		}
//	}
//
// Validate reports every problem it finds, joined into a single error.  A nil
// return means PackageLoader will not reject the package.
//
// Validate performs one check that PackageLoader deliberately does not: it
// reports a package that contributes no builtins, no special operators, no
// macros and has no PackageInit.  Such a package is almost certainly a
// mistake, but "almost" is not enough to fail a load -- a namespace-only
// package is unusual, not illegal.
func Validate(p Package) error {
	if p == nil {
		return errors.New("package is nil")
	}
	var problems []error
	add := func(format string, v ...interface{}) {
		problems = append(problems, fmt.Errorf(format, v...))
	}

	name := p.PackageName()
	if name == "" {
		add("PackageName() returned an empty string")
	}

	for _, problem := range checkPackageMethods(p) {
		add("package %q: %s", name, problem)
	}

	// All three definition kinds share one symbol table, so duplicates are
	// tracked across kinds rather than within each one.
	seen := make(map[string]string)
	kinds := []struct {
		kind string
		defs []lisp.LBuiltinDef
	}{
		{kindBuiltin, packageBuiltins(p)},
		{kindSpecialOp, packageSpecialOps(p)},
		{kindMacro, packageMacros(p)},
	}
	ndef := 0
	for _, k := range kinds {
		ndef += len(k.defs)
		for i, def := range k.defs {
			if isNilDef(def) {
				add("package %q: %s %d is nil", name, k.kind, i)
				continue
			}
			defName := def.Name()
			if problem := checkFormals(def.Formals()); problem != "" {
				add("package %q: %s %q: %s", name, k.kind, defName, problem)
			}
			if problem := checkReservedName(defName); problem != "" {
				add("package %q: %s %q: %s", name, k.kind, defName, problem)
			}
			if prev, ok := seen[defName]; ok {
				add("package %q: %s %q: name is already used by a %s in the same package",
					name, k.kind, defName, prev)
				continue
			}
			seen[defName] = k.kind
		}
	}

	if ndef == 0 {
		if _, ok := p.(PackageInit); !ok {
			add("package %q defines no builtins, no special operators and no macros, and has no PackageInit; loading it has no effect", name)
		}
	}

	return errors.Join(problems...)
}

// checkFormals reports what is wrong with an embedder-supplied formal argument
// list, or "" when the list is usable.
//
// A nil formals list is the motivating case: it registers cleanly and is first
// dereferenced by LEnv.bind at fun.Cells[0].Cells, so the embedder learns
// about it as an opaque internal-panic the first time a user calls the
// function.  lisp.Formals() -- an empty list -- is the correct spelling of
// "takes no arguments".
func checkFormals(formals *lisp.LVal) string {
	if formals == nil {
		return "formals are nil (use lisp.Formals() to declare a function that takes no arguments)"
	}
	if formals.Type == lisp.LError {
		return fmt.Sprintf("formals are an error value: %v", formals)
	}
	if formals.Type != lisp.LSExpr {
		return fmt.Sprintf("formals are a %v, not a list (use lisp.Formals())", formals.Type)
	}
	for i, cell := range formals.Cells {
		if cell == nil {
			return fmt.Sprintf("formal argument %d is nil", i)
		}
		if cell.Type != lisp.LSymbol {
			return fmt.Sprintf("formal argument %d is a %v, not a symbol", i, cell.Type)
		}
	}
	return ""
}

// checkReservedName reports whether name collides with a symbol the reader
// answers without consulting a package's symbol table.  Package.get returns
// true and false unconditionally, so AddBuiltins concludes the name is taken
// even in a package that has just been created, and panics.
func checkReservedName(name string) string {
	if name == lisp.TrueSymbol || name == lisp.FalseSymbol {
		return fmt.Sprintf("%q is a reserved constant and can never be bound", name)
	}
	return ""
}

// checkPackageName reports whether name is already bound in pkg, reproducing
// the collision rule of the LEnv method that registers kind.  AddBuiltins
// rejects any existing binding; AddSpecialOps and AddMacros permit overwriting
// a binding whose value is nil.
func checkPackageName(pkg *lisp.Package, kind, name string) string {
	if problem := checkReservedName(name); problem != "" {
		return problem
	}
	if pkg == nil {
		return ""
	}
	exist, ok := pkg.Symbols[name]
	if !ok {
		return ""
	}
	if kind != kindBuiltin && (exist == nil || exist.IsNil()) {
		return ""
	}
	return fmt.Sprintf("symbol is already defined in package %q", pkg.Name)
}

// isNilDef reports whether def is nil, including a nil pointer stored in a
// non-nil interface.
func isNilDef(def lisp.LBuiltinDef) bool {
	if def == nil {
		return true
	}
	v := reflect.ValueOf(def)
	switch v.Kind() {
	case reflect.Pointer, reflect.Interface, reflect.Map, reflect.Slice, reflect.Func:
		return v.IsNil()
	default:
		return false
	}
}

// packageMethod names an optional Package extension interface together with
// the method that implements it.
type packageMethod struct {
	method string
	want   string
	iface  reflect.Type
}

var packageMethods = []packageMethod{
	{"Builtins", "Builtins() []lisp.LBuiltinDef", reflect.TypeOf((*PackageBuiltins)(nil)).Elem()},
	{"SpecialOps", "SpecialOps() []lisp.LBuiltinDef", reflect.TypeOf((*PackageSpecialOps)(nil)).Elem()},
	{"Macros", "Macros() []lisp.LBuiltinDef", reflect.TypeOf((*PackageMacros)(nil)).Elem()},
	{"PackageInit", "PackageInit(*lisp.LEnv) *lisp.LVal", reflect.TypeOf((*PackageInit)(nil)).Elem()},
	{"PackageDoc", "PackageDoc() string", reflect.TypeOf((*PackageDocumented)(nil)).Elem()},
}

// checkPackageMethods reports methods that were evidently meant to implement
// one of the optional Package interfaces but do not.
//
// Go cannot diagnose an interface someone meant to implement: a Builtins()
// method returning the wrong slice type simply fails the type assertion in
// packageBuiltins, which returns nil, and every builtin vanishes with no
// diagnostic anywhere -- the load succeeds and the symbols are unbound at call
// time.  A method with one of these names and the wrong signature is never
// intentional, so it is worth naming.
func checkPackageMethods(p Package) []string {
	t := reflect.TypeOf(p)
	if t == nil {
		return nil
	}
	var problems []string
	for _, m := range packageMethods {
		if t.Implements(m.iface) {
			continue
		}
		if got, ok := t.MethodByName(m.method); ok {
			problems = append(problems, fmt.Sprintf(
				"method %s does not satisfy %s: have %s, want %s; its definitions are silently ignored",
				m.method, m.iface.Name(), methodSignature(m.method, got.Type), m.want))
			continue
		}
		// A method declared on the pointer receiver is not in the method set
		// of the value type, so passing the package by value drops it.
		if t.Kind() != reflect.Pointer {
			if _, ok := reflect.PointerTo(t).MethodByName(m.method); ok {
				problems = append(problems, fmt.Sprintf(
					"method %s is declared on *%s but the package was passed by value, so it does not satisfy %s; pass &%s{} instead",
					m.method, t.Name(), m.iface.Name(), t.Name()))
			}
		}
	}
	return problems
}

// methodSignature renders a reflect method type as a Go signature, dropping
// the receiver that reflect prepends to the parameter list.
func methodSignature(name string, t reflect.Type) string {
	in := make([]string, 0, t.NumIn())
	for i := 1; i < t.NumIn(); i++ {
		in = append(in, t.In(i).String())
	}
	out := make([]string, 0, t.NumOut())
	for i := range t.NumOut() {
		out = append(out, t.Out(i).String())
	}
	sig := name + "(" + strings.Join(in, ", ") + ")"
	switch len(out) {
	case 0:
	case 1:
		sig += " " + out[0]
	default:
		sig += " (" + strings.Join(out, ", ") + ")"
	}
	return sig
}

// loaderResult validates the value an embedder-supplied Loader returned.
//
// The Loader signature permits a Go nil *LVal, and returning one is an easy
// slip -- lisp.Nil() is the correct spelling of success.  Every reader of a
// Loader's result dereferences it, so an unchecked nil panics in the
// embedder's process at load time.  what identifies the offending loader.
func loaderResult(v *lisp.LVal, what string) *lisp.LVal {
	if v == nil {
		return lisp.Errorf("%s returned a nil *LVal (a Loader must return lisp.Nil() on success)", what)
	}
	return v
}

// loaderName identifies a Loader by the Go function it was built from, so that
// an error about "loader 3 of 7" points at a file and line.
func loaderName(fn Loader) string {
	if fn == nil {
		return "<nil>"
	}
	f := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	if f == nil {
		return "<unknown>"
	}
	file, line := f.FileLine(f.Entry())
	return fmt.Sprintf("%s (%s:%d)", f.Name(), filepath.Base(file), line)
}

// addDefs registers defs with add, rejecting the definitions that would make
// lisp panic or defer a nil dereference to call time.  The checks mirror the
// ones the LEnv methods make before panicking, so a definition that is
// accepted today is still accepted.
func addDefs(env *lisp.LEnv, pkgName string, kind string, defs []lisp.LBuiltinDef, add func(bool, ...lisp.LBuiltinDef)) *lisp.LVal {
	for i, def := range defs {
		if isNilDef(def) {
			return lisp.Errorf("package %q: %s %d is nil", pkgName, kind, i)
		}
		name := def.Name()
		if problem := checkFormals(def.Formals()); problem != "" {
			return lisp.Errorf("package %q: %s %q: %s", pkgName, kind, name, problem)
		}
		if problem := checkPackageName(env.Runtime.Package, kind, name); problem != "" {
			return lisp.Errorf("package %q: %s %q: %s", pkgName, kind, name, problem)
		}
		add(true, def)
	}
	return lisp.Nil()
}
