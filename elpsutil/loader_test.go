// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// nilLoader is the mistake under test: the Loader signature permits a Go nil
// *LVal, and returning one instead of lisp.Nil() is an easy slip. Every reader
// of a Loader's result -- Load, LoadAll and PackageLoader all test
// `x.Type == lisp.LError` -- dereferences it, so an unchecked nil panics
// uncaught in the embedder's process at load time.
func nilLoader(env *lisp.LEnv) *lisp.LVal { return nil }

func okLoader(env *lisp.LEnv) *lisp.LVal { return lisp.Nil() }

// loaderEntryPoint is an exported elpsutil function that runs an
// embedder-supplied Loader, paired with a way to drive it.
type loaderEntryPoint struct {
	// name must match the Go identifier, because the sweep in
	// TestLoaderEntryPointsAreCovered compares this table against the
	// package's declarations.
	name string
	// run invokes the entry point so that fn is the embedder-supplied Loader
	// it ends up calling, and returns the result LVal.
	run func(env *lisp.LEnv, fn elpsutil.Loader) *lisp.LVal
	// wantNil are substrings the error must contain when fn returns a Go nil.
	// Every case requires something that identifies the offending loader: an
	// error naming no loader makes an embedder with a long chain hunt.
	wantNil []string
}

func loaderEntryPoints() []loaderEntryPoint {
	return []loaderEntryPoint{{
		name: "Load",
		run: func(env *lisp.LEnv, fn elpsutil.Loader) *lisp.LVal {
			return elpsutil.Load(env, fn)
		},
		wantNil: []string{"nil *LVal", "lisp.Nil()", "nilLoader", "loader_test.go"},
	}, {
		name: "LoadAll",
		run: func(env *lisp.LEnv, fn elpsutil.Loader) *lisp.LVal {
			return elpsutil.LoadAll(okLoader, fn, okLoader)(env)
		},
		wantNil: []string{"nil *LVal", "lisp.Nil()", "nilLoader", "loader 2 of 3"},
	}, {
		name: "PackageLoader",
		run: func(env *lisp.LEnv, fn elpsutil.Loader) *lisp.LVal {
			// PackageLoader runs exactly one embedder loader, the package's
			// PackageInit.
			return elpsutil.PackageLoader(&testPackage{name: "initpkg", initFn: fn})(env)
		},
		wantNil: []string{"nil *LVal", "lisp.Nil()", "initpkg", "PackageInit"},
	}, {
		name: "LibraryLoader",
		run: func(env *lisp.LEnv, fn elpsutil.Loader) *lisp.LVal {
			return elpsutil.LibraryLoader(&testPackage{name: "libpkg", initFn: fn})(env)
		},
		wantNil: []string{"nil *LVal", "lisp.Nil()", "libpkg", "PackageInit"},
	}}
}

// TestLoadersRejectNilResults covers issue #351 defect 2 for every exported
// entry point that runs an embedder-supplied Loader.
func TestLoadersRejectNilResults(t *testing.T) {
	for _, ep := range loaderEntryPoints() {
		t.Run(ep.name, func(t *testing.T) {
			t.Run("nil_result", func(t *testing.T) {
				env := testEnv(t)
				var rc *lisp.LVal
				require.NotPanics(t, func() {
					rc = ep.run(env, nilLoader)
				}, "%s panicked on a loader returning a Go nil *LVal", ep.name)
				require.NotNil(t, rc)
				require.Equal(t, lisp.LError, rc.Type, "%s accepted a nil loader result: %v", ep.name, rc)
				for _, want := range ep.wantNil {
					require.Contains(t, rc.String(), want)
				}
			})
			t.Run("good_loader", func(t *testing.T) {
				// Correct code must see no difference.
				env := testEnv(t)
				rc := ep.run(env, okLoader)
				require.NotNil(t, rc)
				require.NotEqual(t, lisp.LError, rc.Type, "%s rejected a well-formed loader: %v", ep.name, rc)
			})
		})
	}
}

// TestLoadersRejectNilLoaders covers the two entry points an embedder hands a
// Loader value to directly, where the value itself may be nil.
func TestLoadersRejectNilLoaders(t *testing.T) {
	env := testEnv(t)
	var rc *lisp.LVal
	require.NotPanics(t, func() {
		rc = elpsutil.Load(env, nil)
	})
	require.Equal(t, lisp.LError, rc.Type)
	require.Contains(t, rc.String(), "nil")

	require.NotPanics(t, func() {
		rc = elpsutil.LoadAll(okLoader, nil, okLoader)(env)
	})
	require.Equal(t, lisp.LError, rc.Type)
	require.Contains(t, rc.String(), "loader 2 of 3")
}

// TestLoaderEntryPointsAreCovered is the systemic half of the fix. It parses
// elpsutil's own source and requires every exported function that accepts a
// Loader to appear in the table above. A new entry point added later that
// forgets the nil check fails this test rather than shipping the next instance
// of defect 2.
func TestLoaderEntryPointsAreCovered(t *testing.T) {
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, ".", nil, 0)
	require.NoError(t, err)
	src, ok := pkgs["elpsutil"]
	require.True(t, ok, "did not find package elpsutil in the working directory")

	var declared []string
	for _, file := range src.Files {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv != nil || !fn.Name.IsExported() {
				continue
			}
			if !usesLoader(fn.Type) {
				continue
			}
			declared = append(declared, fn.Name.Name)
		}
	}
	sort.Strings(declared)
	// Guard against the sweep silently finding nothing: a source scan that
	// matches zero functions would pass while asserting nothing at all.
	require.NotEmpty(t, declared, "found no exported functions accepting a Loader; the source scan is broken")
	t.Logf("exported functions accepting a Loader: %v", declared)

	covered := make(map[string]bool)
	for _, ep := range loaderEntryPoints() {
		covered[ep.name] = true
	}
	for _, name := range declared {
		require.True(t, covered[name],
			"elpsutil.%s runs an embedder-supplied Loader but is not covered by TestLoadersRejectNilResults; "+
				"a Loader returning a Go nil *LVal must produce an error naming the loader, not a panic", name)
	}
}

// TestPackageLoaderKindsAreCovered is the same guard for definition kinds. It
// counts the addDefs call sites in PackageLoader -- one per kind of definition
// a Package can contribute -- and requires the defKinds table to match. A
// fourth kind added later is checked by the mistake table in
// TestPackageLoaderRejectsBadDefinitions rather than being validated only for
// builtins.
func TestPackageLoaderKindsAreCovered(t *testing.T) {
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, ".", nil, 0)
	require.NoError(t, err)
	src, ok := pkgs["elpsutil"]
	require.True(t, ok, "did not find package elpsutil in the working directory")

	kinds := make(map[string]bool)
	for _, file := range src.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			id, ok := call.Fun.(*ast.Ident)
			if !ok || id.Name != "addDefs" || len(call.Args) < 3 {
				return true
			}
			if kind, ok := call.Args[2].(*ast.Ident); ok {
				kinds[kind.Name] = true
			}
			return true
		})
	}
	require.NotEmpty(t, kinds, "found no addDefs call sites; the source scan is broken")
	require.Len(t, defKinds("x"), len(kinds),
		"PackageLoader registers %d kinds of definition (%v) but defKinds covers %d",
		len(kinds), kinds, len(defKinds("x")))
}

// usesLoader reports whether fn takes or returns a Loader, i.e. whether it is
// on the path that runs code an embedder supplied. PackageLoader and
// LibraryLoader take a Package but return a Loader that ends up calling the
// package's PackageInit, so both directions count.
func usesLoader(fn *ast.FuncType) bool {
	for _, fields := range []*ast.FieldList{fn.Params, fn.Results} {
		if fields == nil {
			continue
		}
		for _, field := range fields.List {
			typ := field.Type
			if e, ok := typ.(*ast.Ellipsis); ok {
				typ = e.Elt
			}
			if id, ok := typ.(*ast.Ident); ok && id.Name == "Loader" {
				return true
			}
		}
	}
	return false
}

// --- Package method signature detection -------------------------------------

// wrongBuiltins is the mistake the issue records as "harder": a Builtins
// method returning the wrong slice type. It does not satisfy PackageBuiltins,
// so packageBuiltins's type assertion fails and returns nil, and every builtin
// vanishes -- the load succeeds and the symbols are simply unbound at call
// time, with no diagnostic anywhere.
type wrongBuiltins struct{}

func (p *wrongBuiltins) PackageName() string { return "wrongbuiltins" }

func (p *wrongBuiltins) Builtins() []*elpsutil.Builtin {
	return []*elpsutil.Builtin{elpsutil.Function("f", lisp.Formals(), nopBuiltin)}
}

// valueReceiverBuiltins declares Builtins on the pointer receiver but is
// passed by value, so the method is not in its method set and the definitions
// are dropped the same way.
type valueReceiverBuiltins struct{}

func (p valueReceiverBuiltins) PackageName() string { return "valuereceiver" }

func (p *valueReceiverBuiltins) Builtins() []lisp.LBuiltinDef {
	return []lisp.LBuiltinDef{elpsutil.Function("f", lisp.Formals(), nopBuiltin)}
}

// wrongPackageInit mistypes PackageInit, so the package is loaded without ever
// being initialized.
type wrongPackageInit struct{}

func (p *wrongPackageInit) PackageName() string { return "wronginit" }

func (p *wrongPackageInit) PackageInit(env *lisp.LEnv) error { return nil }

func (p *wrongPackageInit) Builtins() []lisp.LBuiltinDef {
	return []lisp.LBuiltinDef{elpsutil.Function("f", lisp.Formals(), nopBuiltin)}
}

// emptyPackage contributes nothing at all. Validate reports it; PackageLoader
// deliberately does not, because a namespace-only package is unusual rather
// than illegal.
type emptyPackage struct{}

func (p *emptyPackage) PackageName() string { return "emptypkg" }

func TestPackageLoaderReportsUnsatisfiedInterfaces(t *testing.T) {
	tests := []struct {
		name string
		pkg  elpsutil.Package
		want []string
	}{{
		name: "wrong Builtins return type",
		pkg:  &wrongBuiltins{},
		want: []string{"wrongbuiltins", "Builtins", "PackageBuiltins", "[]*elpsutil.Builtin", "[]lisp.LBuiltinDef", "silently ignored"},
	}, {
		name: "Builtins on the pointer receiver",
		pkg:  valueReceiverBuiltins{},
		want: []string{"valuereceiver", "Builtins", "declared on *valueReceiverBuiltins", "passed by value"},
	}, {
		name: "wrong PackageInit signature",
		pkg:  &wrongPackageInit{},
		want: []string{"wronginit", "PackageInit", "error", "silently ignored"},
	}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			env := testEnv(t)
			var rc *lisp.LVal
			require.NotPanics(t, func() {
				rc = elpsutil.Load(env, elpsutil.PackageLoader(test.pkg))
			})
			require.Equal(t, lisp.LError, rc.Type,
				"the package loaded cleanly and registered nothing: %v", rc)
			for _, want := range test.want {
				require.Contains(t, rc.String(), want)
			}
			require.Error(t, elpsutil.Validate(test.pkg))
		})
	}
}

// TestValidateReportsEmptyPackage documents the one check Validate makes that
// PackageLoader does not.
func TestValidateReportsEmptyPackage(t *testing.T) {
	err := elpsutil.Validate(&emptyPackage{})
	require.Error(t, err)
	require.Contains(t, err.Error(), "emptypkg")
	require.Contains(t, err.Error(), "has no effect")

	// PackageLoader still loads it: unusual is not illegal.
	env := testEnv(t)
	rc := elpsutil.Load(env, elpsutil.PackageLoader(&emptyPackage{}))
	require.NotEqual(t, lisp.LError, rc.Type, "PackageLoader rejected a namespace-only package: %v", rc)
}

func TestValidateRejectsNilPackage(t *testing.T) {
	require.Error(t, elpsutil.Validate(nil))
	env := testEnv(t)
	var rc *lisp.LVal
	require.NotPanics(t, func() {
		rc = elpsutil.Load(env, elpsutil.PackageLoader(nil))
	})
	require.Equal(t, lisp.LError, rc.Type)
	require.NotPanics(t, func() {
		rc = elpsutil.LibraryLoader(nil)(env)
	})
	require.Equal(t, lisp.LError, rc.Type)
}
