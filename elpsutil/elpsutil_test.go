// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// testPackage is a configurable elpsutil.Package used to exercise the package
// restoration invariant.
type testPackage struct {
	name     string
	doc      string
	init     func(env *lisp.LEnv) *lisp.LVal
	builtins []lisp.LBuiltinDef
	ops      []lisp.LBuiltinDef
	macros   []lisp.LBuiltinDef
}

var (
	_ elpsutil.PackageInit       = (*testPackage)(nil)
	_ elpsutil.PackageDocumented = (*testPackage)(nil)
	_ elpsutil.PackageBuiltins   = (*testPackage)(nil)
	_ elpsutil.PackageSpecialOps = (*testPackage)(nil)
	_ elpsutil.PackageMacros     = (*testPackage)(nil)
)

func (p *testPackage) PackageName() string { return p.name }

func (p *testPackage) PackageDoc() string { return p.doc }

func (p *testPackage) PackageInit(env *lisp.LEnv) *lisp.LVal {
	if p.init == nil {
		return lisp.Nil()
	}
	return p.init(env)
}

func (p *testPackage) Builtins() []lisp.LBuiltinDef { return p.builtins }

func (p *testPackage) SpecialOps() []lisp.LBuiltinDef { return p.ops }

func (p *testPackage) Macros() []lisp.LBuiltinDef { return p.macros }

// nilFun is a trivial builtin implementation used by test packages.
func nilFun(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() }

func testBuiltin(name string) lisp.LBuiltinDef {
	return elpsutil.Function(name, lisp.Formals(), nilFun)
}

func newTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv: %v", rc)
	require.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
	return env
}

// assertBoundIn asserts that sym is bound and exported in package pkg, and
// that it is not present in any other package.
func assertBoundIn(t *testing.T, env *lisp.LEnv, pkg string, sym string) {
	t.Helper()
	for _, name := range env.Runtime.Registry.PackageNames() {
		p := env.Runtime.Registry.Package(name)
		v := p.Get(lisp.Symbol(sym))
		if name == pkg {
			assert.NotEqualf(t, lisp.LError, v.Type,
				"symbol %q should be bound in package %q", sym, pkg)
			assert.Containsf(t, p.Externals(), sym,
				"symbol %q should be exported from package %q", sym, pkg)
			continue
		}
		assert.Equalf(t, lisp.LError, v.Type,
			"symbol %q should not be bound in package %q (expected it in %q)", sym, name, pkg)
	}
}

// TestPackageLoader_EachPackageRestoresPrevious mirrors
// lisplib.TestLoadLibrary_EachPackageRestoresPrevious (the test pinning the fix
// for https://github.com/luthersystems/elps/issues/99) for the embedder-facing
// elpsutil.PackageLoader.  See
// https://github.com/luthersystems/elps/issues/352.
func TestPackageLoader_EachPackageRestoresPrevious(t *testing.T) {
	env := newTestEnv(t)

	pkgs := []*testPackage{
		// A package with no PackageInit at all.
		{name: "plain", builtins: []lisp.LBuiltinDef{testBuiltin("plain-fn")}},
		// A package whose PackageInit does not move.
		{
			name:     "stationary",
			init:     func(env *lisp.LEnv) *lisp.LVal { return lisp.Nil() },
			builtins: []lisp.LBuiltinDef{testBuiltin("stationary-fn")},
		},
		// A package whose PackageInit switches package directly, as
		// elpsutil.Loader's documentation tells embedders to do.
		{
			name: "switcher",
			init: func(env *lisp.LEnv) *lisp.LVal {
				return elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{
					name:     "switcher-dep",
					builtins: []lisp.LBuiltinDef{testBuiltin("switcher-dep-fn")},
				}))
			},
			builtins: []lisp.LBuiltinDef{testBuiltin("switcher-fn")},
			ops:      []lisp.LBuiltinDef{testBuiltin("switcher-op")},
			macros:   []lisp.LBuiltinDef{testBuiltin("switcher-macro")},
		},
		// A package whose PackageInit lands in an unrelated package and never
		// comes back.
		{
			name: "wanderer",
			init: func(env *lisp.LEnv) *lisp.LVal {
				name := lisp.Symbol("elsewhere")
				if e := env.DefinePackage(name); !e.IsNil() {
					return e
				}
				return env.InPackage(name)
			},
			builtins: []lisp.LBuiltinDef{testBuiltin("wanderer-fn")},
			ops:      []lisp.LBuiltinDef{testBuiltin("wanderer-op")},
			macros:   []lisp.LBuiltinDef{testBuiltin("wanderer-macro")},
		},
	}

	for _, p := range pkgs {
		t.Run(p.name, func(t *testing.T) {
			before := env.Runtime.Package.Name
			rc := elpsutil.PackageLoader(p)(env)
			require.Truef(t, rc.IsNil(), "PackageLoader(%s) failed: %v", p.name, rc)
			assert.Equalf(t, before, env.Runtime.Package.Name,
				"PackageLoader(%s) should restore the previous package", p.name)
			for _, fn := range p.builtins {
				assertBoundIn(t, env, p.name, fn.Name())
			}
			for _, fn := range p.ops {
				assertBoundIn(t, env, p.name, fn.Name())
			}
			for _, fn := range p.macros {
				assertBoundIn(t, env, p.name, fn.Name())
			}
		})
	}
}

// TestPackageLoader_RestoresNonUserCaller checks that the package restored is
// the caller's package, not simply the user package.
func TestPackageLoader_RestoresNonUserCaller(t *testing.T) {
	env := newTestEnv(t)
	caller := lisp.Symbol("caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	p := &testPackage{
		name: "callee",
		init: func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{name: "callee-dep"}))
		},
		builtins: []lisp.LBuiltinDef{testBuiltin("callee-fn")},
	}
	rc := elpsutil.PackageLoader(p)(env)
	require.True(t, rc.IsNil(), "PackageLoader: %v", rc)
	assert.Equal(t, "caller", env.Runtime.Package.Name,
		"PackageLoader should restore the caller's package, not the user package")
	assertBoundIn(t, env, "callee", "callee-fn")
}

// TestPackageLoader_PackageDocNotStolen checks that a PackageInit which
// switches packages cannot cause the package documentation of the loaded
// package to be applied elsewhere, and that documentation lands on the loaded
// package.
func TestPackageLoader_PackageDoc(t *testing.T) {
	env := newTestEnv(t)
	userDoc := env.Runtime.Registry.Package(lisp.DefaultUserPackage).Doc
	p := &testPackage{
		name: "documented",
		doc:  "a documented package",
		init: func(env *lisp.LEnv) *lisp.LVal {
			return elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{name: "documented-dep"}))
		},
	}
	require.True(t, elpsutil.PackageLoader(p)(env).IsNil())
	assert.Equal(t, "a documented package", env.Runtime.Registry.Package("documented").Doc)
	assert.Equal(t, userDoc, env.Runtime.Registry.Package(lisp.DefaultUserPackage).Doc,
		"the user package documentation should be untouched")
	assert.Empty(t, env.Runtime.Registry.Package("documented-dep").Doc)
}

// TestPackageLoader_ErrorRestoresPrevious checks that the caller's package is
// restored even when loading fails.
func TestPackageLoader_ErrorRestoresPrevious(t *testing.T) {
	env := newTestEnv(t)
	p := &testPackage{
		name: "failing",
		init: func(env *lisp.LEnv) *lisp.LVal {
			name := lisp.Symbol("failing-elsewhere")
			if e := env.DefinePackage(name); !e.IsNil() {
				return e
			}
			if e := env.InPackage(name); !e.IsNil() {
				return e
			}
			return env.Errorf("init failed")
		},
	}
	rc := elpsutil.PackageLoader(p)(env)
	require.Equal(t, lisp.LError, rc.Type, "PackageLoader should propagate the init error")
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name,
		"PackageLoader should restore the previous package when loading fails")
}

// TestLoad_RestoresPrevious checks elpsutil.Load restores the caller's package.
func TestLoad_RestoresPrevious(t *testing.T) {
	env := newTestEnv(t)
	caller := lisp.Symbol("load-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	rc := elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{
		name:     "load-callee",
		builtins: []lisp.LBuiltinDef{testBuiltin("load-callee-fn")},
	}))
	require.True(t, rc.IsNil(), "Load: %v", rc)
	assert.Equal(t, "load-caller", env.Runtime.Package.Name,
		"Load should restore the caller's package")
	assertBoundIn(t, env, "load-callee", "load-callee-fn")
}

// TestLoad_FromUserPackage pins the conventional top-level behaviour: a load
// started from the user package ends in the user package.
func TestLoad_FromUserPackage(t *testing.T) {
	env := newTestEnv(t)
	rc := elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{name: "top-level"}))
	require.True(t, rc.IsNil(), "Load: %v", rc)
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
}

// TestLoadAll_RestoresPrevious checks elpsutil.LoadAll restores the caller's
// package, both between and after the loaders it runs.
func TestLoadAll_RestoresPrevious(t *testing.T) {
	env := newTestEnv(t)
	caller := lisp.Symbol("loadall-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	var between []string
	loader := elpsutil.LoadAll(
		elpsutil.PackageLoader(&testPackage{
			name:     "loadall-a",
			builtins: []lisp.LBuiltinDef{testBuiltin("loadall-a-fn")},
		}),
		func(env *lisp.LEnv) *lisp.LVal {
			between = append(between, env.Runtime.Package.Name)
			return lisp.Nil()
		},
		elpsutil.PackageLoader(&testPackage{
			name:     "loadall-b",
			builtins: []lisp.LBuiltinDef{testBuiltin("loadall-b-fn")},
		}),
	)
	rc := loader(env)
	require.True(t, rc.IsNil(), "LoadAll: %v", rc)
	assert.Equal(t, []string{"loadall-caller"}, between,
		"LoadAll should run each loader from the caller's package")
	assert.Equal(t, "loadall-caller", env.Runtime.Package.Name,
		"LoadAll should restore the caller's package")
	assertBoundIn(t, env, "loadall-a", "loadall-a-fn")
	assertBoundIn(t, env, "loadall-b", "loadall-b-fn")
}

// TestLibraryLoader_RestoresPrevious checks elpsutil.LibraryLoader restores the
// caller's package and puts each package's symbols in the right place.
func TestLibraryLoader_RestoresPrevious(t *testing.T) {
	env := newTestEnv(t)
	caller := lisp.Symbol("lib-caller")
	require.True(t, env.DefinePackage(caller).IsNil())
	require.True(t, env.InPackage(caller).IsNil())

	rc := elpsutil.LibraryLoader(
		&testPackage{name: "lib-a", builtins: []lisp.LBuiltinDef{testBuiltin("lib-a-fn")}},
		&testPackage{
			name: "lib-b",
			init: func(env *lisp.LEnv) *lisp.LVal {
				return elpsutil.Load(env, elpsutil.PackageLoader(&testPackage{name: "lib-b-dep"}))
			},
			builtins: []lisp.LBuiltinDef{testBuiltin("lib-b-fn")},
		},
	)(env)
	require.True(t, rc.IsNil(), "LibraryLoader: %v", rc)
	assert.Equal(t, "lib-caller", env.Runtime.Package.Name)
	assertBoundIn(t, env, "lib-a", "lib-a-fn")
	assertBoundIn(t, env, "lib-b", "lib-b-fn")
}

// TestPackageLoader_SubstrateShape reproduces the shape of
// luthersystems/substrate's ShiroPackage.PackageInit, which loads a nested
// library and then explicitly switches back into its own package before
// re-exporting the sub-package symbols.  The explicit restoration must remain
// correct (a redundant no-op) after the fix for #352.
func TestPackageLoader_SubstrateShape(t *testing.T) {
	subs := []elpsutil.Package{
		&testPackage{name: "sub-a", builtins: []lisp.LBuiltinDef{testBuiltin("sub-a-fn")}},
		&testPackage{name: "sub-b", builtins: []lisp.LBuiltinDef{testBuiltin("sub-b-fn")}},
	}

	for _, withWorkaround := range []bool{true, false} {
		name := "without-manual-restore"
		if withWorkaround {
			name = "with-manual-restore"
		}
		t.Run(name, func(t *testing.T) {
			env := newTestEnv(t)
			cc := &testPackage{
				name: "cc",
				init: func(env *lisp.LEnv) *lisp.LVal {
					lerr := elpsutil.Load(env, elpsutil.LibraryLoader(subs...))
					if lerr.Type == lisp.LError {
						return lerr
					}
					if withWorkaround {
						// substrate's compensation for #352.
						lerr = env.InPackage(lisp.Symbol("cc"))
						if lerr.Type == lisp.LError {
							return lerr
						}
					}
					for _, sub := range subs {
						lerr := env.UsePackage(lisp.Symbol(sub.PackageName()))
						if lerr.Type == lisp.LError {
							return lerr
						}
						pkg := env.Runtime.Registry.Package(sub.PackageName())
						for _, ext := range pkg.Externals() {
							env.Runtime.Package.Export(ext)
						}
					}
					return lisp.Nil()
				},
				builtins: []lisp.LBuiltinDef{testBuiltin("cc-fn")},
			}

			rc := elpsutil.Load(env, elpsutil.PackageLoader(cc))
			require.True(t, rc.IsNil(), "Load: %v", rc)
			assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)

			ccPkg := env.Runtime.Registry.Package("cc")
			require.NotNil(t, ccPkg)
			// cc's own builtin must be bound in cc.
			assert.NotEqual(t, lisp.LError, ccPkg.Get(lisp.Symbol("cc-fn")).Type,
				"cc-fn should be bound in the cc package")
			assert.Contains(t, ccPkg.Externals(), "cc-fn")
			// The re-exported sub-package symbols must be in cc, not user.
			for _, sym := range []string{"sub-a-fn", "sub-b-fn"} {
				assert.NotEqualf(t, lisp.LError, ccPkg.Get(lisp.Symbol(sym)).Type,
					"%s should be re-exported into the cc package", sym)
				assert.Containsf(t, ccPkg.Externals(), sym, "%s should be external in cc", sym)
			}
			userPkg := env.Runtime.Registry.Package(lisp.DefaultUserPackage)
			for _, sym := range []string{"cc-fn", "sub-a-fn", "sub-b-fn"} {
				assert.Equalf(t, lisp.LError, userPkg.Get(lisp.Symbol(sym)).Type,
					"%s should not leak into the user package", sym)
			}
		})
	}
}

// TestFunctionDoc_Docstring checks the constructor and the accessor. Builtin
// must satisfy the unexported interface lisp checks for when it registers a
// definition -- a duck-typed `interface{ Docstring() string }` -- which is why
// the assertion is written against that shape rather than against the concrete
// type.
func TestFunctionDoc_Docstring(t *testing.T) {
	const docs = "Blend two paths.\n\nDeprecated: use join-paths instead."

	documented, ok := interface{}(elpsutil.FunctionDoc(
		"blend-paths", lisp.Formals("a", "b"), nilFun, docs)).(interface{ Docstring() string })
	require.True(t, ok, "*Builtin must satisfy the docstring interface lisp duck-types")
	assert.Equal(t, docs, documented.Docstring())

	// Function keeps working unchanged, with no documentation.
	plain, ok := interface{}(elpsutil.Function(
		"blend-paths", lisp.Formals("a", "b"), nilFun)).(interface{ Docstring() string })
	require.True(t, ok)
	assert.Empty(t, plain.Docstring())
}

// TestFunctionDoc_ReachesRegisteredValue follows the docstring all the way
// along the path an embedder's linter run uses: FunctionDoc -> AddBuiltins ->
// the registered LVal -> analysis.ExtractPackageExports -> the deprecation
// detector. Every link is somebody else's code, so the round trip is the
// assertion rather than the accessor on its own.
func TestFunctionDoc_ReachesRegisteredValue(t *testing.T) {
	const docs = "Blend two paths.\n\nDeprecated: use join-paths instead."

	env := newTestEnv(t)
	require.True(t, env.DefinePackage(lisp.Symbol("substrate")).IsNil())
	require.True(t, env.InPackage(lisp.Symbol("substrate")).IsNil())
	env.AddBuiltins(true,
		elpsutil.FunctionDoc("blend-paths", lisp.Formals("a", "b"), nilFun, docs),
		elpsutil.Function("join-paths", lisp.Formals("a", "b"), nilFun),
	)
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())

	pkg := env.Runtime.Registry.Package("substrate")
	require.NotNil(t, pkg)
	val, ok := pkg.Symbol("blend-paths")
	require.True(t, ok, "blend-paths should be registered")
	assert.Equal(t, docs, val.Docstring(), "the docstring must reach the registered value")

	exports := analysis.ExtractPackageExports(env.Runtime.Registry)
	byName := make(map[string]analysis.ExternalSymbol)
	for _, sym := range exports["substrate"] {
		byName[sym.Name] = sym
	}
	require.Contains(t, byName, "blend-paths")
	require.Contains(t, byName, "join-paths")

	notice, deprecated := lisp.DeprecationNotice(byName["blend-paths"].DocString)
	assert.True(t, deprecated, "the exported symbol must read as deprecated")
	assert.Equal(t, "use join-paths instead.", notice)

	_, deprecated = lisp.DeprecationNotice(byName["join-paths"].DocString)
	assert.False(t, deprecated, "an undocumented builtin must not read as deprecated")
}
