// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// testPackage is a Package assembled from whatever the test needs it to
// contribute.  The optional Package interfaces are all satisfied, so the
// registration paths under test are always reached.
type testPackage struct {
	name       string
	builtins   []lisp.LBuiltinDef
	specialOps []lisp.LBuiltinDef
	macros     []lisp.LBuiltinDef
	initFn     elpsutil.Loader
}

func (p *testPackage) PackageName() string { return p.name }

func (p *testPackage) Builtins() []lisp.LBuiltinDef { return p.builtins }

func (p *testPackage) SpecialOps() []lisp.LBuiltinDef { return p.specialOps }

func (p *testPackage) Macros() []lisp.LBuiltinDef { return p.macros }

func (p *testPackage) PackageInit(env *lisp.LEnv) *lisp.LVal {
	if p.initFn == nil {
		return lisp.Nil()
	}
	return p.initFn(env)
}

func nopBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return lisp.Nil() }

func testEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

// defKind names one of the three definition lists a Package can contribute.
// Every check that applies to a builtin applies identically to a special
// operator and to a macro -- they are three copies of the same registration
// code in lisp -- so the mistake table below is run against all three.  A
// fourth kind added to PackageLoader must be added here, or the source sweep
// in TestPackageLoaderKindsAreCovered fails.
type defKind struct {
	name  string
	build func(defs ...lisp.LBuiltinDef) *testPackage
}

func defKinds(pkgName string) []defKind {
	return []defKind{
		{"builtin", func(defs ...lisp.LBuiltinDef) *testPackage {
			return &testPackage{name: pkgName, builtins: defs}
		}},
		{"special operator", func(defs ...lisp.LBuiltinDef) *testPackage {
			return &testPackage{name: pkgName, specialOps: defs}
		}},
		{"macro", func(defs ...lisp.LBuiltinDef) *testPackage {
			return &testPackage{name: pkgName, macros: defs}
		}},
	}
}

// TestPackageLoaderRejectsBadDefinitions covers issue #351 defects 1 and 3: a
// definition registered with nil formals, and a definition whose name collides
// with a symbol the package already answers.  Both currently reach lisp -- the
// first as a nil dereference inside LEnv.bind at call time, reported as an
// opaque internal-panic, the second as an outright panic at registration.
//
// elpsutil is the boundary between an embedder's Go code and the interpreter.
// A mistake in a package definition is an ordinary, recoverable failure on a
// path where every other failure returns an *LVal error, so it is caught here
// and reported as one.  The panics in lisp are unchanged: they still mean
// "this is a bug in the interpreter, or in code that had no business calling
// this".
func TestPackageLoaderRejectsBadDefinitions(t *testing.T) {
	tests := []struct {
		name string
		def  func() lisp.LBuiltinDef
		// want are substrings the error must contain.  Every case requires
		// the offending definition's name: an error that says only "invalid
		// formals" makes the embedder hunt.
		want []string
	}{{
		name: "nil formals",
		def:  func() lisp.LBuiltinDef { return elpsutil.Function("takes-nothing", nil, nopBuiltin) },
		want: []string{"testpkg", "takes-nothing", "formals are nil", "lisp.Formals()"},
	}, {
		name: "error formals",
		// lisp.Formals reports a misplaced &rest by returning an error value,
		// which is easy to store unexamined.
		def: func() lisp.LBuiltinDef {
			return elpsutil.Function("bad-rest", lisp.Formals("&rest", "a", "b"), nopBuiltin)
		},
		want: []string{"testpkg", "bad-rest", "formals are an error value"},
	}, {
		name: "formals not a list",
		def: func() lisp.LBuiltinDef {
			return elpsutil.Function("scalar-formals", lisp.Symbol("a"), nopBuiltin)
		},
		want: []string{"testpkg", "scalar-formals", "not a list"},
	}, {
		name: "nil formal cell",
		def: func() lisp.LBuiltinDef {
			return elpsutil.Function("nil-cell", lisp.QExpr([]*lisp.LVal{nil}), nopBuiltin)
		},
		want: []string{"testpkg", "nil-cell", "formal argument 0 is nil"},
	}, {
		name: "non-symbol formal cell",
		def: func() lisp.LBuiltinDef {
			return elpsutil.Function("int-cell", lisp.QExpr([]*lisp.LVal{lisp.Int(1)}), nopBuiltin)
		},
		want: []string{"testpkg", "int-cell", "formal argument 0", "not a symbol"},
	}, {
		name: "reserved name true",
		def:  func() lisp.LBuiltinDef { return elpsutil.Function("true", lisp.Formals(), nopBuiltin) },
		want: []string{"testpkg", `"true"`, "reserved constant"},
	}, {
		name: "reserved name false",
		def:  func() lisp.LBuiltinDef { return elpsutil.Function("false", lisp.Formals(), nopBuiltin) },
		want: []string{"testpkg", `"false"`, "reserved constant"},
	}, {
		name: "nil definition",
		def:  func() lisp.LBuiltinDef { return nil },
		want: []string{"testpkg", "is nil"},
	}, {
		name: "typed nil definition",
		def:  func() lisp.LBuiltinDef { return (*elpsutil.Builtin)(nil) },
		want: []string{"testpkg", "is nil"},
	}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			for _, kind := range defKinds("testpkg") {
				t.Run(kind.name, func(t *testing.T) {
					env := testEnv(t)
					pkg := kind.build(test.def())
					var rc *lisp.LVal
					require.NotPanics(t, func() {
						rc = elpsutil.Load(env, elpsutil.PackageLoader(pkg))
					})
					require.Equal(t, lisp.LError, rc.Type, "load succeeded: %v", rc)
					msg := rc.String()
					for _, want := range test.want {
						require.Contains(t, msg, want)
					}
					// The kind belongs in the message too, so an embedder
					// with a name used by both a builtin and a macro knows
					// which list to look in.
					require.Contains(t, msg, kind.name)

					// Validate is the same checks without an env, for use in
					// an embedder's own test.
					require.Error(t, elpsutil.Validate(pkg))
				})
			}
		})
	}
}

// TestPackageLoaderRejectsDuplicateNames covers the redefinition half of
// defect 3: a name already bound in the target package.  lisp panics with
// "symbol already defined" at registration.
func TestPackageLoaderRejectsDuplicateNames(t *testing.T) {
	env := testEnv(t)
	first := &testPackage{
		name:     "dup",
		builtins: []lisp.LBuiltinDef{elpsutil.Function("f", lisp.Formals(), nopBuiltin)},
	}
	rc := elpsutil.Load(env, elpsutil.PackageLoader(first))
	require.NotEqual(t, lisp.LError, rc.Type, "first load failed: %v", rc)

	second := &testPackage{
		name:     "dup",
		builtins: []lisp.LBuiltinDef{elpsutil.Function("f", lisp.Formals(), nopBuiltin)},
	}
	require.NotPanics(t, func() {
		rc = elpsutil.Load(env, elpsutil.PackageLoader(second))
	})
	require.Equal(t, lisp.LError, rc.Type, "reloading a package redefined f without complaint")
	require.Contains(t, rc.String(), `"f"`)
	require.Contains(t, rc.String(), "already defined")
	require.Contains(t, rc.String(), "dup")
}

// TestPackageLoaderRejectsIntraPackageDuplicates checks a package that names
// the same symbol twice in its own definitions, which Validate can catch
// without an env.
func TestPackageLoaderRejectsIntraPackageDuplicates(t *testing.T) {
	pkg := &testPackage{
		name: "dup2",
		builtins: []lisp.LBuiltinDef{
			elpsutil.Function("f", lisp.Formals(), nopBuiltin),
			elpsutil.Function("f", lisp.Formals("a"), nopBuiltin),
		},
	}
	err := elpsutil.Validate(pkg)
	require.Error(t, err)
	require.Contains(t, err.Error(), `"f"`)

	env := testEnv(t)
	var rc *lisp.LVal
	require.NotPanics(t, func() {
		rc = elpsutil.Load(env, elpsutil.PackageLoader(pkg))
	})
	require.Equal(t, lisp.LError, rc.Type, "load accepted a duplicated name: %v", rc)
}

// TestPackageLoaderAcceptsGoodDefinitions is the other half of the contract:
// correct code sees no difference.  A well-formed package still loads, its
// builtins are still callable, and the values they return are unchanged.
func TestPackageLoaderAcceptsGoodDefinitions(t *testing.T) {
	env := testEnv(t)
	pkg := &testPackage{
		name: "good",
		builtins: []lisp.LBuiltinDef{
			elpsutil.Function("nullary", lisp.Formals(), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return lisp.Int(1)
			}),
			elpsutil.Function("unary", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return args.Cells[0]
			}),
			// A builtin's variadic argument arrives flattened into Cells.
			elpsutil.Function("varargs", lisp.Formals("&rest", "xs"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return lisp.Int(len(args.Cells))
			}),
			elpsutil.Function("optional", lisp.Formals("&optional", "x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return args.Cells[0]
			}),
			elpsutil.Function("keyword", lisp.Formals("&key", "x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return args.Cells[0]
			}),
		},
	}
	require.NoError(t, elpsutil.Validate(pkg))
	rc := elpsutil.Load(env, elpsutil.PackageLoader(pkg))
	require.NotEqual(t, lisp.LError, rc.Type, "load failed: %v", rc)

	for _, test := range []struct {
		expr string
		want string
	}{
		{"(good:nullary)", "1"},
		{"(good:unary 2)", "2"},
		{"(good:varargs 1 2 3)", "3"},
		{"(good:optional)", "()"},
		{"(good:keyword :x 7)", "7"},
	} {
		v := evalString(t, env, test.expr)
		require.NotEqual(t, lisp.LError, v.Type, "%s => %v", test.expr, v)
		require.Equal(t, test.want, v.String(), "%s", test.expr)
	}
}

// TestNilFormalsWouldPanicAtCallTime pins the reason nil formals must be
// rejected at registration: without the boundary check the nil is stored in
// Cells[0] and first dereferenced by LEnv.bind, long after the embedder's
// registration code has returned successfully.
func TestNilFormalsWouldPanicAtCallTime(t *testing.T) {
	env := testEnv(t)
	fn := lisp.FunInPackage("user", "<test>", nil, nopBuiltin)
	env.PutGlobal(lisp.Symbol("nil-formals"), fn)
	v := evalString(t, env, "(nil-formals)")
	require.Equal(t, lisp.LError, v.Type,
		"expected the unguarded path to fail; if this now succeeds the boundary check may be unnecessary")
}

func evalString(t *testing.T, env *lisp.LEnv, expr string) *lisp.LVal {
	t.Helper()
	var v *lisp.LVal
	require.NotPanics(t, func() {
		v = env.LoadString("test.lisp", expr)
	})
	return v
}
