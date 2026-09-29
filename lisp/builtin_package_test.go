// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #736: a name resolves in the package of the code doing the lookup.
// A Go builtin or Go macro registered in a package other than lisp runs in
// its own package, like a Lisp function defined there; the caller's package
// is restored on return, on error, on panic and before a terminal expression.
// Core lisp -- builtins, macros, every special operator -- acts in the
// caller's package.

func currentPackage(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	return lisp.String(env.Runtime.Package.Name)
}

// ownPackageEnv is a user-package environment with two library packages,
// lib and lib2, whose Go builtins report and exercise the current package.
func ownPackageEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	for _, name := range []string{"lib", "lib2"} {
		require.NoError(t, lisp.GoError(env.DefinePackage(lisp.Symbol(name))))
		require.NoError(t, lisp.GoError(env.InPackage(lisp.Symbol(name))))
		env.AddBuiltins(true,
			elpsutil.Function("current", lisp.Formals(), currentPackage),
			// (ident x) is lisp:identity, for the call benchmark.
			elpsutil.Function("ident", lisp.Formals("x"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return args.Cells[0]
			}),
			// (call fn) calls fn with no arguments and returns
			// (package-before result package-after), all as seen by call.
			elpsutil.Function("call", lisp.Formals("fn"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				before := env.Runtime.Package.Name
				v := env.FunCall(args.Cells[0], lisp.Nil())
				if v.Type == lisp.LError {
					return v
				}
				return lisp.QExpr([]*lisp.LVal{lisp.String(before), v, lisp.String(env.Runtime.Package.Name)})
			}),
			// (call-with fn x) is (fn x), for a function value handed in.
			elpsutil.Function("call-with", lisp.Formals("fn", "x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return env.FunCall(args.Cells[0], lisp.QExpr([]*lisp.LVal{args.Cells[1]}))
			}),
			elpsutil.Function("fail", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				return env.Errorf("failed in %s", env.Runtime.Package.Name)
			}),
			elpsutil.Function("explode", lisp.Formals(), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				panic("library builtin panicked")
			}),
			// (term) returns (lisp:qualified-symbol x) as the caller's
			// terminal expression.
			elpsutil.Function("term", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				return env.Terminal(lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:qualified-symbol"), lisp.Symbol("x")}))
			}),
			// (enter pkg) tries to change the caller's package.
			elpsutil.Function("enter", lisp.Formals("pkg"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return env.InPackage(args.Cells[0])
			}),
			// (registered?) reports whether the current package is the
			// registry's own entry for this VM.
			elpsutil.Function("registered?", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				return lisp.Bool(env.Runtime.Package == env.Runtime.Registry.Package(env.Runtime.Package.Name))
			}),
		)
		env.AddMacros(true,
			// (mac) expands to (list "<package during expansion>"
			// (lisp:qualified-symbol x)): the second element is evaluated in
			// the caller.
			elpsutil.Function("mac", lisp.Formals(), func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
				return lisp.SExpr([]*lisp.LVal{
					lisp.Symbol("lisp:list"),
					lisp.String(env.Runtime.Package.Name),
					lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:qualified-symbol"), lisp.Symbol("x")}),
				})
			}),
		)
		env.AddSpecialOps(true,
			// An embedder's special operator is syntax: it never switches.
			elpsutil.Function("op", lisp.Formals(), currentPackage),
		)
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

func evalIn(t testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	return env.LoadString("test", src)
}

// assertLispEqual asserts got is equal? to the value of the expression want.
func assertLispEqual(t *testing.T, env *lisp.LEnv, want string, got *lisp.LVal) {
	t.Helper()
	w := evalIn(t, env, want)
	require.NotEqual(t, lisp.LError, w.Type, "%v", w)
	assert.True(t, lisp.True(got.Equal(w)), "got %v, want %v", got, want)
}

// TestLibraryBuiltinPackageSwitch is the switch/restore matrix.
func TestLibraryBuiltinPackageSwitch(t *testing.T) {
	for _, tc := range []struct {
		name, src, want string
	}{
		{"library builtin runs in its package", `(lib:current)`, `"lib"`},
		{"restored after return", `(lib:current) (lisp:qualified-symbol x)`, `'user:x`},
		{"core builtin acts in the caller", `(lisp:qualified-symbol x)`, `'user:x`},
		{"special op never switches", `(lib:op)`, `"user"`},
		{"call from the library's own Lisp", `(in-package 'lib) (lisp:defun via () (current)) (lisp:in-package 'user) (lib:via)`, `"lib"`},
		{"tail position", `(defun f () (lib:current)) (list (f) (qualified-symbol x))`, `'("lib" user:x)`},
		{"error restores", `(ignore-errors (lib:fail)) (qualified-symbol x)`, `'user:x`},
		{"error raised in the library package", `(handler-bind ((condition (lambda (c &rest _) (lisp:format-string "{}" (lisp:car _))))) (lib:fail))`, `"failed in lib"`},
		{"handler-bind catch restores", `(handler-bind ((condition (lambda (c &rest _) (qualified-symbol x)))) (lib:fail))`, `'user:x`},
		{"callback runs in its own package", `(lib:call (lambda () (list (qualified-symbol x) (lib:current))))`, `'("lib" (user:x "lib") "lib")`},
		{"nested library in callback", `(lib:call (lambda () (lib2:call (lambda () (lib:current)))))`, `'("lib" ("lib2" "lib" "lib2") "lib")`},
		{"callback error restores", `(ignore-errors (lib:call (lambda () (lib2:fail)))) (qualified-symbol x)`, `'user:x`},
		{"terminal expression runs in the caller", `(lib:term)`, `'user:x`},
		{"terminal in tail position", `(defun g () (lib:term)) (g)`, `'user:x`},
		{"library cannot change the caller's package", `(lib:enter 'lib2) (qualified-symbol x)`, `'user:x`},
		{"go macro expands in its package, expansion in caller", `(lib:mac)`, `'("lib" user:x)`},
		{"lisp function from another package still swaps", `(in-package 'other) (lisp:defun who () (lisp:qualified-symbol x)) (lisp:in-package 'user) (other:who)`, `'other:x`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := ownPackageEnv(t)
			v := evalIn(t, env, tc.src)
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			assertLispEqual(t, env, tc.want, v)
			assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name, "caller's package after the call")
		})
	}
}

// A panic out of a library builtin restores the caller's package: the
// deferred restore runs as the panic unwinds through call.
func TestLibraryBuiltinPanicRestoresPackage(t *testing.T) {
	env := ownPackageEnv(t)
	v := evalIn(t, env, `(lib:explode)`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, lisp.GoError(v).Error(), "library builtin panicked")
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)

	// And from a callback, two frames deep.
	v = evalIn(t, env, `(lib:call (lambda () (lib2:explode)))`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Equal(t, lisp.DefaultUserPackage, env.Runtime.Package.Name)
	v = evalIn(t, env, `(qualified-symbol x)`)
	assert.Equal(t, `'user:x`, v.String())
}

// A builtin whose package is not registered, or empty, does not switch,
// mirroring the Lisp path.
func TestUnregisteredPackageDoesNotSwitch(t *testing.T) {
	env := ownPackageEnv(t)
	for _, pkg := range []string{"nosuch", ""} {
		fn := lisp.FunInPackage(pkg, "reporter-"+pkg, lisp.Formals(), currentPackage)
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("reporter"), fn)))
		v := evalIn(t, env, `(reporter)`)
		require.NotEqual(t, lisp.LError, v.Type, "%v", v)
		assert.Equal(t, `"user"`, v.String(), "package %q", pkg)
	}
}

// Forked VMs switch to the fork's own package object, never the template's.
func TestLibraryBuiltinSwitchInTemplateVM(t *testing.T) {
	source := ownPackageEnv(t)
	tmpl, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	for range 2 {
		vm, err := tmpl.NewVM()
		require.NoError(t, err)
		require.NoError(t, lisp.GoError(vm.InPackage(lisp.String(lisp.DefaultUserPackage))))
		v := evalIn(t, vm, `(list (lib:current) (lib:registered?) (lib:call (lambda () (lib2:registered?))) (qualified-symbol x))`)
		require.NotEqual(t, lisp.LError, v.Type, "%v", v)
		w := evalIn(t, vm, `'("lib" true ("lib" true "lib") user:x)`)
		assert.True(t, lisp.True(v.Equal(w)), "got %v", v)
		assert.Same(t, vm.Runtime.Registry.Package(lisp.DefaultUserPackage), vm.Runtime.Package)
	}
}

// TestCoreBuiltinsResolveInCaller is the parity table: every core builtin
// that resolves a symbol or evaluates code does it in the caller's package,
// called directly from package p, which shadows the names, and from a Lisp
// callback a library builtin runs.  The last rows pin the documented
// consequence of the rule: a core builtin handed to a library AS A VALUE
// runs in the library's package, so a quoted name it resolves resolves there.
func TestCoreBuiltinsResolveInCaller(t *testing.T) {
	env := ownPackageEnv(t)
	setup := `
(in-package 'p)
(defun pick (x) (list 'p x))
(defun pick2 (a b) (list 'p a b))
(defun keep? (x) true)
(defun less? (a b) (< a b))
(defmacro pmac (x) (quasiquote (list 'p-mac (unquote x))))
(deftype ptype (x) (list 'p-type x))
(set 'pvar 'p-var)
`
	require.NoError(t, lisp.GoError(evalIn(t, env, setup)))
	for _, tc := range []struct{ name, expr, want string }{
		{"funcall", `(funcall 'pick 1)`, `'(p 1)`},
		{"apply", `(apply 'pick '(1))`, `'(p 1)`},
		{"unpack", `(unpack 'pick '(1))`, `'(p 1)`},
		{"map", `(map 'list 'pick '(1))`, `'((p 1))`},
		{"foldl", `(foldl 'pick2 0 '(1))`, `'(p 0 1)`},
		{"foldr", `(foldr 'pick2 0 '(1))`, `'(p 1 0)`},
		{"select", `(select 'list 'keep? '(1 2))`, `'(1 2)`},
		{"reject", `(reject 'list 'keep? '(1 2))`, `'()`},
		{"stable-sort", `(stable-sort 'less? (list 2 1))`, `'(1 2)`},
		{"search-sorted", `(search-sorted 3 'keep?)`, `0`},
		{"all?", `(all? 'keep? '(1))`, `true`},
		{"any?", `(any? 'keep? '(1))`, `true`},
		{"compose", `(funcall (compose 'pick 'pick) 1)`, `'(p (p 1))`},
		{"flip", `(funcall (flip 'pick2) 1 2)`, `'(p 2 1)`},
		{"eval", `(eval '(pick 1))`, `'(p 1)`},
		{"macroexpand", `(macroexpand '(pmac 1))`, `'(list 'p-mac 1)`},
		{"macroexpand-1", `(macroexpand-1 '(pmac 1))`, `'(list 'p-mac 1)`},
		{"new", `(user-data (new 'ptype 1))`, `'(p-type 1)`},
		{"function", `(funcall (function pick) 1)`, `'(p 1)`},
		{"qualified-symbol", `(qualified-symbol pick)`, `'p:pick`},
		{"load-string", `(load-string "(pick 1)")`, `'(p 1)`},
		{"symbol value", `pvar`, `'p-var`},
	} {
		t.Run(tc.name+"/direct", func(t *testing.T) {
			v := evalIn(t, env, `(in-package 'p) `+tc.expr)
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			assertLispEqual(t, env, tc.want, v)
		})
		t.Run(tc.name+"/in-library-callback", func(t *testing.T) {
			v := evalIn(t, env, `(in-package 'p) (lisp:nth (lib:call (lambda () `+tc.expr+`)) 1)`)
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			assertLispEqual(t, env, tc.want, v)
		})
	}

	// Documented: a core builtin handed to a library as a value runs where
	// the library calls it.  funcall then resolves the quoted name in lib,
	// which has no pick.  Passing the function value is the fix.
	v := evalIn(t, env, `(in-package 'p) (lib:call-with (lambda (f) (funcall f 1)) pick)`)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	assertLispEqual(t, env, `'(p 1)`, v)
	v = evalIn(t, env, `(in-package 'p) (lib:call-with funcall 'pick)`)
	require.Equal(t, lisp.LError, v.Type, "a quoted name passed into a library resolves there")
	assert.Contains(t, lisp.GoError(v).Error(), "unbound symbol: 'pick")
}

// BenchmarkBuiltinCall measures a Go builtin call: a core builtin
// (lisp:identity), and a library builtin with the same body called from its
// own package and across packages (the only case that switches).
func BenchmarkBuiltinCall(b *testing.B) {
	for _, bc := range []struct{ name, pkg, fn string }{
		{"Core", lisp.DefaultUserPackage, "lisp:identity"},
		{"LibSamePkg", "lib", "lib:ident"},
		{"LibCrossPkg", lisp.DefaultUserPackage, "lib:ident"},
	} {
		b.Run(bc.name, func(b *testing.B) {
			env := ownPackageEnv(b)
			fn := env.Get(lisp.Symbol(bc.fn))
			require.Equal(b, lisp.LFun, fn.Type)
			require.NoError(b, lisp.GoError(env.InPackage(lisp.String(bc.pkg))))
			args := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
			b.ReportAllocs()
			b.ResetTimer()
			for range b.N {
				if v := env.FunCall(fn, args); v.Type == lisp.LError {
					b.Fatal(v)
				}
			}
		})
	}
}
