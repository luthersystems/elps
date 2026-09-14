// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func packageBuiltinEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	return env
}

// Eval directly: LoadString restores the current package and would hide D-F8.
func evalPackageBuiltin(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	exprs, err := env.Runtime.Reader.Read("package-builtins.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	return env.Eval(exprs[0])
}

func TestPackageBuiltinExportAtomic(t *testing.T) {
	for _, src := range []string{
		`(export '(a 1 b))`,
		`(export 'a 1 'b)`,
		`(export 'a '(b (c 1)))`,
	} {
		t.Run(src, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			env.Runtime.Package.Exports("existing")
			got := evalPackageBuiltin(t, env, src)
			assert.Equal(t, lisp.LError, got.Type, "expected export error, got %s", got)
			assert.False(t, lisp.IsInternalPanic(got))
			assert.Equal(t, []string{"existing"}, env.Runtime.Package.Externals(), "failed export must not change exports")
		})
	}
}

func TestPackageBuiltinInPackageAtomic(t *testing.T) {
	for _, existing := range []bool{false, true} {
		t.Run(fmt.Sprint(existing), func(t *testing.T) {
			env := packageBuiltinEnv(t)
			before := env.Runtime.Package
			if existing {
				env.Runtime.Registry.DefinePackage("docp2").Doc = "original"
			}
			got := evalPackageBuiltin(t, env, `(in-package 'docp2 "valid" 1)`)
			require.Equal(t, lisp.LError, got.Type)
			assert.Contains(t, got.String(), "docstring argument is not a string")
			assert.Same(t, before, env.Runtime.Package, "invalid docstring switched current package")
			if existing {
				assert.Equal(t, "original", env.Runtime.Registry.Package("docp2").Doc)
			} else {
				assert.Nil(t, env.Runtime.Registry.Package("docp2"), "invalid docstring created a package")
			}
		})
	}
	t.Run("caught", func(t *testing.T) {
		env := packageBuiltinEnv(t)
		before := env.Runtime.Package
		got := evalPackageBuiltin(t, env, `(handler-bind ((error (lambda (&rest args) ()))) (in-package 'docp2 1))`)
		require.True(t, got.IsNil(), "%s", got)
		require.True(t, evalPackageBuiltin(t, env, `(set 'after-error 42)`).Type == lisp.LInt)
		_, ok := before.Symbol("after-error")
		assert.True(t, ok, "binding after caught error went into wrong package")
	})
}

func TestPackageBuiltinQualifiedExport(t *testing.T) {
	for _, src := range []string{`(export 'qe:f)`, `(export "qe:f")`, `(export '(a qe:f b))`} {
		t.Run(src, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			require.True(t, evalPackageBuiltin(t, env, `(in-package 'qf)`).IsNil())
			got := evalPackageBuiltin(t, env, src)
			assert.Equal(t, lisp.LError, got.Type, "expected qualified export error, got %s", got)
			assert.False(t, lisp.IsInternalPanic(got))
			assert.Contains(t, got.String(), "export")
			assert.Contains(t, got.String(), "qe:f")
			assert.Empty(t, env.Runtime.Package.Externals())
			require.True(t, evalPackageBuiltin(t, env, `(in-package 'user)`).IsNil())
			got = evalPackageBuiltin(t, env, `(use-package 'qf)`)
			assert.True(t, got.IsNil(), "rejected export must leave package importable: %s", got)
		})
	}
}

func TestPackageBuiltinDeferredExport(t *testing.T) {
	env := packageBuiltinEnv(t)
	require.True(t, evalPackageBuiltin(t, env, `(in-package 'deferred)`).IsNil())
	require.True(t, evalPackageBuiltin(t, env, `(export 'never-defined)`).IsNil())
	require.True(t, evalPackageBuiltin(t, env, `(in-package 'user)`).IsNil())
	got := evalPackageBuiltin(t, env, `(use-package 'deferred)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Contains(t, got.String(), `package deferred: exported symbol is unbound: never-defined`)
	assert.Contains(t, got.String(), "package-builtins.lisp:1:")
	assert.Contains(t, got.String(), "lisp:use-package")
	assert.NotContains(t, got.String(), "<native code>")
	require.True(t, evalPackageBuiltin(t, env, `(set 'deferred:never-defined 42)`).Type == lisp.LInt)
	require.True(t, evalPackageBuiltin(t, env, `(use-package 'deferred)`).IsNil())
	assert.Equal(t, "42", evalPackageBuiltin(t, env, `never-defined`).String())
}

func TestPackageBuiltinInvalidNames(t *testing.T) {
	for _, name := range []string{"", "a:b", ":kw", "1abc", "-1", "a b", "a;comment", "a#b", "a^b", "a\n", "a٢", "--a"} {
		for _, builtin := range []string{"in-package", "use-package"} {
			t.Run(builtin+"/"+name, func(t *testing.T) {
				env := packageBuiltinEnv(t)
				before := env.Runtime.Package
				// Register invalid names through Go to prove use-package validates even
				// when the requested package exists, instead of merely reporting unknown.
				if builtin == "use-package" {
					env.Runtime.Registry.DefinePackage(name)
				}
				got := evalPackageBuiltin(t, env, fmt.Sprintf("(%s %q)", builtin, name))
				assert.Equal(t, lisp.LError, got.Type, "expected invalid package name error, got %s", got)
				assert.Contains(t, got.String(), "invalid package name")
				assert.False(t, lisp.IsInternalPanic(got))
				assert.Same(t, before, env.Runtime.Package)
				if builtin == "in-package" {
					assert.Nil(t, env.Runtime.Registry.Package(name), "invalid name created a package")
				}
			})
		}
	}
	for _, src := range []string{`(in-package ':kw)`, `(use-package ':kw)`} {
		t.Run(src, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			env.Runtime.Registry.DefinePackage(":kw")
			got := evalPackageBuiltin(t, env, src)
			assert.Equal(t, lisp.LError, got.Type, "expected keyword package name error, got %s", got)
		})
	}
}

func TestPackageBuiltinValidLookalikes(t *testing.T) {
	for _, src := range []string{`(export '(a b))`, `(export "a")`, `(export '(a (b)))`} {
		t.Run(src, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			assert.True(t, evalPackageBuiltin(t, env, src).IsNil())
			assert.Contains(t, env.Runtime.Package.Externals(), "a")
			if strings.Contains(src, "b") {
				assert.Contains(t, env.Runtime.Package.Externals(), "b")
			}
		})
	}
	for _, name := range []string{"valid-name", "p&q", "éλ", "+1", ".1", "-", "--", "-a", "example.com/pkg", "~%?$"} {
		t.Run(name, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			// Reader parity: each accepted name also works as a package qualifier.
			_, err := env.Runtime.Reader.Read("qualified", strings.NewReader(name+":f"))
			require.NoError(t, err)
			got := evalPackageBuiltin(t, env, fmt.Sprintf("(in-package %q)", name))
			require.True(t, got.IsNil(), "%s", got)
			assert.Equal(t, name, env.Runtime.Package.Name)
			require.True(t, evalPackageBuiltin(t, env, `(in-package 'user)`).IsNil())
			got = evalPackageBuiltin(t, env, fmt.Sprintf("(use-package %q)", name))
			assert.True(t, got.IsNil(), "%s", got)
		})
	}
	env := packageBuiltinEnv(t)
	require.True(t, evalPackageBuiltin(t, env, `(in-package 'p "doc")`).IsNil())
	assert.Equal(t, "p", env.Runtime.Package.Name)
	assert.Equal(t, "doc", env.Runtime.Package.Doc)
}

func TestPackageBuiltinGoNamesUnchanged(t *testing.T) {
	for _, name := range []string{"", "a:b", ":kw"} {
		t.Run(name, func(t *testing.T) {
			env := packageBuiltinEnv(t)
			pkg := env.Runtime.Registry.DefinePackage(name)
			require.True(t, env.InPackage(lisp.String(name)).IsNil())
			assert.Same(t, pkg, env.Runtime.Package)
			require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
			require.True(t, env.UsePackage(lisp.String(name)).IsNil())
			registry := lisp.NewRegistry()
			assert.True(t, registry.AddPackage(lisp.NewPackage(name)))
			assert.NotNil(t, registry.Package(name))
		})
	}
}
