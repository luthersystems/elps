// Copyright © 2024 The ELPS authors

package cmd

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

func TestDocCommand_DefaultFlags(t *testing.T) {
	cmd := DocCommand()
	assert.Equal(t, "doc [flags] QUERY", cmd.Use)

	for _, name := range []string{"package", "source-file", "list-packages", "missing", "guide"} {
		assert.NotNil(t, cmd.Flags().Lookup(name), "missing flag: %s", name)
	}
}

func TestDocCommand_WithEnvInjectsEnv(t *testing.T) {
	// Build an env with a custom package.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	lisp.InitializeUserEnv(env)
	lisplib.LoadLibrary(env)

	env.DefinePackage(lisp.Symbol("mypkg"))
	env.InPackage(lisp.Symbol("mypkg"))
	env.AddBuiltins(true, &testBuiltin{
		name:    "my-helper",
		formals: lisp.QExpr([]*lisp.LVal{lisp.Symbol("x")}),
	})
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	var cfg cmdConfig
	WithEnv(env)(&cfg)

	assert.Same(t, env, cfg.env,
		"WithEnv should store the env in cmdConfig")

	// Verify the custom package is accessible via the env's registry.
	pkg, ok := env.Runtime.Registry.Packages["mypkg"]
	assert.True(t, ok, "mypkg should be registered")
	assert.Contains(t, pkg.Externals, "my-helper",
		"my-helper should be exported from mypkg")
}

func TestDocCommand_WithRegistryMergesIntoDocEnv(t *testing.T) {
	// Build a standalone registry with a custom package.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	lisp.InitializeUserEnv(env)

	env.DefinePackage(lisp.Symbol("custpkg"))
	env.InPackage(lisp.Symbol("custpkg"))
	env.AddBuiltins(true, &testBuiltin{
		name:    "cust-fn",
		formals: lisp.QExpr([]*lisp.LVal{lisp.Symbol("x")}),
	})
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	var cfg cmdConfig
	WithRegistry(env.Runtime.Registry)(&cfg)

	assert.Same(t, env.Runtime.Registry, cfg.resolveRegistry(),
		"resolveRegistry should return the injected registry")
	assert.NotNil(t, cfg.registry.Packages["custpkg"],
		"custpkg should be in the registry")
}
