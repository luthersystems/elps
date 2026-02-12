// Copyright © 2024 The ELPS authors

package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLintCommand_DefaultFlags(t *testing.T) {
	cmd := LintCommand()
	assert.Equal(t, "lint [flags] [files...]", cmd.Use)

	// All expected flags should exist
	for _, name := range []string{"json", "checks", "list", "exclude", "workspace", "no-workspace"} {
		assert.NotNil(t, cmd.Flags().Lookup(name), "missing flag: %s", name)
	}
}

func TestLintCommand_WithRegistryInjectsRegistry(t *testing.T) {
	// Build an env with a custom package "testpkg" exporting "custom-fn".
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	lisp.InitializeUserEnv(env)
	lisplib.LoadLibrary(env)

	env.DefinePackage(lisp.Symbol("testpkg"))
	env.InPackage(lisp.Symbol("testpkg"))
	env.AddBuiltins(true, &testBuiltin{
		name:    "custom-fn",
		formals: lisp.QExpr([]*lisp.LVal{lisp.Symbol("x")}),
	})
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	reg := env.Runtime.Registry

	// Write a lisp file that uses testpkg:custom-fn.
	dir := t.TempDir()
	src := `(use-package 'testpkg)
(testpkg:custom-fn 42)
`
	lispFile := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(lispFile, []byte(src), 0o600))

	// Create the lint command with the registry injected.
	cmd := LintCommand(WithRegistry(reg))
	cmd.SetArgs([]string{"--workspace", dir, lispFile})

	// Capture stdout/stderr — we just want to verify no exit(1).
	var stdout, stderr bytes.Buffer
	cmd.SetOut(&stdout)
	cmd.SetErr(&stderr)

	// The command calls os.Exit on diagnostics, so we can't call Execute()
	// directly in-process. Instead, verify the factory wiring by checking
	// that resolveRegistry returns the expected registry.
	var cfg cmdConfig
	WithRegistry(reg)(&cfg)
	assert.Same(t, reg, cfg.resolveRegistry(),
		"WithRegistry should inject the registry")
}

func TestLintCommand_WithEnvResolvesRegistry(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()

	var cfg cmdConfig
	WithEnv(env)(&cfg)

	assert.Same(t, env.Runtime.Registry, cfg.resolveRegistry(),
		"WithEnv should resolve to the env's registry")
}

func TestLintCommand_WithEnvPreferredOverRegistry(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()

	otherReg := lisp.NewRegistry()

	var cfg cmdConfig
	WithEnv(env)(&cfg)
	WithRegistry(otherReg)(&cfg)

	assert.Same(t, env.Runtime.Registry, cfg.resolveRegistry(),
		"env's registry should take precedence over explicit registry")
}

// testBuiltin is a minimal LBuiltinDef for test fixtures.
type testBuiltin struct {
	name    string
	formals *lisp.LVal
}

func (b *testBuiltin) Name() string              { return b.name }
func (b *testBuiltin) Formals() *lisp.LVal        { return b.formals }
func (b *testBuiltin) Eval(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() }
func (b *testBuiltin) Docstring() string           { return "test builtin" }
