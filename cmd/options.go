// Copyright © 2024 The ELPS authors

package cmd

import (
	"context"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/mcpserver"
)

// Option configures an exported command factory (LintCommand, DocCommand).
type Option func(*cmdConfig)

type cmdConfig struct {
	registry  *lisp.PackageRegistry
	env       *lisp.LEnv
	newDocEnv func() (*lisp.LEnv, error)
	runStdio  func(context.Context, *mcpserver.Server) error
}

// WithRegistry injects a PackageRegistry for semantic analysis. The
// registry's Go-registered builtins, special ops, and macros are merged
// with stdlib symbols so that the linter recognises embedder-provided
// functions.
func WithRegistry(reg *lisp.PackageRegistry) Option {
	return func(c *cmdConfig) { c.registry = reg }
}

// WithEnv injects a fully configured LEnv. For the doc command this is
// the environment used for documentation queries. For the lint command
// the env's Runtime.Registry is used for semantic analysis.
func WithEnv(env *lisp.LEnv) Option {
	return func(c *cmdConfig) { c.env = env }
}

// resolveRegistry returns the best available registry from the options.
// If an env was provided its registry is preferred, falling back to an
// explicitly supplied registry.
func (c *cmdConfig) resolveRegistry() *lisp.PackageRegistry {
	if c.env != nil {
		return c.env.Runtime.Registry
	}
	return c.registry
}

// analysisEnv returns the env and registry that the lint, lsp and mcp
// commands analyze with.
//
// Only an env lets them expand the macros of the workspace: each loads
// the workspace macros into it, and a registry cannot evaluate a macro.
// So with neither WithEnv nor WithRegistry the command builds a stdlib env
// with newDocEnv and returns it, not only its registry. Before, plain
// "elps lsp", "elps lint" and "elps mcp" kept only the registry, and a
// function a workspace macro defines was an undefined symbol
// (luthersystems/elps#952). The command owns that env alone, so loading
// the workspace macros into it changes nothing else.
//
// With WithRegistry alone the env is nil: the commands resolve the
// embedder's builtins but expand no workspace macro. A stdlib env would
// lack the embedder's builtins that its macros may call. Such an embedder
// should pass WithEnv.
func (c *cmdConfig) analysisEnv() (analysisSetup, error) {
	if c.env != nil {
		return analysisSetup{env: c.env, registry: c.env.Runtime.Registry}, nil
	}
	if c.registry != nil {
		return analysisSetup{registry: c.registry}, nil
	}
	newDocEnv := c.newDocEnv
	if newDocEnv == nil {
		newDocEnv = lisplib.NewDocEnv
	}
	env, err := newDocEnv()
	if err != nil {
		return analysisSetup{}, err
	}
	return analysisSetup{env: env, registry: env.Runtime.Registry}, nil
}

// analysisSetup is what analysisEnv returns. env is nil for an embedder
// that passed only a registry.
type analysisSetup struct {
	env      *lisp.LEnv
	registry *lisp.PackageRegistry
}
