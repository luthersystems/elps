// Copyright © 2024 The ELPS authors

package cmd

import "github.com/luthersystems/elps/lisp"

// Option configures an exported command factory (LintCommand, DocCommand).
type Option func(*cmdConfig)

type cmdConfig struct {
	registry *lisp.PackageRegistry
	env      *lisp.LEnv
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
