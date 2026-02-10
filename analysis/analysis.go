// Copyright © 2024 The ELPS authors

// Package analysis provides scope-aware semantic analysis for ELPS lisp source.
//
// The analyzer builds a scope tree from parsed expressions, resolves symbol
// references, and identifies unresolved symbols. It is designed to be used
// by lint analyzers for semantic checks like undefined-symbol and unused-variable.
package analysis

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Config controls the behavior of the analyzer.
type Config struct {
	// ExtraGlobals are symbols from other files (e.g. workspace scanning).
	ExtraGlobals []ExternalSymbol

	// PackageExports maps package names to their exported symbols.
	// Used to resolve use-package imports from stdlib and workspace packages.
	PackageExports map[string][]ExternalSymbol

	// Filename is the source file being analyzed.
	Filename string
}

// ExternalSymbol represents a symbol defined in another file.
type ExternalSymbol struct {
	Name      string
	Kind      SymbolKind
	Package   string
	Signature *Signature
	Source    *token.Location
}

// Result holds the output of semantic analysis.
type Result struct {
	RootScope  *Scope
	Symbols    []*Symbol
	References []*Reference
	Unresolved []*UnresolvedRef
}

// Analyze performs semantic analysis on a set of parsed expressions.
// It builds a scope tree, resolves references, and collects unresolved symbols.
func Analyze(exprs []*lisp.LVal, cfg *Config) *Result {
	if cfg == nil {
		cfg = &Config{}
	}

	root := NewScope(ScopeGlobal, nil, nil)
	populateBuiltins(root)

	// Add external symbols from workspace
	for _, ext := range cfg.ExtraGlobals {
		root.Define(&Symbol{
			Name:      ext.Name,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			Exported:  true,
			External:  true,
		})
	}

	a := &analyzer{
		root:   root,
		result: &Result{RootScope: root},
		cfg:    cfg,
	}

	// Phase 1: Pre-scan top-level definitions (forward references)
	a.prescan(exprs, root)

	// Phase 2: Deep recursive walk
	for _, expr := range exprs {
		a.analyzeExpr(expr, root)
	}

	return a.result
}
