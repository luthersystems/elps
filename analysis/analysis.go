// Copyright © 2024 The ELPS authors

// Package analysis provides scope-aware semantic analysis for ELPS lisp source.
//
// The analyzer builds a scope tree from parsed expressions, resolves symbol
// references, and identifies unresolved symbols. It is designed to be used
// by lint analyzers for semantic checks like undefined-symbol and unused-variable.
package analysis

import (
	"github.com/luthersystems/elps/astutil"
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

	// DefForms declares additional definition-form heads for embedding programs.
	// Head is matched exactly against the form head symbol. FormalsIndex must
	// point at the parameter list cell. If BindsName is true, NameIndex points
	// at the defined symbol cell and the analyzer registers it using NameKind.
	DefForms []DefFormSpec

	// PackageImports maps package name to imported package names collected
	// from cross-file use-package declarations during workspace prescan.
	// Per-file analysis applies these imports so symbols from packages
	// imported in other files (e.g. main.lisp) are available.
	PackageImports map[string][]string

	// Filename is the source file being analyzed.
	Filename string
}

// DefFormSpec describes a custom definition-like form.
type DefFormSpec struct {
	Head         string
	FormalsIndex int
	BindsName    bool
	NameIndex    int
	NameKind     SymbolKind
}

// ExternalSymbol represents a symbol defined in another file.
type ExternalSymbol struct {
	Name      string
	Kind      SymbolKind
	Package   string
	Signature *Signature
	Source    *token.Location
	DocString string
}

// Result holds the output of semantic analysis.
type Result struct {
	RootScope  *Scope
	Symbols    []*Symbol
	References []*Reference
	Unresolved []*UnresolvedRef

	// ExtraGlobals are the external symbols that were provided via Config.
	// Stored here so downstream consumers (e.g. lint analyzers) can check
	// for cross-file duplicates without relying on scope lookups that may
	// have been overwritten by local definitions.
	ExtraGlobals []ExternalSymbol
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
		sym := &Symbol{
			Name:      ext.Name,
			Package:   ext.Package,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			Exported:  true,
			External:  true,
		}
		if ext.Package != "" {
			root.DefineQualifiedOnly(sym)
			continue
		}
		root.Define(sym)
	}

	a := &analyzer{
		root:             root,
		result:           &Result{RootScope: root, ExtraGlobals: cfg.ExtraGlobals},
		cfg:              cfg,
		qualifiedSymbols: make(map[string]*Symbol),
	}

	// Phase 1: Pre-scan top-level definitions (forward references)
	a.prescan(exprs, root)

	// Phase 2: Deep recursive walk
	currentPkg := "user"
	for _, expr := range exprs {
		a.analyzeExpr(expr, root, currentPkg)
		if expr != nil && expr.Type == lisp.LSExpr && !expr.Quoted && len(expr.Cells) > 0 &&
			astutil.HeadSymbol(expr) == "in-package" && len(expr.Cells) > 1 {
			if pkgName := extractPackageName(expr.Cells[1]); pkgName != "" {
				currentPkg = pkgName
			}
		}
	}

	return a.result
}
