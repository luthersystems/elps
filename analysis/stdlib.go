// Copyright © 2024 The ELPS authors

package analysis

import "github.com/luthersystems/elps/lisp"

// ExtractPackageExports creates a map of package name to exported symbols
// from a loaded runtime's package registry. This is used to resolve
// use-package imports for packages defined in Go (stdlib) that cannot be
// found by workspace scanning.
func ExtractPackageExports(reg *lisp.PackageRegistry) map[string][]ExternalSymbol {
	if reg == nil {
		return nil
	}
	result := make(map[string][]ExternalSymbol)
	for name, pkg := range reg.Packages {
		var syms []ExternalSymbol
		for _, extName := range pkg.Externals {
			val, ok := pkg.Symbols[extName]
			if !ok {
				continue
			}
			sym := ExternalSymbol{
				Name:      extName,
				Package:   name,
				DocString: val.Docstring(),
			}
			sym.Kind, sym.Signature = classifyLVal(val)
			syms = append(syms, sym)
		}
		if len(syms) > 0 {
			result[name] = syms
		}
	}
	return result
}

// classifyLVal determines the SymbolKind and optional Signature of an LVal.
func classifyLVal(val *lisp.LVal) (SymbolKind, *Signature) {
	if val.Type != lisp.LFun {
		return SymVariable, nil
	}
	var kind SymbolKind
	switch {
	case val.IsMacro():
		kind = SymMacro
	case val.IsSpecialFun():
		kind = SymSpecialOp
	default:
		kind = SymFunction
	}
	// Extract signature from formals (Cells[0] of LFun)
	var sig *Signature
	if len(val.Cells) > 0 {
		sig = signatureFromFormals(val.Cells[0])
	}
	return kind, sig
}
