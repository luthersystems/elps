// Copyright © 2024 The ELPS authors

package lsp

import "github.com/luthersystems/elps/analysis"

// builtinScheme is the URI scheme for virtual builtin documentation files.
// Editors that don't understand this scheme silently ignore navigation.
const builtinScheme = "elps-builtin"

// builtinURI builds a virtual URI for a builtin symbol.
// Format: elps-builtin://lisp/map, elps-builtin://math/sin
func builtinURI(pkg, name string) string {
	return builtinScheme + "://" + pkg + "/" + name
}

// isBuiltin reports whether a symbol has no navigable source file.
// This is true for core builtins (Source == nil) and macro-expanded
// builtins (Source.Pos < 0).
func isBuiltin(sym *analysis.Symbol) bool {
	return sym.Source == nil || sym.Source.Pos < 0
}
