// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// isExpansionMacro also accepts workspace macro imports whose declaration
// location is unavailable. Core macros remain the syntax walker's concern.
func isExpansionMacro(sym *Symbol) bool {
	return sym != nil && sym.Kind == SymMacro &&
		(isUserMacro(sym) || (sym.External && sym.Package != "" && sym.Package != "lisp"))
}

// importedMacroCall lets the expander resolve an analyzer-side import without
// changing its environment. Copy only the call header, cells and head symbol;
// argument nodes retain their identity and source spans are preserved.
func importedMacroCall(node *lisp.LVal, sym *Symbol, pkg string) *lisp.LVal {
	if sym == nil || sym.Package == "" || sym.Package == pkg || strings.Contains(node.Cells[0].Str, ":") {
		return node
	}
	head := node.Cells[0].Copy()
	head.Str = sym.Package + ":" + sym.Name
	call := *node
	call.Cells = append([]*lisp.LVal(nil), node.Cells...)
	call.Cells[0] = head
	return &call
}
