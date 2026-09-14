// Copyright © 2026 The ELPS authors

package astutil

import "github.com/luthersystems/elps/lisp"

// ExportNames extracts candidate export names from argument syntax: strings,
// reader-quoted symbols and nested lists, and (quote x) / (lisp:quote x) calls
// interpreted as standard quoting. Other expressions contribute no names, so
// the result may be incomplete. It does not resolve operators or expand macros.
//
// Preservation callers may always use these candidates as names worth keeping.
// Analysis callers marking symbols Exported or populating cross-file export
// metadata may use them only as a syntactic approximation under the assumption
// of standard, unshadowed operators and directly evaluated export forms, not as
// proof of runtime exports. A caller requiring proof must independently establish
// that context, the export/quote operator semantics, and that every argument is
// known. Unqualified quote may be shadowed; lisp:quote resolves in its package
// but an embedder may register a nonstandard lisp package before sealing it.
// Macro bodies and quasiquote templates do not establish directly evaluated
// exports, even when their argument syntax looks literal.
func ExportNames(args []*lisp.LVal) []string {
	var names []string
	var collect func(*lisp.LVal, bool)
	collect = func(node *lisp.LVal, literal bool) {
		if node == nil {
			return
		}
		literal = literal || node.IsQuoted()
		switch node.Type {
		case lisp.LString:
			names = append(names, node.Str)
		case lisp.LSymbol:
			if literal {
				names = append(names, node.Str)
			}
		case lisp.LSExpr:
			if literal {
				for _, child := range node.Cells {
					collect(child, true)
				}
			} else if head := HeadSymbol(node); (head == "quote" || head == "lisp:quote") && len(node.Cells) == 2 {
				collect(node.Cells[1], true)
			}
		case lisp.LInvalid, lisp.LInt, lisp.LFloat, lisp.LError, lisp.LQSymbol,
			lisp.LFun, lisp.LQuote, lisp.LBytes, lisp.LSortMap, lisp.LArray,
			lisp.LNative, lisp.LTaggedVal, lisp.LMarkTerminal, lisp.LMarkTailRec,
			lisp.LMarkMacExpand, lisp.LTypeMax:
			// Other values do not supply candidate export names.
		}
	}
	for _, arg := range args {
		collect(arg, false)
	}
	return names
}
