// Copyright © 2026 The ELPS authors

package astutil

import "github.com/luthersystems/elps/lisp"

// ExportNames returns statically known names from evaluated export arguments.
// Like builtinExport, literal values may be strings, symbols, or recursively
// nested lists of either. Bare symbols and calls are expressions, so their
// runtime values cannot be registered as static package exports.
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
			// Other values do not supply statically known export names.
		}
	}
	for _, arg := range args {
		collect(arg, false)
	}
	return names
}
