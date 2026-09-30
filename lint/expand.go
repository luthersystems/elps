// Copyright © 2026 The ELPS authors

package lint

import (
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// maxLintExpansions bounds the macro expansions lint's code walkers make
// for one file.  A macro whose expansion keeps producing new macro calls
// would otherwise cost one expansion per level up to the walker's depth
// limit, on every lint run -- every keystroke in the LSP.
const maxLintExpansions = 10000

// passShared is computed once per file and shared by every analyzer.
type passShared struct {
	expanded []expandedForm
	done     bool
}

// expandedForm is one top-level form, fully expanded, and the package it
// was read in.
type expandedForm struct {
	code *lisp.LVal
	pkg  string
}

// budgetExpander stops expanding once the file's budget is spent; the
// walker then treats further macro calls as function calls.
type budgetExpander struct {
	exp  astutil.MacroExpander
	left *int
}

func (b budgetExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	if *b.left <= 0 {
		return nil
	}
	*b.left--
	return b.exp.ExpandMacro(form, pkg)
}

// expandedExprs returns the file's top-level forms fully expanded
// (astutil.ExpandAll) with the semantic analysis macro expander, when there
// is one, and the package each is read in.  It is computed once per file
// and bounded by maxLintExpansions.
func (p *Pass) expandedExprs() []expandedForm {
	if p.shared == nil {
		p.shared = &passShared{}
	}
	if p.shared.done {
		return p.shared.expanded
	}
	p.shared.done = true
	var exp astutil.MacroExpander
	pkg := lisp.DefaultUserPackage
	if p.Semantics != nil {
		if p.Semantics.MacroExpander != nil {
			left := maxLintExpansions
			exp = budgetExpander{exp: p.Semantics.MacroExpander, left: &left}
		}
		if p.Semantics.DefaultPackage != "" {
			pkg = p.Semantics.DefaultPackage
		}
	}
	for _, expr := range p.Exprs {
		// A quoted (in-package ...) is data.
		if !expr.IsQuoted() && HeadSymbol(expr) == "in-package" && len(expr.Cells) > 1 {
			if name := astutil.PackageNameArg(expr.Cells[1]); name != "" {
				pkg = name
			}
		}
		code := expr
		if exp != nil {
			code = astutil.ExpandAll(expr, exp, pkg, nil)
		}
		p.shared.expanded = append(p.shared.expanded, expandedForm{
			code: code,
			pkg:  pkg,
		})
	}
	return p.shared.expanded
}
