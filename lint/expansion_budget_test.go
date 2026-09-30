// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// countingExpander expands (grow) to (list (grow)) forever and records how
// often it was asked, and in which package.
type countingExpander struct {
	calls int
	pkgs  map[string]bool
}

func (c *countingExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	c.calls++
	if c.pkgs == nil {
		c.pkgs = map[string]bool{}
	}
	c.pkgs[pkg] = true
	if form.Cells[0].Str != "grow" {
		return nil
	}
	return lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), lisp.SExpr([]*lisp.LVal{lisp.Symbol("grow")})})
}

// A macro that keeps producing new macro calls costs a bounded number of
// expansions per file, however many analyzers walk expanded code.
func TestLintExpansionIsBoundedPerFile(t *testing.T) {
	exp := &countingExpander{}
	l := &Linter{Analyzers: DefaultAnalyzers()}
	_, err := l.LintFileWithAnalysis([]byte("(grow)\n(rethrow)\n(grow)"), "test.lisp",
		&analysis.Config{MacroExpander: exp})
	require.NoError(t, err)
	// Semantic analysis has its own depth cap (64); lint's walkers share
	// one budget on top of it.
	assert.Less(t, exp.calls, 2*maxLintExpansions)
}

// A quoted (in-package ...) is data and does not switch the package forms
// are expanded in.
func TestLintQuotedInPackageDoesNotSwitchPackage(t *testing.T) {
	exp := &countingExpander{}
	l := &Linter{Analyzers: []*Analyzer{AnalyzerRethrowContext}}
	_, err := l.LintFileWithAnalysis([]byte("'(in-package other)\n(f (rethrow))"), "test.lisp",
		&analysis.Config{MacroExpander: exp})
	require.NoError(t, err)
	assert.False(t, exp.pkgs["other"], "%v", exp.pkgs)
}
