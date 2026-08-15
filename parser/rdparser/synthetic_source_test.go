// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// syntheticSourceNodes walks a parsed expression and returns a description of
// every node carrying a location the interpreter treats as SYNTHETIC: nil, or
// Pos < 0 (lisp.nativeSource, rendered "<native code>").
//
// The three process-wide singletons are excluded.  lisp.Nil(), lisp.Bool(true)
// and lisp.Bool(false) are shared immutable values that legitimately carry a
// synthetic location, and lisp.stampMacroExpansion skips them by identity
// (elps#274), so they are not part of the population this file is about.
func syntheticSourceNodes(v *lisp.LVal, path string) []string {
	if v == nil || v == lisp.Nil() || v == lisp.Bool(true) || v == lisp.Bool(false) {
		return nil
	}
	var found []string
	here := path + "/" + v.Type.String()
	switch {
	case v.Source == nil:
		found = append(found, fmt.Sprintf("%s %q: Source is nil", here, v.Str))
	case v.Source.Pos < 0:
		found = append(found, fmt.Sprintf("%s %q: synthetic Source %v (Pos=%d)",
			here, v.Str, v.Source, v.Source.Pos))
	}
	for _, c := range v.Cells {
		found = append(found, syntheticSourceNodes(c, here)...)
	}
	return found
}

// AssertRealSourceLocations fails t if any node reachable from exprs carries a
// synthetic source location.  Exported to the fuzz targets in this package,
// which apply the same rule to generated input.
func assertRealSourceLocations(t *testing.T, src string, exprs []*lisp.LVal) {
	t.Helper()
	var found []string
	for i, expr := range exprs {
		found = append(found, syntheticSourceNodes(expr, fmt.Sprintf("expr[%d]", i))...)
	}
	if len(found) > 0 {
		t.Errorf("parsing %q produced %d node(s) with a synthetic source location:\n  %s",
			src, len(found), strings.Join(found, "\n  "))
	}
}

// TestParserEmitsNoSyntheticSourceLocations pins the invariant that closes
// elps#370: NOTHING the reader hands back carries a synthetic source location.
//
// lisp.stampMacroExpansion rewrites Source (and, with a debugger attached,
// MacroExpansion) on every expanded node whose location is nil or has Pos < 0.
// Macro arguments are not evaluated, so they arrive as the CALLER'S OWN
// parse-tree nodes and are spliced into the expansion -- which makes any
// parser-produced node with a synthetic location a node the stamp walk will
// write into.  That parse tree is not private to one evaluation: LEnv.load
// evaluates the reader's nodes directly, a function body IS the parse tree it
// was defined from, and a *Package is shared by pointer across the per-request
// environments an embedder derives from one registry.  Two of them expanding
// the same macro call raced on LVal.Source with nothing between them.
//
// The two violations were the head symbols the reader SYNTHESIZES for the #'
// and #^ prefixes -- lisp.Symbol seeds every new symbol with
// lisp.nativeSource() and neither head was given a location of its own.
//
// This is stated as a property over the whole tree rather than as two
// assertions about those two heads on purpose.  The bug was not that those
// particular symbols were wrong; it was that the reader was allowed to emit a
// stampable node at all, and the next desugaring added would reopen it.
//
// The same rule is applied to generated input by FuzzParseProgram and
// FuzzParseFormatting.
func TestParserEmitsNoSyntheticSourceLocations(t *testing.T) {
	srcs := []string{
		// The two regressions.  Both parsed to a form whose HEAD carried
		// "<native code>": (lisp:function foo) and (lisp:expr ...).
		`#'foo`,
		`#'mypkg:myfun`,
		`'#'x`,
		`(map () #'first '((1 2) (3 4)))`,
		`#^(+ %1 1)`,
		`#^()`,
		`#^0`,
		`#^"abc"`,
		`'#^0`,
		`#^'%`,
		// Ordinary forms, which were already clean and must stay so.
		`(defun f (x) (+ x 1))`,
		"; comment\n(a b [c] 'd 1.5 \"s\" :key)",
		"(defmacro m (&rest body) (quasiquote (progn (unquote-splicing body))))",
	}
	for _, src := range srcs {
		t.Run(src, func(t *testing.T) {
			for _, mode := range []struct {
				name    string
				newFunc func(*token.Scanner) *rdparser.Parser
			}{
				{"standard", rdparser.New},
				{"formatting", rdparser.NewFormatting},
			} {
				t.Run(mode.name, func(t *testing.T) {
					p := mode.newFunc(token.NewScanner("test.lisp", strings.NewReader(src)))
					exprs, err := p.ParseProgram()
					require.NoError(t, err)
					assertRealSourceLocations(t, src, exprs)
				})
			}
		})
	}
}

// TestRepoSourcesHaveNoSyntheticLocations runs the same property over the real
// library and example code in the seed corpus -- the population an embedder's
// parse cache would actually hold.
//
// This is a GUARD, not a catch: it passes on main.  No file under _examples or
// lisp/lisplib happens to use #' today (the tree's only #' is in
// editors/vscode/test/grammar, which the corpus does not walk), so there was
// nothing here for the defect to show up in.  It is worth having anyway --
// adding a #' to the standard library is an ordinary thing to do, and without
// this the class would come back through the corpus without a word.
func TestRepoSourcesHaveNoSyntheticLocations(t *testing.T) {
	sources := fuzzseed.LispSources()
	require.NotEmpty(t, sources, "no .lisp sources found; the corpus path is broken")
	for i, src := range sources {
		p := rdparser.New(token.NewScanner("corpus.lisp", bytes.NewReader(src)))
		exprs, err := p.ParseProgram()
		if err != nil {
			continue // not every fixture is a valid program on its own
		}
		assertRealSourceLocations(t, fmt.Sprintf("corpus source %d", i), exprs)
	}
}

// TestSynthesizedHeadsCarryPrefixLocation states what the heads now point AT,
// which the property above deliberately does not.
//
// A synthesized head has no text of its own, so the prefix token that produced
// it is the only honest answer -- and it is a better one than the old
// "<native code>", which reported a function reference the user wrote as
// though the interpreter had invented it.
func TestSynthesizedHeadsCarryPrefixLocation(t *testing.T) {
	t.Run("funref", func(t *testing.T) {
		// "(f #'g)": the #' prefix is at column 4.
		exprs, err := rdparser.New(token.NewScanner("test.lisp", strings.NewReader("(f #'g)"))).ParseProgram()
		require.NoError(t, err)
		require.Len(t, exprs, 1)
		funref := exprs[0].Cells[1]
		require.Equal(t, "lisp:function", funref.Cells[0].Str)
		head := funref.Cells[0]
		require.NotNil(t, head.Source)
		assert.Equal(t, "test.lisp", head.Source.File)
		assert.Equal(t, 1, head.Source.Line)
		assert.Equal(t, 4, head.Source.Col, "head should sit on the #' prefix")
		assert.GreaterOrEqual(t, head.Source.Pos, 0)
		// The head gets a *token.Location of its own.  When this was written
		// it had to have one specially: tokenLVal gave the enclosing
		// s-expression the OPERAND'S Location OBJECT and applyPrefixLocation
		// rewrote that object in place, so `funref.Source` and
		// `funref.Cells[1].Source` were one object and a third borrower would
		// have joined them on it.  elps#426 removed the sharing at the source
		// -- Parser.Location copies -- so all three are distinct now and this
		// assertion is one instance of the general rule
		// TestPrefixFormNodesDoNotShareLocationObject states.
		assert.NotSame(t, head.Source, funref.Cells[1].Source)
		assert.NotSame(t, funref.Source, funref.Cells[1].Source)
	})

	t.Run("unbound expression", func(t *testing.T) {
		// The #^ head sits on the "#^" prefix, at column 1 -- the same rule as
		// #' above, and the location locateSynthesized gives it.
		//
		// CHANGED BY #426, from column 3.  ParseUnbound used to overwrite the
		// head's location with the operand's whenever the operand had a real
		// one, on the reasoning that this "keeps `#^x` reporting the same
		// position it always has".  That held only because of the aliasing:
		// for "#^x" the operand's Location object WAS the enclosing form's,
		// and applyPrefixLocation had moved it to the prefix -- so borrowing
		// it reported column 1.  For "#^(+ %1 1)" the operand is a list, whose
		// closing ")" is consumed before the form is built, so no aliasing
		// occurred and the borrow reported column 3 instead.  The head's
		// column therefore depended on whether the operand was a symbol or a
		// list.  It no longer does.
		exprs, err := rdparser.New(token.NewScanner("test.lisp", strings.NewReader("#^(+ %1 1)"))).ParseProgram()
		require.NoError(t, err)
		require.Len(t, exprs, 1)
		head := exprs[0].Cells[0]
		require.Equal(t, "lisp:expr", head.Str)
		require.NotNil(t, head.Source)
		assert.GreaterOrEqual(t, head.Source.Pos, 0)
		assert.Equal(t, 1, head.Source.Col, "head should sit on the #^ prefix")
		assert.Equal(t, 3, head.Source.EndCol, "head spans exactly the two columns of #^")

		// The same, with a SYMBOL operand: the head's column must not depend
		// on the operand's shape.
		exprs, err = rdparser.New(token.NewScanner("test.lisp", strings.NewReader("#^x"))).ParseProgram()
		require.NoError(t, err)
		require.Len(t, exprs, 1)
		head = exprs[0].Cells[0]
		require.Equal(t, "lisp:expr", head.Str)
		assert.Equal(t, 1, head.Source.Col, "head should sit on the #^ prefix")
		assert.Equal(t, 3, head.Source.EndCol, "head spans exactly the two columns of #^")
	})
}
