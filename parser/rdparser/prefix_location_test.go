// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// prefixSources are the reader shorthands that build more than one LVal from a
// single token, plus longhand and non-prefix controls.
//
// A prefix form parses its operand FIRST and consumes nothing afterwards, so
// p.src.Token is still the operand's token when the enclosing form is built.
// Before #426 that meant tokenLVal handed the enclosing form the operand's own
// *token.Location -- the object, not a copy -- and applyPrefixLocation then
// rewrote it in place to the prefix's column, dragging the operand's reported
// position along with it.
var prefixSources = []string{
	"'a",
	"''a",
	"'(a b)",
	"#'car",
	"#'+",
	"#^a",
	"'#'car",
	"'#^a",
	"(quote x)",
	"(map 'list #'car '((1 2) (3 4)))",
	"(defun f (x) (#'g x))",
}

// locatedNodes walks v and returns every node with a non-nil Source.
func locatedNodes(v *lisp.LVal) []*lisp.LVal {
	var out []*lisp.LVal
	walkLVal(v, func(n *lisp.LVal) {
		if astraw.SourceRef(n) != nil {
			out = append(out, n)
		}
	})
	return out
}

// TestPrefixFormNodesDoNotShareLocationObject is the pointer-identity
// statement of #426: two distinct nodes in one parse tree must never hold the
// same *token.Location.
//
// This is a CATCH, not a guard.  On ce9798d it fails on every prefix
// shorthand: "#'car" gives the enclosing form and the operand "car" one
// object, and "#^a" gives all three nodes one object.
//
// Sharing is a defect independently of which column each node ends up
// reporting.  A *Location that two nodes hold is a *Location either of them
// can write through -- the LSP, the linter and stampMacroExpansion all walk
// the tree and write positions -- so a node's position can be changed by an
// edit aimed at an unrelated node.  applyPrefixLocation is simply the first
// caller in the tree to do it.
func TestPrefixFormNodesDoNotShareLocationObject(t *testing.T) {
	t.Parallel()

	for _, src := range prefixSources {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			for _, expr := range parseAll(t, src) {
				owner := make(map[*token.Location]*lisp.LVal)
				for _, n := range locatedNodes(expr) {
					if prev, dup := owner[astraw.SourceRef(n)]; dup {
						t.Errorf("parsing %q: nodes %v %q and %v %q share one *token.Location (%v); each node must own its position (#426)",
							src, prev.Type, prev.Str, n.Type, n.Str, astraw.SourceRef(n))
						continue
					}
					owner[astraw.SourceRef(n)] = n
				}
			}
		})
	}
}

// synthesizedHeadText maps the head symbol the reader synthesizes for a prefix
// shorthand to the prefix token it stands for.  Those heads have no token of
// their own; locateSynthesized (#419) gives them the prefix token's location,
// so their span is the prefix, not the operand.
var synthesizedHeadText = map[string]string{
	"lisp:function": "#'",
	"lisp:expr":     "#^",
}

// TestSymbolLocationSpansItsOwnText states the invariant #426 breaks in the
// form a reader of the source can check by hand: the span a symbol reports,
// [Pos, EndPos), must be that symbol's own text.
//
// The one licensed exception is a leading run of "'".  `'a` is ONE LVal --
// lisp.Quote copies the symbol and sets Quoted rather than wrapping it -- so
// that node is both the quoted form and the symbol, and its span covering the
// quote is right.  A "#'" or "#^" in front of the text is NOT licensed: those
// desugar to a two-cell s-expression in which the operand is a node of its
// own, and its span must be the operand alone.
//
// This is a CATCH.  On ce9798d, parsing "#'car" reports the symbol "car" as
// spanning "#'car", and parsing "#^a" reports the symbol "a" as spanning
// "#^a" -- both because the operand is holding the enclosing form's rewritten
// Location object.
//
// The longhand "(quote x)" is the control and passes on ce9798d: it reports
// "x" spanning "x".  Two spellings of one form disagreeing about where the
// operand is, is the argument that the shorthand is the one that is wrong.
func TestSymbolLocationSpansItsOwnText(t *testing.T) {
	t.Parallel()

	for _, src := range prefixSources {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			for _, expr := range parseAll(t, src) {
				for _, n := range locatedNodes(expr) {
					if n.Type != lisp.LSymbol {
						continue
					}
					want := n.Str
					if prefix, ok := synthesizedHeadText[n.Str]; ok {
						want = prefix
					}
					loc := astraw.SourceRef(n)
					if loc.Pos < 0 || loc.EndPos > len(src) || loc.EndPos < loc.Pos {
						t.Errorf("parsing %q: symbol %q reports span [%d,%d), outside the source (#426)",
							src, n.Str, loc.Pos, loc.EndPos)
						continue
					}
					got := src[loc.Pos:loc.EndPos]
					if !strings.HasSuffix(got, want) ||
						strings.Trim(strings.TrimSuffix(got, want), "'") != "" {
						t.Errorf("parsing %q: symbol %q reports span [%d,%d) = %q at %v, want %q (optionally behind quotes) (#426)",
							src, n.Str, loc.Pos, loc.EndPos, got, loc, want)
					}
				}
			}
		})
	}
}

// TestNestedPrefixFormEndPositionIsNotCorrupted pins the second-order damage:
// once applyPrefixLocation has rewritten the operand token's own Location, the
// NEXT tokenLVal computes its end position from that rewritten start, because
// token.TokenEnd walks the token text forward from tok.Source.Col -- and
// tok.Source is the object that was just moved.
//
// This is a CATCH.  On ce9798d "'#'car" (6 columns, ending at column 7)
// reports its outer form as ending at column 5, and "'#^a" as ending at
// column 3.  An end position that lands INSIDE the form is not a cosmetic
// error: lsp/position.go, lsp/selection_range.go and lint/lint.go all build
// ranges from EndLine/EndCol.
func TestNestedPrefixFormEndPositionIsNotCorrupted(t *testing.T) {
	t.Parallel()

	for _, src := range []string{"'#'car", "'#^a", "''a", "#'car", "#^a", "'(a b)"} {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			exprs := parseAll(t, src)
			require.Len(t, exprs, 1)
			loc := astraw.SourceRef(exprs[0])
			require.NotNil(t, loc)
			assert.Equal(t, 0, loc.Pos, "outer form must start at the first column of %q", src)
			assert.Equal(t, len(src), loc.EndPos,
				"outer form of %q must end past its last character (#426)", src)
			assert.Equal(t, 1, loc.Col, "outer form of %q starts at column 1", src)
			assert.Equal(t, len(src)+1, loc.EndCol,
				"outer form of %q must end at the exclusive column past its text (#426)", src)
		})
	}
}

// TestParsedNodeSpansContainTheirChildren is a structural restatement: a
// child's reported span must lie inside its parent's.
//
// GUARD, not a catch: it passes unmodified on ce9798d, because sharing one
// Location object between a parent and a child makes their spans IDENTICAL,
// and an identical span is a contained one.  It is here because it is the
// property FuzzPrefixLocationInvariants checks over generated input, and
// because it is the property that stops being free once each node owns its
// own Location -- from here on, containment is a real constraint on the two
// independent numbers rather than an artefact of them being one number.
func TestParsedNodeSpansContainTheirChildren(t *testing.T) {
	t.Parallel()

	for _, src := range prefixSources {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			for _, expr := range parseAll(t, src) {
				var check func(v *lisp.LVal)
				check = func(v *lisp.LVal) {
					for _, c := range v.Cells {
						if astraw.SourceRef(v) != nil && astraw.SourceRef(c) != nil && astraw.SourceRef(c).EndPos > 0 {
							if astraw.SourceRef(c).Pos < astraw.SourceRef(v).Pos || astraw.SourceRef(c).EndPos > astraw.SourceRef(v).EndPos {
								t.Errorf("parsing %q: child %v %q spans [%d,%d) which escapes parent %v %q span [%d,%d) (#426)",
									src, c.Type, c.Str, astraw.SourceRef(c).Pos, astraw.SourceRef(c).EndPos,
									v.Type, v.Str, astraw.SourceRef(v).Pos, astraw.SourceRef(v).EndPos)
							}
						}
						check(c)
					}
				}
				check(expr)
			}
		})
	}
}

// TestParserGivesOneTokensLocationAwayOnce is the narrowest statement of the
// cause, at the accessor rather than at the tree.  Parser.Location() returned
// p.src.Token.Source to every caller, so the two nodes a prefix form builds
// from one token took joint ownership of the scanner's object.  It now gives
// that object to the first caller and an independent copy to every later one,
// so asking twice about one token cannot produce two nodes on one Location.
//
// CATCH on ce9798d.  It is stated over the parse tree rather than by calling
// Location() twice, because Parser's cursor is not reachable from an external
// test -- and because "the operand holds a Location whose Pos was moved to the
// prefix" is the same fact, in the form a user sees it.
func TestParserGivesOneTokensLocationAwayOnce(t *testing.T) {
	t.Parallel()

	funref := parseAll(t, "#'car")[0]
	require.Len(t, funref.Cells, 2)
	operand := funref.Cells[1]
	require.Equal(t, "car", operand.Str)

	require.NotSame(t, astraw.SourceRef(funref), astraw.SourceRef(operand),
		"the #' form and its operand must not share one *token.Location (#426)")

	// The write applyPrefixLocation makes, made by hand: moving the form must
	// not move the operand.
	astraw.SourceRef(funref).Col = 99
	astraw.SourceRef(funref).Pos = 99
	assert.Equal(t, 3, astraw.SourceRef(operand).Col, "moving the #' form moved its operand (#426)")
	assert.Equal(t, 2, astraw.SourceRef(operand).Pos, "moving the #' form moved its operand (#426)")
}
