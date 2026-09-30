// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestUserDefinedPreservesSyntacticPolicy(t *testing.T) {
	source := `'((defun nested (car &rest rest) ()))
(quote (defmacro explicit (cdr) ()))
(lisp:defun qualified (qualified-param) ())
(lisp:lambda (qualified-lambda-param) ())
(set (quote rebound) 1) (set! 'mutated 2)
(lisp:set! 'qualified-target 3)
(lisp:quasiquote ((defun template (template-param) ())
                 (unquote (defun hole (hole-param) ()))))`
	exprs, err := rdparser.New(token.NewScanner("syntax.lisp", strings.NewReader(source))).ParseProgram()
	require.NoError(t, err)
	require.Equal(t, map[string]bool{
		"nested": true, "car": true, "rest": true,
		"explicit": true, "cdr": true, "rebound": true, "mutated": true,
	}, UserDefined(exprs))
	var templates []*lisp.LVal
	Walk(exprs, func(node, parent *lisp.LVal, depth int) {
		if node == exprs[len(exprs)-1] {
			require.Nil(t, parent)
			require.Zero(t, depth)
			templates = append(templates, node)
		}
		require.NotSame(t, exprs[len(exprs)-1], parent, "quasiquote has no visited children")
	})
	require.Len(t, templates, 1)
}

// Walk decides whether to descend after its callback runs, so a callback
// that rewrites a node's head controls pruning of that node.
func TestWalkPrunesOnPostCallbackHead(t *testing.T) {
	for _, tc := range []struct {
		src, newHead string
		want         []string
	}{
		{"(quasiquote (f))", "progn", []string{"progn", "f"}},
		{"(progn (f))", "quasiquote", []string{"quasiquote"}},
	} {
		exprs, err := rdparser.New(token.NewScanner("walk.lisp", strings.NewReader(tc.src))).ParseProgram()
		require.NoError(t, err)
		var heads []string
		WalkSExprs(exprs, func(v *lisp.LVal, _ int) {
			if v == exprs[0] {
				v.Cells[0] = lisp.Symbol(tc.newHead)
			}
			heads = append(heads, HeadSymbol(v))
		})
		require.Equal(t, tc.want, heads, tc.src)
	}
}
