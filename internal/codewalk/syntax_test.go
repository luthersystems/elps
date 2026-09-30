// Copyright © 2026 The ELPS authors

package codewalk_test

import (
	"testing"

	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestSyntaxOperatorVocabulary(t *testing.T) {
	var names []string
	for _, op := range lisp.DefaultSpecialOps() {
		names = append(names, op.Name())
	}
	names = append(names, codewalk.OpDefun, codewalk.OpDefmacro, codewalk.OpDeftype, "test-let", "test-let*")
	for _, name := range names {
		for _, prefix := range []string{"", "lisp:"} {
			for _, quoted := range []bool{false, true} {
				head := lisp.Symbol(prefix + name)
				if quoted {
					head = lisp.Quote(head)
				}
				form := lisp.SExpr([]*lisp.LVal{head})
				codewalk.Syntax(form, nil, 0, func(node, parent *lisp.LVal, op string, depth int) bool {
					require.Same(t, form, node)
					require.Nil(t, parent)
					require.Zero(t, depth)
					require.Equal(t, name, op, head.String())
					return false
				})
			}
		}
	}
	for _, name := range []string{codewalk.OpUnquote, codewalk.OpUnquoteSplicing, "lisp:unquote", "other:lambda", "ordinary"} {
		form := lisp.SExpr([]*lisp.LVal{lisp.Symbol(name)})
		codewalk.Syntax(form, nil, 0, func(_, _ *lisp.LVal, op string, _ int) bool {
			if name == codewalk.OpUnquote || name == codewalk.OpUnquoteSplicing {
				require.Equal(t, name, op)
			} else {
				require.Empty(t, op)
			}
			return false
		})
	}
}

func TestSyntaxVisitsQuotedStructureAndPrunesChildren(t *testing.T) {
	leaf := lisp.Symbol("x")
	quoted := lisp.QExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.Nil(), leaf})
	template := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:quasiquote"), lisp.SExpr([]*lisp.LVal{leaf})})
	root := lisp.SExpr([]*lisp.LVal{quoted, nil, template})
	type event struct {
		node, parent *lisp.LVal
		op           string
		depth        int
	}
	var got []event
	before := quoted.String()
	codewalk.Syntax(root, nil, 0, func(node, parent *lisp.LVal, op string, depth int) bool {
		got = append(got, event{node, parent, op, depth})
		return op != codewalk.OpQuasiquote
	})
	require.Equal(t, []event{
		{root, nil, "", 0},
		{quoted, root, codewalk.OpLambda, 1},
		{quoted.Cells[0], quoted, "", 2},
		{quoted.Cells[1], quoted, "", 2},
		{leaf, quoted, "", 2},
		{template, root, codewalk.OpQuasiquote, 1},
	}, got)
	require.Equal(t, before, quoted.String())
	require.True(t, quoted.IsQuoted())
}
