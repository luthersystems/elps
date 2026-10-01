// Copyright © 2026 The ELPS authors

package codewalk_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestFormalsOccurrencesBeforeGrammarRejection(t *testing.T) {
	for _, source := range []string{
		`(flet ((1 (x x) ignored) (empty ())))`,
		`(lisp:flet ((1 (x x) ignored) [empty ()]))`,
		`(labels ((1 (x x) ignored) (empty ())))`,
		`(macrolet ((1 (x x) ignored) (empty ())))`,
	} {
		t.Run(source, func(t *testing.T) {
			exprs, err := rdparser.New(token.NewScanner("formals.lisp", strings.NewReader(source))).ParseProgram()
			require.NoError(t, err)
			form := exprs[0]
			var occurrences []codewalk.Node
			entered := false
			w := codewalk.Walker{
				Visit: func(n *codewalk.Node) bool {
					if n.Event == lisp.WalkEnter {
						entered = true
					}
					return true
				},
				Formals: func(n *codewalk.Node) {
					require.False(t, entered)
					require.Equal(t, codewalk.FormalsOccurrence, n.Event)
					require.Equal(t, codewalk.Parameters, n.Role)
					require.Equal(t, codewalk.Operator(form), n.Op)
					require.Same(t, form, n.Owner)
					occurrences = append(occurrences, *n)
				},
			}
			require.Same(t, form, w.Walk(form))
			require.Len(t, occurrences, 2)
			for i, occurrence := range occurrences {
				binding := form.Cells[1].Cells[i]
				require.Same(t, binding, occurrence.Binding)
				require.Same(t, binding.Cells[1], occurrence.Formals)
				require.Same(t, occurrence.Formals, occurrence.Node)
			}
			require.Empty(t, occurrences[1].Formals.Cells)
		})
	}
}

func TestSyntacticFormalsRolesAndTemplatePolicy(t *testing.T) {
	source := `(lambda ()) (lisp:defun 1 ()) (defmacro m malformed)
(deftype ty ()) (benchmark "b" ())
(lambda bad (lambda (&rest) 1))
(quote (lambda (x x))) '(lambda (x x))
(quasiquote (unquote (lambda (x x))) (quasiquote (unquote (lambda (&rest)))))`
	exprs, err := rdparser.New(token.NewScanner("formals.lisp", strings.NewReader(source))).ParseProgram()
	require.NoError(t, err)
	var operators []string
	var roles []codewalk.FormalsRole
	var formals []*lisp.LVal
	w := codewalk.Walker{SyntacticCalls: true, Formals: func(n *codewalk.Node) {
		operators = append(operators, n.Op)
		roles = append(roles, n.Role)
		formals = append(formals, n.Formals)
	}}
	for _, expr := range exprs {
		require.Same(t, expr, w.Walk(expr))
	}
	require.Equal(t, []string{"lambda", "defun", "defmacro", "deftype", "benchmark", "lambda", "lambda"}, operators)
	require.Equal(t, []codewalk.FormalsRole{
		codewalk.Parameters, codewalk.Parameters, codewalk.Parameters,
		codewalk.Constructor, codewalk.Benchmark, codewalk.Parameters, codewalk.Parameters,
	}, roles)
	require.Same(t, exprs[0].Cells[1], formals[0])
	require.Same(t, exprs[2].Cells[2], formals[2])
	require.Equal(t, lisp.LSymbol, formals[2].Type)
}

func TestSourceScopeCategoryContext(t *testing.T) {
	source := `(let ((x outer)) (flet ((f () x)) (labels ((g () (f))) (g))))
(macrolet ((m () ignored)) (m)) (lambda ()) (expr %) (dotimes (i 1) i)
(test "t" body) (test-let "t" ((x 1)) x) (test-let* "t" ((x 1)) x)
(defun f () body) (defmacro m () body) (deftype ty () body)`
	exprs, err := rdparser.New(token.NewScanner("scopes.lisp", strings.NewReader(source))).ParseProgram()
	require.NoError(t, err)
	type scope struct {
		category codewalk.ScopeCategory
		function bool
		outer    bool
	}
	var got []scope
	w := codewalk.Walker{Visit: func(n *codewalk.Node) bool {
		if n.Event == lisp.WalkEnter {
			got = append(got, scope{n.Scope, n.Function, n.Outer})
		}
		return true
	}}
	for _, expr := range exprs {
		w.Walk(expr)
	}
	require.Equal(t, []scope{
		{codewalk.ScopeLocal, false, false}, {codewalk.ScopeFunction, false, true},
		{codewalk.ScopeFunctions, false, false}, {codewalk.ScopeFunction, true, true},
		{codewalk.ScopeFunctions, false, false}, {codewalk.ScopeFunction, true, false},
		{codewalk.ScopeMacros, false, false}, {codewalk.ScopeAnonymous, true, false},
		{codewalk.ScopeAnonymous, true, false}, {codewalk.ScopeLoop, false, false},
		{codewalk.ScopeTest, false, false}, {codewalk.ScopeLocal, false, false},
		{codewalk.ScopeFunction, false, true}, {codewalk.ScopeLocal, false, false},
		{codewalk.ScopeFunction, true, false}, {codewalk.ScopeFunction, true, false},
		{codewalk.ScopeFunction, true, false},
	}, got)
}
