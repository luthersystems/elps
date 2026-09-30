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

func TestReferenceVisitorPreservesOccurrences(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("refs.lisp", strings.NewReader(`
(let ((x outer))
  (flet ((f (y) (+ x y)))
    (set! x (f x))
    (function f)
    (quasiquote (x (unquote x)))
    'ignored))`))).ParseProgram()
	require.NoError(t, err)
	var want, got []*lisp.LVal
	w := codewalk.Walker{Visit: func(n *codewalk.Node) bool {
		if n.Event == lisp.WalkRef || n.Event == lisp.WalkSet {
			want = append(want, n.Node)
		}
		return true
	}}
	w.Walk(exprs[0])
	w.Reference = func(v *lisp.LVal) { got = append(got, v) }
	w.Visit = func(n *codewalk.Node) bool {
		require.NotEqual(t, lisp.WalkRef, n.Event)
		require.NotEqual(t, lisp.WalkSet, n.Event)
		return true
	}
	w.Walk(exprs[0])
	require.NotEmpty(t, want)
	require.Equal(t, want, got)
}

func TestFormsVisitorPreservesRuntimeGrammar(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("forms.lisp", strings.NewReader(`
(let ((lambda list))
  (lambda (car))
  (lambda (x) (cons x))
  (macrolet ((m (x) (list x))) (m (car)))
  (quasiquote (data (unquote (car))))
  '(ignored (car)))`))).ParseProgram()
	require.NoError(t, err)
	var want, got []lisp.WalkNode
	w := lisp.CodeWalker{Visit: func(n *lisp.WalkNode) bool {
		if n.Event == lisp.WalkForm {
			want = append(want, *n)
		}
		return true
	}}
	w.Walk(exprs[0])
	w.Visit = func(n *lisp.WalkNode) bool {
		require.Equal(t, lisp.WalkForm, n.Event)
		got = append(got, *n)
		return true
	}
	codewalk.Forms(&w, exprs[0])
	require.NotEmpty(t, want)
	require.Equal(t, want, got)
	// The adapter must not leave the public walker's visitor filtered.
	var events []lisp.WalkEvent
	w.Visit = func(n *lisp.WalkNode) bool {
		events = append(events, n.Event)
		return true
	}
	w.Walk(exprs[0])
	require.Contains(t, events, lisp.WalkRef)
}

func TestEndVisitorPreservesSkippedForms(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("ends.lisp", strings.NewReader(`
(f (g 1) (stop (omitted 2)) (h 3))`))).ParseProgram()
	require.NoError(t, err)
	var want, got []int
	w := codewalk.Walker{Visit: func(n *codewalk.Node) bool {
		if n.Event == codewalk.End {
			want = append(want, n.Depth)
		}
		return n.Event != lisp.WalkForm || n.Node.Cells[0].Str != "stop"
	}}
	w.Walk(exprs[0])
	require.Equal(t, []int{1, 1, 1, 0}, want)
	w.End = func(depth int) { got = append(got, depth) }
	w.Visit = func(n *codewalk.Node) bool {
		require.NotEqual(t, codewalk.End, n.Event)
		return n.Event != lisp.WalkForm || n.Node.Cells[0].Str != "stop"
	}
	w.Walk(exprs[0])
	require.Equal(t, want, got)
}

func TestFormVisitorPreservesClassification(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("forms.lisp", strings.NewReader(`
(let ((x (stop (omitted)))) (f x) (lambda (y) (+ x y)))`))).ParseProgram()
	require.NoError(t, err)
	type form struct {
		node  *lisp.LVal
		op    string
		depth int
	}
	var want, got []form
	w := codewalk.Walker{Visit: func(n *codewalk.Node) bool {
		if n.Event == lisp.WalkForm {
			want = append(want, form{n.Node, n.Op, n.Depth})
			return n.Node.Cells[0].Str != "stop"
		}
		return true
	}}
	w.Walk(exprs[0])
	w.Form = func(node *lisp.LVal, op string, depth int) bool {
		got = append(got, form{node, op, depth})
		return node.Cells[0].Str != "stop"
	}
	w.Visit = func(n *codewalk.Node) bool {
		require.NotEqual(t, lisp.WalkForm, n.Event)
		return true
	}
	w.Walk(exprs[0])
	require.NotEmpty(t, want)
	require.Equal(t, want, got)
}

func TestDeclarationVisitorOmitsDefinitionBodies(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("definitions.lisp", strings.NewReader(`
(defun f (x) (g x)) (defmacro m (y) (quasiquote (g y))) (deftype t (z) z)`))).ParseProgram()
	require.NoError(t, err)
	var want, got []codewalk.Node
	w := codewalk.Walker{Visit: func(n *codewalk.Node) bool {
		if n.Event == lisp.WalkDefine {
			want = append(want, *n)
		}
		return n.Event != lisp.WalkForm || n.Depth == 0
	}}
	for _, expr := range exprs {
		w.Walk(expr)
	}
	w.DeclarationsOnly = true
	w.Visit = func(n *codewalk.Node) bool {
		require.Contains(t, []lisp.WalkEvent{lisp.WalkForm, lisp.WalkDefine}, n.Event)
		if n.Event == lisp.WalkDefine {
			got = append(got, *n)
		}
		return true
	}
	for _, expr := range exprs {
		w.Walk(expr)
	}
	require.Len(t, want, 3)
	require.Equal(t, want, got)
}

func TestEndDepthTracksNestedSelectedForms(t *testing.T) {
	exprs, err := rdparser.New(token.NewScanner("ends.lisp", strings.NewReader(`
(watch (f 1) (watch (g 2)) (h 3))`))).ParseProgram()
	require.NoError(t, err)
	active := -1
	var stack, ended []int
	w := codewalk.Walker{EndDepth: &active, Form: func(node *lisp.LVal, _ string, depth int) bool {
		if node.Cells[0].Str == "watch" {
			stack = append(stack, depth)
			active = depth
		}
		return true
	}, End: func(depth int) {
		ended = append(ended, depth)
		stack = stack[:len(stack)-1]
		active = -1
		if len(stack) > 0 {
			active = stack[len(stack)-1]
		}
	}}
	w.Walk(exprs[0])
	require.Equal(t, []int{1, 0}, ended)
	require.Equal(t, -1, active)
}
