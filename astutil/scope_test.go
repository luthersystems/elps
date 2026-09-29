// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func names(syms []*lisp.LVal) []string {
	out := []string{}
	for _, s := range syms {
		out = append(out, s.Str)
	}
	return out
}

func bindingNames(bs []*Binding) []string {
	out := []string{}
	for _, b := range bs {
		out = append(out, b.Name.Str)
	}
	return out
}

func TestFreeVars(t *testing.T) {
	for _, tc := range []struct {
		src  string
		free []string
	}{
		{`(+ a b)`, []string{"+", "a", "b"}},
		{`(let ((a 1)) (+ a b))`, []string{"+", "b"}},
		{`(let ((a a)) a)`, []string{"a"}},     // init sees the outer a
		{`(let* ((a 1) (b a)) b)`, []string{}}, // sequential
		{`(lambda (x &optional y &rest z) (list x y z w))`, []string{"list", "w"}},
		{`(flet ((f (x) (f x))) (f 1))`, []string{"f"}}, // flet bodies do not see f
		{`(labels ((f (x) (f x))) (f 1))`, []string{}},
		{`(dotimes (i n i) (g i))`, []string{"n", "g"}},
		{`(set! x (list 'y))`, []string{"x", "list"}},
		{`(quote (a b))`, []string{}},
		{`(quasiquote (a (unquote b)))`, []string{"b"}},
		{`(expr (+ % k))`, []string{"+", "k"}},
		{`(handler-bind ((condition (lambda (c &rest a) (h c)))) (body))`, []string{"h", "body"}},
		{`(defun f (a) (g a))`, []string{"g"}},
		{`(cond ((p x) 1) (:else y))`, []string{"p", "x", "y"}},
		{`(f x x)`, []string{"f", "x"}}, // deduplicated, first occurrence order
	} {
		t.Run(tc.src, func(t *testing.T) {
			assert.Equal(t, tc.free, names(FreeVars(parseOne(t, tc.src))))
		})
	}
}

func TestAnalyzeScopes(t *testing.T) {
	form := parseOne(t, `(let ((a 1) (unused 2)) (lambda (x) (set! a x) (+ a outer)))`)
	sc := AnalyzeScopes(form)
	require.Len(t, sc.Root.Children, 1)
	let := sc.Root.Children[0]
	assert.Equal(t, "let", let.Op)
	assert.False(t, let.Function)
	assert.Equal(t, []string{"a", "unused"}, bindingNames(let.Bindings))

	a, unused := let.Bindings[0], let.Bindings[1]
	assert.True(t, a.Used())
	assert.False(t, unused.Used())
	require.Len(t, a.Refs, 2)
	assert.True(t, a.Refs[0].Set)
	assert.Same(t, a, a.Refs[1].Binding)

	require.Len(t, let.Children, 1)
	fn := let.Children[0]
	assert.True(t, fn.Function)
	assert.Equal(t, []string{"x"}, bindingNames(fn.Bindings))
	// The lambda captures a (a binding of an enclosing scope) and uses the
	// free variables + and outer.
	assert.Equal(t, []string{"a"}, bindingNames(fn.Captured()))
	assert.Equal(t, []string{"a", "+", "outer"}, names(refNodes(fn.Free())))
	assert.Equal(t, []string{"+", "outer"}, names(FreeVars(form)))
}

func refNodes(refs []*Ref) []*lisp.LVal {
	var out []*lisp.LVal
	seen := map[string]bool{}
	for _, r := range refs {
		if !seen[r.Node.Str] {
			seen[r.Node.Str] = true
			out = append(out, r.Node)
		}
	}
	return out
}

func TestLiveAcross(t *testing.T) {
	isPoint := func(v *lisp.LVal) bool { return HeadSymbol(v) == "pause" }
	live := func(src string) [][]string {
		var out [][]string
		for _, l := range LiveAcross(parseOne(t, src), isPoint) {
			out = append(out, bindingNames(l.Live))
		}
		return out
	}
	// a and b are bound before the pause; only b is read after it.
	assert.Equal(t, [][]string{{"b"}},
		live(`(let* ((a 1) (b 2)) (f a) (pause) (g b))`))
	// A name bound after the point is not live across it.
	assert.Equal(t, [][]string{{}},
		live(`(let ((a 1)) (pause) (let ((c 2)) c))`))
	// Arguments of the point are consumed by it.
	assert.Equal(t, [][]string{{}},
		live(`(let ((a 1)) (pause a))`))
	// A set! alone is not a read.
	assert.Equal(t, [][]string{{}},
		live(`(let ((a 1)) (pause) (set! a 2))`))
	// Inside a loop, a read earlier in the body happens again after the
	// point on the next turn.
	assert.Equal(t, [][]string{{"a"}},
		live(`(let ((a 1)) (while true (f a) (pause)))`))
	assert.Equal(t, [][]string{{"a", "i"}},
		live(`(let ((a 1)) (dotimes (i 3) (f a i) (pause)))`))
	// A closure created before the point may run after it.
	assert.Equal(t, [][]string{{"a"}},
		live(`(let ((a 1)) (set 'k (lambda () a)) (pause))`))
	// Two points, one result each, in walk order.
	assert.Equal(t, [][]string{{"a", "b"}, {"b"}},
		live(`(let ((a 1) (b 2)) (pause) (f a) (pause) (g b))`))
}

func TestRename(t *testing.T) {
	form := parseOne(t, "(let ((a 1)\n      (b 2))\n  (lambda (x) (set! a x) (+ a b)))")
	sc := AnalyzeScopes(form)
	a := sc.Root.Children[0].Bindings[0]
	out, err := sc.Rename(form, map[*Binding]string{a: "acc"})
	require.NoError(t, err)
	assert.Equal(t, "(let ((acc 1) (b 2)) (lambda (x) (set! acc x) (+ acc b)))", out.String())
	assert.Equal(t, "(let ((a 1) (b 2)) (lambda (x) (set! a x) (+ a b)))", form.String(), "input unchanged")
	// Renamed symbols keep their source locations.
	loc, ok := out.Cells[1].Cells[0].Cells[0].Source()
	require.True(t, ok)
	assert.Equal(t, 1, loc.Line)

	// Renaming a to b would capture the reference to b in the body.
	_, err = sc.Rename(form, map[*Binding]string{a: "b"})
	require.Error(t, err)
	assert.Contains(t, err.Error(), "capture")
	// Renaming a to + would capture the free reference to +.
	_, err = sc.Rename(form, map[*Binding]string{a: "+"})
	require.Error(t, err)

	// An inner binding of the new name would capture a's references.
	inner := parseOne(t, `(let ((a 1)) (let ((z 2)) (+ a z)))`)
	isc := AnalyzeScopes(inner)
	_, err = isc.Rename(inner, map[*Binding]string{isc.Root.Children[0].Bindings[0]: "z"})
	require.Error(t, err)

	// RenameFresh gives every binding a name that occurs nowhere else.
	fresh, err := sc.Rename(form, sc.FreshNames(form, "v"))
	require.NoError(t, err)
	assert.True(t, strings.HasPrefix(fresh.String(), "(let ((v1 1) (v2 2)) (lambda (v3) (set! v1 v3)"), fresh.String())
	assert.Equal(t, names(FreeVars(form)), names(FreeVars(fresh)))
}

// Analysis runs over expanded code: a macro's expansion is what binds.
func TestFreeVarsAfterExpansion(t *testing.T) {
	exp := &bindExpander{}
	form := ExpandAll(parseOne(t, `(with-x 1 (+ x y))`), exp, "user", nil)
	assert.Equal(t, []string{"+", "y"}, names(FreeVars(form)))
}

// bindExpander expands (with-x v body...) to (let ((x v)) body...).
type bindExpander struct{}

func (bindExpander) ExpandMacro(form *lisp.LVal, _ string) *lisp.LVal {
	if form.Cells[0].Str != "with-x" || len(form.Cells) < 2 {
		return nil
	}
	cells := []*lisp.LVal{
		lisp.Symbol("let"),
		lisp.SExpr([]*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("x"), form.Cells[1]})}),
	}
	return lisp.SExpr(append(cells, form.Cells[2:]...))
}
