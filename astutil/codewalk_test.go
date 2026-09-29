// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func parseOne(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	exprs, err := parser.NewReader().Read("test.lisp", strings.NewReader(src))
	require.NoError(t, err)
	require.Len(t, exprs, 1)
	return exprs[0]
}

// swapExpander expands (swap a b) to (list b a) and records the package.
type swapExpander struct{ pkgs []string }

func (e *swapExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	e.pkgs = append(e.pkgs, pkg)
	if form.Cells[0].Str != "swap" || len(form.Cells) != 3 {
		return nil
	}
	return lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), form.Cells[2], form.Cells[1]})
}

func TestExpandAll(t *testing.T) {
	exp := &swapExpander{}
	form := parseOne(t, `(let ((x (swap 1 2))) (f '(swap 3 4) (swap x (swap 5 6))))`)
	out := ExpandAll(form, exp, "my-pkg", nil)
	assert.Equal(t, `(let ((x (list 2 1))) (f '(swap 3 4) (list (list 6 5) x)))`, out.String())
	assert.Equal(t, `(let ((x (swap 1 2))) (f '(swap 3 4) (swap x (swap 5 6))))`, form.String())
	for _, p := range exp.pkgs {
		assert.Equal(t, "my-pkg", p)
	}

	// A lexically bound head is not offered to the expander.
	exp.pkgs = nil
	shadow := parseOne(t, `(flet ((swap (a b) a)) (swap 1 2))`)
	assert.Same(t, shadow, ExpandAll(shadow, exp, "user", nil))

	// Without an expander, nothing changes.
	assert.Same(t, form, ExpandAll(form, nil, "user", nil))
}

func TestWalkCode(t *testing.T) {
	form := parseOne(t, `(defun f (a &rest b) (g a (quasiquote (h (unquote b) c))))`)
	var refs, binds, defs []string
	WalkCode(form, func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkRef:
			refs = append(refs, n.Node.Str)
		case lisp.WalkBind:
			binds = append(binds, n.Node.Str)
		case lisp.WalkDefine:
			defs = append(defs, n.Node.Str)
		default:
		}
		return true
	})
	assert.Equal(t, []string{"f"}, defs)
	assert.Equal(t, []string{"a", "b"}, binds)
	assert.Equal(t, []string{"g", "a", "b"}, refs)
}
