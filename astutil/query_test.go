// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodeRoles(t *testing.T) {
	form := parseOne(t, `(let ([x (f 1)] [y '(g 2)]) (quasiquote (h (unquote x))) (set! y (list x)))`)
	r := ClassifyNodes(form)
	let := form
	bindings := let.Cells[1]
	pairX, pairY := bindings.Cells[0], bindings.Cells[1]
	body1, body2 := let.Cells[2], let.Cells[3]

	assert.Equal(t, roleCode, r.Role(let))
	assert.Equal(t, RoleSyntax, r.Role(bindings), "the binding list")
	assert.Equal(t, RoleSyntax, r.Role(pairX), "a [x e] binding reads as a quoted list but is syntax")
	assert.Equal(t, roleCode, r.Role(pairX.Cells[0]))
	assert.Equal(t, roleCode, r.Role(pairX.Cells[1]), "(f 1)")
	assert.Equal(t, RoleData, r.Role(pairY.Cells[1]), "'(g 2)")
	assert.Equal(t, RoleData, r.Role(pairY.Cells[1].Cells[0]), "inside quoted data")
	// A quasiquote template is data except for its holes.
	assert.Equal(t, RoleData, r.Role(body1.Cells[1].Cells[0]), "h in the template")
	hole := body1.Cells[1].Cells[1]
	assert.Equal(t, roleCode, r.Role(hole.Cells[1]), "x in the unquote hole")
	assert.Equal(t, roleCode, r.Role(body2.Cells[1]))
	assert.Equal(t, roleNone, r.Role(lisp.Symbol("elsewhere")))
}

func TestFindCalls(t *testing.T) {
	form := parseOne(t, `(progn
  (emit 1)
  (lambda () (emit 2))
  (handler-bind ((condition (lambda (c &rest a) (emit 3)))) (emit 4))
  (quasiquote (emit 5 (unquote (lisp:emit 6))))
  '(emit 7)
  (flet ((emit (x) x)) (emit 8))
  (flet ((f () (emit 9))) (emit 10)))`)
	sites := FindCalls(form, "emit")
	var got []string
	for _, s := range sites {
		var label strings.Builder
		label.WriteString(s.Form.Cells[1].String() + ":")
		for _, e := range s.Enclosing {
			if e.Function {
				label.WriteString("fn(" + e.Op + ") ")
			} else {
				label.WriteString(e.Op + " ")
			}
		}
		got = append(got, label.String())
	}
	assert.Equal(t, []string{
		"1:progn ",
		"2:progn lambda fn(lambda) ",
		"3:progn handler-bind lambda fn(lambda) ",
		"4:progn handler-bind ",
		"6:progn quasiquote ",
		"9:progn flet fn(flet) ",
		"10:progn flet ",
	}, got)
	assert.Empty(t, FindCalls(parseOne(t, `'(emit 1)`), "emit"))
}

// Structure inside an unquote hole is syntax, not template data.
func TestNodeRolesInsideHole(t *testing.T) {
	form := parseOne(t, `(quasiquote (a (unquote (let ((x (car))) (lambda (y) x)))))`)
	r := ClassifyNodes(form)
	let := form.Cells[1].Cells[1].Cells[1]
	assert.Equal(t, roleCode, r.Role(let))
	assert.Equal(t, RoleSyntax, r.Role(let.Cells[1]), "binding list")
	assert.Equal(t, RoleSyntax, r.Role(let.Cells[2].Cells[1]), "lambda formals")
	assert.Equal(t, RoleData, r.Role(form.Cells[1].Cells[0]))
}

// Shared and cyclic quoted data is classified once per node.
func TestNodeRolesSharedData(t *testing.T) {
	x := lisp.Symbol("a")
	for range 60 {
		x = lisp.SExpr([]*lisp.LVal{x, x})
	}
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), x})
	r := ClassifyNodes(form)
	assert.Equal(t, RoleData, r.Role(x))

	cyc := lisp.SExpr([]*lisp.LVal{lisp.Symbol("b"), nil})
	cyc.Cells[1] = cyc
	r = ClassifyNodes(lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), cyc}))
	assert.Equal(t, RoleData, r.Role(cyc))
}

// Enclosures follow structure: a form after a handler-bind is not inside
// it, and a dotimes result is not in a function body.
func TestFindCallsStructuralAncestry(t *testing.T) {
	form := parseOne(t, `(progn (handler-bind ((condition (lambda (c) c))) (f)) (emit 1) (dotimes (i 2 (emit 2)) i))`)
	sites := FindCalls(form, "emit")
	require.Len(t, sites, 2)
	for _, s := range sites {
		for _, e := range s.Enclosing {
			assert.NotEqual(t, "handler-bind", e.Op)
			assert.False(t, e.Function)
		}
	}
}
