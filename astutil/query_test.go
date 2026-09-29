// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFreeVarsIn(t *testing.T) {
	form := parseOne(t, `(lambda (x) (let ((y b)) (+ x y a c a)))`)
	// Of the locals a, b, c and x around the form, it uses a, b and c; its
	// own x is not free.
	assert.Equal(t, []string{"b", "a", "c"}, names(FreeVarsIn(form, []string{"a", "b", "c", "x", "unused"})))
	assert.Empty(t, FreeVarsIn(form, nil))
}

func TestNodeRoles(t *testing.T) {
	form := parseOne(t, `(let ([x (f 1)] [y '(g 2)]) (quasiquote (h (unquote x))) (set! y (list x)))`)
	r := ClassifyNodes(form)
	let := form
	bindings := let.Cells[1]
	pairX, pairY := bindings.Cells[0], bindings.Cells[1]
	body1, body2 := let.Cells[2], let.Cells[3]

	assert.Equal(t, RoleCode, r.Role(let))
	assert.Equal(t, RoleSyntax, r.Role(bindings), "the binding list")
	assert.Equal(t, RoleSyntax, r.Role(pairX), "a [x e] binding reads as a quoted list but is syntax")
	assert.Equal(t, RoleBinding, r.Role(pairX.Cells[0]))
	assert.Equal(t, RoleCode, r.Role(pairX.Cells[1]), "(f 1)")
	assert.Equal(t, RoleData, r.Role(pairY.Cells[1]), "'(g 2)")
	assert.Equal(t, RoleData, r.Role(pairY.Cells[1].Cells[0]), "inside quoted data")
	// A quasiquote template is data except for its holes.
	assert.Equal(t, RoleData, r.Role(body1.Cells[1].Cells[0]), "h in the template")
	hole := body1.Cells[1].Cells[1]
	assert.Equal(t, RoleCode, r.Role(hole.Cells[1]), "x in the unquote hole")
	assert.Equal(t, RoleSet, r.Role(body2.Cells[1]))
	assert.Equal(t, RoleNone, r.Role(lisp.Symbol("elsewhere")))
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
	require.True(t, ContainsCall(form, "emit"))
	assert.False(t, ContainsCall(form, "absent"))
	assert.False(t, ContainsCall(parseOne(t, `'(emit 1)`), "emit"))
}
