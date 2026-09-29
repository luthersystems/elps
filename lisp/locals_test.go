// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// localsEnv returns an env with a builtin (record-locals) that appends the
// caller's Locals, rendered as "name=value ..." to *seen.
func localsEnv(t *testing.T, seen *[]string) *lisp.LEnv {
	t.Helper()
	env := newSortErrorEnv(t)
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
	env.AddBuiltins(true, elpsutil.Function("record-locals", lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			var parts []string
			for _, b := range env.Locals() {
				parts = append(parts, b.Name+"="+b.Value.String())
			}
			*seen = append(*seen, strings.Join(parts, " "))
			return lisp.Nil()
		}))
	return env
}

func TestLocals(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		src  string
		want []string
	}{
		{"top level has no locals", `(set 'global 1) (record-locals)`, []string{""}},
		{"function parameters, sorted", `(defun f (zeta alpha &optional opt) (record-locals)) (f 1 2)`,
			[]string{"alpha=2 opt=() zeta=1"}},
		{"let inside a function", `(defun f (a) (let ((b (+ a 1))) (record-locals))) (f 1)`,
			[]string{"a=1 b=2"}},
		{"inner binding shadows outer", `(defun f (x) (let ((x "inner")) (record-locals))) (f "outer")`,
			[]string{`x="inner"`}},
		{"closures see their lexical scope", `(defun f (a) (flet ((g (b) (record-locals))) (g 2))) (f 1)`,
			[]string{"a=1 b=2"}},
		{"flet and labels function bindings are locals", `(defun f () (labels ((helper (n) n)) (record-locals))) (f)`,
			[]string{"helper=(lambda (n) n)"}},
		{"lambda passed to a higher-order function", `(map 'list (lambda (item) (record-locals)) '(5 6))`,
			[]string{"item=5", "item=6"}},
		{"rest and key parameters", `(defun f (&rest more) (record-locals)) (f 1 2)
(defun k (&key size) (record-locals)) (k :size 3)`,
			[]string{"more='(1 2)", "size=3"}},
		{"package globals are not locals", `(set 'counter 0) (defun f () (record-locals)) (f)`,
			[]string{""}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var seen []string
			env := localsEnv(t, &seen)
			v := env.LoadString("locals.lisp", tc.src)
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			require.Equal(t, tc.want, seen)
		})
	}
}

func TestLocalsNil(t *testing.T) {
	t.Parallel()
	var env *lisp.LEnv
	require.Empty(t, env.Locals())
	require.Empty(t, lisp.NewEnv(nil).Locals())
}
