// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// checkClosure evaluates src, whose value must be a lambda, and returns
// the messages CheckClosure reports for it.
func checkClosure(t *testing.T, env *lisp.LEnv, src string) []string {
	t.Helper()
	fn := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(fn))
	issues, lerr := env.CheckClosure(fn)
	require.Nil(t, lerr)
	out := make([]string, len(issues))
	for i, is := range issues {
		out[i] = is.Message
	}
	return out
}

func TestCheckClosure(t *testing.T) {
	t.Parallel()
	env := newCowTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("test", `
(defun two (a b) (+ a b))
(defun opt (a &optional b) a)
(defun rest (a &rest b) a)
(defun keys (a &key k) a)
(defmacro mac (a) a)
(set 'value 5)`)))
	cases := []struct {
		name string
		src  string
		want []string
	}{
		{"clean", `(let ((x 1)) (lambda (y) (two x y)))`, []string{}},
		{"unbound read", `(lambda () (list missing))`, []string{"reads missing, which is not defined"}},
		{"unbound call", `(lambda () (nope 1))`, []string{"calls nope, which is not defined"}},
		{"not a function", `(lambda () (value 1))`, []string{"calls value, which is bound to a value of type int, not a function"}},
		{"too few", `(lambda () (two 1))`, []string{"calls two with 1 argument(s); two takes 2"}},
		{"too many", `(lambda () (two 1 2 3))`, []string{"calls two with 3 argument(s); two takes 2"}},
		{"optional", `(lambda () (list (opt 1) (opt 1 2) (opt 1 2 3)))`, []string{"calls opt with 3 argument(s); opt takes 1 to 2"}},
		{"rest", `(lambda () (list (rest) (rest 1 2 3)))`, []string{"calls rest with 0 argument(s); rest takes at least 1"}},
		{"keys", `(lambda () (list (keys 1) (keys 1 :k 2) (keys 1 :k)))`, []string{"calls keys with 2 argument(s); keys takes 1 plus keyword pairs"}},
		{"builtin arity", `(lambda () (cons 1))`, []string{"calls cons with 1 argument(s); cons takes 2"}},
		{"set! of a global", `(lambda () (set! nothing 1) (set! value 2))`, []string{"sets nothing, which is not defined"}},
		{"function", `(lambda () (list (function two) (function absent)))`, []string{"reads absent, which is not defined"}},
		{"captured names are locals", `(let ((h (lambda (x) x)) (v 1)) (lambda () (h v 1 2)))`, []string{}},
		{"local bindings", `(lambda (a) (let* ((b a) (c b)) (flet ((f (x) (+ x c))) (labels ((g (n) (g n))) (f (g b))))))`, []string{}},
		{"let scope", `(lambda () (let ((a 1) (b a)) b))`, []string{"reads a, which is not defined"}},
		{"flet scope", `(lambda () (flet ((f (x) (f x))) (f 1)))`, []string{"calls f, which is not defined"}},
		{"cond and handler-bind", `(lambda (x) (cond ((two x) (gone)) (:else x)) (handler-bind ((error (lambda (c &rest _) (lost c)))) x))`,
			[]string{"calls gone, which is not defined", "calls lost, which is not defined", "calls two with 1 argument(s); two takes 2"}},
		{"macro arguments are not code", `(lambda () (mac (anything at all)) (mac 1 2))`, []string{"calls mac with 2 argument(s); mac takes 1"}},
		{"quoted data", `(lambda () '(missing 1 2) (quote (also missing)))`, []string{}},
		{"thread steps", `(lambda (x) (thread-first x (two 1)) (thread-last x (two)) (thread-first x (two 1 2)))`,
			[]string{"calls two with 1 argument(s); two takes 2", "calls two with 3 argument(s); two takes 2"}},
		{"qualified", `(lambda () (lisp:cons 1 2) (user:two 1 2) (user:gone) (nopkg:x))`,
			[]string{"calls nopkg:x, which is not defined", "calls user:gone, which is not defined"}},
		{"dotimes", `(lambda () (dotimes (i 3) (two i i)))`, []string{}},
		{"builtin macro expands", `(lambda (m) (get-default m "k" (absent)) (trace (two 1)))`,
			[]string{"calls absent, which is not defined", "calls two with 1 argument(s); two takes 2"}},
		{"builtin macro arity", `(lambda (m) (get-default m))`, []string{"calls get-default with 1 argument(s); get-default takes 3"}},
		{"with-cleanup and while", `(lambda () (with-cleanup ((cleanup)) (while (cond1) (body))))`,
			[]string{"calls body, which is not defined", "calls cleanup, which is not defined", "calls cond1, which is not defined"}},
		{"quasiquote holes", "(lambda (x) (quasiquote (a (unquote (b x)) (unquote-splicing c))))",
			[]string{"calls b, which is not defined", "reads c, which is not defined"}},
		{"sorted and once", `(lambda () (zz) (aa) (zz) (aa 1))`, []string{"calls aa, which is not defined", "calls zz, which is not defined"}},
	}
	for _, c := range cases {
		got := checkClosure(t, env, c.src)
		assert.Equal(t, c.want, got, c.name)
	}
}

// A closure's globals resolve in its own package, and the check reads the
// current bindings: a rebinding after the closure was made is what it
// checks against.
func TestCheckClosureCurrentBindings(t *testing.T) {
	t.Parallel()
	env := newCowTestEnv(t)
	fn := env.LoadString("test", `
(in-package 'other)
(defun helper (a) a)
(lambda () (helper 1))`)
	require.NoError(t, lisp.GoError(fn))
	require.NoError(t, lisp.GoError(env.LoadString("test", `(in-package 'user)`)))
	issues, lerr := env.CheckClosure(fn)
	require.Nil(t, lerr)
	assert.Empty(t, issues)
	require.NoError(t, lisp.GoError(env.LoadString("test", `(in-package 'other) (defun helper (a b) a) (in-package 'user)`)))
	issues, lerr = env.CheckClosure(fn)
	require.Nil(t, lerr)
	assert.Equal(t, []lisp.ClosureIssue{{Name: "helper", Kind: lisp.ClosureArity, Args: 1, Message: "calls helper with 1 argument(s); helper takes 2"}}, issues)
}

func TestCheckClosureRefuses(t *testing.T) {
	t.Parallel()
	env := newCowTestEnv(t)
	for _, src := range []string{`1`, `'cons`, `(lambda () 1)`} {
		v := env.LoadString("test", src)
		require.NoError(t, lisp.GoError(v))
		if v.Type == lisp.LSymbol {
			v = env.Get(v)
		}
		_, lerr := env.CheckClosure(v)
		if v.Type == lisp.LFun && v.Builtin() == nil {
			assert.Nil(t, lerr)
			continue
		}
		require.NotNil(t, lerr, src)
		assert.Contains(t, lerr.String(), "not a lambda")
	}
}

// The walk charges steps, so a step budget stops it.
func TestCheckClosureChargesSteps(t *testing.T) {
	t.Parallel()
	env := newCowTestEnv(t)
	fn := env.LoadString("test", `(lambda () (list (list (list 1)) (list (list 2)) (list (list 3))))`)
	require.NoError(t, lisp.GoError(fn))
	env.Runtime.SetStepBudget(1000)
	before := env.Runtime.Steps()
	_, lerr := env.CheckClosure(fn)
	require.Nil(t, lerr)
	assert.Equal(t, int64(8), env.Runtime.Steps()-before, "one step per list walked")
	env.Runtime.SetStepBudget(2)
	_, lerr = env.CheckClosure(fn)
	require.NotNil(t, lerr)
	assert.Contains(t, lerr.String(), "step budget exceeded")
}
