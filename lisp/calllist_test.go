// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These tests cover the reuse of the per-call argument list
// (lisp/calllist.go).  They run in both builds.  In the default build a
// list that was returned too early would be handed to the next call, and
// the values below would be the next call's arguments.  Under elpscheck the
// list is poisoned instead, and a reader that kept it sees an invalid value.

// A program's results do not depend on the reuse.
func TestCallListReuseKeepsResults(t *testing.T) {
	for _, tc := range []struct {
		name, src, want string
	}{
		{"arguments survive the call in a closure", `
			(defun make-adder (n) (lambda (x) (+ x n)))
			(set 'add5 (make-adder 5))
			(set 'add7 (make-adder 7))
			(list (add5 1) (add7 1) (add5 10))`, "'(6 8 15)"},
		{"a returned argument survives later calls", `
			(defun id (x) x)
			(set 'a (id '(1 2)))
			(id '(3 4))
			(id 5)
			a`, "'(1 2)"},
		{"a &rest list is the call's own", `
			(defun rest-of (&rest xs) xs)
			(set 'a (rest-of 1 2))
			(set 'b (rest-of 3 4))
			(rest-of 5 6)
			(list a b)`, "'('(1 2) '(3 4))"},
		{"a &rest list after required formals", `
			(defun tail-of (x &rest xs) xs)
			(set 'a (tail-of 0 1 2))
			(tail-of 9 8 7)
			a`, "'(1 2)"},
		{"optional and key formals", `
			(defun opt (a &optional b) (list a b))
			(defun key (a &key b) (list a b))
			(list (opt 1) (opt 1 2) (key 1) (key 1 :b 2))`, "'('(1 ()) '(1 2) '(1 ()) '(1 2))"},
		{"recursion deeper than the free list", `
			(defun count-up (n) (if (= n 0) 0 (+ 1 (count-up (- n 1)))))
			(count-up 200)`, "200"},
		{"a tail call carries its list to the outer frame", `
			(defun sum-to (n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
			(sum-to 300 0)`, "45150"},
		{"mutual tail calls", `
			(defun even-p (n) (if (= n 0) true (odd-p (- n 1))))
			(defun odd-p (n) (if (= n 0) false (even-p (- n 1))))
			(list (even-p 100) (odd-p 100) (even-p 7))`, "'(true false false)"},
		{"nested calls as arguments", `
			(defun pair (a b) (list a b))
			(pair (pair 1 2) (pair (pair 3 4) 5))`, "'('(1 2) '('(3 4) 5))"},
		{"the operators whose lists are reused", `
			(set 'x 1)
			(set 'acc '())
			(let ((a 1) (b 2))
				(let* ((c (+ a b)) (d (* c 2)))
					(set! x (progn 0 (cond ((> d 100) 'big) (:else (if (and a b) (or false d) 0)))))))
			(when (= x 6) (set! acc (cons 'when acc)))
			(unless (= x 6) (set! acc (cons 'unless acc)))
			(set 'n 0)
			(while (< n 3) (set! n (+ n 1)))
			(list x acc n (default () 4) (quote (1 2)) (funcall (function +) 1 2))`,
			"'(6 '('when) 3 4 '(1 2) 3)"},
		{"an error in a call does not disturb the next call", `
			(defun fail (x) (error 'boom x))
			(defun id (x) x)
			(set 'r (handler-bind ((condition (lambda (c &rest _) 'caught))) (fail 1)))
			(list r (id 2))`, "'('caught 2)"},
		{"the wrong number of arguments", `
			(defun two (a b) (list a b))
			(set 'r (handler-bind ((condition (lambda (c &rest _) 'caught))) (two 1)))
			(list r (two 1 2))`, "'('caught '(1 2))"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			got := env.LoadString("calllist_test.lisp", tc.src)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
}

// A VM that a template mints starts with no free lists and gives the same
// results as its source.
func TestCallListReuseInTemplateVMs(t *testing.T) {
	env := newCallSemanticsEnv(t)
	got := env.LoadString("calllist_test.lisp", `
		(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
		(fib 12)`)
	require.Equal(t, "144", got.String())
	tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	for range 3 {
		vm, err := tmpl.NewVM()
		require.NoError(t, err)
		require.Zero(t, lisp.CallListsFree(vm.Runtime), "a new VM took its source's free lists")
		got := vm.LoadString("calllist_test.lisp", `(fib 15)`)
		require.Equal(t, "610", got.String())
	}
}
