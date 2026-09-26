// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestBuiltinArgumentBinding drives real builtins through bind: the
// positional and &rest shapes bind's fast path takes, and the &optional,
// &key and wrong-arity calls it hands to the general binder.  The error
// rows pin the full text, including the function name the call stack
// supplies, so a fast path that bound a call it should have rejected, or
// rejected one with a different message, fails here.
func TestBuiltinArgumentBinding(t *testing.T) {
	tests := elpstest.TestSuite{
		{"positional", elpstest.TestSequence{
			{`(car '(1 2))`, `1`, ""},
			{`(cons 1 '(2))`, `'(1 2)`, ""},
			{`(car)`, `test:1:1: lisp:car: invalid number of arguments: 0`, ""},
			{`(car '(1) '(2))`, `test:1:1: lisp:car: invalid number of arguments: 2`, ""},
			{`(cons 1)`, `test:1:1: lisp:cons: invalid number of arguments: 1`, ""},
			{`(defun f (x) (cons x))`, `()`, ""},
			{`(f 1)`, `test:1:14: lisp:cons: invalid number of arguments: 1`, ""},
		}},
		{"rest", elpstest.TestSequence{
			{`(list)`, `'()`, ""},
			{`(list 1 2 3)`, `'(1 2 3)`, ""},
			{`(+)`, `0`, ""},
			{`(+ 1 2 3)`, `6`, ""},
			{`(funcall + 1 2)`, `3`, ""},
			{`(funcall list)`, `'()`, ""},
			{`(funcall)`, `test:1:1: lisp:funcall: invalid number of arguments: 0`, ""},
			{`(append 'vector)`, `test:1:1: lisp:append: invalid number of arguments: 1`, ""},
			{`(append 'vector (vector 1) 2 3)`, `(vector 1 2 3)`, ""},
			{`(apply list 1 '(2 3))`, `'(1 2 3)`, ""},
			// The list a &rest builtin receives is its own: list returns
			// it, and mutating the result must not reach the literal
			// arguments or a later call.
			{`(set 'a (list 1 2))`, `'(1 2)`, ""},
			{`(set 'b (list 1 2))`, `'(1 2)`, ""},
			{`(set 'v (vector 1 2))`, `(vector 1 2)`, ""},
			{`(append! v 3)`, `(vector 1 2 3)`, ""},
			{`(list a b v)`, `'('(1 2) '(1 2) (vector 1 2 3))`, ""},
		}},
		{"optional", elpstest.TestSequence{
			{`(trace)`, `test:1:1: lisp:trace: invalid number of arguments: 0`, ""},
			{`(trace 1 "m" 2)`, `test:1:1: lisp:trace: invalid number of arguments: 3`, ""},
		}},
		{"key", elpstest.TestSequence{
			{`(load-string "(+ 1 2)" :name "k")`, `3`, ""},
			{`(load-string "(+ 1 2)")`, `3`, ""},
			{`(load-string)`, `test:1:1: lisp:load-string: invalid number of arguments: 0`, ""},
			{`(load-string "1" :bogus 2)`, `test:1:1: lisp:load-string: unrecognized keyword argument: bogus`, ""},
			{`(load-string "1" :name)`, `test:1:1: lisp:load-string: function called with an odd number of keyword arguments`, ""},
			{`(load-string "1" 2 3)`, `test:1:1: lisp:load-string: argument is not a keyword: int`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
