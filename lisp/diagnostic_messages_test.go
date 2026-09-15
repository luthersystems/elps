// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestBuiltinDiagnosticMessages(t *testing.T) {
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"reverse", elpstest.TestSequence{
			{`(reverse 'list "hello")`, `test:1:1: lisp:reverse: second argument is not a proper sequence: string`, ""},
		}},
		{"car", elpstest.TestSequence{
			{`(car (vector 1))`, `test:1:1: lisp:car: argument is not a list: array`, ""},
		}},
		{"cdr", elpstest.TestSequence{
			{`(cdr (vector 1))`, `test:1:1: lisp:cdr: argument is not a list: array`, ""},
		}},
		{"rest", elpstest.TestSequence{
			{`(rest 1)`, `test:1:1: lisp:rest: argument is not a proper sequence: int`, ""},
		}},
		{"stable-sort", elpstest.TestSequence{
			{`(stable-sort < 1)`, `test:1:1: lisp:stable-sort: second argument is not a proper list: int`, ""},
		}},
		{"insert-index", elpstest.TestSequence{
			{`(insert-index 'list () "0" 1)`, `test:1:1: lisp:insert-index: third argument is not an integer: string`, ""},
		}},
		{"insert-sorted", elpstest.TestSequence{
			{`(insert-sorted 'list () 1 2)`, `test:1:1: lisp:insert-sorted: third argument is not a function: int`, ""},
		}},
		{"slice", elpstest.TestSequence{
			{`(slice 'list () 0 "1")`, `test:1:1: lisp:slice: fourth argument is not an integer: string`, ""},
		}},
		{"aref", elpstest.TestSequence{
			{`(aref (vector 1) 1)`, `test:1:1: lisp:aref: index 1 out of bounds for array dimension 0 of '(1)`, ""},
		}},
	})
}

func TestCondDefaultSpellings(t *testing.T) {
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"bare else", elpstest.TestSequence{
			{`(cond (else 1) (true 2))`, `test:1:1: lisp:cond: invalid syntax: else`, ""},
			{`(cond (false 1) (else 2))`, `2`, ""},
			{`(cond (else))`, `()`, ""},
		}},
		{"keyword else", elpstest.TestSequence{
			{`(cond (:else 1) (true 2))`, `test:1:1: lisp:cond: invalid syntax: else`, ""},
			{`(cond (false 1) (:else 2))`, `2`, ""},
			{`(cond (:else))`, `()`, ""},
		}},
		{"ordinary tests", elpstest.TestSequence{
			{`(cond (true))`, `()`, ""},
			{`(cond (:other 1) (else 2))`, `1`, ""},
			{`(cond (true 1) (:else 2))`, `1`, ""},
		}},
	})
}
