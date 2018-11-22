// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestScope(t *testing.T) {
	tests := elpstest.TestSuite{
		{"lexical scope", elpstest.TestSequence{
			// simple lexical scoping tests
			{"(let ((x 1)) x)", "1", ""},
			{"x", "test:1: unbound symbol: x", ""},
			{"(set 'x 1)", "1", ""},
			{"(let ((x 2)) x)", "2", ""},
			{"x", "1", ""},
			{"(let ((x 3)) (defun fn (y) (+ x y)))", "()", ""},
			{"(let ((x 2)) (fn 2))", "5", ""},
			{"(((lambda (x) (lambda () (+ x 2))) 3))", "5", ""},
		}},
		{"special operators", elpstest.TestSequence{
			{"(let ((x 1) (y 2)) (if x (+ x y) y))", "3", ""},
			{`(let ((x 1) (y 2))
				(cond
					((< y 0) y)
					(else x)))`, "1", ""},
		}},
		{"docs", elpstest.TestSequence{
			// scope example from documentation
			{"(let ((x 1) (y 2)) (defun add-y (x) (+ x y)))", "()", ""},
			{"(add-y 3)", "5", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
