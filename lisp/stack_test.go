// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestStack(t *testing.T) {
	tests := elpstest.TestSuite{
		{"basic tail recursion optimization", elpstest.TestSequence{
			// One of the most trivial tail recursive functions.
			{`(defun tr1 (x) (if (< 0 x) (tr1 (- x 1)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr1 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: test:1:42: lisp:debug-stack
  height 2: test:1:16: lisp:if [terminal]
  height 1: test:1:15: user:tr1 [terminal]
  height 0: test:1:1: lisp:let [terminal]
`},
			// The funcall builtin is capable of triggering tail recursion optimization.
			{`(defun tr2 (x) (if (< 0 x) (funcall tr2 (- x 1)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr2 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: test:1:50: lisp:debug-stack
  height 2: test:1:16: lisp:if [terminal]
  height 1: test:1:15: user:tr2 [terminal]
  height 0: test:1:1: lisp:let [terminal]
`},
			// Even if there is a let that binds lexical variables before the
			// tail-recursive call tail recursion can succeed because argument
			// values are evaluated before the stack is collapsed.
			{`(defun tr3 (x) (if (< 0 x) (let ([y (- x 1)]) (tr3 y)) (debug-stack)))`, `()`, ``},
			{`(let ([x 10]) (tr3 x))`, `()`, `Stack Trace [4 frames -- entrypoint last]:
  height 3: test:1:56: lisp:debug-stack
  height 2: test:1:16: lisp:if [terminal]
  height 1: test:1:15: user:tr3 [terminal]
  height 0: test:1:1: lisp:let [terminal]
`},
			// No tail recursion optimization in this final test.  The
			// non-terminal (TROBlock) handler-bind frame prevents tail
			// recursion
			{`(defun tr4 (x) (if (< 0 x) (handler-bind ([condition (lambda (&rest x))])  (tr4 (- x 1))) (debug-stack)))`, `()`, ``},
			{`(let ([x 1]) (tr4 x))`, `()`, `Stack Trace [7 frames -- entrypoint last]:
  height 6: test:1:91: lisp:debug-stack
  height 5: test:1:16: lisp:if [terminal]
  height 4: test:1:76: user:tr4 [terminal]
  height 3: test:1:28: lisp:handler-bind [tro-blocked]
  height 2: test:1:16: lisp:if [terminal]
  height 1: test:1:14: user:tr4 [terminal]
  height 0: test:1:1: lisp:let [terminal]
`},
		}},
		{"effective stack height", elpstest.TestSequence{
			{`(defun recursive () (+ 1 (recursive)))`, `()`, ``},
			{`(defun tail-recursive (n) (if (> n 0) (tail-recursive (- n 1)) ()))`, `()`, ``},
			{`(recursive)`, `test:1:26: recursive: physical stack height exceeded maximum: 25001`, ``},
			// NOTE:  It's a little hard to control the function reported in
			// the following error message.  It may change if there is a change
			// in the function definition or to the maximum logical stack
			// height.
			{`(tail-recursive 100000)`, `test:1:31: lisp:if: logical stack height exceeded maximum: 50001`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
