// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestSpecialOp(t *testing.T) {
	debugstack := `Stack Trace [4 frames -- entrypoint last]:
  height 3: test:4: lisp:debug-stack
  height 2: test:4: lisp:progn
  height 1: test:2: lisp:let
  height 0: test:1: lisp:let [terminal]
`
	tests := elpstest.TestSuite{
		{"if", elpstest.TestSequence{
			// if operator
			{"(if () 1 2)", "2", ""},
			{"(if true 1 2)", "1", ""},
			{"(if true (set 'x 1) (set 'x 2))", "1", ""},
			{"x", "1", ""},
			{"(if () (set 'x 1) (set 'x 2))", "2", ""},
			{"x", "2", ""},
			{"(if '(()) 1 2)", "1", ""},
			{`(if "false" 1 2)`, "1", ""},
		}},
		{"let", elpstest.TestSequence{
			{`(let ())`, "()", ""},
			{`(let ((x 1)) x)`, "1", ""},
			{`(let ([x 1]) x)`, "1", ""},
			{`(let ([x 1] [y 2]) (+ x y))`, "3", ""},
			{`(let ([x 0])
				(let (
						[x 1]
						[y (+ (progn (debug-stack) x) 1)])
					(+ x y)))`, "2", debugstack},
		}},
		{"set!", elpstest.TestSequence{
			{`(set 'x 1)`, `1`, ``},
			{`(set! x 2)`, `()`, ``},
			{`x`, `2`, ``},
			{`(let ([x 'a]) (set! x 'b) x)`, `'b`, ``},
			{`x`, `2`, ``},
			{`(set! false x)`, `test:1: lisp:set!: cannot rebind constant: false`, ``},
			{`(set! foo 3)`, `test:1: lisp:set!: symbol not bound: foo`, ``},
		}},
		{"let*", elpstest.TestSequence{
			{`(let* ())`, "()", ""},
			{`(let* ((x 1)) x)`, "1", ""},
			{`(let* ([x 1]) x)`, "1", ""},
			{`(let* ([x 1] [y 2]) (+ x y))`, "3", ""},
			{`(let* ([x 0]) (let* ([x 1] [y (+ x 1)]) (+ x y)))`, "3", ""},
			// BUG:  let* should function like the following commented tests
			// according to scheme and CL.  Instead let* is able to define
			// recursive functions.  And lambdas in a let* can see bindings
			// defined later than it.
			//{`(let ((bar 0)) (let* ((foo (lambda () bar)) (bar 1)) (foo)))`, `0`, ``},
			//{`(let* ((f (lambda (x)
			//			(if (= 0 x) 0 (f (- x 1))))))
			//	(f 10))`, `test:2: lisp:if: unbound symbol: f`, ``},
		}},
		{"flet", elpstest.TestSequence{
			{`(flet [])`, `()`, ``},
			{`(flet ([f (x) x]) (f 2))`, `2`, ``},
			{`(flet ([f (x) (f (+ x 1))]) (f 0))`, `test:1: f: unbound symbol: f`, ``},
			{`(defun orig () 1)`, `()`, ``},
			{`(flet ([orig () 2] [f () (orig)]) (f))`, `1`, ``},
		}},
		{"labels", elpstest.TestSequence{
			{`(labels [])`, "()", ""},
			{`(labels ([f (x) x]) (f 2))`, "2", ""},
			{`(labels ([f (x y) (if (= x 0) y (f (- x 1) (+ y 1)))]) (f 3 2))`, "5", ""},
			{`(defun orig () 1)`, "()", ""},
			{`(labels ([orig () 2] [f () (orig)]) (f))`, "2", ""},
			{`(labels ([f () (orig)] [orig () 2]) (f))`, "2", ""},
		}},
		{"dotimes", elpstest.TestSequence{
			{`(dotimes (n 5) (debug-print n))`, "()", "0\n1\n2\n3\n4\n"},
			{`(dotimes (n 0) (debug-print n))`, "()", ""},
			{`(dotimes (n -1) (debug-print n))`, "()", ""},
			{`(dotimes (i 3)
			    (set! i 2)
			    (debug-print i))`, "()", "2\n2\n2\n"},
			{`(dotimes (n 5))`, "()", ""},
			{`(dotimes (n 5) 1)`, "()", ""}, // return doesn't depend on the body's result
			{`(dotimes (n 5 n))`, "5", ""},
			{`(dotimes (n -1 n))`, "0", ""},
			{`(dotimes (n -1 n))`, "0", ""},
			{`(set 'x 0)`, "0", ""},
			{`(dotimes (i 5 x) (set 'x (+ x i)))`, "10", ""},
			{`(let* ([n 10])
			    (dotimes (n 5))
			    n)`, "10", ""},
		}},
		{"cond", elpstest.TestSequence{
			{`(cond)`, "()", ""},
			{`(cond (else 1))`, "1", ""},
			{`(cond (:else 1))`, "1", ""},
			{`(cond (true 2) (else 1))`, "2", ""},
			{`(cond (true) (else 1))`, "()", ""},
			{`(cond (true 1 (+ 1 2)) (else 1))`, "3", ""},
			{`(cond ((< 1 2) 3) (else 1))`, "3", ""},
		}},
		{"expr", elpstest.TestSequence{
			{`((expr ()))`, "()", ""},
			{`((expr "hello"))`, `"hello"`, ""},
			{`((expr %) 123)`, "123", ""},
			{`((expr %2) 'a 'b)`, "'b", ""},
			{`((expr (reverse 'list %&rest)) 1 2 3)`, "'(3 2 1)", ""},
			{`((expr %&optional))`, "()", ""},
			{`((expr %&optional) 1)`, "1", ""},
			{`((expr (cons 1 %&optional)))`, "'(1)", ""},
			{`((expr (cons 1 %&optional)) '(2))`, "'(1 2)", ""},
		}},
		{"#^", elpstest.TestSequence{
			{`(#^())`, "()", ""},
			{`(#^"hello")`, `"hello"`, ""},
			{`(#^% 123)`, "123", ""},
			{`(#^%2 'a 'b)`, "'b", ""},
			{`(#^(reverse 'list %&rest) 1 2 3)`, "'(3 2 1)", ""},
			{`(#^%&optional)`, "()", ""},
			{`(#^%&optional 1)`, "1", ""},
			{`(#^(cons 1 %&optional))`, "'(1)", ""},
			{`(#^(cons 1 %&optional) '(2))`, "'(1 2)", ""},
			{`(#^'(cons 1 %))`, "'(cons 1 %)", ""},
			{`(#^(list 1 '%))`, "'(1 '%)", ""},
		}},
		{"threading", elpstest.TestSequence{
			{`(thread-last 1 (+ 2) (< 2))`, `true`, ""},
			{`(thread-last 1 (+ 2) (> 2))`, `false`, ""},
			{`(thread-first 1 (+   2) (<   2))`, `false`, ""},
			{`(thread-first 1 (+   2) (>   2))`, `true`, ""},
			{`(map 'list (lambda (x) (thread-last x (+ 2) (< 2))) '(1 -1))`, `'(true false)`, ""},
			{`(map 'list (lambda (x) (thread-first x (+ 2) (< 2))) '(1 -1))`, `'(false true)`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
