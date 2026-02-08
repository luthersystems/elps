// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestMacros(t *testing.T) {
	tests := elpstest.TestSuite{
		{"test-trace", elpstest.TestSequence{
			{"(set 'x 0)", "0", ""},
			{"(defun fun () (set 'x (+ x 1)))", "()", ""},
			{"(trace (fun))", "1", "\"TRACE\" 1\n"},
			{`(trace (fun) "TRACE2")`, "2", "\"TRACE2\" 2\n"},
		}},
		{"quasiquote", elpstest.TestSequence{
			{`(quasiquote ())`, `'()`, ``},
			{`(quasiquote '())`, `''()`, ``},
			{`(quasiquote ''())`, `'''()`, ``},
			{`(quasiquote ('''''test))`, `'('''''test)`, ``},
			{`(quasiquote (unquote '()))`, `''()`, ``},
			{`(quasiquote '(unquote '()))`, `'''()`, ``},
			{`(quasiquote ''(unquote '()))`, `''''()`, ``},
			{`(quasiquote '''(unquote '()))`, `'''''()`, ``},
			{`(let* ([x (list 1 2 3)]) (quasiquote (debug-print (unquote x))))`, `'(debug-print '(1 2 3))`, ``},
			{"(quasiquote (reverse 'list '(1 2 3)))", "'(reverse 'list '(1 2 3))", ""},
			{"(quasiquote (unquote (reverse 'list '(1 2 3))))", "''(3 2 1)", ""},
			{"(quasiquote ((unquote-splicing (reverse 'list '(1 2 3)))))", "'(3 2 1)", ""},
			{"(quasiquote (unquote '(reverse 'list '(1 2 3))))", "''(reverse 'list '(1 2 3))", ""},
			{"(quasiquote ((unquote-splicing '(reverse 'list '(1 2 3)))))", "'(reverse 'list '(1 2 3))", ""},
			{"(quasiquote (1 2 (unquote-splicing '(3 4)) 5))", "'(1 2 3 4 5)", ""},
			{"(let ((xs '(2 1))) (quasiquote (concat 'list '(1 2) (unquote xs))))", "'(concat 'list '(1 2) '(2 1))", ""},
			{"(quasiquote (unquote test-symbol))", "test:1:22: lisp:quasiquote: unbound symbol: test-symbol", ""},
			{"(quasiquote (list (unquote-splicing test-symbol)))", "test:1:37: lisp:quasiquote: unbound symbol: test-symbol", ""},
			{"(quasiquote (list (unquote-splicing 1 2)))", "test:1:19: lisp:quasiquote: unquote-splicing: one argument expected (got 2)", ""},
			{"(quasiquote (list (unquote 1 2)))", "test:1:19: lisp:quasiquote: unquote: one argument expected (got 2)", ""},
			{"(quasiquote (unquote-splicing '(+ 2 3)))", "test:1:13: lisp:quasiquote: unquote-splicing used in an invalid context", ""},
			{"(quasiquote '(unquote-splicing '(+ 2 3)))", "test:1:40: lisp:quasiquote: unquote-splicing used in an invalid context", ""},
			{"(quasiquote ''(unquote-splicing '(+ 2 3)))", "test:1:41: lisp:quasiquote: unquote-splicing used in an invalid context", ""},
			{"(quasiquote ((unquote-splicing '(+ 2 3))))", "'(+ 2 3)", ""},
		}},
		{"defmacro", elpstest.TestSequence{
			{"(defmacro m0 () (quasiquote (+ 1 1)))", "()", ""},
			{"(defmacro m1 (x) (quasiquote (+ (unquote x) 1)))", "()", ""},
			{"(defmacro m2 (x y) (quasiquote (+ (unquote x) (unquote y))))", "()", ""},
			{"(m0)", "2", ""},
			{"(m1 1)", "2", ""},
			{"(m2 1 2)", "3", ""},
			{`(macroexpand '(m0))`, `'(+ 1 1)`, ""},
			{`(macroexpand '(m1 (* 2 3)))`, `'(+ (* 2 3) 1)`, ""},
			{`(macroexpand '(m2 (* 2 3) (m0)))`, `'(+ (* 2 3) (m0))`, ""},
			{`(macroexpand-1 '(m0))`, `'(+ 1 1)`, ""},
			{`(macroexpand-1 '(m1 (* 2 3)))`, `'(+ (* 2 3) 1)`, ""},
			{`(macroexpand-1 '(m2 (* 2 3) (m0)))`, `'(+ (* 2 3) (m0))`, ""},
		}},
		{"macroexpand", elpstest.TestSequence{
			{`(defmacro // (&rest x) (quasiquote (or (unquote-splicing x))))`, `()`, ""},
			{`(defmacro m (&rest x) (quasiquote (// (unquote (car x)) (m (unquote-splicing (cdr x))))))`, `()`, ""},
			{`(macroexpand '(m 1 2 3))`, `'(or 1 (m 2 3))`, ""},
			{`(macroexpand-1 '(m 1 2 3))`, `'(// 1 (m 2 3))`, ""},
		}},
		{"defmacro advanced 1", elpstest.TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()", ""},
			{"(set 'z 1)", "1", ""},
			{"(m1 z)", "4", ""},
		}},
		{"defmacro advanced 2", elpstest.TestSequence{
			{"(defmacro m1 (x) (quasiquote (let ((y (+ (unquote x) 1))) (+ y y))))", "()", ""},
			{"(set 'z 1)", "1", ""},
			{"(m1 z)", "4", ""},
		}},
		{"defmacro repeated expansion", elpstest.TestSequence{
			{`(defmacro mlen (&rest xs) (if (nil? xs) 0 (quasiquote (+ 1 (mlen (unquote-splicing (cdr xs)))))))`, `()`, ``},
			{`(macroexpand '(mlen))`, `0`, ``},
			{`(mlen)`, `0`, ``},
			{`(macroexpand '(mlen a))`, `'(+ 1 (mlen))`, ``},
			{`(mlen a)`, `1`, ``},
			{`(macroexpand '(mlen a b))`, `'(+ 1 (mlen b))`, ``},
			{`(mlen a b)`, `2`, ``},
		}},
		{"defmacro optional args", elpstest.TestSequence{
			{"(defmacro mrest (&rest xs) (quasiquote (- (unquote-splicing xs))))", "()", ""},
			{"(defmacro mopt (&optional x) (let ([sym (gensym)]) (quasiquote (let ([(unquote sym) (unquote x)]) (if (unquote sym) (mrest (unquote sym)) (mrest 0))))))", "()", ""},
			{"(defmacro mopt1 (&optional x) (let ([x (if (nil? x) 0 x)]) (quasiquote (if (unquote x) (mrest (unquote x)) (mrest 0)))))", "()", ""},
			{"(mopt 1)", "-1", ""},
			{"(mopt)", "0", ""},
			{"(mopt (+ 1 1))", "-2", ""},
			{"(mrest (mopt (+ 1 1)))", "2", ""},
			{"(mopt1 1)", "-1", ""},
			{"(mopt1)", "0", ""},
			{"(mopt1 (+ 1 1))", "-2", ""},
			{"(mrest (mopt1 (+ 1 1)))", "2", ""},
		}},
		{"my-defun", elpstest.TestSequence{
			{"(defmacro my-defun (name formals &rest exprs) (quasiquote (defun (unquote name) (unquote formals) (unquote-splicing exprs))))", "()", ""},
			{"(my-defun test-fun (x y) (debug-print x) (debug-print y) (+ x y))", "()", ""},
			{"(test-fun 1 2)", "3", "1\n2\n"},
			{"test-fun", "(lambda (x y) (debug-print x) (debug-print y) (+ x y))", ""},
		}},
		{"macrolet", elpstest.TestSequence{
			{`(defun f (x y)
				(macrolet ([test-x (z) (quasiquote (if (< x 10) y (unquote z)))])
					(+ y (test-x (* y y)))))
			`, `()`, ``},
			{`(f 1 3)`, `6`, ``},
			{`(f 11 3)`, `12`, ``},
			{`(test-x 11 3)`, `test:1:2: unbound symbol: test-x`, ``},
			{`(macrolet (
				[m1 (y) (quasiquote (+ 1 (unquote y)))]
				[m2 (y) (quasiquote (* (unquote (m1 y)) 2))])
				(m2 3))`, `test:3:38: lisp:quasiquote: unbound symbol: m1`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func TestMacroExpansionSourceLocations(t *testing.T) {
	// Issue #43: Macro-expanded code should use call-site locations, not
	// the macro definition site or "<native code>".
	//
	// Before the fix, (defun true () 42) produced:
	//   "<native code>: lisp:set: cannot rebind constant: true"
	// After the fix it produces:
	//   "test:1:1: lisp:set: cannot rebind constant: true"
	//
	// The elpstest framework uses exact string equality, so these
	// assertions implicitly verify that "<native code>" is absent.
	tests := elpstest.TestSuite{
		{"defun-rebind-constant", elpstest.TestSequence{
			// defun is a builtin macro that generates (progn (set 'name fn) ()).
			// All generated nodes have Pos < 0 (native). After stamping,
			// the error should include the call site location.
			{"(defun true () 42)", "test:1:1: lisp:set: cannot rebind constant: true", ""},
		}},
		{"defun-rebind-false", elpstest.TestSequence{
			{"(defun false () 42)", "test:1:1: lisp:set: cannot rebind constant: false", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

func BenchmarkMacroDefun(b *testing.B) {
	elpstest.RunBenchmark(b, `
		(defun benchmark () (debug-print 1 2 3))
		(defun benchmark (a) (debug-print a))
		(defun benchmark (a b) (debug-print a b))
		(defun benchmark (a b c) (debug-print a b c))
		(defun benchmark (a b &optional c) (debug-print a b c))
		(defun benchmark (a &optional b c) (debug-print a b c))
		(defun benchmark (&optional a b c) (debug-print a b c))
		(defun benchmark (a b &key c) (debug-print a b c))
		(defun benchmark (a &key b c) (debug-print a b c))
		(defun benchmark (&key a b c) (debug-print a b c))
		(defun benchmark (&rest a) (apply debug-print a))
	`)
}
