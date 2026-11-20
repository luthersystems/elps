// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// Each flagged form fails at run time: a quoted symbol at the head of a list
// made by list or cons is data, not a function (luthersystems/elps#999).
func TestMacroQuotedHead(t *testing.T) {
	diags := lintCheck(t, AnalyzerMacroQuotedHead, `(defmacro add (a b) (list '+ a b))
(defmacro add2 (a b) (lisp:list (quote +) a b))
(defmacro my-when (c &rest body)
  (quasiquote (if (unquote c) (unquote (cons 'progn body)) ())))
(defmacro nested (c x)
  "doc"
  (let ((y x))
    (if c (list 'if c (list 'progn y)) (cons 'list (list y)))))
(defmacro sp () (quasiquote (progn (unquote-splicing (list (list 'f 1))))))
(defmacro conds (c) (cond (c (list 'g)) (:else (progn (list 'h)))))`)
	assertDiagOnLine(t, diags, 1, "built with list has the quoted head '+")
	assertDiagOnLine(t, diags, 2, "quoted head '+")
	assertDiagOnLine(t, diags, 4, "built with cons has the quoted head 'progn")
	assertDiagOnLine(t, diags, 8, "quoted head 'if")
	assertDiagOnLine(t, diags, 8, "quoted head 'progn")
	assertDiagOnLine(t, diags, 8, "quoted head 'list")
	assertDiagOnLine(t, diags, 9, "quoted head 'f")
	assertDiagOnLine(t, diags, 10, "quoted head 'g")
	assertDiagOnLine(t, diags, 10, "quoted head 'h")
	assert.Len(t, diags, 9)
}

// Lookalikes that work at run time are not flagged.
func TestMacroQuotedHeadValid(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerMacroQuotedHead, `(defmacro ok1 (a b) (quasiquote (+ (unquote a) (unquote b))))
(defmacro ok2 (x)
  (let ((keys (list 'a 'b)))
    (quasiquote (member (unquote x) (quote (unquote keys))))))
(defmacro ok3 (a) (list (first (quote (+))) a 1))
(defmacro ok4 (a) (let ((h '+)) (list a h)))
(defmacro ok5 (&rest body) (quasiquote (progn (unquote-splicing body))))
(defmacro ok6 (x) (quasiquote (quasiquote (f (unquote (list 'g (unquote x)))))))
(defun not-a-macro () (list '+ 1 2))
(set 'data (list 'a 1))
(defmacro ok7 (a) (list '+ a 1)) ; nolint:macro-quoted-head`))
	// A file that defines its own list is not checked.
	assertNoDiags(t, lintCheck(t, AnalyzerMacroQuotedHead, `(defun list (&rest xs) xs)
(defmacro m () (list 'f))`))
}
