; SYNTAX TEST "source.elps" "Special operators and control flow"

(if (> x 0) "pos" "neg")
;^^ keyword.control.elps

(cond ((= x 1) "one") ((= x 2) "two"))
;^^^^ keyword.control.elps

(and a b c)
;^^^ keyword.control.elps

(or a b c)
;^^ keyword.control.elps

(progn (do-a) (do-b))
;^^^^^ keyword.control.elps

(set x 42)
;^^^ keyword.control.elps

(set! x 43)
;^^^^ keyword.control.elps

(export 'my-func)
;^^^^^^ keyword.control.elps

(use-package 'math)
;^^^^^^^^^^^ keyword.control.elps

(in-package 'user)
;^^^^^^^^^^ keyword.control.elps

(handler-bind ((error-handler (lambda (e) e)))
;^^^^^^^^^^^^ keyword.control.elps
  (risky-operation))

(ignore-errors (risky-operation))
;^^^^^^^^^^^^^ keyword.control.elps

(dotimes (i 10) (debug-print i))
;^^^^^^^ keyword.control.elps

(thread-first x (f) (g))
;^^^^^^^^^^^^ keyword.control.elps

(thread-last x (f) (g))
;^^^^^^^^^^^ keyword.control.elps

(function my-fn)
;^^^^^^^^ keyword.control.elps

(expr (+ %1 %2))
;^^^^ keyword.control.elps

(qualified-symbol my-sym)
;^^^^^^^^^^^^^^^^ keyword.control.elps

(quasiquote (a b c))
;^^^^^^^^^^ keyword.control.elps

(unquote x)
;^^^^^^^ keyword.control.elps

(unquote-splicing xs)
;^^^^^^^^^^^^^^^^ keyword.control.elps

(test "my test"
;^^^^ keyword.control.elps
  (assert= 1 1))

(test-let ((x 1))
;^^^^^^^^ keyword.control.elps
  (assert= x 1))
