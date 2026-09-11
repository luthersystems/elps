; Copyright © 2018 The ELPS authors

(in-package 'sicp/diff)
(use-package 'testing)

(defun cadr (s)
  (car (cdr s)))
(defun caddr (s)
  (car (cdr (cdr s))))

(defun variable? (e) (symbol? e))
(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(defun number= (a b)
  (and (number? a) (number? b) (= a b)))

(defun sum? (e)
  (and (list? e) (equal? '+ (car e))))
(defun sum-addend (e)
  (cadr e))
(defun sum-augend (e)
  (caddr e))
(defun make-sum (a b)
  (cond
    ((number= 0 a) b)
    ((number= 0 b) a)
    (:else (list '+ a b))))

(defun product? (e)
  (and (list? e) (equal? '* (car e))))
(defun product-multiplier (e)
  (cadr e))
(defun product-multiplicand (e)
  (caddr e))
(defun make-product (a b)
  (cond
    ((number= 0 a) 0)
    ((number= 0 b) 0)
    ((number= 1 a) b)
    ((number= 1 b) a)
    (:else (list '* a b))))

(defun deriv (expression variable)
  (cond
    ((not (variable? variable)) (error 'invalid-argument "second argument is not a variable"))
    ((number? expression) 0)
    ((variable? expression) (if (same-variable? expression variable) 1 0))
    ((sum? expression)
     (make-sum (deriv (sum-addend expression) variable)
               (deriv (sum-augend expression) variable)))
    ((product? expression)
     (let [(e1 (product-multiplier expression))
           (e2 (product-multiplicand expression))]
       (make-sum (make-product e1 (deriv e2 variable))
                 (make-product (deriv e1 variable) e2))))
    (:else (error 'invalid-expression
                  "unable to differentiate expression"
                  expression))))

(assert-equal 1 (deriv '(+ x 3) 'x))
(assert-equal 'y (deriv '(* x y) 'x))
(assert-equal '(+ (* x y) (* y (+ x 3)))
              (deriv '(* (* x y) (+ x 3)) 'x))
