; Copyright © 2018 The ELPS authors

; basic examples adapted from SICP
(in-package 'sicp)

(use-package 'testing)

(export 'remainder)
(defun remainder (a b)
  (mod a b))

(export 'gcd)
(defun gcd (a b)
  (cond
    ((< a 0) (gcd (math:abs a) b))
    ((< b 0) (gcd a (math:abs b)))
    ((= a 0) b)
    ((= b 0) a)
    (:else (gcd b (remainder a b)))))

(assert= 1 (gcd 5 3))
(assert= 1 (gcd 3 5))
(assert= 2 (gcd 4 2))
(assert= 2 (gcd 2 4))
(assert= 2 (gcd 2 4))
(assert= 20 (gcd 60 80))

(defun square (x) ; nolint
  (* x x))

(defun divides? (n d)
  (= 0 (remainder n d)))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond
    ((> (* test-divisor test-divisor) n) n)
    ((divides? n test-divisor) test-divisor)
    (:else (find-divisor n (+ 1 test-divisor)))))

(export 'prime?)
(defun prime? (n)
  (= n (smallest-divisor n)))

(defun accumulate-iter (combiner null-value term a next done? &optional match?)
  (if (done? a)
    null-value
    (let [(curr    (term a))
          (match?  (if (nil? match?) (lambda (_) true) match?))] ; nolint:shadowing
      (accumulate-iter combiner
                       (if (match? curr)
                         (funcall combiner null-value curr)
                         (:else null-value))
                       term
                       (next a)
                       next
                       done?
                       match?))))

(defun sum (term a next b) ; nolint:unused-variable
  (accumulate-iter '+ 0
                   term a
                   #^(+ 1 %)
                   #^(> % b)))

(assert= 6 (sum identity 1 #^(+ 1 %) 3))
(assert= 6 (sum identity 1 #^(+ 1 %) 3))

(defun make-rat (numer denom)
  (rat-norm (list numer denom)))

(defun rat-norm (r)
  (cond
    ((< (rat-denom r) 0)
     (rat-norm (list (- (rat-numer r))
                     (- (rat-denom r)))))
    ((> (rat-denom r) 1)
     (let [(c (gcd (rat-numer r) (rat-denom r)))]
       (list (to-int (/ (rat-numer r) c))
             (to-int (/ (rat-denom r) c)))))
    (:else
      r)))

(defun rat-numer (r)
  (first r))

(defun rat-denom (r)
  (second r))

(defun rat-neg (r) ; nolint:unused-function
  (make-rat (- (rat-numer r)) (rat-denom r)))

(defun rat-add (a b)
  (make-rat (+ (* (rat-numer a) (rat-denom b))
               (* (rat-numer b) (rat-denom a)))
            (* (rat-denom a) (rat-denom b))))

(defun rat-sub (a b) ; nolint:unused-function
  (make-rat (- (* (rat-numer a) (rat-denom b))
               (* (rat-numer b) (rat-denom a)))
            (* (rat-denom a) (rat-denom b))))

(defun rat-mul (a b)
  (make-rat (* (rat-numer a) (rat-numer b))
            (* (rat-denom a) (rat-denom b))))

(defun rat-div (a b)
  (make-rat (* (rat-numer a) (rat-denom b))
            (* (rat-denom a) (rat-numer b))))

(defun rat= (a b)
  (and (= (rat-numer a) (rat-numer b))
       (= (rat-denom a) (rat-denom b))))

(defun rat-string (r)
  (format-string "{}/{}" (rat-numer r) (rat-denom r)))

(assert= 1 (rat-numer (make-rat 1 2)))
(assert= 2 (rat-denom (make-rat 1 2)))
(assert= -1 (rat-numer (make-rat -1 2)))
(assert= 2 (rat-denom (make-rat -1 2)))
(assert= -1 (rat-numer (make-rat 1 -2)))
(assert= 2 (rat-denom (make-rat 1 -2)))

(assert (rat= (make-rat 1 2)
              (make-rat 30 60)))
(assert (rat= (make-rat -1 2)
              (make-rat 1 -2)))
(assert (rat= (make-rat 1 2)
              (make-rat 1 2)))

(assert-string= "1/1" (rat-string (make-rat 1 1)))
(assert-string= "1/2" (rat-string (make-rat 1 2)))
(assert-string= "1/2" (rat-string (make-rat -1 -2)))
(assert-string= "-1/2" (rat-string (make-rat -1 2)))
(assert-string= "-1/2" (rat-string (make-rat 1 -2)))
(assert-string= "1/2" (rat-string (make-rat 3 6)))
(assert-string= "-1/2" (rat-string (make-rat -3 6)))
(assert-string= "-1/2" (rat-string (make-rat 3 -6)))

(assert-string= "5/6" (rat-string (rat-add (make-rat 1 2)
                                           (make-rat 1 3))))
(assert-string= "1/6" (rat-string (rat-mul (make-rat 1 2)
                                           (make-rat 1 3))))
(assert-string= "2/1" (rat-string (rat-div (make-rat 1 2)
                                           (make-rat 1 4))))
(defun filter (predicate seq)
  (cond ((nil? seq) ())
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (:else (filter predicate (cdr seq)))))

(defun accumulate (op initial seq)
  (if (nil? seq)
    initial
    (funcall op
             (car seq)
             (accumulate op initial (cdr seq)))))

(defun map (p seq)
  (accumulate (lambda (x ys) (cons (p x) ys))
              ()
              seq))

(defun append (lis1 lis2)
  (accumulate 'cons lis2 lis1))

(defun length (lis) ; nolint
  (accumulate '+ 0 seq)) ; nolint:undefined-symbol

(assert-equal '(2 4 6 8) (map #^(* 2 %) '(1 2 3 4)))
(assert-equal '(1 2 3 4) (append '(1) '(2 3 4)))

(defun horner-eval (x coefficient-list)
  (accumulate (lambda (a higher) (+ a (* x higher)))
              0.0
              coefficient-list))

(assert= 7 (horner-eval 2 (list 1 3)))
(assert= 79 (horner-eval 2 (list 1 3 0 5 0 1)))

(defun fold-right (op initial seq) (accumulate op initial seq))

(defun fold-left (op initial seq)
  (if (nil? seq)
    initial
    (fold-left op
               (funcall op initial (car seq))
               (cdr seq))))

(assert= 1.5 (fold-right / 1 (list 1 2 3)))
(assert= (/ 1 6) (fold-left / 1 (list 1 2 3)))
(assert-equal (list 1 (list 2 (list 3 ()))) (fold-right 'list () (list 1 2 3)))
(assert-equal (list (list (list () 1) 2) 3) (fold-left 'list () (list 1 2 3)))

(defun flatmap (proc seq)
  (accumulate 'append () (map proc seq)))

(defun enumerate-tree (tree)
  (let [(enumerate-subtrees (lambda (tree) ; nolint:shadowing
                              (cond ((nil? tree) ())
                                    ((not (list? tree)) (list tree))
                                    (:else (enumerate-tree tree)))))]
    (flatmap enumerate-subtrees tree)))

(assert-equal '(1 2 3 4 5) (enumerate-tree (list 1 (list 2 3) (list 4 (list 5)))))

; inclusive enumeration
(defun enumerate-interval (a b)
  (if (> a b)
    ()
    (cons a (enumerate-interval (+ 1 a) b))))

(defun unique-pairs (a b)
  (flatmap (lambda (i) (map #^(list i %)
                            (enumerate-interval (+ i 1) b)))
           (enumerate-interval a (- b 1))))

(assert-equal (list (list 1 2) (list 1 3) (list 2 3)) (unique-pairs 1 3))

(defun prime-sum? (pair)
  (prime? (+ (first pair) (second pair))))

(defun prime-sum-pairs (n)
  (filter prime-sum?
          (unique-pairs 1 n)))

(assert-equal (list (list 1 2)
                    (list 1 4)
                    (list 1 6)
                    (list 2 3)
                    (list 2 5)
                    (list 3 4)
                    (list 5 6))
              (prime-sum-pairs 6))

(defun ordered-triples (a b)
  (flatmap (lambda (i) (map #^(cons i %) (unique-pairs (+ i 1) b)))
           (enumerate-interval a (- b 2))))

(assert-equal () (ordered-triples 1 2))
(assert-equal (list (list 1 2 3)) (ordered-triples 1 3))
(assert-equal (list (list 1 2 3)
                    (list 1 2 4)
                    (list 1 3 4)
                    (list 2 3 4))
              (ordered-triples 1 4))
