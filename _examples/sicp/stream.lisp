; Copyright © 2018 The ELPS authors

(in-package 'sicp/stream)

(load-file "sicp.lisp")

(use-package 'sicp)
(use-package 'testing)

(export 'delay)
(defmacro delay (expr) ; nolint:shadowing
  (let ([valsym (gensym)]
        [funsym (gensym)])
    (quasiquote (progn
                  (let* ([(unquote valsym) ()]
                         [(unquote funsym) (lambda ()
                                             (set! (unquote valsym) (unquote expr)))])
                    (lambda ()
                      (funcall (unquote funsym))
                      (set! (unquote funsym) #^())
                      (unquote valsym)))))))

(let ([x 0]) ; nolint:shadowing
  (let ([f (delay (set! x (+ x 1)))])
    (f)
    (f)
    (assert= 1 x)))

(export 'stream-concat)
(defun stream-concat (&rest s)
  (cond ((nil? s) the-empty-stream)
        ((stream-null? (car s)) (apply stream-concat (cdr s)))
        (:else (stream-cons (stream-car (car s))
                            (apply stream-concat
                                   (stream-cdr (car s))
                                   (cdr s))))))

(export 'stream-flatmap)
(defun stream-flatmap (proc s)
  (if (stream-null? s)
    the-empty-stream
    (stream-concat (funcall proc (stream-car s))
                   (stream-flatmap proc (stream-cdr s)))))

(export 'stream-repeat)
(defun stream-repeat (x) ; nolint:shadowing
  (stream-cons x (stream-repeat x)))

(export 'stream-collect)
(defun stream-collect (s)
  (if (stream-null? s)
    ()
    (cons (stream-car s) (stream-collect (stream-cdr s)))))

(export 'stream-ref)
(defun stream-ref (s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(export 'stream-take)
(defun stream-take (s n)
  (if (<= n 0)
    the-empty-stream
    (stream-cons (stream-car s)
                 (stream-take (stream-cdr s) (- n 1)))))

(export 'stream-drop)
(defun stream-drop (s n)
  (if (<= n 0) s (stream-drop (stream-cdr s) (- n 1))))

(export 'stream-slice)
(defun stream-slice (s start end)
  (stream-take (stream-drop s start) (- end start)))

(export 'stream-map)
(defun stream-map (proc &rest s)
  (if (any? 'stream-null? s)
    the-empty-stream
    (stream-cons (apply proc (map 'list 'stream-car s))
                 (apply 'stream-map proc (map 'list stream-cdr s)))))

(export 'stream-for-each)
(defun stream-for-each (proc s)
  (if (stream-null? s)
    'done
    (progn
      (funcall proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(export 'stream-debug)
(defun stream-debug (s) ; nolint:unused-function
  (stream-for-each 'debug-print s))

(export 'stream-cons)
(defmacro stream-cons (a b)
  (quasiquote (list (unquote a) (delay (unquote b)))))

(export 'the-empty-stream)
(set 'the-empty-stream ())
(export 'stream-null?)
(defun stream-null? (s) (nil? s))
(export 'stream-car)
(defun stream-car (s) (first s))
(export 'stream-cdr)
(defun stream-cdr (s) (funcall (second s)))

(defun stream-enumerate-interval (low &optional high)
  (if (and high (> low high))
    the-empty-stream
    (stream-cons low (stream-enumerate-interval (+ low 1) high))))

(assert= 10 (stream-ref (stream-enumerate-interval 0) 10))
(assert= 100 (stream-ref (stream-enumerate-interval 0) 100))
(set 'finite-stream (stream-enumerate-interval 0 10))
(assert= 5 (stream-ref (stream-enumerate-interval 0) 5))
(assert= 5 (stream-ref (stream-enumerate-interval 0) 5))

(export 'stream-filter)
(defun stream-filter (pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((funcall pred (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (:else (stream-filter pred (stream-cdr s)))))

(set 'prime-stream (stream-filter 'prime? (stream-enumerate-interval 3 100)))
(assert-equal '(3 5 7 11) (stream-collect (stream-take prime-stream 4)))
(assert-equal '(7 11 13 17 19) (stream-collect (stream-slice prime-stream 2 7)))

(set 'fibs (stream-cons 1 (stream-cons 1 (stream-map '+ fibs (stream-cdr fibs)))))
(assert-equal '(1 1 2 3 5) (stream-collect (stream-take fibs 5)))

(assert-equal '(1 1 2 2 2 3 3 3 3)
              (stream-collect (stream-concat (stream-take (stream-repeat 1) 2)
                                             (stream-take (stream-repeat 2) 3)
                                             (stream-take (stream-repeat 3) 4))))

(assert-equal '(1 2 2 3 3 3)
              (stream-collect
                (stream-flatmap (lambda (n) (stream-take (stream-repeat n) n))
                                (stream-take (stream-enumerate-interval 1 1000) 3))))

(defun interleave (s1 s2)
  (if (stream-null? s1)
    s2
    (stream-cons (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(set 'positive-integers (stream-cons 1 (stream-map #^(+ 1 %) positive-integers)))

(defun pairs (s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave (stream-map (lambda (x) (list (stream-car s) x)) ; nolint:shadowing
                            (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))

(set 'upper-integer-pairs (pairs positive-integers positive-integers))

(assert-equal '('(1 1)
                '(1 2)
                '(2 2)
                '(1 3)
                '(2 3)
                '(1 4)
                '(3 3)
                '(1 5)
                '(2 4)) ; this order sucks
              (stream-collect (stream-take upper-integer-pairs 9)))

(defun stream-fold (proc z s)
  (if (stream-null? s)
    z
    (stream-fold proc (funcall proc z (stream-car s)) (stream-cdr s))))

(defun partial-streams (s)
  (stream-map 'stream-take (stream-repeat s) positive-integers))

(assert-equal '('(1)
                '(1 2)
                '(1 2 3)
                '(1 2 3 4)
                '(1 2 3 4 5)
                '(1 2 3 4 5 6)
                '(1 2 3 4 5 6 7)
                '(1 2 3 4 5 6 7 8)
                '(1 2 3 4 5 6 7 8 9))
              (stream-collect (stream-take (stream-map 'stream-collect (partial-streams positive-integers))  9)))

(defun better-pairs-streams (s t)
  (stream-map (lambda (ps ti)
                (stream-map #^(list % ti) ps))
              (partial-streams s)
              t))

(defun better-pairs (s t)
  ; a function like the external stream-concat but it operates on a potentially
  ; infinite stream-of-streams instead of a finite list of stream arguments.
  (labels ([stream-concat (sos) ; nolint:shadowing
            (cond
              ((stream-null? sos) the-empty-stream)
              ((stream-null? (stream-car sos)) (stream-concat (stream-cdr sos)))
              (:else
                (stream-cons (stream-car (stream-car sos))
                             (stream-concat (stream-cons (stream-cdr (stream-car sos))
                                                         (stream-cdr sos))))))])
    (stream-concat (better-pairs-streams s t))))

(assert-equal '('(1 1)
                '(1 2)
                '(2 2)
                '(1 3)
                '(2 3)
                '(3 3)
                '(1 4)
                '(2 4)
                '(3 4)) ; this order is good 👍
              (stream-collect (stream-take (better-pairs positive-integers positive-integers) 9)))
