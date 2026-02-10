; Copyright © 2018 The ELPS authors

(in-package 'sicp/complex)
(use-package 'math)
(use-package 'testing)

(export 'square)
(defun square (x) ; nolint:shadowing
  (* x x))

(defun pair? (value)
  (and (list? value) (= 2 (length value))))

(export 'attach-tag)
(defun attach-tag (type-tag contents) ; nolint:shadowing
  (list type-tag contents))

(export 'type-tag)
(defun type-tag (datum)
  (if (pair? datum)
    (first datum)
    (error 'argument-error "bad tagged datum" datum)))

(export 'contents)
(defun contents (datum)
  (if (pair? datum)
    (second datum)
    (error 'argument-error "bad tagged datum" datum)))

(defun rectangular? (z) ; nolint:unused-function
  (equal? (type-tag z) 'rectangular))

(defun polar? (z) ; nolint:unused-function
  (equal? (type-tag z) 'polar))

(defun rectangular-real-part (z)
  (first z))

(defun rectangular-imag-part (z)
  (second z))

(defun rectangular-magnitude (z)
  (sqrt (+ (square (rectangular-real-part z))
           (square (rectangular-imag-part z)))))

(defun rectangular-angle (z)
  (atan (rectangular-imag-part z)
        (rectangular-real-part z)))

(defun make-rectangular-from-real-imag (x y) ; nolint:shadowing
  (attach-tag 'rectangular (list x y)))

(defun make-rectangular-from-mag-ang (r a) ; nolint:unused-function
  (attach-tag 'rectangular (list (* r (cos a))
                                 (* r (sin a)))))

(defun polar-real-part (z)
  (* (polar-magnitude z) (cos (polar-angle z))))

(defun polar-imag-part (z)
  (* (polar-magnitude z) (sin (polar-angle z))))

(defun polar-magnitude (z)
  (first z))

(defun polar-angle (z)
  (second z))

(defun make-polar-from-real-imag (x y) ; nolint:shadowing
  (if (and (= 0 x)
           (= 0 y))
    (attach-tag 'polar (list 0 0))
    (attach-tag 'polar (list (sqrt (+ (square x) (square y)))
                             (atan y x)))))

(defun make-polar-from-mag-ang (r a)
  (if (= 0 r)
    (attach-tag 'polar (list 0 0))
    (attach-tag 'polar (list r a))))

(export 'make-complex-from-real-imag)
(defun make-complex-from-real-imag (x y) ; nolint:shadowing
  (make-rectangular-from-real-imag x y))

(export 'make-complex-from-mag-ang)
(defun make-complex-from-mag-ang (r a)
  (make-polar-from-real-imag r a))

(export 'complex-real-part)
(defun complex-real-part (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-real-part (contents z)))
    ((equal? 'polar (type-tag z)) (polar-real-part (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-imag-part)
(defun complex-imag-part (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-imag-part (contents z)))
    ((equal? 'polar (type-tag z)) (polar-imag-part (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-magnitude)
(defun complex-magnitude (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-magnitude (contents z)))
    ((equal? 'polar (type-tag z)) (polar-magnitude (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-angle)
(defun complex-angle (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-angle (contents z)))
    ((equal? 'polar (type-tag z)) (polar-angle (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-add)
(defun complex-add (z1 z2) ; nolint:unused-function
  (make-complex-from-real-imag (+ (complex-real-part z1)
                                  (complex-real-part z2))
                               (+ (complex-imag-part z1)
                                  (complex-imag-part z2))))

(export 'complex-sub)
(defun complex-sub (z1 z2) ; nolint:unused-function
  (make-complex-from-real-imag (- (complex-real-part z1)
                                  (complex-real-part z2))
                               (- (complex-imag-part z1)
                                  (complex-imag-part z2))))

(export 'complex-mul)
(defun complex-mul (z1 z2) ; nolint:unused-function
  (make-complex-from-mag-ang (* (complex-magnitude z1)
                                (complex-magnitude z2))
                             (+ (complex-angle z1)
                                (complex-angle z2))))

(export 'complex-div)
(defun complex-div (z1 z2) ; nolint:unused-function
  (make-complex-from-mag-ang (/ (complex-magnitude z1)
                                (complex-magnitude z2))
                             (- (complex-angle z1)
                                (complex-angle z2))))

(assert= 0 (complex-angle (make-rectangular-from-real-imag 0 0)))
(assert= 0 (complex-angle (make-rectangular-from-real-imag 1 0)))
(assert= (/ math:pi 2) (complex-angle (make-rectangular-from-real-imag 0 1)))
(assert= (/ math:pi 4) (complex-angle (make-rectangular-from-real-imag 1 1)))
(assert= 1 (complex-magnitude (make-rectangular-from-real-imag 1 0)))
(assert= 1 (complex-magnitude (make-rectangular-from-real-imag 0 1)))
(assert= (sqrt 2) (complex-magnitude (make-rectangular-from-real-imag 1 1)))

(assert= 0 (complex-angle (make-polar-from-mag-ang 1 0)))
(assert= 0 (complex-angle (make-polar-from-mag-ang 0 (/ math:pi 2))))  ; angle ignored
(assert= (/ math:pi 2) (complex-angle (make-polar-from-mag-ang 1 (/ math:pi 2))))
(assert= 0 (complex-angle (make-polar-from-real-imag 1 0)))
(assert= (/ math:pi 2) (complex-angle (make-polar-from-real-imag 0 1)))
(assert= (/ math:pi 4) (complex-angle (make-polar-from-real-imag 1 1)))
