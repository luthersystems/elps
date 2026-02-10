; Copyright © 2018 The ELPS authors

(load-file "sicp.lisp")
(load-file "complex.lisp")

(use-package 'sicp)
(use-package 'sicp/complex)
(use-package 'math)
(use-package 'testing)

;  Don't blow the dispatch table away if we reload the file
(set 'dispatch-table (or (ignore-errors dispatch-table) (sorted-map)))

; BUG:  Function dispatch-put is not intended to handle differing type-arity
; among implementations for an op.
(defun dispatch-put (op-symbol operand-types operator)
  (let ([op-table (or (get dispatch-table op-symbol) (sorted-map))])
    (assoc! dispatch-table op-symbol op-table)
    (labels ([dig (table operand-types value) ; nolint:shadowing
              (cond
                ((nil? operand-types)
                 (error 'nil-type-parameters "type parameters are nil"))
                ((nil? (rest operand-types))
                 (assoc! table (first operand-types) value))
                (:else
                  (let* ([operand-type (first operand-types)]
                         [sub-types (rest operand-types)]
                         [sub-table (or (get table operand-type) (sorted-map))])
                    (assoc! table operand-type sub-table)
                    (dig sub-table sub-types value))))])
      (dig op-table operand-types operator)
      ())))

; BUG:  Function dispatch-get is not intended to handle differing type-arity
; among implementations for an op.
(defun dispatch-get (op-symbol operand-types)
  (let ([op-table (get dispatch-table op-symbol)])
    (labels ([dig (table operand-types) ; nolint:shadowing
              (cond
                ((nil? operand-types)
                 table)
                ((not (sorted-map? table)) ())
                ((symbol? operand-types) (get table operand-types))
                ((not (key? table (first operand-types)))
                 ())
                (:else
                  (let* ([operand-type (first operand-types)]
                         [sub-types (rest operand-types)]
                         [sub-table (get table operand-type)])
                    (dig sub-table sub-types))))])
      (if (nil? op-table)
        ()
        (dig op-table operand-types)))))

(defun dispatch-call (op-symbol operand-types &rest operands)
  (let ([fun (dispatch-get op-symbol operand-types)])
    (if fun
      (apply fun operands)
      (error 'invalid-operands
             "no operator implementation for operands"
             (list op-symbol operand-types)))))

(defun install-rectangular-package ()
  (labels ([real-part (z) (first z)] ; nolint:shadowing
           [imag-part (z) (second z)] ; nolint:shadowing
           [make-from-real-imag (x y) (list x y)] ; nolint:shadowing
           [magnitude (z) ; nolint:shadowing
            (sqrt (+ (square (real-part z))
                     (square (imag-part z))))]
           [angle (z) ; nolint:shadowing
            (let ([x (real-part z)] ; nolint
                  [y (imag-part z)]) ; nolint
              (atan (imag-part z)
                    (real-part z)))]
           [make-from-mag-ang (r a) ; nolint:shadowing
            (if (= 0 r)
              (list 0 0)
              (list (* r (cos a)) (* r (sin a))))]
           [tag (x) (attach-tag 'rectangular x)])
    (dispatch-put 'real-part '(rectangular) real-part)
    (dispatch-put 'imag-part '(rectangular) imag-part)
    (dispatch-put 'magnitude '(rectangular) magnitude)
    (dispatch-put 'angle '(rectangular) angle)
    (dispatch-put 'make-from-real-imag '(rectangular)
                  (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(rectangular)
                  (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun install-polar-package ()
  (labels ([magnitude (z) (first z)] ; nolint:shadowing
           [angle (z) (second z)] ; nolint:shadowing
           [make-from-mag-ang (r a) (list r a)] ; nolint:shadowing
           [real-part (z) ; nolint:shadowing
            (* (magnitude z) (cos (angle z)))]
           [imag-part (z) ; nolint:shadowing
            (* (magnitude z) (sin (angle z)))]
           [make-from-real-imag (x y) ; nolint:shadowing
            (list (sqrt (+ (square (real-part z)) ; nolint:undefined-symbol
                           (square (imag-part z)))) ; nolint:undefined-symbol
                  (atan y x))]
           [tag (x) (attach-tag 'polar x)])
    (dispatch-put 'real-part '(polar) real-part)
    (dispatch-put 'imag-part '(polar) imag-part)
    (dispatch-put 'magnitude '(polar) magnitude)
    (dispatch-put 'angle '(polar) angle)
    (dispatch-put 'make-from-real-imag '(polar)
                  (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(polar)
                  (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let* ([type-tags (map 'list 'type-tag args)]
         [proc (dispatch-get op type-tags)])
    (cond
      ((nil? proc) (error 'invalid-method "no operation for types" (list op type-tags)))
      ((sorted-map? proc) (error 'invalid-types "invalid type-parameters for operation" (list op type-tags)))
      (:else (apply proc (map 'list 'contents args))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (dispatch-call 'make-from-real-imag '(rectangular) x y))

(defun make-from-mag-ang (r a)
  (dispatch-call 'make-from-mag-ang '(polar) r a))

(trace (install-rectangular-package))
(trace (install-polar-package))

(assert= 0 (angle (make-from-real-imag 0 0)))
(assert= 0 (angle (make-from-real-imag 1 0)))
(assert= (/ math:pi 2) (angle (make-from-real-imag 0 1)))
(assert= (/ math:pi 4) (angle (make-from-real-imag 1 1)))
(assert= 1 (magnitude (make-from-real-imag 1 0)))
(assert= 1 (magnitude (make-from-real-imag 0 1)))
(assert= (sqrt 2) (magnitude (make-from-real-imag 1 1)))

(assert= 0 (angle (make-from-mag-ang 1 0)))
(assert= (/ math:pi 2) (angle (make-from-mag-ang 1 (/ math:pi 2))))
(assert= 1 (real-part (make-from-mag-ang 1 0)))
(assert= 0 (imag-part (make-from-mag-ang 0 1)))
(assert= -1 (imag-part (make-from-mag-ang 1 (/ (* 3 math:pi) 2))))
(assert= -1 (real-part (make-from-mag-ang 1 math:pi)))

(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

(defun install-scheme-number-package ()
  (labels ([tag (x) (attach-tag 'scheme-number x)])
    (dispatch-put 'add '(scheme-number scheme-number)
                  (lambda (x y) (tag (+ x y))))
    (dispatch-put 'sub '(scheme-number scheme-number)
                  (lambda (x y) (tag (- x y))))
    (dispatch-put 'mul '(scheme-number scheme-number)
                  (lambda (x y) (tag (* x y))))
    (dispatch-put 'div '(scheme-number scheme-number)
                  (lambda (x y) (tag (/ x y))))
    (dispatch-put 'make '(scheme-number)
                  (lambda (x) (tag x)))
    'done))

(trace (install-scheme-number-package))

(defun make-scheme-number (x)
  (dispatch-call 'make '(scheme-number) x))

(assert= 2 (contents (add (make-scheme-number 1) (make-scheme-number 1))))
(assert= 5 (contents (sub (make-scheme-number 2) (make-scheme-number -3))))
(assert= 6 (contents (mul (make-scheme-number 2) (make-scheme-number 3))))
(assert= 0.5 (contents (div (make-scheme-number 1) (make-scheme-number 2))))

(defun install-rational-package ()
  (labels ([numer (x) (first x)] ; nolint:shadowing
           [denom (x) (second x)] ; nolint:shadowing
           [make-rat (p q)
            (let ([g (gcd p q)])
              (list (/ p g) (/ q g)))]
           [add-rat (x y)
            (make-rat (+ (* (numer x) (denom y))
                         (* (denom x) (numer y)))
                      (* (denom x) (denom y)))]
           [sub-rat (x y)
            (make-rat (- (* (numer x) (denom y))
                         (* (denom x) (numer y)))
                      (* (denom x) (denom y)))]
           [mul-rat (x y)
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y)))]
           [div-rat (x y)
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y)))]
           [tag (x) (attach-tag 'rational x)])
    (dispatch-put 'make '(rational)
                  (lambda (p q) (tag (make-rat p q))))
    (dispatch-put 'numer '(rational) numer)
    (dispatch-put 'denom '(rational) denom)
    (dispatch-put 'add '(rational rational)
                  (lambda (x y) (tag (add-rat x y))))
    (dispatch-put 'sub '(rational rational)
                  (lambda (x y) (tag (sub-rat x y))))
    (dispatch-put 'mul '(rational rational)
                  (lambda (x y) (tag (mul-rat x y))))
    (dispatch-put 'div '(rational rational)
                  (lambda (x y) (tag (div-rat x y))))
    'dane))

(trace (install-rational-package))

(defun make-rational (p q)
  (dispatch-call 'make '(rational) p q))

(defun numer (r) (apply-generic 'numer r))
(defun denom (r) (apply-generic 'denom r))

(assert= 0 (numer (make-rational 0 10)))
(assert= 1 (denom (make-rational 0 10)))
(assert= 0 (numer (mul (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 0 (numer (div (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 1 (numer (add (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 2 (denom (add (make-rational 0 10)
                       (make-rational 1 2))))

(defun install-complex-package ()
  (labels ([make-from-real-imag (x y) ; nolint:shadowing
            (dispatch-call 'make-from-real-imag 'rectangular x y)]
           [make-from-mag-ang (r a) ; nolint:shadowing
            (dispatch-call 'make-from-mag-ang 'polar r a)]
           [add-complex (z1 z2)
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2)))]
           [sub-complex (z1 z2)
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2)))]
           [mul-complex (z1 z2)
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2)))]
           [div-complex (z1 z2)
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2)))]
           [tag (z) (attach-tag 'complex z)])
    (dispatch-put 'add '(complex complex)
                  (lambda (z1 z2) (tag (add-complex z1 z2))))
    (dispatch-put 'sub '(complex complex)
                  (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (dispatch-put 'mul '(complex complex)
                  (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (dispatch-put 'div '(complex complex)
                  (lambda (z1 z2) (tag (div-complex z1 z2))))
    (dispatch-put 'make-from-real-imag '(complex)
                  (lambda (x y) (make-from-real-imag x y)))
    (dispatch-put 'make-from-mag-ang '(complex)
                  (lambda (r a) (make-from-mag-ang r a)))
    'done))

(defun make-complex-real-imag (x y) (dispatch-call 'make-from-real-imag 'complex x y)) ; nolint
(defun make-complex-mag-ang (r a) (dispatch-call 'make-from-mag-ang 'complex r a)) ; nolint:unused-function

(trace (install-complex-package))

; the same tests from above working with 'rectangular and 'polar types now
; using 'complex types and working with multiple layers of abstraction.
(assert= 0 (angle (make-complex-from-real-imag 0 0)))
(assert= 0 (angle (make-complex-from-real-imag 1 0)))
(assert= (/ math:pi 2) (angle (make-complex-from-real-imag 0 1)))
(assert= (/ math:pi 4) (angle (make-complex-from-real-imag 1 1)))
(assert= 1 (magnitude (make-complex-from-real-imag 1 0)))
(assert= 1 (magnitude (make-complex-from-real-imag 0 1)))
(assert= (sqrt 2) (magnitude (make-complex-from-real-imag 1 1)))
