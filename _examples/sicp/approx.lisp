; Copyright © 2018 The ELPS authors

(in-package 'sicp/approx)

(load-file "stream.lisp")

(use-package 'sicp/stream)
(use-package 'testing)

(defun sqrt-improve (guess x)
  ; average guess and x/guess
  (/ (+ guess (/ x guess)) 2))

(defun sqrt-stream (x)
  ; let initializers cannot capture their own binding. Use an explicitly
  ; recursive function for the delayed tail (docs/lang.md: let vs let*).
  (labels ([guesses (guess)
             (stream-cons guess (guesses (sqrt-improve guess x)))])
    (guesses 1.0)))

(assert-equal '(1.0 1.5 1.4166666666666665)
              (stream-collect (stream-take (sqrt-stream 2) 3)))
(assert-equal '(1.0 2.5)
              (stream-collect (stream-take (sqrt-stream 4) 2)))

(debug-print '(sqrt-stream 2))
(stream-debug (stream-take (sqrt-stream 2) 7))

(defun pi-summands (n)
  (stream-cons (/ 1.0 n)
               (stream-map '- (pi-summands (+ n 2)))))

(defun partial-sums (s)
  (if (stream-null? s)
    the-empty-stream
    (stream-cons (stream-car s)
                 (stream-map '+
                             (partial-sums (stream-cdr s))
                             (stream-repeat (stream-car s))))))

(set 'pi-stream (stream-map #^(* 4 %) (partial-sums (pi-summands 1))))

(debug-print 'pi-stream)
(stream-debug (stream-take pi-stream 7))

(defun square (x) (* x x))
(defun euler-transform (s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(debug-print '(euler-transform pi-stream))
(stream-debug (stream-take (euler-transform pi-stream) 7))

(defun make-tableau (transform s)
  (stream-cons s (make-tableau transform (funcall transform s))))

(defun accelerated-sequence (transform s)
  (stream-map 'stream-car (make-tableau transform s)))

(debug-print '(accelerated-sequence 'euler-transform pi-stream))
(stream-debug (stream-take (accelerated-sequence 'euler-transform pi-stream) 8))
