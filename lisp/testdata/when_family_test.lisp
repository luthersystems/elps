(use-package 'testing)

(test "when evaluates body in order"
  (let ([log '()])
    (assert-equal 'b (when true (set! log (cons 'a log)) 'b))
    (assert-equal '(a) log)
    (assert-nil (when () (set! log '())))
    (assert-equal '(a) log)))

(test "unless is the inverse of when"
  (assert-equal 3 (unless () 1 2 3))
  (assert-nil (unless 'x 1)))

(test "default evaluates its value once"
  (let ([n 0])
    (assert-equal 1 (default (progn (set! n (+ n 1)) n) 'unused))
    (assert-equal 1 n)
    (assert-equal 'fallback (default () 'fallback))
    (assert-equal false (default false 'fallback))))

(test "while loops until the condition is falsey"
  (let ([i 0] [sum 0])
    (assert-nil (while (< i 10) (set! sum (+ sum i)) (set! i (+ i 1))))
    (assert-equal 45 sum)))

(test "when is in tail position"
  (labels ([count-down (n) (when (> n 0) (count-down (- n 1)))])
    (assert-nil (count-down 100000))))
