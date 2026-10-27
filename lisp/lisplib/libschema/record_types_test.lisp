(use-package 'testing)

; Record types (luthersystems/elps#895): s:make-record, s:method,
; s:defmethod, s:set! and s:fields.

; condition-of returns the condition the thunk raises, or 'none.
(defun condition-of (thunk)
  (handler-bind ((condition (lambda (c &rest _) c)))
    (funcall thunk)
    'none))

(set 'order-status (s:make-validator "order-status" s:string (s:in "open" "paid")))

(set 'order
     (s:make-record "order"
                    (s:has-key "id" s:string)
                    (s:has-key "lines" (s:make-validator "lines" s:sorted-map (s:map-of s:string s:int)))
                    (s:has-key "status" order-status)
                    (s:may-have-key "note" s:any)
                    (s:method add-line (sku qty)
                              (s:set! this 'lines (assoc (this 'lines) sku (+ qty (get-default (this 'lines) sku 0)))))
                    (s:method qty ()
                              (foldl (lambda (n sku) (+ n (get (this 'lines) sku))) 0 (keys (this 'lines))))
                    (s:method pay ()
                              (s:set! this 'status "paid"))
                    (s:method self () this)))

(defun new-order ()
  (order :id "o1" :lines (sorted-map) :status "open"))

(test "construct-and-call"
  (let ([o (new-order)])
    (assert-equal "o1" (o 'id))
    (assert-nil (o 'note))
    (assert-equal 0 (o 'qty))
    (o 'add-line "a" 2)
    (o 'add-line "a" 3)
    (o 'add-line "b" 1)
    (assert-equal 6 (o 'qty))
    (o 'pay)
    (assert-equal "paid" (o 'status))
    (assert (s:record? o))
    (assert-not (s:record? (sorted-map)))
    (assert-nil (s:validate order o))))

(test "construct-from-map"
  (let* ([row (sorted-map "id" "o2" "lines" (sorted-map "x" 1) "status" "open")]
         [o (order row)])
    (assert-equal 1 (o 'qty))
    ; The object owns a copy of the map it was given.
    (assoc! row "id" "changed")
    (assert-equal "o2" (o 'id))))

(test "fields-is-a-copy"
  (let* ([o (new-order)]
         [f (s:fields o)])
    (assert-equal "o1" (get f "id"))
    (assoc! f "id" "changed")
    (assert-equal "o1" (o 'id))
    ; A round trip through the fields rebuilds an equal object.
    (assert-equal "o1" ((order (s:fields o)) 'id))))

(test "this-shares-state"
  (let* ([o (new-order)]
         [s (o 'self)])
    (s 'add-line "z" 4)
    (assert-equal 4 (o 'qty))))

(test "construct-errors"
  (assert-equal 'wrong-type (condition-of (lambda () (order :id 7 :lines (sorted-map) :status "open"))))
  (assert-equal 'failed-constraint (condition-of (lambda () (order :lines (sorted-map) :status "open"))))
  (assert-equal 'bad-arguments (condition-of (lambda () (order :id "o" :lines (sorted-map) :status "open" :bogus 1))))
  (assert-equal 'bad-arguments (condition-of (lambda () (order :id "o" :lines))))
  (assert-equal 'bad-arguments (condition-of (lambda () (order "id" "o")))))

(test "set-checks-the-field"
  (let ([o (new-order)])
    (assert-equal 'wrong-type (condition-of (lambda () (s:set! o 'status "lost")))) ; nolint:record-set-outside-method
    (assert-equal "open" (o 'status))
    (assert-equal 'bad-arguments (condition-of (lambda () (s:set! o 'bogus 1)))) ; nolint:record-set-outside-method
    (assert-equal 'bad-arguments (condition-of (lambda () (s:set! (sorted-map) 'id "x")))))) ; nolint:record-set-outside-method

(test "call-errors"
  (let ([o (new-order)])
    (assert-equal 'unknown-operation (condition-of (lambda () (o 'add-lin "a" 1))))
    (assert-equal 'unknown-operation (condition-of (lambda () (o "qty"))))
    (assert-equal 'bad-arguments (condition-of (lambda () (o 'id "extra"))))))

(test "validator-use"
  (set 'cart (s:make-validator "cart" s:sorted-map (s:has-key "order" order)))
  (assert-nil (s:validate cart (sorted-map "order" (new-order))))
  (assert-equal 'wrong-type (condition-of (lambda () (s:validate cart (sorted-map "order" (sorted-map))))))
  (assert-equal 'wrong-type (condition-of (lambda () (s:validate order (sorted-map))))))

(set 'counter
     (s:make-record "counter"
                    (s:has-key "n" s:int)
                    :check (lambda (c) (when (< (c 'n) 0) (error 'negative-count (c 'n))))))

(s:defmethod counter inc () (s:set! this 'n (+ 1 (this 'n))))

(test "check-and-defmethod"
  (let ([c (counter :n 0)])
    (c 'inc)
    (c 'inc)
    (assert-equal 2 (c 'n)))
  (assert-equal 'negative-count (condition-of (lambda () (counter :n -1))))
  (assert-equal 'bad-arguments (condition-of (lambda () (s:defmethod counter inc () 0))))
  (assert-equal 'bad-arguments (condition-of (lambda () (s:defmethod counter n () 0))))
  (s:defmethod counter inc () :replace true (s:set! this 'n (+ 10 (this 'n))))
  (let ([c (counter :n 0)])
    (c 'inc)
    (assert-equal 10 (c 'n))))

; A type built inside a closure: the closure's local is a private helper.
(set 'pricer
     (labels ([cents (dollars) (* 100 dollars)])
       (s:make-record "pricer"
                      (s:has-key "dollars" s:int)
                      (s:method in-cents () (cents (this 'dollars))))))

(test "closure-helpers"
  (assert-equal 300 ((pricer :dollars 3) 'in-cents)))

(test "definition-errors"
  (assert-equal 'bad-arguments (condition-of (lambda () (s:make-record "x" (s:has-key "a" s:int) (s:has-key "a" s:int))))) ; nolint:record-type-binding
  (assert-equal 'bad-arguments (condition-of (lambda () (s:make-record "x" (s:has-key "a" s:int) (s:method a () 1))))) ; nolint:record-type-binding
  (assert-equal 'bad-arguments (condition-of (lambda () (s:make-record "x" (s:method m () 1) (s:method m () 2))))) ; nolint:record-type-binding
  (assert-equal 'bad-arguments (condition-of (lambda () (s:make-record "x" s:int)))) ; nolint:record-type-binding
  (assert-equal 'bad-arguments (condition-of (lambda () (s:make-record "x" (s:method m (this) 1))))) ; nolint:record-type-binding,undefined-symbol
  (assert-equal 'bad-arguments (condition-of (lambda () (s:defmethod (sorted-map) m () 1)))))

(set 'range
     (s:make-record "range"
                    (s:has-key "lo" s:int)
                    (s:has-key "hi" s:int)
                    (s:method set-lo (n) (s:set! this 'lo n))
                    (s:method shift (n)
                              (s:set! this 'lo (+ n (this 'lo)))
                              (s:set! this 'hi (+ n (this 'hi))))
                    (s:method half-shift (n)
                              (s:set! this 'lo (+ n (this 'lo)))
                              (error 'stopped n))
                    :check (lambda (r) (when (> (r 'lo) (r 'hi)) (error 'bad-range (r 'lo))))))

(test "check-runs-after-each-method"
  (let ([r (range :lo 1 :hi 5)])
    (r 'shift 10)
    (assert-equal 11 (r 'lo))
    (assert-equal 15 (r 'hi))
    ; The method breaks :check: the call fails and the fields stay.
    (assert-equal 'bad-range (condition-of (lambda () (r 'set-lo 99))))
    (assert-equal 11 (r 'lo))
    ; The method fails part way: the fields stay.
    (assert-equal 'stopped (condition-of (lambda () (r 'half-shift 2))))
    (assert-equal 11 (r 'lo))))

(test "check-condition-keeps-its-data"
  (let ([r (range :lo 1 :hi 5)])
    (handler-bind ([bad-range (lambda (c &rest data) (assert-equal 99 (car data)))])
      (r 'set-lo 99))))
