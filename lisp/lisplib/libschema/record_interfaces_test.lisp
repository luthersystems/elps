(use-package 'testing)

; Interfaces (luthersystems/elps#900): s:make-interface checks that a
; record object has a set of methods, as a Go interface does.

(set 'payable (s:make-interface "payable" (pay (amount)) (cancel ())))

(set 'invoice
     (s:make-record "invoice"
                    (s:has-key "total" s:int)
                    (s:method pay (o amount) (s:set! o 'total (- (o 'total) amount)))
                    (s:method cancel (_o) ())))

(set 'refund
     (s:make-record "refund"
                    (s:has-key "n" s:int)
                    (s:method pay (_o &optional _amount _note) ())
                    (s:method cancel (_o &rest _why) ())))

(set 'note
     (s:make-record "note"
                    (s:has-key "text" s:string)
                    (s:method pay (_o) ())
                    (s:method cancel (_o) ())))

(set 'draft
     (s:make-record "draft"
                    (s:has-key "text" s:string)
                    (s:method pay (_o amount) amount)))

(defun failure (f)
  (handler-bind ((condition (lambda (c &rest _) c)))
    (funcall f)))

(test "exact-signature"
  (assert-nil (s:validate payable (invoice :total 5))))

(test "wider-signature"
  ; a method that accepts every call the interface allows matches
  (assert-nil (s:validate payable (refund :n 1))))

(test "wrong-arity"
  (assert-equal 'wrong-type (failure (lambda () (s:validate payable (note :text "x"))))))

(test "missing-method"
  (assert-equal 'wrong-type (failure (lambda () (s:validate payable (draft :text "x"))))))

(test "added-method-counts"
  (s:defmethod draft cancel (_o) ())
  (assert-nil (s:validate payable (draft :text "x"))))

(test "not-a-record"
  (assert-equal 'wrong-type (failure (lambda () (s:validate payable 5))))
  (assert-equal 'wrong-type (failure (lambda () (s:validate payable (lambda (x) x))))))

(set 'ledger
     (s:make-record "ledger"
                    (s:has-key "payer" payable)
                    (s:may-have-key "others" (s:of payable))))

(test "interface-field"
  (let ([l (ledger :payer (invoice :total 5) :others (vector (refund :n 1)))])
    (assert-equal 5 ((l 'payer) 'total)))
  (assert-equal 'wrong-type (failure (lambda () (ledger :payer (note :text "x"))))))

(test "definition-errors"
  (assert-equal 'bad-arguments (failure (lambda () (s:make-interface 'payable (pay ())))))
  (assert-equal 'bad-arguments (failure (lambda () (s:make-interface "x" pay))))
  (assert-equal 'bad-arguments (failure (lambda () (s:make-interface "x" (pay amount)))))
  (assert-equal 'bad-arguments (failure (lambda () (s:make-interface "x" (pay ()) (pay ()))))))
