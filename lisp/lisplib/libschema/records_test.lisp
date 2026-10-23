(use-package 'testing)

; Records: a nested order built from libschema validators, with the
; additions for lists, sequences, nil, map-of and describe.

(defun wrong-type (thunk)
  (handler-bind (('wrong-type (lambda (&rest _e) "WRONG"))
                 ('failed-constraint (lambda (&rest _e) "FAILED")))
    (funcall thunk)))

(test "list-sequence-nil"
  (set 'ls (s:make-validator "ls" s:list (s:of s:int)))
  (assert-nil (s:validate ls '(1 2 3)))
  (assert-nil (s:validate ls ()))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate ls (vector 1 2)))))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate ls '(1 "x")))))
  (set 'sq (s:make-validator "sq" s:sequence (s:of s:string)))
  (assert-nil (s:validate sq '("a" "b")))
  (assert-nil (s:validate sq (vector "a" "b")))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate sq "ab"))))
  (set 'opt (s:make-validator "opt" s:sorted-map (s:has-key "note" s:string s:nil)))
  (assert-nil (s:validate opt (sorted-map "note" "hi")))
  (assert-nil (s:validate opt (sorted-map "note" ())))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate opt (sorted-map "note" 3))))))

(test "map-of"
  (set 'prices (s:make-validator "prices" s:sorted-map (s:map-of s:string s:number)))
  (assert-nil (s:validate prices (sorted-map "a" 1 "b" 2.5)))
  (assert-nil (s:validate prices (sorted-map)))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate prices (sorted-map "a" "x")))))
  (assert-equal "WRONG" (wrong-type (lambda () (s:validate prices (sorted-map 1 2))))))

(test "nested-order"
  (set 'tier (s:make-validator "tier" s:string (s:in "gold" "silver" "basic")))
  (set 'customer (s:make-validator "customer" s:sorted-map
                                   (s:no-other-keys
                                     (s:has-key "id" s:string)
                                     (s:has-key "tier" tier))))
  (set 'line (s:make-validator "line" s:sorted-map
                               (s:no-other-keys
                                 (s:has-key "sku" s:string)
                                 (s:has-key "qty" (s:make-validator "qty" s:int (s:positive)))
                                 (s:has-key "price" s:number))))
  (set 'order (s:make-validator "order" s:sorted-map
                                (s:no-other-keys
                                  (s:has-key "id" s:string)
                                  (s:has-key "customer" customer)
                                  (s:has-key "lines" (s:make-validator "lines" s:sequence (s:of line)))
                                  (s:may-have-key "note" s:string s:nil))))
  (set 'good (sorted-map "id" "o1"
                         "customer" (sorted-map "id" "c1" "tier" "gold")
                         "lines" (list (sorted-map "sku" "a" "qty" 2 "price" 5))))
  (assert-nil (s:validate order good))
  (assert-equal "WRONG"
                (wrong-type (lambda () (s:validate order (assoc good "customer" (sorted-map "id" "c1" "tier" "tin"))))))
  (assert-equal "WRONG"
                (wrong-type (lambda () (s:validate order (assoc good "lines" (list (sorted-map "sku" "a" "qty" 0 "price" 5))))))))

(test "describe"
  (set 'tier (s:make-validator "tier" s:string (s:in "gold" "silver")))
  (assert-equal (sorted-map "op" "type" "name" "tier" "type" "string"
                            "children" (vector (sorted-map "op" "in" "values" (vector "gold" "silver"))))
                (s:describe tier))
  (set 'customer (s:make-validator "customer" s:sorted-map
                                   (s:no-other-keys
                                     (s:has-key "id" s:string)
                                     (s:may-have-key "tier" tier s:nil))))
  (assert-equal (sorted-map
                  "op" "type" "name" "customer" "type" "sorted-map"
                  "children" (vector
                               (sorted-map "op" "no-other-keys"
                                           "children" (vector
                                                        (sorted-map "op" "has-key" "key" "id"
                                                                    "children" (vector (sorted-map "op" "type" "type" "string")))
                                                        (sorted-map "op" "may-have-key" "key" "tier"
                                                                    "children" (vector (s:describe tier)
                                                                                       (sorted-map "op" "type" "type" "nil")))))))
                (s:describe customer))
  (assert-equal (sorted-map "op" "map-of"
                            "children" (vector (sorted-map "op" "type" "type" "string")
                                               (sorted-map "op" "type" "type" "number")))
                (s:describe (s:map-of s:string s:number)))
  (assert-equal (sorted-map "op" "gt" "values" (vector 3)) (s:describe (s:gt 3))))
