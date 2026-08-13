(use-package 'elpspath)
(use-package 'testing)

;;; ---- positional-arg path operations ----

(test-let "? simple key"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? val "hello") "world")))

(test-let "? nested key"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (assert (string= (elpspath:? val "a" "b") "world")))

(test-let "? array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (assert (string= (elpspath:? val "items" 0) "a")))

(test-let "? negative index"
  ((val (vector "a" "b" "c")))
  (assert (string= (elpspath:? val -1) "c")))

(test-let "? iterator"
  ((val (vector (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert-equal (vector 1 2) (elpspath:? val '* "a")))

(test-let "? range"
  ((val (vector "a" "b" "c" "d")))
  (assert-equal (vector "b" "c") (elpspath:? val '(range 1 3))))

(test-let "? root"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? (elpspath:? val) "hello") "world")))

(test-let "?set! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?set! val "hello" "42")
  (assert (string= (elpspath:? val "hello") "42")))

(test-let "?set! nested"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (elpspath:?set! val "a" "b" 23)
  (assert (= (elpspath:? val "a" "b") 23)))

(test-let "?set! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?set! val "items" 0 "x")
  (assert (string= (elpspath:? val "items" 0) "x")))

(test-let* "?set copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?set val "hello" "42")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (string= (elpspath:? new-val "hello") "42")))

(test-let* "?set nested copy"
  ((val (sorted-map "a" (sorted-map "b" "world")))
   (new-val (elpspath:?set val "a" "b" 23)))
  (assert (string= (elpspath:? val "a" "b") "world"))
  (assert (= (elpspath:? new-val "a" "b") 23)))

(test-let "?del! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?del! val "hello")
  (assert (empty? val)))

(test-let "?del! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?del! val "items" 1)
  (assert-equal (vector "a" "c") (elpspath:? val "items")))

(test-let* "?del copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?del val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (empty? new-val)))

(test-let "?nil! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?nil! val "hello")
  (assert-equal () (elpspath:? val "hello")))

(test-let "?nil! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?nil! val "items" 1)
  (assert-equal (vector "a" () "c") (elpspath:? val "items")))

(test-let* "?nil copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?nil val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert-equal () (elpspath:? new-val "hello")))

(test-let "? list support"
  ((val (sorted-map "hello" (list "world")))
   (nested-val (list (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert (string= (elpspath:? val "hello" 0) "world"))
  (assert-equal (list 1 2) (elpspath:? nested-val '* "a")))

; The package's own documentation example (issue #395).  Before copyMap was
; made deep, the write through the redacted copy reached the patient record
; it was supposed to leave alone.
(test-let* "?nil copy is deep"
  ((patient (sorted-map "ssn" "123" "address" (sorted-map "city" "London")))
   (redacted (elpspath:?nil patient "ssn")))
  (elpspath:?set! redacted "address" "city" "REDACTED")
  (assert (string= "REDACTED" (elpspath:? redacted "address" "city")))
  (assert (string= "London" (elpspath:? patient "address" "city"))))

; The structural variant: ?del! through a ?set copy must not restructure the
; source's array or rewrite its dims.
(test-let* "?set copy is deep"
  ((src (sorted-map "arr" (vector 1 2 3)))
   (cp (elpspath:?set src "tag" "x")))
  (elpspath:?del! cp "arr" 0)
  (assert-equal (vector 2 3) (elpspath:? cp "arr"))
  (assert-equal (vector 1 2 3) (elpspath:? src "arr")))

; A copy must not demote a quoted list to an s-expression: the quote flag is
; part of the value, and an unquoted LSExpr is an expression rather than a
; list.
(test-let* "copy preserves quoting"
  ((src (sorted-map "l" '(1 2 3)))
   (cp (elpspath:?set src "k" "v")))
  (assert-equal '(1 2 3) (elpspath:? cp "l"))
  (assert-equal '(99 2 3) (elpspath:?set '(1 2 3) 0 99)))
