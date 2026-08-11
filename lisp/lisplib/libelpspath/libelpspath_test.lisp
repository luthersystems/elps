(use-package 'elpspath)
(use-package 'testing)

;;; ---- v2: positional-arg path operations ----

(test-let "v2 ? simple key"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? val "hello") "world")))

(test-let "v2 ? nested key"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (assert (string= (elpspath:? val "a" "b") "world")))

(test-let "v2 ? array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (assert (string= (elpspath:? val "items" 0) "a")))

(test-let "v2 ? negative index"
  ((val (vector "a" "b" "c")))
  (assert (string= (elpspath:? val -1) "c")))

(test-let "v2 ? iterator"
  ((val (vector (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert-equal (vector 1 2) (elpspath:? val '* "a")))

(test-let "v2 ? range"
  ((val (vector "a" "b" "c" "d")))
  (assert-equal (vector "b" "c") (elpspath:? val '(range 1 3))))

(test-let "v2 ? root"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? (elpspath:? val) "hello") "world")))

(test-let "v2 ?set! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?set! val "hello" "42")
  (assert (string= (elpspath:? val "hello") "42")))

(test-let "v2 ?set! nested"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (elpspath:?set! val "a" "b" 23)
  (assert (= (elpspath:? val "a" "b") 23)))

(test-let "v2 ?set! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?set! val "items" 0 "x")
  (assert (string= (elpspath:? val "items" 0) "x")))

(test-let* "v2 ?set copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?set val "hello" "42")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (string= (elpspath:? new-val "hello") "42")))

(test-let* "v2 ?set nested copy"
  ((val (sorted-map "a" (sorted-map "b" "world")))
   (new-val (elpspath:?set val "a" "b" 23)))
  (assert (string= (elpspath:? val "a" "b") "world"))
  (assert (= (elpspath:? new-val "a" "b") 23)))

(test-let "v2 ?del! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?del! val "hello")
  (assert (empty? val)))

(test-let "v2 ?del! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?del! val "items" 1)
  (assert-equal (vector "a" "c") (elpspath:? val "items")))

(test-let* "v2 ?del copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?del val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (empty? new-val)))

(test-let "v2 ?nil! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?nil! val "hello")
  (assert-equal () (elpspath:? val "hello")))

(test-let "v2 ?nil! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?nil! val "items" 1)
  (assert-equal (vector "a" () "c") (elpspath:? val "items")))

(test-let* "v2 ?nil copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?nil val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert-equal () (elpspath:? new-val "hello")))

(test-let "v2 ? list support"
  ((val (sorted-map "hello" (list "world")))
   (nested-val (list (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert (string= (elpspath:? val "hello" 0) "world"))
  (assert-equal (list 1 2) (elpspath:? nested-val '* "a")))
