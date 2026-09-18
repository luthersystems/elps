; Copyright © 2026 The ELPS authors

(use-package 'testing)

(test "null-top-level"
  (assert-string= "null" (json:dump-string json:null)))

(test "null-map-value"
  (assert-string= """{"k":null}"""
                  (json:dump-string (sorted-map "k" json:null))))

(test "null-vector"
  (assert-string= "[null]" (json:dump-string (vector json:null))))

(test "null-list"
  (assert-string= "[null]" (json:dump-string (list json:null))))

(test "null-nested"
  (assert-string= """{"k":[{"n":[null]}]}"""
                  (json:dump-string
                    (sorted-map "k" (vector (sorted-map "n" (list json:null)))))))

(test "null-and-nil"
  (assert-string= "[null,null]" (json:dump-string (list json:null ()))))

(test "null-dump-bytes-and-message"
  (assert-string= "null" (to-string (json:dump-bytes json:null)))
  (assert-string= "null" (to-string (json:message-bytes (json:dump-message json:null)))))

(test "null-load-is-nil"
  (assert-equal () (json:load-string "null"))
  (assert-equal () (json:load-bytes (to-bytes "null")))
  (assert-equal () (json:load-message (json:dump-message ())))
  (assert-equal (vector () ()) (json:load-string "[null,null]"))
  (assert-equal () (get (json:load-string """{"k":null}""") "k"))
  (assert-equal (sorted-map "k" (vector (sorted-map "n" (vector ()))))
                (json:load-string """{"k":[{"n":[null]}]}""")))

(test "null-round-trip"
  (assert-equal () (json:load-string (json:dump-string json:null)))
  (assert-equal (vector () ())
                (json:load-string (json:dump-string (list json:null ())))))

(test "null-lookalikes"
  (assert-string= "\"json:null\"" (json:dump-string "json:null"))
  (assert-string= "\"null\"" (json:dump-string 'null))
  (assert-string= "\"other:null\"" (json:dump-string 'other:null))
  (assert-string= "json:null" (json:load-string "\"json:null\"")))
