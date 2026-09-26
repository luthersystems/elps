; Copyright © 2018 The ELPS authors

(in-package 'libjson-string-numbers-test)

(use-package 'testing)

(test "string-numbers?"
  (assert-equal 'false (json:string-numbers?))
  (json:use-string-numbers true)
  (assert-equal 'true (json:string-numbers?))
  (assert-string= "\"1\"" (json:dump-string 1))
  (json:use-string-numbers false)
  (assert-equal 'false (json:string-numbers?))
  (assert-string= "1" (json:dump-string 1)))

; Malformed input stays catchable as json:syntax-error under :string-numbers,
; as it is under :exact-integers and in the default mode.
(test "string-numbers-syntax-errors-stay-catchable"
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "[1,]" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "1 2" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-bytes (to-bytes "{\"a\":") :string-numbers true)))
  ; The serializer-wide mode takes the same path.
  (with-cleanup ((json:use-string-numbers false))
    (json:use-string-numbers true)
    (assert-string= "syntax"
                    (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                      (json:load-string "tru")))))
