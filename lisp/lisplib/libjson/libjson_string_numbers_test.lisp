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
