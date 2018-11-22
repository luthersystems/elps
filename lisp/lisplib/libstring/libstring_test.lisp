; Copyright © 2018 The ELPS authors

(use-package 'string)
(use-package 'testing)

(test "uppercase"
      (assert-string= "" (uppercase ""))
      (assert-string= "ABC" (uppercase "abc")))

(test "lowercase"
      (assert-string= "" (lowercase ""))
      (assert-string= "abc" (lowercase "ABC")))

(test "split"
      (assert-string= "ghi"
                      (nth (split "abc:def:ghi" ":") 2))
      (assert-equal '("abc" "def")
                    (split "abc:def" ":"))
      (assert-equal '("abc")
                    (split "abc" ":"))
      (assert-equal '("")
                    (split "" ":"))
      (assert-equal '("" "")
                    (split ":" ":")))
