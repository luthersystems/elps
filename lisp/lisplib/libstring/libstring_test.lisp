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

(test "repeat"
      (assert-string= "" (repeat "x" 0))
      (assert-string= "x" (repeat "x" 1))
      (assert-string= "xx" (repeat "x" 2))
      (assert-string= "abcabcabc" (repeat "abc" 3))
      (assert-string= "" (repeat "" 1000)))

(test "trim-space"
      (assert-string= "" (trim-space ""))
      (assert-string= "" (trim-space " "))
      (assert-string= "" (trim-space "\t"))
      (assert-string= "" (trim-space "\n"))
      (assert-string= "" (trim-space "\n\t"))
      (assert-string= "abc" (trim-space " abc"))
      (assert-string= "abc" (trim-space "\n abc"))
      (assert-string= "abc" (trim-space "abc \t"))
      (assert-string= "abc\t def" (trim-space "\tabc\t def\n\n")))

(test "trim"
      (assert-string= "" (trim "" ""))
      (assert-string= "" (trim "" "abc"))
      (assert-string= "abc" (trim "abc" ""))
      (assert-string= "" (trim "abc" "abc"))
      (assert-string= "ab" (trim "abc" "c"))
      (assert-string= "b" (trim "abc" "ca"))
      (assert-string= "a" (trim "abc" "cb")))

(test "trim-left"
      (assert-string= "" (trim-left "" ""))
      (assert-string= "" (trim-left "" "abc"))
      (assert-string= "abc" (trim-left "abc" ""))
      (assert-string= "" (trim-left "abc" "abc"))
      (assert-string= "abc" (trim-left "abc" "c"))
      (assert-string= "bc" (trim-left "abc" "ca"))
      (assert-string= "abc" (trim-left "abc" "cb")))

(test "trim-right"
      (assert-string= "" (trim-right "" ""))
      (assert-string= "" (trim-right "" "abc"))
      (assert-string= "abc" (trim-right "abc" ""))
      (assert-string= "" (trim-right "abc" "abc"))
      (assert-string= "ab" (trim-right "abc" "c"))
      (assert-string= "ab" (trim-right "abc" "ca"))
      (assert-string= "a" (trim-right "abc" "cb")))
