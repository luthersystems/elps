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

(test "has-prefix?"
  (assert (has-prefix? "" ""))
  (assert (has-prefix? "abc" ""))
  (assert-not (has-prefix? "" "a"))
  (assert (has-prefix? "abc" "a"))
  (assert (has-prefix? "abc" "abc"))
  (assert-not (has-prefix? "abc" "abcd"))
  (assert-not (has-prefix? "abc" "b"))
  (assert-not (has-prefix? "abc" "A"))
  (assert (has-prefix? "日本語" "日本"))
  (assert-not (has-prefix? "日本語" "本"))
  (assert (has-prefix? "José" "Jos"))
  (assert-equal 'true (has-prefix? "abc" "ab"))
  (assert-equal 'false (has-prefix? "abc" "bc")))

(test "has-suffix?"
  (assert (has-suffix? "" ""))
  (assert (has-suffix? "abc" ""))
  (assert-not (has-suffix? "" "c"))
  (assert (has-suffix? "abc" "c"))
  (assert (has-suffix? "abc" "abc"))
  (assert-not (has-suffix? "abc" "zabc"))
  (assert-not (has-suffix? "abc" "b"))
  (assert (has-suffix? "日本語" "本語"))
  (assert-not (has-suffix? "日本語" "日"))
  (assert (has-suffix? "José" "é"))
  (assert-equal 'true (has-suffix? "abc" "bc"))
  (assert-equal 'false (has-suffix? "abc" "ab")))

(test "contains?"
  (assert (contains? "" ""))
  (assert (contains? "abc" ""))
  (assert-not (contains? "" "a"))
  (assert (contains? "abc" "a"))
  (assert (contains? "abc" "b"))
  (assert (contains? "abc" "c"))
  (assert (contains? "abc" "abc"))
  (assert-not (contains? "abc" "abcd"))
  (assert-not (contains? "abc" "ac"))
  (assert (contains? "日本語" "本"))
  (assert-not (contains? "日本語" "中"))
  (assert-equal 'true (contains? "abc" "bc"))
  (assert-equal 'false (contains? "abc" "x")))

(test "trim-prefix"
  (assert-string= "" (trim-prefix "" ""))
  (assert-string= "" (trim-prefix "" "a"))
  (assert-string= "abc" (trim-prefix "abc" ""))
  (assert-string= "" (trim-prefix "abc" "abc"))
  (assert-string= "abc" (trim-prefix "abc" "abcd"))
  (assert-string= "bc" (trim-prefix "abc" "a"))
  (assert-string= "abc" (trim-prefix "abc" "b"))
  ; Only one occurrence is removed.
  (assert-string= "abab" (trim-prefix "ababab" "ab"))
  (assert-string= "aa" (trim-prefix "aaa" "a"))
  ; The prefix is a whole string, not a cutset as with trim-left.
  (assert-string= "abc" (trim-prefix "abc" "ba"))
  (assert-string= "c" (trim-left "abc" "ba"))
  (assert-string= "語" (trim-prefix "日本語" "日本"))
  (assert-string= "日本語" (trim-prefix "日本語" "本")))

(test "trim-suffix"
  (assert-string= "" (trim-suffix "" ""))
  (assert-string= "" (trim-suffix "" "a"))
  (assert-string= "abc" (trim-suffix "abc" ""))
  (assert-string= "" (trim-suffix "abc" "abc"))
  (assert-string= "abc" (trim-suffix "abc" "zabc"))
  (assert-string= "ab" (trim-suffix "abc" "c"))
  (assert-string= "abc" (trim-suffix "abc" "b"))
  ; Only one occurrence is removed.
  (assert-string= "abab" (trim-suffix "ababab" "ab"))
  (assert-string= "aa" (trim-suffix "aaa" "a"))
  ; The suffix is a whole string, not a cutset as with trim-right.
  (assert-string= "abc" (trim-suffix "abc" "cb"))
  (assert-string= "a" (trim-right "abc" "cb"))
  (assert-string= "日本" (trim-suffix "日本語" "語"))
  (assert-string= "日本語" (trim-suffix "日本語" "本")))
