; Copyright © 2018 The ELPS authors

(use-package 'testing)
(use-package 'regexp)

(test "regexp-compile"
  (set 're (regexp-compile "abc?"))
  (assert-string= "abc?" (regexp-pattern re))
  (set 're (regexp-compile """abc?\(\)"""))
  (assert-string= "abc?\\(\\)" (regexp-pattern re))
  (set 're (regexp-compile "abc\n"))
  (assert-string= "abc\n" (regexp-pattern re)))

(test "regexp-match?"
  (defmacro assert-match (patt text)
    (let ([pattsym (gensym)])
      (quasiquote
        (let ([(unquote pattsym) (unquote patt)])
          (assert (funcall 'regexp:regexp-match?
                           (unquote pattsym)
                           (unquote text))
                  "pattern {} does not match text: {}"
                  (quote (unquote pattsym))
                  (quote (unquote text)))))))
  (assert-match "abc?" "ab")
  (assert-match "abc?" "abc")
  (assert-match """^\n*$""" "")
  (assert-match """^\n*$""" "\n")
  (assert-match """^\n*$""" "\n\n")
  (assert-match "^\n*$" "\n\n")
  (assert-match """\s""" "abc\n"))
