(use-package 'elpslang)
(use-package 'testing)

(defun node-token (node) (car (user-data node)))
(defun node-children (node) (car (cdr (user-data node))))
(defun node-type (node) (token-type (node-token node)))
(defun token-type (token) (car (user-data token)))
(defun token-text (token) (car (cdr (user-data token))))
(defun token-loc (token) (car (cdr (cdr (user-data token)))))


(test "simple-parse"
  (assert-equal 'hash-bang (node-type (first (parse-ast "test.lisp" "#!/usr/bin/env elps"))))
  (assert-equal 'comment (node-type (first (parse-ast "test.lisp" ";; 123"))))
  (assert-equal 'int (node-type (first (parse-ast "test.lisp" "123"))))
  (assert-equal 'float (node-type (first (parse-ast "test.lisp" "1.3"))))
  (assert-equal 'quote (node-type (first (parse-ast "test.lisp" "'123"))))
  (assert-equal 'string (node-type (first (parse-ast "test.lisp" "\"123\""))))
  (assert-equal 'string-raw (node-type (first (parse-ast "test.lisp" "\"\"\"123\"\"\""))))
  (let ([ast (parse-ast "test.lisp" "(defun fn () (debug-print 123))")])
    (debug-print ast)
    (assert-equal 1 (length ast))
    (set! ast (first ast))
    (assert-equal 'paren-left (node-type ast))
    (assert-equal 'symbol (thread-first ast
                                        (node-children)
                                        (second)
                                        (node-type)))
    (assert-equal "fn" (thread-first ast
                                     (node-children)
                                     (second)
                                     (node-token)
                                     (token-text)))))


(defun simple-format (source)
  (let ([ast (parse-ast "test.lisp" source)])
    (apply format-ast ast)))

(defun dedent (text)
  (string:join (dedent-lines (split-lines text)) "\n"))

(defun dedent-lines (lines)
  (cond ((empty? lines) lines)
        ((all? #^(string:prefix? % " ") lines)
         (dedent-lines (map 'list #^(string:trim-prefix % " ") lines)))
        ((all? #^(string:prefix? % "\t") lines)
         (dedent-lines (map 'list #^(string:trim-prefix % "\t") lines)))
        (:else
          (let* ([last (- (length lines) 1)])
            (if (equal? "" (nth lines last))
              (slice 'list lines 0 last)
              lines)))))

(defun split-lines (text)
  (let* ([lines (string:split text "\n")])
    (if (equal? "" (first lines))
      (cdr lines)
      lines)))

(test "simple-format"
  (assert-equal """symbol test""" (simple-format """test"""))
  (assert-equal """int 123""" (simple-format """123"""))
  (assert-equal """float 1.1""" (simple-format """1.1""")) ; not accurately representable in ieee754
  (assert-equal """symbol -""" (simple-format """-"""))
  (assert-equal """int -123""" (simple-format """-123"""))
  (assert-equal """float -1.1""" (simple-format """-1.1""")) ; not accurately representable in ieee754
  (assert-equal "string \"test\"" (simple-format "\"test\""))
  (assert-equal """comment ; a comment""" (simple-format """; a comment"""))
  (assert-equal (dedent """
                        string "hello"
                        comment ; a comment
                        """)
                (simple-format """
                               "hello"; a comment
                               """))
  (assert-equal (dedent """
                        paren-left (
                          symbol defun
                          symbol fn
                          paren-left (
                            symbol a
                            symbol b
                          comment ; a comment
                          string "a docstring"
                          paren-left (
                            symbol +
                            symbol a
                            symbol b
                        comment ;; add some numbers
                        paren-left (
                          symbol let
                          paren-left (
                            brace-left [
                              symbol x
                              int 1
                            brace-left [
                              symbol y
                              int 2
                          paren-left (
                            symbol fn
                            symbol x
                            symbol y
                        """) ;))))]]) right parens to match left parens in the test
                (simple-format """
                               (defun fn (a b) ; a comment
                                 "a docstring"
                                 (+ a b))
                                ;; add some numbers
                               (let ([x 1]
                                     [y 2])
                                 (fn x y))
                               """))
  )
