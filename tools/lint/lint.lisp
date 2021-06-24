(in-package 'lint)
(use-package 'elpslang)

(defun node-token (node) (car (user-data node)))
(defun node-children (node) (car (cdr (user-data node))))
(defun node-type (node) (token-type (node-token node)))
(defun token-type (token) (car (user-data token)))
(defun token-text (token) (car (cdr (user-data token))))
(defun token-loc (token) (car (cdr (cdr (user-data token))))) ;; uh huh

; ok
(defun new-type-error (message v)
  (let ([a 1]))
  (error 'type-error (format-string "{} {}"
                                    message
                                    (to-string (type v)))))

(export 'lint-source)
(defun lint-source (filename source)
  """
  Parse source code (using filename for token locations) and lint the
  contained elps expressions.

  Returns a list of lints (triples of a code, a location and a message).
  """
  (let* ([ast (parse-ast filename source)])
    (apply concat 'vector
           (bad-block-comment-prefixes '() ast)
           (bad-inline-comment-prefixes '() ast)
           (empty-let-bodies ast)
           (map 'list lint-node ast))))

(defun lint-node (node)
  (let ([c-lints (apply concat 'list (map 'list lint-node (node-children node)))])
    (concat 'list c-lints (lint-self node))))

(defun lint-self (node)
  (list))

(defun bad-block-comment-prefixes (prev node-list)
  (if (empty? node-list)
    (lint-ok)
    (let ([a (car node-list)]
          [after-a (cdr node-list)])
      (if (equal? 'comment (node-type a))
        (if (inline? prev a)
          (bad-block-comment-prefixes (last-token a) after-a)
          (concat 'list 
                  (if (string:prefix? (token-text (node-token a)) ";;")
                    (lint-ok)
                    (make-lint (node-token a)
                               "block comments should be prefixed by two semicolons ;;"))
                  (bad-block-comment-prefixes (last-token a) after-a)))
        (concat 'list
                ;; because a is not a comment we have to try and search its
                ;; children for comments
                (bad-block-comment-prefixes (node-token a) (node-children a))
                (bad-block-comment-prefixes (last-token a) after-a))))))

(defun bad-inline-comment-prefixes (prev node-list)
  (if (empty? node-list)
    (lint-ok)
    (let ([a (car node-list)]
          [after-a (cdr node-list)])
      (if (equal? 'comment (node-type a))
        (if (inline? prev a)
          (concat 'list 
                  (if (string:prefix? (token-text (node-token a)) ";;")
                    (make-lint (node-token a)
                               "inline comments should be prefixed by a single semicolon ;")
                    (lint-ok))
                  (bad-inline-comment-prefixes (last-token a) after-a))
          (bad-inline-comment-prefixes (last-token a) after-a))
        (concat 'list
                ;; because a is not a comment we have to try and search its
                ;; children for comments
                (bad-inline-comment-prefixes (node-token a) (node-children a))
                (bad-inline-comment-prefixes (last-token a) after-a))))))

(defun empty-let-bodies (node-list)
  (if (empty? node-list)
    (lint-ok)
    (let ([a (car node-list)]
          [after-a (cdr node-list)])
      (concat 'list
              (if (and (or (let-expression? a)
                           (let*-expression? a))
                       (empty? (let-body a)))
                (make-lint (last-token a) "empty body in let or let* expression")
                (lint-ok))
              (empty-let-bodies (node-children a))
              (empty-let-bodies after-a)))))

(defun let-expression? (node)
  (and (equal? 'paren-left (node-type node))
       (let ([a (car (node-children node))])
         (literal-symbol? a "let" "lisp:let"))))

(defun let*-expression? (node)
  (and (equal? 'paren-left (node-type node))
       (let ([a (car (node-children node))])
         (literal-symbol? a "let*" "lisp:let*"))))

(defun let-body (node)
  (let ([c (node-children node)])
    (if (> 3 (length c))
      '()
      ;; last is paren-right, car is 'let, cadr is bindings, 
      (drop-last (cdr (cdr c))))))

(defun literal-symbol? (node &rest text)
  (and (equal? 'symbol (node-type node))
       (let ([sym (token-text (node-token node))])
         (any? #^(string= % sym) text))))

(defun lint-ok () (list))

(defun make-lint (tok msg)
  (list (list tok msg)))

(export 'format-lint)
(defun format-lint (lint)
  """
  Renders a lint as one-line, human readable message.
  """
  (format-string "{}: {}"
                 (user-data (token-loc (car lint)))
                 (car (cdr lint))))

(defun inline? (a b)
  """
  Returns true iff b starts inline with a.  If a is nil there is no way for b
  to be inline with it and thus false is returned.
  """
  (and (not (nil? a))
       (equal? (token-loc (last-token a))
               (token-loc (node-token b)))))

(defun last-token (node)
  (if (type? 'elpslang:token node)
    node
    (if (nil? (node-children node))
      (node-token node)
      (last-token (last (node-children node))))))

(defun last (lis)
  (if (empty? (cdr lis))
    (car lis)
    (last (cdr lis))))

(defun drop-last (lis)
  (if (empty? lis)
    (error 'argument-error "argument is empty")
    (if (empty? (cdr lis))
      '()
      (cons (car lis) (drop-last (cdr lis))))))
