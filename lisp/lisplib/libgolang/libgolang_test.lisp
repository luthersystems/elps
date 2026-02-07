; Copyright © 2018 The ELPS authors

(use-package 'golang-test)
(use-package 'testing)

(test-let* "struct"
  ((struct (make-test-struct))
   (field (curry-function 'golang:struct-field struct)))  ; partially bind function args
  (assert (string= (golang:string (field "StringField"))
                   "test-string"))
  (assert (= (golang:int (field "IntField"))
             123))
  (assert (= (golang:float (field "FloatField"))
             12.34)))
