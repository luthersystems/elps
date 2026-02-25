; structured.lisp — test program with structured types for variable expansion tests.
(defun test-structured ()
  (let ((my-list (list 10 20 30))
        (my-map (sorted-map "a" 1 "b" 2))
        (my-array (make-array 3)))
    (aset my-array 0 "x")
    (aset my-array 1 "y")
    (aset my-array 2 "z")
    my-list))

(test-structured)
