(defun print-it (x)
  (debug-print x))

(defun add-it (x y)
  "@trace"
  (+ x y))

(defun recurse-it (x)
  (if (< x 4)
    (recurse-it (- x 1))
    (add-it x 3)))

(print-it "Hello")
(print-it (add-it (add-it 3 (recurse-it 5)) 8))
