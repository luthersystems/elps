(defun print-it (x)
  (debug-print x))

(defun add-it (x y)
  "@trace{ Add-It }"
  (+ x y))

(defun recurse-it (x)
  (if (< x 4)
    (recurse-it (- x 1))
    (add-it x 3)))

(print-it "Hello")
(print-it (add-it (add-it 3 (recurse-it 5)) 8)) ; span 1..3

(labels
  ([add-it-again (x)
                 "@trace { Add-It-Again }" ; span 4
                 (+ x 1)])
  (add-it-again 2))

(let ([l (lambda (x) "@trace{ lambda }" x)])
  (l 42)) ; span 5

(map () (lambda (x) "@trace" x) '(1 2)) ; span 6, 7
