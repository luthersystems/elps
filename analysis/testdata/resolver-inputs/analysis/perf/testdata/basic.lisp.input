; Basic test: simple function with a call
(defun greet (name) ; nolint:unused-function
  "Greet someone."
  (concat 'string "Hello, " name))

(defun process-item (item) ; nolint:unused-function
  "Process a single item."
  (db-put item)) ; nolint:undefined-symbol

; N+1 pattern: expensive call inside a map
(defun process-batch (items) ; nolint:unused-function
  "Process a batch of items — maps db-put over each element."
  (map 'list (lambda (item) (db-put item)) items)) ; nolint:undefined-symbol

; Nested iteration with expensive call
(defun process-matrix (rows n) ; nolint:unused-function
  "Process all items: outer map, inner dotimes."
  (map 'list
       (lambda (row)
         (dotimes (i n)
           (db-put (nth row i)))) ; nolint:undefined-symbol
       rows))
