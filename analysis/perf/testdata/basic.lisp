; Basic test: simple function with a call
(defun greet (name)
  "Greet someone."
  (concat 'string "Hello, " name))

(defun process-item (item)
  "Process a single item."
  (db-put item))

; N+1 pattern: expensive call inside a map
(defun process-batch (items)
  "Process a batch of items — maps db-put over each element."
  (map 'list (lambda (item) (db-put item)) items))

; Nested iteration with expensive call
(defun process-matrix (rows n)
  "Process all items: outer map, inner dotimes."
  (map 'list
       (lambda (row)
         (dotimes (i n)
           (db-put (nth row i))))
       rows))
