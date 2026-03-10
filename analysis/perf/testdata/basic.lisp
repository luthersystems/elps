; Basic test: simple function with a call
(defun greet (name)
  "Greet someone."
  (concat "Hello, " name))

(defun process-item (item)
  "Process a single item."
  (db-put item))

; N+1 pattern: expensive call inside a loop
(defun process-batch (items)
  "Process a batch of items."
  (dolist (item items)
    (db-put item)))

; Nested loops with expensive call
(defun process-matrix (matrix)
  "Process all items in a matrix."
  (dolist (row matrix)
    (dolist (item row)
      (db-put item))))
