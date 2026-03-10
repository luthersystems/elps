; This function should be excluded from analysis
;; elps-analyze-disable
(defun noisy-but-ok (items)
  "Known hot path, intentionally excluded."
  (dolist (item items)
          (db-put item)))
