; Mutual recursion
(defun ping (n)
  "Ping calls pong."
  (if (= n 0)
    true
    (pong (- n 1))))

(defun pong (n)
  "Pong calls ping."
  (if (= n 0)
    true
    (ping (- n 1))))

; Self-recursion
(defun factorial (n)
  "Compute factorial recursively."
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))
