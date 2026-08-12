(defmacro cyc () (let ([x (vector 1)]) (append! x x) x))
(debug-print "before")
(cyc)
(debug-print "after")
