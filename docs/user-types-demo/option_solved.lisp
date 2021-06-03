(deftype some (v) v)
(defun something (v) (new some v))
(defun something? (v) (type? some v))

(defun get-something (v)
  (if (something? v)
    (user-data v)
    (error 'type-error "argument is not something")))

;(map 'something fn v)
(defun something-map (fn v)
  (new something (get-something v)))

(set 'm (sorted-map "name" "anya"
                    "email" ()))

;(equal? (get m "email") (get m "birthdate"))

(deftype none ())
(defun nothing () (new none))

(defun nothing? (v) (type? none v))
(defun optional? (v) (or (nothing? v)
                         (something? v)))

(defun lookup (m k)
  (if (key? m k)
    (something (get m k))
    (nothing)))

(debug-print (lookup m "name"))
(debug-print (lookup m "email"))
;(equal? (lookup m "email") (lookup m "birthdate"))

;(map 'optional fn v)
(defun optional-map (fn v)
  (cond ((nothing? v) v)
        ((something? v) (new something (funcall fn (user-data v))))
        (:else (error 'type-error "argument is not an option"))))

(debug-print (optional-map string:uppercase (something "abc")))
(debug-print (optional-map string:uppercase (nothing)))
(defun send-sms (phone msg)
  (error 'unimplemented "I can't send sms"))
(optional-map #^(send-sms % "hello")
              (lookup m "phone"))
