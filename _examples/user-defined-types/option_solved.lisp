(in-package 'option)
(export 'x)
(set 'x 1)
(deftype none ())
(defun nothing () (new none))
(defun nothing? (v) (type? none v))

(deftype some (v) v)
(defun something (v) (new some v))
(defun something? (v) (type? some v))

(defun get-something (v)
  (if (something? v)
    (user-data v)
    (error 'type-error "argument is not something")))

;(map 'something fn v)
(defun something-map (fn v)
  (something (funcall fn (get-something v))))

(export 'optional?)
(defun optional? (v) (or (nothing? v)
                         (something? v)))

(export 'lookup)
(defun lookup (m k)
  """
  returns an optional which contains the value of `k` in `m`.

  returns none if k is not m.

  also does other stuff?
  """
  (if (key? m k)
    (something (get m k))
    (nothing)))


(set 'm (sorted-map "name" "anya"
                    "email" ()))
(debug-print (equal? (get m "email") (get m "birthdate")))
(debug-print (lookup m "name"))
(debug-print (lookup m "email"))
(debug-print (lookup m "birthdate"))
(debug-print (equal? (lookup m "email") (lookup m "birthdate")))

;(map 'optional fn v)
(defun optional-map (fn v)
  (cond ((nothing? v) v)
        ((something? v) (something-map fn v))
        (:else (error 'type-error "argument is not an option"))))

(debug-print (optional-map string:uppercase (something "abc")))
(debug-print (optional-map string:uppercase (nothing)))
(defun send-sms (phone msg)
  (error 'unimplemented "I can't send sms"))
(optional-map #^(send-sms % "hello")
              (lookup m "phone"))
