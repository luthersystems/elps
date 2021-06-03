(set 'address-book (sorted-map))

(defun make-contact-info (name phone)
  (sorted-map "name" name "phone" phone))

(defun create-contact! (name phone)
  (assoc! address-book name (make-contact-info name phone)))

(defun say-hello (contact)
  (debug-print (format-string "Hello, {}" (get contact "name"))))

(create-contact! "Alice" "+1-555-555-5555")
(say-hello (get address-book "Alice"))
;; "Hello, Alice"

(say-hello (sorted-map "name" "Bob"))
;; "Hello, Bob"

(say-hello (sorted-map "Name" "Carol"))
;; "Hello, ()"
