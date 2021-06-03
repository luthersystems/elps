(set 'address-book (sorted-map))

(defun make-contact-info (name phone email)
  (sorted-map "name" name
              "phone" phone
              "email" email))

(defun create-contact! (name phone email)
  (assoc! address-book name (make-contact-info name phone email)))

(defun say-hello (contact)
  (format-string "Hello, {}" (get contact "name")))
