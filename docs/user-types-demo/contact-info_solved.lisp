(deftype contact-info (name phone email)
  (sorted-map "name" name
              "phone" phone
              "email" email))

(defun contact-info? (obj)
  (type? contact-info obj))

(defun contact-name (contact)
  (get (user-data contact) "name"))

(defun say-hello (contact)
  (if (contact-info? contact)
    (format-string "Hello, {}" (contact-name contact))
    (error 'type-error "argument is not contact-info")))
