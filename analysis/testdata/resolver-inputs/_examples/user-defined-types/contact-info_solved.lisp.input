(deftype contact-info (name phone)
  (sorted-map "name" name "phone" phone))

(defun contact-info? (obj) (type? contact-info obj))

(defun contact-name (contact)
  (if (contact-info? contact)
    (get (user-data contact) "name")
    (error 'type-error "argument is not contact-info")))

(defun say-hello (contact)
  (debug-print (format-string "Hello, {}" (contact-name contact))))

(say-hello (new contact-info "Alice" "+1-555-555-5555"))
;; "Hello, Alice"

;; (say-hello (sorted-map "name" "Bob"))
;; type-error: argument is not contact-info
