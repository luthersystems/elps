(use-package 'regexp)

(let ([phone-re (regexp-compile "^[+][0-9]+ ([0-9]|-| )+$")])
  (defun phone-ok? (str) (and (string? str) (regexp-match? phone-re str))))

(deftype phone-number (str)
  (if (phone-ok? str) str (error 'format-error "invalid phone number")))

(debug-print (new phone-number "+1 5555555"))
;; {user:phone-number "+1 555 555 5555"}

;; (debug-print (new phone-number "+1 5555ABC"))
;; (debug-print (new phone-number 15555555))
;; format-error: invalid phone number
