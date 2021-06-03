(use-package 'regexp)

(set 'phone-regexp (regexp-compile "[+][0-9]+ ([0-9]|-| )+"))

(deftype phone-number (str)
  (if (regexp-match? phone-regexp str)
    str
    (error 'format-error "invalid phone number")))
