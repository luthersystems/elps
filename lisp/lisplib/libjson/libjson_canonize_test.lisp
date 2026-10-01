; Copyright © 2026 The ELPS authors

(use-package 'testing)

(test "canonize-condition-data"
  (assert-equal (vector 'json:canonize-error :leading-tilde "$[\"box\"][0]")
                (handler-bind ([json:canonize-error
                                (lambda (condition message case path)
                                  (assert (> (length message) 0))
                                  (vector condition case path))])
                  (json:canonize (sorted-map "box" (vector "~value")))))
  (assert-equal :negative-zero
                (handler-bind ([json:canonize-error
                                (lambda (_condition _message case _path) case)])
                  (json:canonize -0.0)))
  (assert-equal :float-range
                (handler-bind ([json:canonize-error
                                (lambda (_condition _message case _path) case)])
                  (json:canonize 1e20))))

(test "canonize-handler-branches-and-falls-back"
  (let ([payload (sorted-map "name" "~draft")])
    (assert-string= (json:dump-string payload)
                    (handler-bind ([json:canonize-error
                                    (lambda (_condition _message case _path)
                                      (if (equal? case :leading-tilde)
                                        (json:dump-string payload)
                                        (error 'unexpected-case case)))])
                      (json:dump-string (json:canonize payload))))))

(test "canonize-errors-can-be-ignored"
  (assert-nil (ignore-errors (json:canonize "~value")))
  (assert-nil (ignore-errors (json:canonize -0.0)))
  (assert-nil (ignore-errors (json:canonize 1e20)))
  (assert-nil (ignore-errors (json:canonize (sorted-map 9 "nine" 10 "ten")))))

(test "canonical-dump-propagates-canonize-condition"
  (assert-equal :leading-tilde
                (handler-bind ([json:canonize-error
                                (lambda (_condition _message case _path) case)])
                  (json:dump-string "~value" :canonize true)))
  (assert-equal :key-type
                (handler-bind ([json:canonize-error
                                (lambda (_condition _message case _path) case)])
                  (json:dump-bytes (sorted-map 9 "nine") :canonize true)))
  (assert-nil (ignore-errors (json:dump-bytes "~value" :canonize true))))
