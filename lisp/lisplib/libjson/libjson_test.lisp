; Copyright © 2018 The ELPS authors

(use-package 'testing)

(test "use-string-numbers"
  (assert-string= "\"1\"" (to-string (json:message-bytes (json:dump-message 1 :string-numbers true))))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers true))
  (assert-string= "1" (json:load-string "1" :string-numbers true))

  ; Assert that, when :string-numbers is not given (or nil) the setting
  ; defaults back to the last call of use-string-numbers (or false if never
  ; called)
  (assert-string= "1" (json:dump-string 1 :string-numbers ()))
  (assert= 1 (json:load-string "1" :string-numbers ()))
  (assert-string= "1" (json:dump-string 1))
  (assert= 1 (json:load-string "1"))
  (assert-nil (json:use-string-numbers true))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers ()))
  (assert-string= "1" (json:load-string "1" :string-numbers ()))
  (assert-string= "\"1\"" (json:dump-string 1))
  (assert-string= "1" (json:load-string "1"))
  ; the value false can be used to override a true setting of
  ; use-string-numbers
  (assert-string= "1" (json:dump-string 1 :string-numbers false))
  (assert= 1 (json:load-string "1" :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers false)
                                         :string-numbers true))
  (assert= 1 (json:load-message (json:dump-message 1 :string-numbers false)
                                :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers true)
                                         :string-numbers false))
  (assert-string= """{"data":"1","id":1}"""
                  (json:dump-string (sorted-map "id"    1
                                                "data"  (json:dump-message 1 :string-numbers true))
                                    :string-numbers false))
  )

(test "marshal"
  (assert-string= """null"""
                  (json:dump-string ()))
  (assert-string= """true"""
                  (json:dump-string true))
  (assert-string= """false"""
                  (json:dump-string false))
  (assert-string= """12"""
                  (json:dump-string 12))
  (assert-string= """[1,2]"""
                  (json:dump-string '(1 2)))
  (assert-string= """[1,2]"""
                  (json:dump-string [1 2]))
  (assert-string= """[1,2]"""
                  (json:dump-string (vector 1 2)))
  (assert-string= """{}"""
                  (json:dump-string (sorted-map)))
  (assert-string= """{"a":1}"""
                  (json:dump-string (sorted-map "a" 1)))
  (assert-string= """{"a":1,"b":2}"""
                  (json:dump-string (sorted-map "a" 1 "b" 2)))
  (assert-string= """{"a":{"b":"c"}}"""
                  (json:dump-string (sorted-map "a" (sorted-map "b" "c")))))

(test "unmarshal"
  (set 'js-val (json:load-string """null"""))
  (assert-nil js-val)
  (set 'js-val (json:load-string """true"""))
  (assert js-val)
  (assert-string= "true" (to-string js-val))
  (set 'js-val (json:load-string """false"""))
  (assert-not js-val)
  (assert-not-nil js-val)
  (assert-string= "false" (to-string js-val))
  (set 'js-val (json:load-string """4.0"""))
  (assert= 4 js-val)
  (set 'js-val (json:load-string """[1, 2, 3]"""))
  (assert-equal (vector 1 2 3) js-val)
  (set 'js-val (json:load-string """[]"""))
  (assert-equal (vector) js-val)
  (set 'js-val (json:load-string """[[]]"""))
  (assert-equal (vector (vector)) js-val)
  (set 'js-val (json:load-string """{"a":[], "b":null,"c":{}}"""))
  (assert (sorted-map? js-val))
  (assert-equal '("a" "b" "c") (keys js-val))
  (assert-equal (vector) (get js-val "a"))
  (assert-nil (get js-val "b"))
  (assert (sorted-map? (get js-val "c")))
  (assert= 0 (length (keys (get js-val "c")))))

(test "unmarshal-syntax-error"
  (assert-string= "syntax-error"
                  (handler-bind ([json:syntax-error (lambda (c _) "syntax-error")])
                    (json:load-string "{false:true}")))
  (assert-string= "ok-json"
                  (handler-bind ([json:syntax-error (lambda (c _) "syntax-error")])
                    (json:load-string "\"ok-json\""))))
