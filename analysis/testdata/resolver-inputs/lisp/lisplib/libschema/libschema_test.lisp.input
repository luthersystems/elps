(use-package 'testing)

; Several tests below reuse a generic name (myint, mystring, x) already bound
; by an earlier, independent test. Each (test ...) body runs in its own
; isolated environment at runtime, so this is not really a rebind -- but the
; set-usage analyzer reads the file as one flat top-level sequence and cannot
; see that isolation, so it reports a false "already bound" positive on the
; later definitions. Tagged ; nolint:set-usage at each site below.

(test "make-validator-string"
  (set 'mystring (s:make-validator "mystring" s:string))
  (set 'x "hello")
  (assert-nil (s:validate mystring x))
  (set 'y 9.0)
  (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
    (assert-equal (s:validate mystring y) "ERROR"))
  (set 'myconditionalstring (s:make-validator "myconditionalstring" s:string (s:in "x" "y" "z")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalstring y)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalstring x)))
  (assert-nil (s:validate mystring "x"))
  (assert-nil (s:validate mystring "y"))
  )

(test "make-validator-regexp"
  (set 'mystring (s:make-validator "mystring" s:string (s:regexp "^Hello"))) ; nolint:set-usage
  (assert-nil (s:validate mystring "Hello mum"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mystring "goodbye mum")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mystring "well hello there")))
  (set 'isodate (s:make-validator "isodate" s:string (s:regexp "^([1-9][0-9]{3})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])?$")))
  (assert-nil (s:validate isodate "2020-04-31"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate isodate "3/4/21")))
  (assert-equal  "ERROR" (handler-bind (('bad-arguments (lambda (&rest _e) "ERROR")))
                           (set 'x (s:make-validator "x" s:string (s:regexp "*"))))) ; nolint:set-usage
  )

(test "make-validator-int"
  (set 'myint (s:make-validator "myint" s:int))
  (assert-nil (s:validate myint 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint 91.3)))
  )

(test "make-validator-bool"
  (set 'mybool (s:make-validator "mybool" s:bool (s:is-true)))
  (assert-nil (s:validate mybool true))
  ; A boolean is the symbol, not a string that spells it.
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mybool "true")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mybool "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybool 'false)))
  )

(test "make-validator-bool-false"
  (set 'myboolf (s:make-validator "myboolf" s:bool (s:is-false)))
  (assert-nil (s:validate myboolf 'false))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myboolf "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolf 'true)))
  )

(test "make-validator-any-truthy"
  (set 'myboolty (s:make-validator "myboolty" s:any (s:is-truthy)))
  (assert-nil (s:validate myboolty true))
  (assert-nil (s:validate myboolty 'true))
  (assert-nil (s:validate myboolty "true"))
  (assert-nil (s:validate myboolty "hello"))
  (assert-nil (s:validate myboolty 100))
  (assert-nil (s:validate myboolty 91.3))
  (assert-nil (s:validate myboolty (vector 3 4 5)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolty 'false)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolty 0)))
  )

(test "make-validator-any-falsy"
  (set 'mybooltf (s:make-validator "mybooltf" s:any (s:is-falsy)))
  (assert-nil (s:validate mybooltf false))
  (assert-nil (s:validate mybooltf "false"))
  (assert-nil (s:validate mybooltf ""))
  (assert-nil (s:validate mybooltf 0))
  (assert-nil (s:validate mybooltf -91.3))
  (assert-nil (s:validate mybooltf (vector)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybooltf 'true)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybooltf 11)))
  )

(test "make-validator-float"
  (set 'myflt (s:make-validator "myflt" s:float))
  (assert-nil (s:validate myflt 4.3))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myflt "error")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myflt 91)))
  )

(test "make-validator-number"
  (set 'mynum (s:make-validator "mynum" s:number))
  (assert-nil (s:validate mynum 4.3))
  (assert-nil (s:validate mynum 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mynum "error")))
  )

(test "make-validator-int-positive"
  (set 'myint (s:make-validator "myint" s:int (s:positive))) ; nolint:set-usage
  (assert-nil (s:validate myint 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint -5)))
  )

(test "make-validator-int-negative"
  (set 'myint (s:make-validator "myint" s:int (s:negative))) ; nolint:set-usage
  (assert-nil (s:validate myint -5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 5)))
  )

(test "make-validator-int-constrained"
  (set 'myint (s:make-validator "myint" s:int (s:gt 4) (s:lte 91))) ; nolint:set-usage
  (assert-nil (s:validate myint 5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 93)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 4)))
  )

(test "make-validator-int-constrained2"
  (set 'myint (s:make-validator "myint" s:int (s:gte 4) (s:lt 91))) ; nolint:set-usage
  (assert-nil (s:validate myint 5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 91)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 3)))
  )

(test "make-validator-int-constrained3"
  (set 'myint (s:make-validator "myint" s:int (s:in 6 7 11))) ; nolint:set-usage
  (assert-nil (s:validate myint 6))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 8)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint -6)))
  )

(test "make-validator-array"
  (set 'my4thingarray (s:make-validator "my4thingarray" s:array (s:len 4)))
  (set 'my4to6thingarray (s:make-validator "my4to6thingarray" s:array (s:lengte 4) (s:lenlt 7)))
  (set 'mystringarray (s:make-validator "mystringarray" s:array (s:of s:string)))
  (set 'mystringorfunctionarray (s:make-validator "mystringorfunctionarray" s:array (s:of s:string s:fun)))
  (set 'myint (s:make-validator "myint" s:int (s:gt 11))) ; nolint:set-usage
  (set 'myfancyarray (s:make-validator "myfancyarray" s:array (s:of myint)))
  (assert-nil (s:validate my4thingarray (vector 1 2 3 4)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate my4thingarray (vector 1 2 3 4 5))))
  (assert-nil (s:validate my4to6thingarray (vector 1 2 3 4 5)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate my4thingarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate mystringarray (vector "a" "b" "c")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mystringarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate mystringorfunctionarray (vector "a" "b" "c" assert-nil)))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mystringorfunctionarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate myfancyarray (vector 13 87 2222)))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myfancyarray (vector 13 87 9))))
  )

(test "make-validator-map"
  (set 'mymap (s:make-validator "mymap" s:sorted-map (s:has-key "name" s:string) (s:may-have-key "middle-name" s:string)))
  (set 'mymapdone (s:make-validator "mymapdone" s:sorted-map (s:no-other-keys (s:has-key "name" s:string) (s:may-have-key "middle-name" s:string))))
  (set 'myint (s:make-validator "myint" s:int (s:gt 4))) ; nolint:set-usage
  (set 'mycomplicatedmap (s:make-validator "mycomplicatedmap" s:sorted-map (s:has-key "name" s:string) (s:has-key "id" myint) (s:has-key "innermap" mymap)))
  (set 'myconditionalmap (s:make-validator "myconditionalmap" s:sorted-map (s:has-key "name" s:string) (s:has-key "id" s:int) (s:has-key "writer" s:bool) (s:when "name" (s:in "Reuben" "Sam") "writer" (s:is-true))))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver")))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver" 'middle-name "Wendell")))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver" 'middle-name "Wendell" 'last-name "Holmes")))
  (assert-nil (s:validate mymapdone (sorted-map 'name "Oliver")))
  (assert-nil (s:validate mymapdone (sorted-map 'name "Oliver" 'middle-name "Wendell")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mymapdone (sorted-map 'name "Oliver" 'middle-name "Wendell" 'last-name "Holmes"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mymap (sorted-map 'name "Oliver" 'middle-name 74))))
  (assert-nil (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 6 'innermap (sorted-map 'name "Oliver" 'middle-name "Wendell"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 6 'innermap "hello"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 3 'innermap (sorted-map 'name "Oliver" 'middle-name "Wendell")))))
  (assert-nil (s:validate myconditionalmap (sorted-map 'name "Reuben" 'id 5 'writer true)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalmap (sorted-map 'name "Reuben" 'id 5 'writer false))))
  )

(test "not"
  (set 'not-test (s:make-validator "not-test" s:string (s:not (s:in "a" "b" "c"))))
  (assert-nil (s:validate not-test "x"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate not-test "a")))
  )

(test "lisp-deftype"
  (deftype mystring (s) (to-string s))
  (set 'v (s:make-validator mystring s:string (s:in "a" "b" "c")))
  (assert-nil (s:validate v (new mystring "c")))
  (assert-equal "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                          (s:validate v (new mystring "x"))))
  (assert-nil (s:validate v (new mystring "c")))
  (deftype mynil ())
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate v (new mynil))))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate v "a"))))

; Issue #325.  s:may-have-key looked its key up as a symbol while s:has-key
; looked the same key up as a string.  lisp.Map is an interface: the built-in
; sorted-map keys on the string value either way, so the asymmetry is
; invisible here, but the map json:load-string returns rejects a non-string
; key outright -- so may-have-key never found anything and silently passed
; whatever it was given.  The key type is now string everywhere.
(test "may-have-key-key-type"
  (set 'optstr (s:make-validator "optstr" s:sorted-map (s:may-have-key "a" s:string)))

  ; string-keyed literal map
  (assert-nil (s:validate optstr (sorted-map "a" "str")))
  (assert-nil (s:validate optstr (sorted-map "z" 0)))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (sorted-map "a" 1))))

  ; symbol-keyed literal map -- a string lookup must still match it
  (assert-nil (s:validate optstr (sorted-map 'a "str")))
  (assert-nil (s:validate optstr (sorted-map 'z 0)))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (sorted-map 'a 1))))

  ; json-decoded map -- the case that silently passed before the fix
  (assert-nil (s:validate optstr (json:load-string "{\"a\": \"str\"}")))
  (assert-nil (s:validate optstr (json:load-string "{\"z\": 0}")))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (json:load-string "{\"a\": 1}"))))

  ; s:has-key must give the same verdict whenever the key is present
  (set 'reqstr (s:make-validator "reqstr" s:sorted-map (s:has-key "a" s:string)))
  (assert-nil (s:validate reqstr (sorted-map "a" "str")))
  (assert-nil (s:validate reqstr (sorted-map 'a "str")))
  (assert-nil (s:validate reqstr (json:load-string "{\"a\": \"str\"}")))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate reqstr (json:load-string "{\"a\": 1}"))))
  )

; #736: a prefixed library builtin must never write into the caller's
; package, so s:deftype (which bound its validator as a global under the
; caller's own name) was removed. Callers now bind the result of
; s:make-validator themselves with core set -- see e.g.
; make-validator-string above. s:deftype is simply gone; the symbol
; resolves as unbound like any other typo.
(test "deftype-removed"
  (assert-equal (list 'error "unbound symbol: deftype")
                (handler-bind (('error (lambda (&rest e) e)))
                  (s:deftype "removed" s:string))))
