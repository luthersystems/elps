; Copyright © 2026 The ELPS authors

; Lisp-level tests for issue #350 -- integers above 2^53 and the
; :exact-integers opt-in that makes them survive a load.
;
; Like libjson_cycle_test.lisp, this lives in its own file rather than in
; libjson_test.lisp because BenchmarkPackage/$load parses and evaluates that
; file on every iteration: source added to it is charged to a benchmark meant
; to measure the loader, and the CI benchmark gate reads the jump as a
; regression.  See TestPackageExactIntegers.

(use-package 'testing)

; The defect, from the phylum author's side.  Every number decodes as a float,
; a float carries 53 bits of integer precision, and nothing anywhere says so.
(test "default-rounds-silently"
  ; 2^53+1 comes back as 2^53.
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993"))))
  (assert-string= "9007199254740992"
                  (json:dump-string (json:load-string "9007199254740993")))
  ; int64 max comes back as something that is not even an int64.
  (assert-string= "9223372036854776000"
                  (json:dump-string (json:load-string "9223372036854775807")))
  ; A value just below the boundary is unaffected.
  (assert-string= "9007199254740991"
                  (json:dump-string (json:load-string "9007199254740991"))))

; THE HIDING MECHANISM.  This is why the defect sat open since 2018: the
; corrupted value still compares = to the integer it was supposed to be, so a
; phylum can read a corrupted identifier, check it against the value it
; expected, match, and carry on.  Nothing signals.
;
; The assertion is deliberately written the "wrong" way round -- it asserts the
; corruption is INVISIBLE -- because that makes it a tripwire.  If the default
; is ever flipped, this test fails and names what changed, instead of the
; change reaching a node quietly.
(test "corruption-is-invisible-by-default"
  (let ([loaded (json:load-string "9007199254740993")])
    ; It equals the integer it was meant to be ...
    (assert= 9007199254740993 loaded)
    ; ... and it equals the DIFFERENT integer it was actually rounded to, so
    ; two distinct documents are indistinguishable once loaded.
    (assert= loaded (json:load-string "9007199254740992"))
    ; The only thing that gives it away is its type.
    (assert-string= "float" (to-string (type loaded)))))

; The opt-in.
(test "exact-integers-round-trip"
  (assert-string= "int" (to-string (type (json:load-string "9007199254740993" :exact-integers true))))
  (assert= 9007199254740993 (json:load-string "9007199254740993" :exact-integers true))
  (assert-string= "9007199254740993"
                  (json:dump-string (json:load-string "9007199254740993" :exact-integers true)))
  (assert-string= "9223372036854775807"
                  (json:dump-string (json:load-string "9223372036854775807" :exact-integers true)))
  (assert-string= "-9223372036854775808"
                  (json:dump-string (json:load-string "-9223372036854775808" :exact-integers true)))
  ; Just below 2^53: an int now, and the same digits either way.
  (assert-string= "9007199254740991"
                  (json:dump-string (json:load-string "9007199254740991" :exact-integers true)))
  ; Nested, which is where real documents keep their identifiers.
  (assert-string= """{"id":9007199254740993}"""
                  (json:dump-string
                    (json:load-string """{"id":9007199254740993}""" :exact-integers true))))

; Under the opt-in the two documents that were indistinguishable above are
; distinguishable, which is the whole point of turning it on.
(test "exact-integers-distinguishes"
  (assert-not (=  (json:load-string "9007199254740993" :exact-integers true)
                 (json:load-string "9007199254740992" :exact-integers true))))

; Anything that cannot be represented fails LOUDLY rather than rounding.
(test "exact-integers-range-error"
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string "9223372036854775808" :exact-integers true)))
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string "123456789012345678901234567890" :exact-integers true)))
  ; ... including from inside a container.
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string """{"a":[9223372036854775808]}""" :exact-integers true))))

; The rule is syntactic: a number written with a fraction or an exponent is
; still a float, exactly as it is by default.
(test "exact-integers-leaves-floats-alone"
  (assert-string= "float" (to-string (type (json:load-string "1.5" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "1.0" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "1e2" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "-0" :exact-integers true))))
  (assert-string= "-0" (json:dump-string (json:load-string "-0" :exact-integers true)))
  (assert-string= "100" (json:dump-string (json:load-string "1e2" :exact-integers true))))

; Malformed input stays catchable as json:syntax-error under the opt-in.  The
; opt-in has to use a streaming decoder, which reports some malformed documents
; differently from json.Unmarshal; an adopter's handler-bind must not quietly
; stop firing.
(test "exact-integers-syntax-errors-stay-catchable"
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "{false:true}" :exact-integers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "" :exact-integers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "1 2" :exact-integers true))))

; :string-numbers still wins when both are set, so a caller that already uses
; it sees no change at all.
(test "string-numbers-takes-precedence"
  (assert-string= "string"
                  (to-string (type (json:load-string "9007199254740993"
                                                     :string-numbers true
                                                     :exact-integers true))))
  (assert-string= "9007199254740993"
                  (json:load-string "9007199254740993"
                                    :string-numbers true
                                    :exact-integers true)))

; The serializer-wide default, and that an explicit keyword still overrides it.
(test "use-exact-integers-default"
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993"))))
  (assert-nil (json:use-exact-integers true))
  (assert-string= "int" (to-string (type (json:load-string "9007199254740993"))))
  (assert-string= "9007199254740993" (json:dump-string (json:load-string "9007199254740993")))
  ; An explicit false still opts back out.
  (assert-string= "float"
                  (to-string (type (json:load-string "9007199254740993" :exact-integers false))))
  (assert-nil (json:use-exact-integers false))
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993")))))

; load-bytes and load-message honour the keyword too.
(test "exact-integers-on-every-load-entry-point"
  (assert= 9007199254740993
           (json:load-bytes (to-bytes "9007199254740993") :exact-integers true))
  (assert= 9007199254740993
           (json:load-message (json:dump-message 9007199254740993) :exact-integers true)))
