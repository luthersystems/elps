; Copyright © 2018 The ELPS authors

; The tests for values that contain themselves (issue #390) live in their own
; file rather than in libjson_test.lisp because BenchmarkPackage/$load
; benchmarks parsing and evaluating that file: source added to it is charged to
; a benchmark that is meant to measure the loader, and the CI benchmark gate
; reads the resulting jump as a regression.  Keep this file out of the
; benchmarked corpus; see TestPackageCyclicValue.

(use-package 'testing)

; A value that contains itself has no JSON representation, and before issue
; #390 serializing one killed the host process with a stack overflow that
; recover() could not catch.  json:dump-* now refuses with an ordinary
; condition that handler-bind can catch.
(test "dump-cyclic-value"
  (let ([m (sorted-map)])
    (assoc! m "k" m)
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (json:dump-string m)))
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (json:dump-bytes m))))
  ; Sharing a value is not a cycle: both copies still serialize.
  (let ([x (sorted-map "a" 1)])
    (assert-string= """[{"a":1},{"a":1}]""" (json:dump-string (vector x x)))))
