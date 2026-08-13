; Copyright © 2026 The ELPS authors

; The tests for values that contain themselves (issue #393) live in their own
; file rather than in libelpspath_test.lisp, following libjson: a load
; benchmark over the main test file charges any source added to it to a
; measurement meant for the loader.  See TestPackageCyclicValue.

(use-package 'testing)
(use-package 'elpspath)

; append! mutates a container in place, so a program can store a container
; inside itself.  Before issue #393 every elpspath builtin walked such a value
; until the goroutine stack overflowed and the Go runtime killed the host --
; a failure recover() cannot catch, so handler-bind never saw it.  The kernel
; had already been bounded (#391) and rendered the same value as #<cycle>, so
; the process died through the path engine while the printer survived.
(test "cyclic-value-refused"
  (let ([v (vector 1 2)])
    (append! v v)
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (elpspath:? v 0)))
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (elpspath:?set v "k" "v")))
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (elpspath:?del! v 0))))
  (let ([m (sorted-map)])
    (assoc! m "k" m)
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (elpspath:? m "k")))
    (assert-string= "caught"
                    (handler-bind ([error (lambda (_c _) "caught")])
                      (elpspath:?nil m "k"))))
  ; Sharing a value is not a cycle: a DAG is still queried and copied in full.
  (let* ([x (sorted-map "a" 1)]
         [both (vector x x)])
    (assert-equal (vector 1 1) (elpspath:? both '* "a"))
    (assert-equal (vector 1 2) (elpspath:? (elpspath:?set (sorted-map "v" (vector 1 2)) "k" 1) "v"))))
