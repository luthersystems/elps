(use-package 'testing)
; mark is bound by the runner's SetupFn in package foo, which lint cannot see.
(test "setup package kept" (assert= 42 mark)) ; nolint:undefined-symbol
