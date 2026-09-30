; Copyright © 2018 The ELPS authors

(in-package 'test2)

; test1.lisp is in the same directory as this file and the following load-file
; command should succeed (using the included RelativeFileSystemLibrary
; implementation) even when test2.lisp is not loaded from a process working in
; the same directory.
(load-file "test1.lisp")

(defun this-is-a-test2 () "TESTING")
(export 'this-is-a-test2)
