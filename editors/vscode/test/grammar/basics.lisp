; SYNTAX TEST "source.elps" "Basic syntax elements"

; This is a comment
; <------------------ comment.line.semicolon.elps

"hello world"
; <---------- string.quoted.double.elps

"""raw string"""
; <------------- string.quoted.raw.elps

42
; <- constant.numeric.integer.elps

-7
; <- constant.numeric.integer.elps

3.14
; <-- constant.numeric.float.elps

1e10
; <-- constant.numeric.float.elps

2.5e-3
; <---- constant.numeric.float.elps

#xFF
; <-- constant.numeric.hex.elps

#o77
; <-- constant.numeric.octal.elps

:my-keyword
; <-------- constant.other.keyword.elps

()
; <- constant.language.nil.elps

(if true "yes" "no")
;   ^^^^ constant.language.boolean.elps

(if false "yes" "no")
;   ^^^^^ constant.language.boolean.elps

'quoted
; <- keyword.operator.quote.elps

#'func-ref
; <- keyword.operator.function-quote.elps

#^expr-shorthand
; <- keyword.operator.expr-shorthand.elps

(defun f (x &optional y)
;           ^^^^^^^^^ variable.parameter.elps
  x)

(defun g (x &rest ys)
;           ^^^^^ variable.parameter.elps
  x)

(defun h (x &key name)
;           ^^^^ variable.parameter.elps
  name)
