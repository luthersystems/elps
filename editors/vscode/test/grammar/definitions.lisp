; SYNTAX TEST "source.elps" "Definition forms"

(defun my-func (x y)
;^^^^^ keyword.control.definition.elps
;      ^^^^^^^ entity.name.function.elps
  (+ x y))

(defmacro my-macro (body)
;^^^^^^^^ keyword.control.definition.elps
;         ^^^^^^^^ entity.name.function.macro.elps
  body)

(deftype my-type ()
;^^^^^^^ keyword.control.definition.elps
;        ^^^^^^^ entity.name.type.elps
  ())

(defconst my-const 42)
;^^^^^^^^ keyword.control.definition.elps
;         ^^^^^^^^ variable.other.constant.elps

(lambda (x) (* x x))
;^^^^^^ keyword.control.definition.elps

(let ((x 1) (y 2))
;^^^ keyword.control.definition.elps
  (+ x y))

(let* ((a 1) (b (+ a 1)))
;^^^^ keyword.control.definition.elps
  b)

(flet ((helper (x) (+ x 1)))
;^^^^ keyword.control.definition.elps
  (helper 5))

(labels ((fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
;^^^^^^ keyword.control.definition.elps
  (fact 5))
