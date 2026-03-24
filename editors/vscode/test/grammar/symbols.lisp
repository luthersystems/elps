; SYNTAX TEST "source.elps" "Symbols and qualified symbols"

my-var
; <--- variable.other.elps

math:sqrt
; <------ variable.other.qualified.elps

(pkg:my-func x)
;^^^^^^^^^^^ entity.name.function.qualified.elps
