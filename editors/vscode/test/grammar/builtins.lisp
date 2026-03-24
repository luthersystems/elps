; SYNTAX TEST "source.elps" "Builtin functions"

(+ 1 2)
;^ support.function.elps

(- 3 1)
;^ support.function.elps

(* 2 3)
;^ support.function.elps

(/ 10 2)
;^ support.function.elps

(= a b)
;^ support.function.elps

(< a b)
;^ support.function.elps

(<= a b)
;^^ support.function.elps

(> a b)
;^ support.function.elps

(>= a b)
;^^ support.function.elps

(car xs)
;^^^ support.function.elps

(cdr xs)
;^^^ support.function.elps

(cons 1 xs)
;^^^^ support.function.elps

(list 1 2 3)
;^^^^ support.function.elps

(map f xs)
;^^^ support.function.elps

(concat a b)
;^^^^^^ support.function.elps

(append a b)
;^^^^^^ support.function.elps

(reverse xs)
;^^^^^^^ support.function.elps

(length xs)
;^^^^^^ support.function.elps

(sorted-map :a 1 :b 2)
;^^^^^^^^^^ support.function.elps

(get m :key)
;^^^ support.function.elps

(assoc m :key val)
;^^^^^ support.function.elps

(keys m)
;^^^^ support.function.elps

(funcall f x)
;^^^^^^^ support.function.elps

(apply f args)
;^^^^^ support.function.elps

(not x)
;^^^ support.function.elps

(nil? x)
;^^^^ support.function.elps

(list? x)
;^^^^^ support.function.elps

(string? x)
;^^^^^^^ support.function.elps

(equal? a b)
;^^^^^^ support.function.elps

(to-string x)
;^^^^^^^^^ support.function.elps

(to-int x)
;^^^^^^ support.function.elps

(format-string "hello %s" name)
;^^^^^^^^^^^^^ support.function.elps

(string= a b)
;^^^^^^^ support.function.elps

(symbol= a b)
;^^^^^^^ support.function.elps

(error "boom")
;^^^^^ support.function.elps

(rethrow)
;^^^^^^^ support.function.elps

(eval expr)
;^^^^ support.function.elps

(macroexpand form)
;^^^^^^^^^^^ support.function.elps

(macroexpand-1 form)
;^^^^^^^^^^^^^ support.function.elps

(gensym)
;^^^^^^ support.function.elps

(identity x)
;^^^^^^^^ support.function.elps

(vector 1 2 3)
;^^^^^^ support.function.elps

(debug-print x)
;^^^^^^^^^^^ support.function.elps

(get-default m :key "fallback")
;^^^^^^^^^^^ support.function.elps

(trace expr)
;^^^^^ support.function.elps

(curry-function f 1)
;^^^^^^^^^^^^^^ support.function.elps
