; highlights.scm — Syntax highlighting queries for ELPS
;
; Capture names follow the tree-sitter convention:
;   https://tree-sitter.github.io/tree-sitter/syntax-highlighting

; Comments
(comment) @comment

; Strings and raw strings
(string) @string
(raw_string) @string

; Numbers
(integer) @number
(float) @number.float
(octal_integer) @number
(hex_integer) @number

; Keywords (:name)
(keyword) @string.special.symbol

; Nil
(nil) @constant.builtin

; Boolean literals (true/false are symbols in ELPS but semantically booleans)
((symbol) @boolean
 (#any-of? @boolean "true" "false"))

; --- Definition forms ---

; defun — function definition
(defun_form
  operator: (symbol) @keyword
  name: (symbol) @function)

(defun_form
  operator: (symbol) @keyword
  name: (qualified_symbol) @function)

; defmacro — macro definition
(defmacro_form
  operator: (symbol) @keyword
  name: (symbol) @function.macro)

(defmacro_form
  operator: (symbol) @keyword
  name: (qualified_symbol) @function.macro)

; deftype — type definition
(deftype_form
  operator: (symbol) @keyword
  name: (symbol) @type)

(deftype_form
  operator: (symbol) @keyword
  name: (qualified_symbol) @type)

; lambda
(lambda_form
  operator: (symbol) @keyword)

; let forms (let, let*, flet, labels, macrolet)
(let_form
  operator: (symbol) @keyword)

; defconst
(defconst_form
  operator: (symbol) @keyword
  name: (symbol) @constant)

(defconst_form
  operator: (symbol) @keyword
  name: (qualified_symbol) @constant)

; Docstrings in defconst
(defconst_form
  docstring: (string) @string.documentation)

; --- Formals (parameter lists) ---
(formals
  (symbol) @variable.parameter)

; --- Special operators ---
; These are symbols that appear as the first element of a list and are
; recognized as special forms by the ELPS interpreter.
(list . (symbol) @keyword
 (#any-of? @keyword
  "if" "cond" "and" "or" "progn"
  "set" "set!" "export" "use-package" "in-package"
  "handler-bind" "ignore-errors"
  "dotimes" "assert"
  "thread-first" "thread-last"
  "quasiquote" "unquote" "unquote-splicing"))

; --- Built-in function calls ---
; First symbol in a list that is a known builtin gets @function.builtin
(list . (symbol) @function.builtin
 (#any-of? @function.builtin
  "+" "-" "*" "/" "=" "<" "<=" ">" ">=" "mod" "pow" "max" "min"
  "car" "cdr" "rest" "first" "second" "nth" "cons" "list"
  "concat" "append" "append!" "reverse" "slice" "length" "aref"
  "map" "foldl" "foldr" "select" "reject" "zip"
  "sorted-map" "get" "assoc" "assoc!" "dissoc" "dissoc!" "keys" "key?"
  "funcall" "apply" "compose" "flip" "unpack"
  "type" "type?" "nil?" "list?" "sorted-map?" "array?" "bool?"
  "number?" "int?" "float?" "symbol?" "string?" "bytes?" "empty?"
  "true?" "tagged-value?" "vector?"
  "equal?" "all?" "any?" "not"
  "to-string" "to-bytes" "to-int" "to-float" "format-string"
  "string=" "string<" "string<=" "string>" "string>="
  "error" "rethrow"
  "eval" "macroexpand" "macroexpand-1"
  "gensym" "identity"
  "vector" "make-sequence" "insert-index" "insert-sorted"
  "search-sorted" "stable-sort"
  "append-bytes" "append-bytes!"
  "load-string" "load-bytes" "load-file"
  "debug-print" "debug-stack"
  "new" "user-data"))

; --- Prefix forms ---
(quote "'") @operator
(function_quote "#'") @operator
(expr_shorthand "#^") @operator

; Hashbang
(hashbang) @keyword.directive

; --- Qualified symbols ---
; Qualified symbols (package:name) used as function calls
(list . (qualified_symbol) @function.call)

; Qualified symbols in other positions
(qualified_symbol) @variable

; --- Fallback: plain symbols ---
; Function calls: first symbol in a list (lower priority)
(list . (symbol) @function.call)

; All other symbols
(symbol) @variable

; --- Punctuation ---
["(" ")" "[" "]"] @punctuation.bracket
