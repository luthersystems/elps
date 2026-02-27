; locals.scm — Scope and local variable tracking for ELPS

; --- Scope definitions ---

; defun/defmacro/deftype create a new scope
(defun_form) @local.scope
(defmacro_form) @local.scope
(deftype_form) @local.scope

; lambda creates a new scope
(lambda_form) @local.scope

; let forms create a new scope
(let_form) @local.scope

; --- Definitions ---

; Function/macro/type names are definitions
(defun_form name: (symbol) @local.definition)
(defmacro_form name: (symbol) @local.definition)
(deftype_form name: (symbol) @local.definition)
(defconst_form name: (symbol) @local.definition)

; Parameters in formals are definitions
(formals (symbol) @local.definition)

; let binding names are definitions
(let_binding (symbol) @local.definition)

; --- References ---

; Any symbol is a reference (tree-sitter resolves against definitions)
(symbol) @local.reference
