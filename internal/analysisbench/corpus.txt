; Fixed benchmark corpus, concatenated from the files named below.

; Source: _examples/oop/oop.lisp
;; package 'oop exports a simple object-oriented type system to serve as an
;; example of how an application might make use of ``deftype'' and build a
;; function dispatch system to implement object methods.
(in-package 'oop)

;; method-table is a nested map of type-name -> method-name -> method-implementation
(set 'method-table (sorted-map))

;; type-name returns the type symbol that type-specifier corresponds to.
;; Symbols are returned unaltered and typedef objects will return the symbol of
;; the defined type.
(defun type-name (type-specifier) ; nolint:unused-function
  (cond ((symbol? type-specifier)
         type-specifier)
        ((type? 'lisp:typedef type-specifier)
         (first (user-data type-specifier)))
        (:else (error 'type-error (format-string "invalid type specifier: {}" (type type-specifier))))))

;; method-name checks whether sym is a valid method name and returns a
;; canonical method name for looking up the named method on a type.
(defun method-name (sym)
  (cond ((not (symbol? sym))
         (error 'type-error (format-string "name is not a symbol: {}" (type sym))))
        ((string= "" (to-string sym))
         (error 'type-error (format-string "invalid name: {}" (to-string sym))))
        ((not (string= ":" (slice 'string (to-string sym) 0 1)))
         (error 'type-error (format-string "invalid name: {}" (to-string sym))))
        (:else sym)))

;; defmethod declares or overwrites a method on a specified type.  The method
;; args must include the method receiver (i.e. self) as the first argument.
(export 'defmethod)
(defmacro defmethod (type-specifier method-name method-args &rest method-body) ; nolint:shadowing
  (cond ((not (symbol? type-specifier))
         (error 'type-error "first argument is not a valid type specifier"))
        ((not (symbol? method-name))
         (error 'type-error "second argument is not a symbol"))
        ((not (list? method-args))
         (error 'type-error "third argument is not a list"))
        ((empty? method-args)
         (error 'type-error "methods must take at least one argument"))
        (:else
          (let* ([type-name (gensym)] ; nolint:shadowing
                 [fn (gensym)]
                 [fname (gensym)]
                 [type-table (gensym)])
            (quasiquote (lisp:let* ([(unquote type-name) (oop:type-name (unquote type-specifier))]
                                    [(unquote fname) (oop:method-name (unquote method-name))]
                                    [(unquote fn) (lisp:lambda (unquote method-args)
                                                    (unquote-splicing method-body))]
                                    [(unquote type-table) (get oop:method-table (unquote type-name))])
                          (lisp:if (lisp:nil? (unquote type-table))
                            (lisp:assoc! oop:method-table
                                         (unquote type-name)
                                         (lisp:sorted-map (unquote method-name) (unquote fn)))
                            (lisp:assoc! (unquote type-table)
                                         (unquote method-name)
                                         (unquote fn)))))))))

;; methods returns a list of methods defined for (type obj).
(export 'methods)
(defun methods (obj)
  (let* ([typ (type obj)]
         [type-table (cond
                       ((equal? 'symbol typ)
                        (get method-table obj))
                       ((equal? 'lisp:typedef typ)
                        (get method-table (first (user-data obj))))
                       (:else
                         (get method-table typ)))])
    (if (nil? type-table)
      '()
      (keys type-table))))

;; get-method returns the function that implements method m on (type obj).
(export 'get-method)
(defun get-method (obj m)
  (let* ([name (method-name m)] ; nolint:unused-variable
         [type-table (get method-table (type obj))])
    (get type-table m)))

;; invoke calls method m on object, passing args as the method's arguments.
(export 'invoke)
(defun invoke (obj m &rest args)
  (let* ([fn (get-method obj m)])
    (if (nil? fn)
      (error 'unknown-method (format-string "method not found for type {}: {}" (type obj) m))
      (apply fn obj args))))

;; method? returns true if (type obj) has implementations for all method names
;; m.
(export 'method?)
(defun method? (obj &rest m)
  (let* ([m (map 'list method-name m)]
         [type-table (get method-table (type obj))])
    (all? #^(key? type-table %) m)))

(defun to-list (obj)
  (invoke (simple-sequence obj) :to-list))

(defun simple-sequence? (obj)
  (method? obj :to-list))

(defun simple-sequence (obj)
  (if (simple-sequence? obj)
    (invoke obj :to-list)
    (error 'type-error (format-string "type is not a simple-sequence: {}" (type obj)))))

;; interface? takes a simple-sequence of methods names and returns true if obj
;; implements all named methods.
(defun interface? (obj m)
  (apply method? obj (to-list m)))

;; Create implementations for the simple-sequence interface for list and array
;; types.
(defmethod 'list :to-list (self) self)
(defmethod 'array :to-list (self)
  (if (vector? self)
    (map 'list identity self)
    (error 'type-error (format-string "multi-dimensional array cannot be converted to a list"))))

;; Define a new complex number type.
(deftype 'complex (real imag)
  (sorted-map :real real :imag imag))

(defmethod complex :real (self)
  (get (user-data self) :real))

(defmethod complex :imag (self)
  (get (user-data self) :imag))

(set 'complex-interface (vector :real :imag))

(defun complex? (obj)
  (interface? obj complex-interface))

;; Tests using the oop:complex type.
(set 'c (new complex 0 1))
(debug-print c)
(assert (= 0 (invoke c :real)))
(assert (= 1 (invoke c :imag)))
(assert (complex? c))
(assert (equal? (list ':imag ':real) (methods c)))
(assert (equal? (list ':imag ':real) (methods complex)))
(assert (equal? (list ':imag ':real) (methods 'oop:complex)))
(assert (nil? (methods 'complex))) ;; nil -- requires qualified symbols

(defun pairs (lis)
  (let* ([p (vector)])
    (if (nil? (foldl (lambda (acc x)
                       (if (= 0 (length acc))
                         (list x)
                         (progn (append! p (list (first acc) x))
                           (list))))
                     '()
                     lis))
      p
      (error 'value-error (format-string "list has uneven length: {}" p)))))

;; struct is container with of a set of named fields (slots to store values)
(deftype 'struct (&rest fields)
  (let* ([data (sorted-map)]
         [field-pairs (pairs fields)])
    (map '()
         (lambda (field-val)
           (assoc! data
                   ;; Field names aren't method names but method-name is fine
                   (method-name (first field-val))
                   (second field-val)))
         field-pairs)
    data))

;; method :var looks up the value of a field
(defmethod struct :var (self name)
  (let* ([data (user-data self)]
         [field (method-name name)])
    (if (key? data field)
      (get data field)
      (error 'struct-field (format-string "unknown struct field: {}" field)))))

;; var looks up a named struct field in the given object and returns its value.
;; obj must either be a struct or it must have a :struct method.  If obj is not
;; a struct then a chain of :struct method calls must yield a struct.
(export 'var)
(defun var (obj name)
  (if (type? struct obj)
    (invoke obj :var name)
    (let* ([fn (get-method obj :struct)])
      (if (nil? fn)
        (error 'type-error (format-string "argument is not a struct: {}" (type obj)))
        (var (fn obj) name)))))

;; rational represents a rational number and wraps a struct with two fields.
(deftype 'rational (n d)
  (cond ((not (int? n)) ; nolint:cond-missing-else
         (error 'type-error (format-string "first argument is not an int: {}" (type n))))
        ((not (int? d))
         (error 'type-error (format-string "second argument is not an int: {}" (type d)))))
  (new struct
       :numer n
       :denom d))

;; method :struct implements the interface desired by ``var''.
(defmethod rational :struct (self) (user-data self))

;; method :to-float converts the rational number to a float.
(defmethod rational :to-float (self)
  (/ (to-float (var self :numer))
     (to-float (var self :denom))))

;; method :mul multiplies self by x and returns a rational result.
(defmethod rational :mul (self x)
  (cond ((type? 'int x)
         (new rational (* x (var self :numer))
              (var self :denom)))
        ((type? rational x)
         (new rational
              (* (var self :numer)
                 (var x :numer))
              (* (var self :denom)
                 (var x :denom))))
        (:else (error 'type-error (format-string "cannot multiply rational with type: {}" (type x))))))

(set 'r (new rational 3 2))
(debug-print r)
(assert (= 3 (var r :numer)))
(assert (= 2 (var r :denom)))
(assert (= 1.5 (invoke r :to-float)))
(assert (= 7.5 (thread-first r
                             (invoke :mul 5)
                             (invoke :to-float))))
(assert (= 0.75 (thread-first r
                              (invoke :mul (new rational 1 2))
                              (invoke :to-float))))

; Source: _examples/sicp/approx.lisp
; Copyright © 2018 The ELPS authors

(in-package 'sicp/approx)

(load-file "stream.lisp")

(use-package 'sicp/stream)
(use-package 'testing)

(defun sqrt-improve (guess x)
  ; average guess and x/guess
  (/ (+ guess (/ x guess)) 2))

(defun sqrt-stream (x)
  ; let initializers cannot capture their own binding. Use an explicitly
  ; recursive function for the delayed tail (docs/lang.md: let vs let*).
  (labels ([guesses (guess)
            (stream-cons guess (guesses (sqrt-improve guess x)))])
    (guesses 1.0)))

(assert-equal '(1.0 1.5 1.4166666666666665)
              (stream-collect (stream-take (sqrt-stream 2) 3)))
(assert-equal '(1.0 2.5)
              (stream-collect (stream-take (sqrt-stream 4) 2)))

(debug-print '(sqrt-stream 2))
(stream-debug (stream-take (sqrt-stream 2) 7))

(defun pi-summands (n)
  (stream-cons (/ 1.0 n)
               (stream-map '- (pi-summands (+ n 2)))))

(defun partial-sums (s)
  (if (stream-null? s)
    the-empty-stream
    (stream-cons (stream-car s)
                 (stream-map '+
                             (partial-sums (stream-cdr s))
                             (stream-repeat (stream-car s))))))

(set 'pi-stream (stream-map #^(* 4 %) (partial-sums (pi-summands 1))))

(debug-print 'pi-stream)
(stream-debug (stream-take pi-stream 7))

(defun square (x) (* x x))
(defun euler-transform (s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(debug-print '(euler-transform pi-stream))
(stream-debug (stream-take (euler-transform pi-stream) 7))

(defun make-tableau (transform s)
  (stream-cons s (make-tableau transform (funcall transform s))))

(defun accelerated-sequence (transform s)
  (stream-map 'stream-car (make-tableau transform s)))

(debug-print '(accelerated-sequence 'euler-transform pi-stream))
(stream-debug (stream-take (accelerated-sequence 'euler-transform pi-stream) 8))

; Source: _examples/sicp/complex.lisp
; Copyright © 2018 The ELPS authors

(in-package 'sicp/complex)
(use-package 'math)
(use-package 'testing)

(export 'square)
(defun square (x)
  (* x x))

(defun pair? (value)
  (and (list? value) (= 2 (length value))))

(export 'attach-tag)
(defun attach-tag (type-tag contents) ; nolint:shadowing
  (list type-tag contents))

(export 'type-tag)
(defun type-tag (datum)
  (if (pair? datum)
    (first datum)
    (error 'argument-error "bad tagged datum" datum)))

(export 'contents)
(defun contents (datum)
  (if (pair? datum)
    (second datum)
    (error 'argument-error "bad tagged datum" datum)))

(defun rectangular? (z) ; nolint:unused-function
  (equal? (type-tag z) 'rectangular))

(defun polar? (z) ; nolint:unused-function
  (equal? (type-tag z) 'polar))

(defun rectangular-real-part (z)
  (first z))

(defun rectangular-imag-part (z)
  (second z))

(defun rectangular-magnitude (z)
  (sqrt (+ (square (rectangular-real-part z))
           (square (rectangular-imag-part z)))))

(defun rectangular-angle (z)
  (atan (rectangular-imag-part z)
        (rectangular-real-part z)))

(defun make-rectangular-from-real-imag (x y)
  (attach-tag 'rectangular (list x y)))

(defun make-rectangular-from-mag-ang (r a) ; nolint:unused-function
  (attach-tag 'rectangular (list (* r (cos a))
                                 (* r (sin a)))))

(defun polar-real-part (z)
  (* (polar-magnitude z) (cos (polar-angle z))))

(defun polar-imag-part (z)
  (* (polar-magnitude z) (sin (polar-angle z))))

(defun polar-magnitude (z)
  (first z))

(defun polar-angle (z)
  (second z))

(defun make-polar-from-real-imag (x y)
  (if (and (= 0 x)
           (= 0 y))
    (attach-tag 'polar (list 0 0))
    (attach-tag 'polar (list (sqrt (+ (square x) (square y)))
                             (atan y x)))))

(defun make-polar-from-mag-ang (r a)
  (if (= 0 r)
    (attach-tag 'polar (list 0 0))
    (attach-tag 'polar (list r a))))

(export 'make-complex-from-real-imag)
(defun make-complex-from-real-imag (x y)
  (make-rectangular-from-real-imag x y))

(export 'make-complex-from-mag-ang)
(defun make-complex-from-mag-ang (r a)
  (make-polar-from-real-imag r a))

(export 'complex-real-part)
(defun complex-real-part (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-real-part (contents z)))
    ((equal? 'polar (type-tag z)) (polar-real-part (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-imag-part)
(defun complex-imag-part (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-imag-part (contents z)))
    ((equal? 'polar (type-tag z)) (polar-imag-part (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-magnitude)
(defun complex-magnitude (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-magnitude (contents z)))
    ((equal? 'polar (type-tag z)) (polar-magnitude (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-angle)
(defun complex-angle (z)
  (cond
    ((equal? 'rectangular (type-tag z)) (rectangular-angle (contents z)))
    ((equal? 'polar (type-tag z)) (polar-angle (contents z)))
    (:else (error 'invalid-argument "argument is not a complex number" z))))

(export 'complex-add)
(defun complex-add (z1 z2)
  (make-complex-from-real-imag (+ (complex-real-part z1)
                                  (complex-real-part z2))
                               (+ (complex-imag-part z1)
                                  (complex-imag-part z2))))

(export 'complex-sub)
(defun complex-sub (z1 z2)
  (make-complex-from-real-imag (- (complex-real-part z1)
                                  (complex-real-part z2))
                               (- (complex-imag-part z1)
                                  (complex-imag-part z2))))

(export 'complex-mul)
(defun complex-mul (z1 z2)
  (make-complex-from-mag-ang (* (complex-magnitude z1)
                                (complex-magnitude z2))
                             (+ (complex-angle z1)
                                (complex-angle z2))))

(export 'complex-div)
(defun complex-div (z1 z2)
  (make-complex-from-mag-ang (/ (complex-magnitude z1)
                                (complex-magnitude z2))
                             (- (complex-angle z1)
                                (complex-angle z2))))

(assert= 0 (complex-angle (make-rectangular-from-real-imag 0 0)))
(assert= 0 (complex-angle (make-rectangular-from-real-imag 1 0)))
(assert= (/ math:pi 2) (complex-angle (make-rectangular-from-real-imag 0 1)))
(assert= (/ math:pi 4) (complex-angle (make-rectangular-from-real-imag 1 1)))
(assert= 1 (complex-magnitude (make-rectangular-from-real-imag 1 0)))
(assert= 1 (complex-magnitude (make-rectangular-from-real-imag 0 1)))
(assert= (sqrt 2) (complex-magnitude (make-rectangular-from-real-imag 1 1)))

(assert= 0 (complex-angle (make-polar-from-mag-ang 1 0)))
(assert= 0 (complex-angle (make-polar-from-mag-ang 0 (/ math:pi 2))))  ; angle ignored
(assert= (/ math:pi 2) (complex-angle (make-polar-from-mag-ang 1 (/ math:pi 2))))
(assert= 0 (complex-angle (make-polar-from-real-imag 1 0)))
(assert= (/ math:pi 2) (complex-angle (make-polar-from-real-imag 0 1)))
(assert= (/ math:pi 4) (complex-angle (make-polar-from-real-imag 1 1)))

; Source: _examples/sicp/diff.lisp
; Copyright © 2018 The ELPS authors

(in-package 'sicp/diff)
(use-package 'testing)

(defun cadr (s)
  (car (cdr s)))
(defun caddr (s)
  (car (cdr (cdr s))))

(defun variable? (e) (symbol? e))
(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(defun number= (a b)
  (and (number? a) (number? b) (= a b)))

(defun sum? (e)
  (and (list? e) (equal? '+ (car e))))
(defun sum-addend (e)
  (cadr e))
(defun sum-augend (e)
  (caddr e))
(defun make-sum (a b)
  (cond
    ((number= 0 a) b)
    ((number= 0 b) a)
    (:else (list '+ a b))))

(defun product? (e)
  (and (list? e) (equal? '* (car e))))
(defun product-multiplier (e)
  (cadr e))
(defun product-multiplicand (e)
  (caddr e))
(defun make-product (a b)
  (cond
    ((number= 0 a) 0)
    ((number= 0 b) 0)
    ((number= 1 a) b)
    ((number= 1 b) a)
    (:else (list '* a b))))

(defun deriv (expression variable)
  (cond
    ((not (variable? variable)) (error 'invalid-argument "second argument is not a variable"))
    ((number? expression) 0)
    ((variable? expression) (if (same-variable? expression variable) 1 0))
    ((sum? expression)
     (make-sum (deriv (sum-addend expression) variable)
               (deriv (sum-augend expression) variable)))
    ((product? expression)
     (let [(e1 (product-multiplier expression))
           (e2 (product-multiplicand expression))]
       (make-sum (make-product e1 (deriv e2 variable))
                 (make-product (deriv e1 variable) e2))))
    (:else (error 'invalid-expression
                  "unable to differentiate expression"
                  expression))))

(assert-equal 1 (deriv '(+ x 3) 'x))
(assert-equal 'y (deriv '(* x y) 'x))
(assert-equal '(+ (* x y) (* y (+ x 3)))
              (deriv '(* (* x y) (+ x 3)) 'x))

; Source: _examples/sicp/scheme-math.lisp
; Copyright © 2018 The ELPS authors

(load-file "sicp.lisp")
(load-file "complex.lisp")

(use-package 'sicp)
(use-package 'sicp/complex)
(use-package 'math)
(use-package 'testing)

;  Don't blow the dispatch table away if we reload the file
(set 'dispatch-table (or (ignore-errors dispatch-table) (sorted-map)))

; NOTE: dispatch-put is deliberately simple for this example: it assumes every
; implementation of an op takes the same number of operand types.
(defun dispatch-put (op-symbol operand-types operator)
  (let ([op-table (or (get dispatch-table op-symbol) (sorted-map))])
    (assoc! dispatch-table op-symbol op-table)
    (labels ([dig (table operand-types value) ; nolint:shadowing
              (cond
                ((nil? operand-types)
                 (error 'nil-type-parameters "type parameters are nil"))
                ((nil? (rest operand-types))
                 (assoc! table (first operand-types) value))
                (:else
                  (let* ([operand-type (first operand-types)]
                         [sub-types (rest operand-types)]
                         [sub-table (or (get table operand-type) (sorted-map))])
                    (assoc! table operand-type sub-table)
                    (dig sub-table sub-types value))))])
      (dig op-table operand-types operator)
      ())))

; NOTE: dispatch-get is deliberately simple for this example: it assumes every
; implementation of an op takes the same number of operand types.
(defun dispatch-get (op-symbol operand-types)
  (let ([op-table (get dispatch-table op-symbol)])
    (labels ([dig (table operand-types) ; nolint:shadowing
              (cond
                ((nil? operand-types)
                 table)
                ((not (sorted-map? table)) ())
                ((symbol? operand-types) (get table operand-types))
                ((not (key? table (first operand-types)))
                 ())
                (:else
                  (let* ([operand-type (first operand-types)]
                         [sub-types (rest operand-types)]
                         [sub-table (get table operand-type)])
                    (dig sub-table sub-types))))])
      (if (nil? op-table)
        ()
        (dig op-table operand-types)))))

(defun dispatch-call (op-symbol operand-types &rest operands)
  (let ([fun (dispatch-get op-symbol operand-types)])
    (if fun
      (apply fun operands)
      (error 'invalid-operands
             "no operator implementation for operands"
             (list op-symbol operand-types)))))

(defun install-rectangular-package ()
  (labels ([real-part (z) (first z)] ; nolint:shadowing
           [imag-part (z) (second z)] ; nolint:shadowing
           [make-from-real-imag (x y) (list x y)] ; nolint:shadowing
           [magnitude (z) ; nolint:shadowing
            (sqrt (+ (square (real-part z))
                     (square (imag-part z))))]
           [angle (z) ; nolint:shadowing
            (let ([x (real-part z)] ; nolint:unused-variable
                  [y (imag-part z)]) ; nolint:unused-variable
              (atan (imag-part z)
                    (real-part z)))]
           [make-from-mag-ang (r a) ; nolint:shadowing
            (if (= 0 r)
              (list 0 0)
              (list (* r (cos a)) (* r (sin a))))]
           [tag (x) (attach-tag 'rectangular x)])
    (dispatch-put 'real-part '(rectangular) real-part)
    (dispatch-put 'imag-part '(rectangular) imag-part)
    (dispatch-put 'magnitude '(rectangular) magnitude)
    (dispatch-put 'angle '(rectangular) angle)
    (dispatch-put 'make-from-real-imag '(rectangular)
                  (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(rectangular)
                  (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun install-polar-package ()
  (labels ([magnitude (z) (first z)] ; nolint:shadowing
           [angle (z) (second z)] ; nolint:shadowing
           [make-from-mag-ang (r a) (list r a)] ; nolint:shadowing
           [real-part (z) ; nolint:shadowing
            (* (magnitude z) (cos (angle z)))]
           [imag-part (z) ; nolint:shadowing
            (* (magnitude z) (sin (angle z)))]
           [make-from-real-imag (x y) ; nolint:shadowing
            (list (sqrt (+ (square (real-part z)) ; nolint:undefined-symbol
                           (square (imag-part z)))) ; nolint:undefined-symbol
                  (atan y x))]
           [tag (x) (attach-tag 'polar x)])
    (dispatch-put 'real-part '(polar) real-part)
    (dispatch-put 'imag-part '(polar) imag-part)
    (dispatch-put 'magnitude '(polar) magnitude)
    (dispatch-put 'angle '(polar) angle)
    (dispatch-put 'make-from-real-imag '(polar)
                  (lambda (x y) (tag (make-from-real-imag x y))))
    (dispatch-put 'make-from-mag-ang '(polar)
                  (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let* ([type-tags (map 'list 'type-tag args)]
         [proc (dispatch-get op type-tags)])
    (cond
      ((nil? proc) (error 'invalid-method "no operation for types" (list op type-tags)))
      ((sorted-map? proc) (error 'invalid-types "invalid type-parameters for operation" (list op type-tags)))
      (:else (apply proc (map 'list 'contents args))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (dispatch-call 'make-from-real-imag '(rectangular) x y))

(defun make-from-mag-ang (r a)
  (dispatch-call 'make-from-mag-ang '(polar) r a))

(trace (install-rectangular-package))
(trace (install-polar-package))

(assert= 0 (angle (make-from-real-imag 0 0)))
(assert= 0 (angle (make-from-real-imag 1 0)))
(assert= (/ math:pi 2) (angle (make-from-real-imag 0 1)))
(assert= (/ math:pi 4) (angle (make-from-real-imag 1 1)))
(assert= 1 (magnitude (make-from-real-imag 1 0)))
(assert= 1 (magnitude (make-from-real-imag 0 1)))
(assert= (sqrt 2) (magnitude (make-from-real-imag 1 1)))

(assert= 0 (angle (make-from-mag-ang 1 0)))
(assert= (/ math:pi 2) (angle (make-from-mag-ang 1 (/ math:pi 2))))
(assert= 1 (real-part (make-from-mag-ang 1 0)))
(assert= 0 (imag-part (make-from-mag-ang 0 1)))
(assert= -1 (imag-part (make-from-mag-ang 1 (/ (* 3 math:pi) 2))))
(assert= -1 (real-part (make-from-mag-ang 1 math:pi)))

(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

(defun install-scheme-number-package ()
  (labels ([tag (x) (attach-tag 'scheme-number x)])
    (dispatch-put 'add '(scheme-number scheme-number)
                  (lambda (x y) (tag (+ x y))))
    (dispatch-put 'sub '(scheme-number scheme-number)
                  (lambda (x y) (tag (- x y))))
    (dispatch-put 'mul '(scheme-number scheme-number)
                  (lambda (x y) (tag (* x y))))
    (dispatch-put 'div '(scheme-number scheme-number)
                  (lambda (x y) (tag (/ x y))))
    (dispatch-put 'make '(scheme-number)
                  (lambda (x) (tag x)))
    'done))

(trace (install-scheme-number-package))

(defun make-scheme-number (x)
  (dispatch-call 'make '(scheme-number) x))

(assert= 2 (contents (add (make-scheme-number 1) (make-scheme-number 1))))
(assert= 5 (contents (sub (make-scheme-number 2) (make-scheme-number -3))))
(assert= 6 (contents (mul (make-scheme-number 2) (make-scheme-number 3))))
(assert= 0.5 (contents (div (make-scheme-number 1) (make-scheme-number 2))))

(defun install-rational-package ()
  (labels ([numer (x) (first x)] ; nolint:shadowing
           [denom (x) (second x)] ; nolint:shadowing
           [make-rat (p q)
            (let ([g (gcd p q)])
              (list (/ p g) (/ q g)))]
           [add-rat (x y)
            (make-rat (+ (* (numer x) (denom y))
                         (* (denom x) (numer y)))
                      (* (denom x) (denom y)))]
           [sub-rat (x y)
            (make-rat (- (* (numer x) (denom y))
                         (* (denom x) (numer y)))
                      (* (denom x) (denom y)))]
           [mul-rat (x y)
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y)))]
           [div-rat (x y)
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y)))]
           [tag (x) (attach-tag 'rational x)])
    (dispatch-put 'make '(rational)
                  (lambda (p q) (tag (make-rat p q))))
    (dispatch-put 'numer '(rational) numer)
    (dispatch-put 'denom '(rational) denom)
    (dispatch-put 'add '(rational rational)
                  (lambda (x y) (tag (add-rat x y))))
    (dispatch-put 'sub '(rational rational)
                  (lambda (x y) (tag (sub-rat x y))))
    (dispatch-put 'mul '(rational rational)
                  (lambda (x y) (tag (mul-rat x y))))
    (dispatch-put 'div '(rational rational)
                  (lambda (x y) (tag (div-rat x y))))
    'dane))

(trace (install-rational-package))

(defun make-rational (p q)
  (dispatch-call 'make '(rational) p q))

(defun numer (r) (apply-generic 'numer r))
(defun denom (r) (apply-generic 'denom r))

(assert= 0 (numer (make-rational 0 10)))
(assert= 1 (denom (make-rational 0 10)))
(assert= 0 (numer (mul (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 0 (numer (div (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 1 (numer (add (make-rational 0 10)
                       (make-rational 1 2))))
(assert= 2 (denom (add (make-rational 0 10)
                       (make-rational 1 2))))

(defun install-complex-package ()
  (labels ([make-from-real-imag (x y) ; nolint:shadowing
            (dispatch-call 'make-from-real-imag 'rectangular x y)]
           [make-from-mag-ang (r a) ; nolint:shadowing
            (dispatch-call 'make-from-mag-ang 'polar r a)]
           [add-complex (z1 z2)
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2)))]
           [sub-complex (z1 z2)
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2)))]
           [mul-complex (z1 z2)
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2)))]
           [div-complex (z1 z2)
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2)))]
           [tag (z) (attach-tag 'complex z)])
    (dispatch-put 'add '(complex complex)
                  (lambda (z1 z2) (tag (add-complex z1 z2))))
    (dispatch-put 'sub '(complex complex)
                  (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (dispatch-put 'mul '(complex complex)
                  (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (dispatch-put 'div '(complex complex)
                  (lambda (z1 z2) (tag (div-complex z1 z2))))
    (dispatch-put 'make-from-real-imag '(complex)
                  (lambda (x y) (make-from-real-imag x y)))
    (dispatch-put 'make-from-mag-ang '(complex)
                  (lambda (r a) (make-from-mag-ang r a)))
    'done))

(defun make-complex-real-imag (x y) (dispatch-call 'make-from-real-imag 'complex x y)) ; nolint:unused-function
(defun make-complex-mag-ang (r a) (dispatch-call 'make-from-mag-ang 'complex r a)) ; nolint:unused-function

(trace (install-complex-package))

; the same tests from above working with 'rectangular and 'polar types now
; using 'complex types and working with multiple layers of abstraction.
(assert= 0 (angle (make-complex-from-real-imag 0 0)))
(assert= 0 (angle (make-complex-from-real-imag 1 0)))
(assert= (/ math:pi 2) (angle (make-complex-from-real-imag 0 1)))
(assert= (/ math:pi 4) (angle (make-complex-from-real-imag 1 1)))
(assert= 1 (magnitude (make-complex-from-real-imag 1 0)))
(assert= 1 (magnitude (make-complex-from-real-imag 0 1)))
(assert= (sqrt 2) (magnitude (make-complex-from-real-imag 1 1)))

; Source: _examples/sicp/sicp.lisp
; Copyright © 2018 The ELPS authors

; basic examples adapted from SICP
(in-package 'sicp)

(use-package 'testing)

(export 'remainder)
(defun remainder (a b)
  (mod a b))

(export 'gcd)
(defun gcd (a b)
  (cond
    ((< a 0) (gcd (math:abs a) b))
    ((< b 0) (gcd a (math:abs b)))
    ((= a 0) b)
    ((= b 0) a)
    (:else (gcd b (remainder a b)))))

(assert= 1 (gcd 5 3))
(assert= 1 (gcd 3 5))
(assert= 2 (gcd 4 2))
(assert= 2 (gcd 2 4))
(assert= 2 (gcd 2 4))
(assert= 20 (gcd 60 80))

(defun square (x) ; nolint:unused-function
  (* x x))

(defun divides? (n d)
  (= 0 (remainder n d)))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond
    ((> (* test-divisor test-divisor) n) n)
    ((divides? n test-divisor) test-divisor)
    (:else (find-divisor n (+ 1 test-divisor)))))

(export 'prime?)
(defun prime? (n)
  (= n (smallest-divisor n)))

(defun accumulate-iter (combiner null-value term a next done? &optional match?)
  (if (done? a)
    null-value
    (let [(curr    (term a))
          (match?  (if (nil? match?) (lambda (_) true) match?))]
      (accumulate-iter combiner
                       (if (match? curr)
                         (funcall combiner null-value curr)
                         (:else null-value))
                       term
                       (next a)
                       next
                       done?
                       match?))))

(defun sum (term a next b) ; nolint:unused-variable
  (accumulate-iter '+ 0
                   term a
                   #^(+ 1 %)
                   #^(> % b)))

(assert= 6 (sum identity 1 #^(+ 1 %) 3))
(assert= 6 (sum identity 1 #^(+ 1 %) 3))

(defun make-rat (numer denom)
  (rat-norm (list numer denom)))

(defun rat-norm (r)
  (cond
    ((< (rat-denom r) 0)
     (rat-norm (list (- (rat-numer r))
                     (- (rat-denom r)))))
    ((> (rat-denom r) 1)
     (let [(c (gcd (rat-numer r) (rat-denom r)))]
       (list (to-int (/ (rat-numer r) c))
             (to-int (/ (rat-denom r) c)))))
    (:else
      r)))

(defun rat-numer (r)
  (first r))

(defun rat-denom (r)
  (second r))

(defun rat-neg (r) ; nolint:unused-function
  (make-rat (- (rat-numer r)) (rat-denom r)))

(defun rat-add (a b)
  (make-rat (+ (* (rat-numer a) (rat-denom b))
               (* (rat-numer b) (rat-denom a)))
            (* (rat-denom a) (rat-denom b))))

(defun rat-sub (a b) ; nolint:unused-function
  (make-rat (- (* (rat-numer a) (rat-denom b))
               (* (rat-numer b) (rat-denom a)))
            (* (rat-denom a) (rat-denom b))))

(defun rat-mul (a b)
  (make-rat (* (rat-numer a) (rat-numer b))
            (* (rat-denom a) (rat-denom b))))

(defun rat-div (a b)
  (make-rat (* (rat-numer a) (rat-denom b))
            (* (rat-denom a) (rat-numer b))))

(defun rat= (a b)
  (and (= (rat-numer a) (rat-numer b))
       (= (rat-denom a) (rat-denom b))))

(defun rat-string (r)
  (format-string "{}/{}" (rat-numer r) (rat-denom r)))

(assert= 1 (rat-numer (make-rat 1 2)))
(assert= 2 (rat-denom (make-rat 1 2)))
(assert= -1 (rat-numer (make-rat -1 2)))
(assert= 2 (rat-denom (make-rat -1 2)))
(assert= -1 (rat-numer (make-rat 1 -2)))
(assert= 2 (rat-denom (make-rat 1 -2)))

(assert (rat= (make-rat 1 2)
              (make-rat 30 60)))
(assert (rat= (make-rat -1 2)
              (make-rat 1 -2)))
(assert (rat= (make-rat 1 2)
              (make-rat 1 2)))

(assert-string= "1/1" (rat-string (make-rat 1 1)))
(assert-string= "1/2" (rat-string (make-rat 1 2)))
(assert-string= "1/2" (rat-string (make-rat -1 -2)))
(assert-string= "-1/2" (rat-string (make-rat -1 2)))
(assert-string= "-1/2" (rat-string (make-rat 1 -2)))
(assert-string= "1/2" (rat-string (make-rat 3 6)))
(assert-string= "-1/2" (rat-string (make-rat -3 6)))
(assert-string= "-1/2" (rat-string (make-rat 3 -6)))

(assert-string= "5/6" (rat-string (rat-add (make-rat 1 2)
                                           (make-rat 1 3))))
(assert-string= "1/6" (rat-string (rat-mul (make-rat 1 2)
                                           (make-rat 1 3))))
(assert-string= "2/1" (rat-string (rat-div (make-rat 1 2)
                                           (make-rat 1 4))))
(defun filter (predicate seq)
  (cond ((nil? seq) ())
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (:else (filter predicate (cdr seq)))))

(defun accumulate (op initial seq)
  (if (nil? seq)
    initial
    (funcall op
             (car seq)
             (accumulate op initial (cdr seq)))))

; SICP intentionally reimplements map using accumulate for teaching.
(defun map (p seq) ; nolint:builtin-shadowing
  (accumulate (lambda (x ys) (cons (p x) ys))
              ()
              seq))

; SICP intentionally reimplements append using accumulate for teaching.
(defun append (lis1 lis2) ; nolint:builtin-shadowing
  (accumulate 'cons lis2 lis1))

; SICP intentionally reimplements length using accumulate for teaching.
(defun length (seq) ; nolint:unused-function,builtin-shadowing
  (accumulate '+ 0 seq))

(assert-equal '(2 4 6 8) (map #^(* 2 %) '(1 2 3 4)))
(assert-equal '(1 2 3 4) (append '(1) '(2 3 4)))

(defun horner-eval (x coefficient-list)
  (accumulate (lambda (a higher) (+ a (* x higher)))
              0.0
              coefficient-list))

(assert= 7 (horner-eval 2 (list 1 3)))
(assert= 79 (horner-eval 2 (list 1 3 0 5 0 1)))

(defun fold-right (op initial seq) (accumulate op initial seq))

(defun fold-left (op initial seq)
  (if (nil? seq)
    initial
    (fold-left op
               (funcall op initial (car seq))
               (cdr seq))))

(assert= 1.5 (fold-right / 1 (list 1 2 3)))
(assert= (/ 1 6) (fold-left / 1 (list 1 2 3)))
(assert-equal (list 1 (list 2 (list 3 ()))) (fold-right 'list () (list 1 2 3)))
(assert-equal (list (list (list () 1) 2) 3) (fold-left 'list () (list 1 2 3)))

(defun flatmap (proc seq)
  (accumulate 'append () (map proc seq)))

(defun enumerate-tree (tree)
  (let [(enumerate-subtrees (lambda (tree) ; nolint:shadowing
                              (cond ((nil? tree) ())
                                    ((not (list? tree)) (list tree))
                                    (:else (enumerate-tree tree)))))]
    (flatmap enumerate-subtrees tree)))

(assert-equal '(1 2 3 4 5) (enumerate-tree (list 1 (list 2 3) (list 4 (list 5)))))

; inclusive enumeration
(defun enumerate-interval (a b)
  (if (> a b)
    ()
    (cons a (enumerate-interval (+ 1 a) b))))

(defun unique-pairs (a b)
  (flatmap (lambda (i) (map #^(list i %)
                            (enumerate-interval (+ i 1) b)))
           (enumerate-interval a (- b 1))))

(assert-equal (list (list 1 2) (list 1 3) (list 2 3)) (unique-pairs 1 3))

(defun prime-sum? (pair)
  (prime? (+ (first pair) (second pair))))

(defun prime-sum-pairs (n)
  (filter prime-sum?
          (unique-pairs 1 n)))

(assert-equal (list (list 1 2)
                    (list 1 4)
                    (list 1 6)
                    (list 2 3)
                    (list 2 5)
                    (list 3 4)
                    (list 5 6))
              (prime-sum-pairs 6))

(defun ordered-triples (a b)
  (flatmap (lambda (i) (map #^(cons i %) (unique-pairs (+ i 1) b)))
           (enumerate-interval a (- b 2))))

(assert-equal () (ordered-triples 1 2))
(assert-equal (list (list 1 2 3)) (ordered-triples 1 3))
(assert-equal (list (list 1 2 3)
                    (list 1 2 4)
                    (list 1 3 4)
                    (list 2 3 4))
              (ordered-triples 1 4))

; Source: _examples/sicp/stream.lisp
; Copyright © 2018 The ELPS authors

(in-package 'sicp/stream)

(load-file "sicp.lisp")

(use-package 'sicp)
(use-package 'testing)

(export 'delay)
(defmacro delay (expr) ; nolint:shadowing
  (let ([valsym (gensym)]
        [funsym (gensym)])
    (quasiquote (progn
                  (let* ([(unquote valsym) ()]
                         [(unquote funsym) (lambda ()
                                             (set! (unquote valsym) (unquote expr)))])
                    (lambda ()
                      (funcall (unquote funsym))
                      (set! (unquote funsym) #^())
                      (unquote valsym)))))))

(let ([x 0])
  (let ([f (delay (set! x (+ x 1)))])
    (f)
    (f)
    (assert= 1 x)))

(export 'stream-concat)
(defun stream-concat (&rest s)
  (cond ((nil? s) the-empty-stream)
        ((stream-null? (car s)) (apply stream-concat (cdr s)))
        (:else (stream-cons (stream-car (car s))
                            (apply stream-concat
                                   (stream-cdr (car s))
                                   (cdr s))))))

(export 'stream-flatmap)
(defun stream-flatmap (proc s)
  (if (stream-null? s)
    the-empty-stream
    (stream-concat (funcall proc (stream-car s))
                   (stream-flatmap proc (stream-cdr s)))))

(export 'stream-repeat)
(defun stream-repeat (x)
  (stream-cons x (stream-repeat x)))

(export 'stream-collect)
(defun stream-collect (s)
  (if (stream-null? s)
    ()
    (cons (stream-car s) (stream-collect (stream-cdr s)))))

(export 'stream-ref)
(defun stream-ref (s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(export 'stream-take)
(defun stream-take (s n)
  (if (<= n 0)
    the-empty-stream
    (stream-cons (stream-car s)
                 (stream-take (stream-cdr s) (- n 1)))))

(export 'stream-drop)
(defun stream-drop (s n)
  (if (<= n 0) s (stream-drop (stream-cdr s) (- n 1))))

(export 'stream-slice)
(defun stream-slice (s start end)
  (stream-take (stream-drop s start) (- end start)))

(export 'stream-map)
(defun stream-map (proc &rest s)
  (if (any? 'stream-null? s)
    the-empty-stream
    (stream-cons (apply proc (map 'list 'stream-car s))
                 (apply 'stream-map proc (map 'list stream-cdr s)))))

(export 'stream-for-each)
(defun stream-for-each (proc s)
  (if (stream-null? s)
    'done
    (progn
      (funcall proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(export 'stream-debug)
(defun stream-debug (s)
  (stream-for-each 'debug-print s))

(export 'stream-cons)
(defmacro stream-cons (a b)
  (quasiquote (list (unquote a) (delay (unquote b)))))

(export 'the-empty-stream)
(set 'the-empty-stream ())
(export 'stream-null?)
(defun stream-null? (s) (nil? s))
(export 'stream-car)
(defun stream-car (s) (first s))
(export 'stream-cdr)
(defun stream-cdr (s) (funcall (second s)))

(defun stream-enumerate-interval (low &optional high)
  (if (and high (> low high))
    the-empty-stream
    (stream-cons low (stream-enumerate-interval (+ low 1) high))))

(assert= 10 (stream-ref (stream-enumerate-interval 0) 10))
(assert= 100 (stream-ref (stream-enumerate-interval 0) 100))
(set 'finite-stream (stream-enumerate-interval 0 10))
(assert= 5 (stream-ref (stream-enumerate-interval 0) 5))
(assert= 5 (stream-ref (stream-enumerate-interval 0) 5))

(export 'stream-filter)
(defun stream-filter (pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((funcall pred (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (:else (stream-filter pred (stream-cdr s)))))

(set 'prime-stream (stream-filter 'prime? (stream-enumerate-interval 3 100)))
(assert-equal '(3 5 7 11) (stream-collect (stream-take prime-stream 4)))
(assert-equal '(7 11 13 17 19) (stream-collect (stream-slice prime-stream 2 7)))

(set 'fibs (stream-cons 1 (stream-cons 1 (stream-map '+ fibs (stream-cdr fibs)))))
(assert-equal '(1 1 2 3 5) (stream-collect (stream-take fibs 5)))

(assert-equal '(1 1 2 2 2 3 3 3 3)
              (stream-collect (stream-concat (stream-take (stream-repeat 1) 2)
                                             (stream-take (stream-repeat 2) 3)
                                             (stream-take (stream-repeat 3) 4))))

(assert-equal '(1 2 2 3 3 3)
              (stream-collect
                (stream-flatmap (lambda (n) (stream-take (stream-repeat n) n))
                                (stream-take (stream-enumerate-interval 1 1000) 3))))

(defun interleave (s1 s2)
  (if (stream-null? s1)
    s2
    (stream-cons (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(set 'positive-integers (stream-cons 1 (stream-map #^(+ 1 %) positive-integers)))

(defun pairs (s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))

(set 'upper-integer-pairs (pairs positive-integers positive-integers))

(assert-equal '('(1 1)
                '(1 2)
                '(2 2)
                '(1 3)
                '(2 3)
                '(1 4)
                '(3 3)
                '(1 5)
                '(2 4)) ; this order sucks
              (stream-collect (stream-take upper-integer-pairs 9)))

(defun stream-fold (proc z s)
  (if (stream-null? s)
    z
    (stream-fold proc (funcall proc z (stream-car s)) (stream-cdr s))))

(defun partial-streams (s)
  (stream-map 'stream-take (stream-repeat s) positive-integers))

(assert-equal '('(1)
                '(1 2)
                '(1 2 3)
                '(1 2 3 4)
                '(1 2 3 4 5)
                '(1 2 3 4 5 6)
                '(1 2 3 4 5 6 7)
                '(1 2 3 4 5 6 7 8)
                '(1 2 3 4 5 6 7 8 9))
              (stream-collect (stream-take (stream-map 'stream-collect (partial-streams positive-integers))  9)))

(defun better-pairs-streams (s t)
  (stream-map (lambda (ps ti)
                (stream-map #^(list % ti) ps))
              (partial-streams s)
              t))

(defun better-pairs (s t)
  ; a function like the external stream-concat but it operates on a potentially
  ; infinite stream-of-streams instead of a finite list of stream arguments.
  (labels ([stream-concat (sos) ; nolint:shadowing
            (cond
              ((stream-null? sos) the-empty-stream)
              ((stream-null? (stream-car sos)) (stream-concat (stream-cdr sos)))
              (:else
                (stream-cons (stream-car (stream-car sos))
                             (stream-concat (stream-cons (stream-cdr (stream-car sos))
                                                         (stream-cdr sos))))))])
    (stream-concat (better-pairs-streams s t))))

(assert-equal '('(1 1)
                '(1 2)
                '(2 2)
                '(1 3)
                '(2 3)
                '(3 3)
                '(1 4)
                '(2 4)
                '(3 4)) ; this order is good 👍
              (stream-collect (stream-take (better-pairs positive-integers positive-integers) 9)))

; Source: _examples/user-defined-types/address-book.lisp
(set 'address-book (sorted-map))

(defun make-contact-info (name phone)
  (sorted-map "name" name "phone" phone))

(defun create-contact! (name phone)
  (assoc! address-book name (make-contact-info name phone)))

(defun say-hello (contact)
  (debug-print (format-string "Hello, {}" (get contact "name"))))

(create-contact! "Alice" "+1-555-555-5555")
(say-hello (get address-book "Alice"))
;; "Hello, Alice"

(say-hello (sorted-map "name" "Bob"))
;; "Hello, Bob"

(say-hello (sorted-map "Name" "Carol"))
;; "Hello, ()"

; Source: _examples/user-defined-types/contact-info_solved.lisp
(deftype contact-info (name phone)
  (sorted-map "name" name "phone" phone))

(defun contact-info? (obj) (type? contact-info obj))

(defun contact-name (contact)
  (if (contact-info? contact)
    (get (user-data contact) "name")
    (error 'type-error "argument is not contact-info")))

(defun say-hello (contact)
  (debug-print (format-string "Hello, {}" (contact-name contact))))

(say-hello (new contact-info "Alice" "+1-555-555-5555"))
;; "Hello, Alice"

;; (say-hello (sorted-map "name" "Bob"))
;; type-error: argument is not contact-info

; Source: _examples/user-defined-types/option_solved.lisp
(in-package 'option)
(export 'x)
(set 'x 1)
(deftype none ())
(defun nothing () (new none))
(defun nothing? (v) (type? none v))

(deftype some (v) v)
(defun something (v) (new some v))
(defun something? (v) (type? some v))

(defun get-something (v)
  (if (something? v)
    (user-data v)
    (error 'type-error "argument is not something")))

;(map 'something fn v)
(defun something-map (fn v)
  (something (funcall fn (get-something v))))

(export 'optional?)
(defun optional? (v) (or (nothing? v)
                         (something? v)))

(export 'lookup)
(defun lookup (m k) ; nolint:shadowing
  """
  returns an optional which contains the value of `k` in `m`.

  returns none if k is not m.

  also does other stuff?
  """
  (if (key? m k)
    (something (get m k))
    (nothing)))

(set 'm (sorted-map "name" "anya"
                    "email" ()))
(debug-print (equal? (get m "email") (get m "birthdate")))
(debug-print (lookup m "name"))
(debug-print (lookup m "email"))
(debug-print (lookup m "birthdate"))
(debug-print (equal? (lookup m "email") (lookup m "birthdate")))

;(map 'optional fn v)
(defun optional-map (fn v)
  (cond ((nothing? v) v)
        ((something? v) (something-map fn v))
        (:else (error 'type-error "argument is not an option"))))

(debug-print (optional-map string:uppercase (something "abc")))
(debug-print (optional-map string:uppercase (nothing)))
(defun send-sms (phone msg) ; nolint:unused-variable
  (error 'unimplemented "I can't send sms"))
(optional-map #^(send-sms % "hello")
              (lookup m "phone"))

; Source: _examples/user-defined-types/phone_solved.lisp
(use-package 'regexp)

(let ([phone-re (regexp-compile "^[+][0-9]+ ([0-9]|-| )+$")])
  (defun phone-ok? (str) (and (string? str) (regexp-match? phone-re str))))

(deftype phone-number (str)
  (if (phone-ok? str) str (error 'format-error "invalid phone number"))) ; nolint:undefined-symbol

(debug-print (new phone-number "+1 5555555"))
;; {user:phone-number "+1 555 555 5555"}

;; (debug-print (new phone-number "+1 5555ABC"))
;; (debug-print (new phone-number 15555555))
;; format-error: invalid phone number

; Source: lisp/lisplib/libelpspath/libelpspath_cycle_test.lisp
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

; Source: lisp/lisplib/libelpspath/libelpspath_test.lisp
(use-package 'elpspath)
(use-package 'testing)

;;; ---- positional-arg path operations ----

(test-let "? simple key"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? val "hello") "world")))

(test-let "? nested key"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (assert (string= (elpspath:? val "a" "b") "world")))

(test-let "? array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (assert (string= (elpspath:? val "items" 0) "a")))

(test-let "? negative index"
  ((val (vector "a" "b" "c")))
  (assert (string= (elpspath:? val -1) "c")))

(test-let "? iterator"
  ((val (vector (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert-equal (vector 1 2) (elpspath:? val '* "a")))

(test-let "? range"
  ((val (vector "a" "b" "c" "d")))
  (assert-equal (vector "b" "c") (elpspath:? val '(range 1 3))))

; The open-ended slice: (range from) with no end runs to the end of the
; array (issue #563). The end is resolved against the input at evaluation
; time, so the same path step gives the right answer for arrays of
; different lengths -- which the two-argument form cannot express.
(test-let "? range with an implicit end"
  ((val (vector "a" "b" "c" "d")))
  (assert-equal (vector "b" "c" "d") (elpspath:? val '(range 1)))
  (assert-equal (vector "a" "b" "c" "d") (elpspath:? val '(range 0)))
  (assert-equal (vector "c" "d") (elpspath:? val '(range -2)))
  (assert-equal (vector) (elpspath:? val '(range 4))))

; One path value, two inputs of different lengths.
(test "? range with an implicit end tracks the input length"
  (let ([short (vector 1 2)]
        [long (vector 1 2 3 4 5)])
    (assert-equal (vector 2) (elpspath:? short '(range 1)))
    (assert-equal (vector 2 3 4 5) (elpspath:? long '(range 1)))))

; Every rangePath operation passes implicitTo through, not just Get, so
; the mutating ops take the open form as well.
(test-let "?set! range with an implicit end"
  ((val (vector 1 2 3 4)))
  (elpspath:?set! val '(range 2) (vector 90 91))
  (assert-equal (vector 1 2 90 91) val))

; A range set is a SPLICE, not an element-wise assignment: the replacement
; may be any length and the sequence grows or shrinks to suit. That is easy
; to assume otherwise from the equal-length case above, which is the only
; one this file used to carry, so all three relative lengths are here.
(test "?set over an implicit end splices rather than matching lengths"
  (let ([v (vector 1 2 3 4 5)])
    ; shorter than the window it replaces
    (assert-equal (vector 1 91) (elpspath:?set v '(range 1) (vector 91)))
    ; equal
    (assert-equal (vector 1 2 3 91 92) (elpspath:?set v '(range 3) (vector 91 92)))
    ; longer
    (assert-equal (vector 1 2 3 4 91 92 93 94)
                  (elpspath:?set v '(range 4) (vector 91 92 93 94)))
    ; empty: the window is removed and nothing takes its place
    (assert-equal (vector 1 2) (elpspath:?set v '(range 2) (vector)))
    ; the copying form leaves the source alone through all of it
    (assert-equal (vector 1 2 3 4 5) v)))

; from == n names an empty window at the very end, so the splice is a pure
; append -- the one spelling that adds elements without removing any.
(test "?set at the end of an implicit range appends"
  (assert-equal (vector 1 2 3 91 92)
                (elpspath:?set (vector 1 2 3) '(range 3) (vector 91 92))))

; A negative from counts back from the end and is resolved BEFORE the
; implicit end is filled in, so the two rewrites have to compose.
(test "?set with a negative from and an implicit end"
  (assert-equal (vector 1 2 3 91 92 93)
                (elpspath:?set (vector 1 2 3 4 5) '(range -2) (vector 91 92 93))))

; Past the end is an error rather than an append. ignore-errors yields nil
; on a raise, and a successful ?set here would yield a vector, so nil means
; the raise happened.
(test "?set past the end of an implicit range raises"
  (assert-nil (ignore-errors
                (elpspath:?set (vector 1 2 3) '(range 9) (vector 91)))))

(test-let "?del range with an implicit end"
  ((val (vector 1 2 3 4)))
  (assert-equal (vector 1 2) (elpspath:?del val '(range 2)))
  (assert-equal (vector 1 2 3 4) val))

(test-let "?nil range with an implicit end"
  ((val (vector 1 2 3 4)))
  (assert-equal (vector 1 2 () ()) (elpspath:?nil val '(range 2)))
  (assert-equal (vector 1 2 3 4) val))

; Either side of the accepted 1-or-2 is still an error. ignore-errors
; yields nil on a raise, and a successful ? with a range step over a
; non-empty vector always yields a vector, so nil here means the raise
; happened -- the accepted arities in the same test keep it from passing
; vacuously.
(test-let "? range arity is 1 or 2"
  ((val (vector "a" "b" "c")))
  (assert-not-nil (ignore-errors (elpspath:? val '(range 1))))
  (assert-not-nil (ignore-errors (elpspath:? val '(range 1 2))))
  (assert-nil (ignore-errors (elpspath:? val '(range))))
  (assert-nil (ignore-errors (elpspath:? val '(range 0 1 2)))))

(test-let "? root"
  ((val (sorted-map "hello" "world")))
  (assert (string= (elpspath:? (elpspath:? val) "hello") "world")))

(test-let "?set! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?set! val "hello" "42")
  (assert (string= (elpspath:? val "hello") "42")))

(test-let "?set! nested"
  ((val (sorted-map "a" (sorted-map "b" "world"))))
  (elpspath:?set! val "a" "b" 23)
  (assert (= (elpspath:? val "a" "b") 23)))

(test-let "?set! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?set! val "items" 0 "x")
  (assert (string= (elpspath:? val "items" 0) "x")))

(test-let* "?set copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?set val "hello" "42")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (string= (elpspath:? new-val "hello") "42")))

(test-let* "?set nested copy"
  ((val (sorted-map "a" (sorted-map "b" "world")))
   (new-val (elpspath:?set val "a" "b" 23)))
  (assert (string= (elpspath:? val "a" "b") "world"))
  (assert (= (elpspath:? new-val "a" "b") 23)))

(test-let "?del! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?del! val "hello")
  (assert (empty? val)))

(test-let "?del! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?del! val "items" 1)
  (assert-equal (vector "a" "c") (elpspath:? val "items")))

(test-let* "?del copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?del val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert (empty? new-val)))

(test-let "?nil! simple key"
  ((val (sorted-map "hello" "world")))
  (elpspath:?nil! val "hello")
  (assert-equal () (elpspath:? val "hello")))

(test-let "?nil! array index"
  ((val (sorted-map "items" (vector "a" "b" "c"))))
  (elpspath:?nil! val "items" 1)
  (assert-equal (vector "a" () "c") (elpspath:? val "items")))

(test-let* "?nil copy"
  ((val (sorted-map "hello" "world"))
   (new-val (elpspath:?nil val "hello")))
  (assert (string= (elpspath:? val "hello") "world"))
  (assert-equal () (elpspath:? new-val "hello")))

(test-let "? list support"
  ((val (sorted-map "hello" (list "world")))
   (nested-val (list (sorted-map "a" 1) (sorted-map "a" 2))))
  (assert (string= (elpspath:? val "hello" 0) "world"))
  (assert-equal (list 1 2) (elpspath:? nested-val '* "a")))

; The package's own documentation example (issue #395).  Before copyMap was
; made deep, the write through the redacted copy reached the patient record
; it was supposed to leave alone.
(test-let* "?nil copy is deep"
  ((patient (sorted-map "ssn" "123" "address" (sorted-map "city" "London")))
   (redacted (elpspath:?nil patient "ssn")))
  (elpspath:?set! redacted "address" "city" "REDACTED")
  (assert (string= "REDACTED" (elpspath:? redacted "address" "city")))
  (assert (string= "London" (elpspath:? patient "address" "city"))))

; The structural variant: ?del! through a ?set copy must not restructure the
; source's array or rewrite its dims.
(test-let* "?set copy is deep"
  ((src (sorted-map "arr" (vector 1 2 3)))
   (cp (elpspath:?set src "tag" "x")))
  (elpspath:?del! cp "arr" 0)
  (assert-equal (vector 2 3) (elpspath:? cp "arr"))
  (assert-equal (vector 1 2 3) (elpspath:? src "arr")))

; A copy must not demote a quoted list to an s-expression: the quote flag is
; part of the value, and an unquoted LSExpr is an expression rather than a
; list.
(test-let* "copy preserves quoting"
  ((src (sorted-map "l" '(1 2 3)))
   (cp (elpspath:?set src "k" "v")))
  (assert-equal '(1 2 3) (elpspath:? cp "l"))
  (assert-equal '(99 2 3) (elpspath:?set '(1 2 3) 0 99)))

; The range getter's view must not carry the source's spare capacity, or an
; (append! ...) into that capacity writes through to the source -- issues
; #369 and #373.  The kernel settled that class by clamping every sequence
; view where it is produced; this asserts rangePath.Get is clamped with it,
; side by side with the kernel producer it has to match.
(test-let* "range view does not alias through spare capacity"
  ((src (vector 1 2 3 4 5))
   (view (elpspath:? src '(range 0 3))))
  (append! view 99)
  (assert-equal (vector 1 2 3 99) view)
  (assert-equal (vector 1 2 3 4 5) src))

; The control: the kernel's own producer, whose answer this one now matches.
; If this arm ever goes red the settlement moved and the clamp above should
; move with it, not be dropped silently.
(test-let* "kernel slice view does not alias either"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 3)))
  (append! view 99)
  (assert-equal (vector 1 2 3 99) view)
  (assert-equal (vector 1 2 3 4 5) src))

;;; ---- issue #471: a delete through a view must not touch the source ----
;;;
;;; A view is an ordinary array LVal whose cells are a window onto a longer
;;; sequence, so the mutating builtins accept one and cannot tell.  The two
;;; deleteMutate paths used to compact IN PLACE, shifting the tail left
;;; through the aliased source's own backing array.  The view's answer came
;;; out right -- a left shift copies before it overwrites -- and only the
;;; source was wrecked, which is why nothing caught it.
;;;
;;; The source cannot shrink, so there is no "correct" amount for it to
;;; change: the requirement is that it does not change at all.

(test-let* "?del! index through a kernel slice view leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 3)))
  (elpspath:?del! view 0)
  (assert-equal (vector 2 3) view)
  (assert-equal (vector 1 2 3 4 5) src))

(test-let* "?del! range through a kernel slice view leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 3)))
  (elpspath:?del! view '(range 0 1))
  (assert-equal (vector 2 3) view)
  (assert-equal (vector 1 2 3 4 5) src))

; The other producer of a view in the tree is this package's own range Get,
; and it reaches the same defect by a route that never mentions `slice`.
(test-let* "?del! through elpspath's own range view leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (view (elpspath:? src '(range 0 3))))
  (elpspath:?del! view 0)
  (assert-equal (vector 2 3) view)
  (assert-equal (vector 1 2 3 4 5) src))

; A view does not have to be anchored at 0, and the shift landed wherever the
; window did: this arm reported (vector 1 2 4 5 5) before the fix.
(test-let* "?del! through an offset view leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 2 5)))
  (elpspath:?del! view 0)
  (assert-equal (vector 4 5) view)
  (assert-equal (vector 1 2 3 4 5) src))

; A full-length view aliases just as completely as a partial one; the window
; being the whole sequence is not the same as owning it.  Reported
; (vector 2 3 4 5 5) before the fix.
(test-let* "?del! through a full-length view leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 5)))
  (elpspath:?del! view 0)
  (assert-equal (vector 2 3 4 5) view)
  (assert-equal (vector 1 2 3 4 5) src))

; A view reached through a document, rather than bound to a variable the
; caller thinks of as a view.
(test-let* "?del! through a view stored in a document leaves the source alone"
  ((src (vector 1 2 3 4 5))
   (doc (sorted-map "v" (slice 'vector src 0 3))))
  (elpspath:?del! doc "v" 0)
  (assert-equal (vector 2 3) (elpspath:? doc "v"))
  (assert-equal (vector 1 2 3 4 5) src))

; The controls that separate #471 from what is expected and from what was
; already fixed.  These are GUARDS: they pass both before and after the fix,
; and exist so the semantics around it are re-decided rather than drifted.
;
; Assigning an element through a view is ordinary aliasing and is documented:
; setMutate does cells[index] = newIn, and a view shares its elements.
(test-let* "?set! at an index through a view does reach the source"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 3)))
  (elpspath:?set! view 0 97)
  (assert-equal (vector 97 2 3) view)
  (assert-equal (vector 97 2 3 4 5) src))

; The range splice was the same defect and was fixed earlier; it builds its
; result in a slice it allocates.
(test-let* "?set! range splice through a view does not reach the source"
  ((src (vector 1 2 3 4 5))
   (view (slice 'vector src 0 3)))
  (elpspath:?set! view '(range 0 1) (vector 90 91))
  (assert-equal (vector 90 91 2 3) view)
  (assert-equal (vector 1 2 3 4 5) src))

;;; ---- parse-path: a string path converted to positional steps ----
;;;
;;; The point of the conversion is that the steps apply straight into the ?
;;; family, so a path that arrived as a string can be converted once and
;;; then used many times without re-parsing.

(test "parse-path renders each grammar form as a step"
  (assert-equal '() (elpspath:parse-path "."))
  (assert-equal '("a") (elpspath:parse-path ".a"))
  (assert-equal '("a" "b") (elpspath:parse-path ".a.b"))
  (assert-equal '("first name") (elpspath:parse-path ".[\"first name\"]"))
  (assert-equal '("a" 0) (elpspath:parse-path ".a[0]"))
  (assert-equal '("a" -1) (elpspath:parse-path ".a[-1]"))
  (assert-equal '("a" '(range 1 3)) (elpspath:parse-path ".a[1:3]"))
  (assert-equal '("a" '(range 1)) (elpspath:parse-path ".a[1:]")))

(test-let* "parse-path steps apply into the ? family"
  ((obj (sorted-map "items" (vector (sorted-map "id" 1) (sorted-map "id" 2) (sorted-map "id" 3)))))
  (assert-equal (vector 1 2 3) (apply elpspath:? (cons obj (elpspath:parse-path ".items[].id"))))
  (assert-equal 1 (apply elpspath:? (cons obj (elpspath:parse-path ".items[0].id"))))
  (assert-equal (vector (sorted-map "id" 2) (sorted-map "id" 3))
                (apply elpspath:? (cons obj (elpspath:parse-path ".items[1:]"))))
  ; the identity selector yields no steps, and applying none is the identity
  (assert-equal obj (apply elpspath:? (cons obj (elpspath:parse-path ".")))))

(test-let* "parse-path steps apply into a mutating operation"
  ((obj (sorted-map "items" (vector (sorted-map "id" 1) (sorted-map "id" 2)))))
  (apply elpspath:?set! (concat 'list (list obj) (elpspath:parse-path ".items[0].id") (list 99)))
  (assert-equal 99 (elpspath:? obj "items" 0 "id")))

; A raise and a successful empty result are BOTH () under ignore-errors, and
; () is what the identity selector legitimately returns -- so asserting nil
; here would pass whether parse-path raised or silently returned no steps.
; No steps is the IDENTITY path, so that difference is the safety property:
; a swallowed error would turn a malformed selector into "the whole
; document" for the ?set idiom the docstring recommends. The sentinel
; separates the two cases; TestBuiltinParsePathRejectsBadSelector is the
; same property in Go, where the error type is directly observable.
(test "parse-path raises on a selector the string operations reject"
  (let ([tried (lambda (sel) (ignore-errors (elpspath:parse-path sel) 'parsed))])
    (assert-nil (funcall tried ""))
    (assert-nil (funcall tried "a"))
    (assert-nil (funcall tried ".["))
    (assert-nil (funcall tried ".my-key"))
    ; and the sentinel really does come back when parsing succeeds, so the
    ; assertions above cannot pass by the lambda always returning nil
    (assert-equal 'parsed (funcall tried "."))
    (assert-equal 'parsed (funcall tried ".a"))))

(test "parse-path requires a string, not a symbol that looks like one"
  ; LSymbol also carries a string payload and .a is a legal elps symbol, so
  ; without the type check a quoted symbol parses as though it were the
  ; selector string.
  (let ([tried (lambda (sel) (ignore-errors (elpspath:parse-path sel) 'parsed))])
    (assert-nil (funcall tried '.a))
    (assert-nil (funcall tried 0))
    (assert-nil (funcall tried ()))))

; The docstring is the ONLY lisp-facing documentation of this string
; grammar, so the traps it names are pinned here rather than left to rot.
; A key syntax rule nobody can see is a key syntax rule nobody follows.
(test "parse-path key syntax matches what the docstring promises"
  (let ([tried (lambda (sel) (ignore-errors (elpspath:parse-path sel) 'parsed))])
    ; a bare .key is [A-Za-z_][A-Za-z_0-9]* only, so kebab-case and
    ; non-ASCII keys must be bracketed and quoted
    (assert-nil (funcall tried ".my-key"))
    (assert-nil (funcall tried ".0abc"))
    (assert-nil (funcall tried ".$private"))
    (assert-equal '("my-key") (elpspath:parse-path ".[\"my-key\"]"))
    (assert-equal '("$private") (elpspath:parse-path ".[\"$private\"]"))
    (assert-equal '("_ok9") (elpspath:parse-path "._ok9"))
    ; snake_case -- what these paths in practice actually address -- needs
    ; no bracketing at all
    (assert-equal '("field_mask" "paths") (elpspath:parse-path ".field_mask.paths"))
    (assert-equal '("first_name") (elpspath:parse-path ".first_name"))
    (assert-equal '("") (elpspath:parse-path ".[\"\"]"))
    ; and the bracketed form really addresses the key
    (assert-equal 42 (apply elpspath:? (cons (sorted-map "my-key" 42)
                                             (elpspath:parse-path ".[\"my-key\"]"))))))

(test "parse-path discards the jq optional-selector suffix"
  ; ".a?" is exactly ".a": nothing in the engine suppresses errors per step
  (assert-equal (elpspath:parse-path ".a") (elpspath:parse-path ".a?"))
  (assert-equal '("a" 0) (elpspath:parse-path ".a[0]?")))

; Source: lisp/lisplib/libgolang/libgolang_test.lisp
; Copyright © 2018 The ELPS authors

(use-package 'golang-test)
(use-package 'testing)

(test-let* "struct"
  ((struct (make-test-struct)) ; nolint:undefined-symbol
   (field (curry-function 'golang:struct-field struct)))  ; partially bind function args
  (assert (string= (golang:string (field "StringField"))
                   "test-string"))
  (assert (= (golang:int (field "IntField"))
             123))
  (assert (= (golang:float (field "FloatField"))
             12.34)))

; Source: lisp/lisplib/libjson/libjson_cycle_test.lisp
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

; Source: lisp/lisplib/libjson/libjson_integer_test.lisp
; Copyright © 2026 The ELPS authors

; Lisp-level tests for issue #350 -- integers above 2^53 and the
; :exact-integers opt-in that makes them survive a load.
;
; Like libjson_cycle_test.lisp, this lives in its own file rather than in
; libjson_test.lisp because BenchmarkPackage/$load parses and evaluates that
; file on every iteration: source added to it is charged to a benchmark meant
; to measure the loader, and the CI benchmark gate reads the jump as a
; regression.  See TestPackageExactIntegers.

(use-package 'testing)

; The defect, from the phylum author's side.  Every number decodes as a float,
; a float carries 53 bits of integer precision, and nothing anywhere says so.
(test "default-rounds-silently"
  ; 2^53+1 comes back as 2^53.
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993"))))
  (assert-string= "9007199254740992"
                  (json:dump-string (json:load-string "9007199254740993")))
  ; int64 max comes back as something that is not even an int64.
  (assert-string= "9223372036854776000"
                  (json:dump-string (json:load-string "9223372036854775807")))
  ; A value just below the boundary is unaffected.
  (assert-string= "9007199254740991"
                  (json:dump-string (json:load-string "9007199254740991"))))

; THE HIDING MECHANISM.  This is why the defect sat open since 2018: the
; corrupted value still compares = to the integer it was supposed to be, so a
; phylum can read a corrupted identifier, check it against the value it
; expected, match, and carry on.  Nothing signals.
;
; The assertion is deliberately written the "wrong" way round -- it asserts the
; corruption is INVISIBLE -- because that makes it a tripwire.  If the default
; is ever flipped, this test fails and names what changed, instead of the
; change reaching a node quietly.
(test "corruption-is-invisible-by-default"
  (let ([loaded (json:load-string "9007199254740993")])
    ; It equals the integer it was meant to be ...
    (assert= 9007199254740993 loaded)
    ; ... and it equals the DIFFERENT integer it was actually rounded to, so
    ; two distinct documents are indistinguishable once loaded.
    (assert= loaded (json:load-string "9007199254740992"))
    ; The only thing that gives it away is its type.
    (assert-string= "float" (to-string (type loaded)))))

; The opt-in.
(test "exact-integers-round-trip"
  (assert-string= "int" (to-string (type (json:load-string "9007199254740993" :exact-integers true))))
  (assert= 9007199254740993 (json:load-string "9007199254740993" :exact-integers true))
  (assert-string= "9007199254740993"
                  (json:dump-string (json:load-string "9007199254740993" :exact-integers true)))
  (assert-string= "9223372036854775807"
                  (json:dump-string (json:load-string "9223372036854775807" :exact-integers true)))
  (assert-string= "-9223372036854775808"
                  (json:dump-string (json:load-string "-9223372036854775808" :exact-integers true)))
  ; Just below 2^53: an int now, and the same digits either way.
  (assert-string= "9007199254740991"
                  (json:dump-string (json:load-string "9007199254740991" :exact-integers true)))
  ; Nested, which is where real documents keep their identifiers.
  (assert-string= """{"id":9007199254740993}"""
                  (json:dump-string
                    (json:load-string """{"id":9007199254740993}""" :exact-integers true))))

; Under the opt-in the two documents that were indistinguishable above are
; distinguishable, which is the whole point of turning it on.
(test "exact-integers-distinguishes"
  (assert-not (=  (json:load-string "9007199254740993" :exact-integers true)
                 (json:load-string "9007199254740992" :exact-integers true))))

; Anything that cannot be represented fails LOUDLY rather than rounding.
(test "exact-integers-range-error"
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string "9223372036854775808" :exact-integers true)))
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string "123456789012345678901234567890" :exact-integers true)))
  ; ... including from inside a container.
  (assert-string= "caught"
                  (handler-bind ([json:integer-range-error (lambda (_c _) "caught")])
                    (json:load-string """{"a":[9223372036854775808]}""" :exact-integers true))))

; The rule is syntactic: a number written with a fraction or an exponent is
; still a float, exactly as it is by default.
(test "exact-integers-leaves-floats-alone"
  (assert-string= "float" (to-string (type (json:load-string "1.5" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "1.0" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "1e2" :exact-integers true))))
  (assert-string= "float" (to-string (type (json:load-string "-0" :exact-integers true))))
  (assert-string= "-0" (json:dump-string (json:load-string "-0" :exact-integers true)))
  (assert-string= "100" (json:dump-string (json:load-string "1e2" :exact-integers true))))

; Malformed input stays catchable as json:syntax-error under the opt-in.  The
; opt-in has to use a streaming decoder, which reports some malformed documents
; differently from json.Unmarshal; an adopter's handler-bind must not quietly
; stop firing.
(test "exact-integers-syntax-errors-stay-catchable"
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "{false:true}" :exact-integers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "" :exact-integers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "1 2" :exact-integers true))))

; :string-numbers still wins when both are set, so a caller that already uses
; it sees no change at all.
(test "string-numbers-takes-precedence"
  (assert-string= "string"
                  (to-string (type (json:load-string "9007199254740993"
                                                     :string-numbers true
                                                     :exact-integers true))))
  (assert-string= "9007199254740993"
                  (json:load-string "9007199254740993"
                                    :string-numbers true
                                    :exact-integers true)))

; The serializer-wide default, and that an explicit keyword still overrides it.
(test "use-exact-integers-default"
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993"))))
  (assert-nil (json:use-exact-integers true))
  (assert-string= "int" (to-string (type (json:load-string "9007199254740993"))))
  (assert-string= "9007199254740993" (json:dump-string (json:load-string "9007199254740993")))
  ; An explicit false still opts back out.
  (assert-string= "float"
                  (to-string (type (json:load-string "9007199254740993" :exact-integers false))))
  (assert-nil (json:use-exact-integers false))
  (assert-string= "float" (to-string (type (json:load-string "9007199254740993")))))

; load-bytes and load-message honour the keyword too.
(test "exact-integers-on-every-load-entry-point"
  (assert= 9007199254740993
           (json:load-bytes (to-bytes "9007199254740993") :exact-integers true))
  (assert= 9007199254740993
           (json:load-message (json:dump-message 9007199254740993) :exact-integers true)))

; Source: lisp/lisplib/libjson/libjson_null_test.lisp
; Copyright © 2026 The ELPS authors

(use-package 'testing)

(test "null-top-level"
  (assert-string= "null" (json:dump-string json:null)))

(test "null-map-value"
  (assert-string= """{"k":null}"""
                  (json:dump-string (sorted-map "k" json:null))))

(test "null-vector"
  (assert-string= "[null]" (json:dump-string (vector json:null))))

(test "null-list"
  (assert-string= "[null]" (json:dump-string (list json:null))))

(test "null-nested"
  (assert-string= """{"k":[{"n":[null]}]}"""
                  (json:dump-string
                    (sorted-map "k" (vector (sorted-map "n" (list json:null)))))))

(test "null-and-nil"
  (assert-string= "[null,null]" (json:dump-string (list json:null ()))))

(test "null-dump-bytes-and-message"
  (assert-string= "null" (to-string (json:dump-bytes json:null)))
  (assert-string= "null" (to-string (json:message-bytes (json:dump-message json:null)))))

(test "null-load-is-nil"
  (assert-equal () (json:load-string "null"))
  (assert-equal () (json:load-bytes (to-bytes "null")))
  (assert-equal () (json:load-message (json:dump-message ())))
  (assert-equal (vector () ()) (json:load-string "[null,null]"))
  (assert-equal () (get (json:load-string """{"k":null}""") "k"))
  (assert-equal (sorted-map "k" (vector (sorted-map "n" (vector ()))))
                (json:load-string """{"k":[{"n":[null]}]}""")))

(test "null-round-trip"
  (assert-equal () (json:load-string (json:dump-string json:null)))
  (assert-equal (vector () ())
                (json:load-string (json:dump-string (list json:null ())))))

(test "null-lookalikes"
  (assert-string= "\"json:null\"" (json:dump-string "json:null"))
  (assert-string= "\"null\"" (json:dump-string 'null))
  (assert-string= "\"other:null\"" (json:dump-string 'other:null))
  (assert-string= "json:null" (json:load-string "\"json:null\"")))

; Source: lisp/lisplib/libjson/libjson_string_numbers_test.lisp
; Copyright © 2018 The ELPS authors

(in-package 'libjson-string-numbers-test)

(use-package 'testing)

(test "string-numbers?"
  (assert-equal 'false (json:string-numbers?))
  (json:use-string-numbers true)
  (assert-equal 'true (json:string-numbers?))
  (assert-string= "\"1\"" (json:dump-string 1))
  (json:use-string-numbers false)
  (assert-equal 'false (json:string-numbers?))
  (assert-string= "1" (json:dump-string 1)))

; Malformed input stays catchable as json:syntax-error under :string-numbers,
; as it is under :exact-integers and in the default mode.
(test "string-numbers-syntax-errors-stay-catchable"
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "[1,]" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-string "1 2" :string-numbers true)))
  (assert-string= "syntax"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                    (json:load-bytes (to-bytes "{\"a\":") :string-numbers true)))
  ; The serializer-wide mode takes the same path.
  (with-cleanup ((json:use-string-numbers false))
    (json:use-string-numbers true)
    (assert-string= "syntax"
                    (handler-bind ([json:syntax-error (lambda (_c _) "syntax")])
                      (json:load-string "tru")))))

; Source: lisp/lisplib/libjson/libjson_test.lisp
; Copyright © 2018 The ELPS authors

(use-package 'testing)

(test "use-string-numbers"
  (assert-string= "\"1\"" (to-string (json:message-bytes (json:dump-message 1 :string-numbers true))))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers true))
  (assert-string= "1" (json:load-string "1" :string-numbers true))

  ; Assert that, when :string-numbers is not given (or nil) the setting
  ; defaults back to the last call of use-string-numbers (or false if never
  ; called)
  (assert-string= "1" (json:dump-string 1 :string-numbers ()))
  (assert= 1 (json:load-string "1" :string-numbers ()))
  (assert-string= "1" (json:dump-string 1))
  (assert= 1 (json:load-string "1"))
  (assert-nil (json:use-string-numbers true))
  (assert-string= "\"1\"" (json:dump-string 1 :string-numbers ()))
  (assert-string= "1" (json:load-string "1" :string-numbers ()))
  (assert-string= "\"1\"" (json:dump-string 1))
  (assert-string= "1" (json:load-string "1"))
  ; the value false can be used to override a true setting of
  ; use-string-numbers
  (assert-string= "1" (json:dump-string 1 :string-numbers false))
  (assert= 1 (json:load-string "1" :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers false)
                                         :string-numbers true))
  (assert= 1 (json:load-message (json:dump-message 1 :string-numbers false)
                                :string-numbers false))
  (assert-string= "1" (json:load-message (json:dump-message 1 :string-numbers true)
                                         :string-numbers false))
  (assert-string= """{"data":"1","id":1}"""
                  (json:dump-string (sorted-map "id"    1
                                                "data"  (json:dump-message 1 :string-numbers true))
                                    :string-numbers false))
  )

(test "marshal"
  (assert-string= """null"""
                  (json:dump-string ()))
  (assert-string= """true"""
                  (json:dump-string true))
  (assert-string= """false"""
                  (json:dump-string false))
  (assert-string= """12"""
                  (json:dump-string 12))
  (assert-string= """[1,2]"""
                  (json:dump-string '(1 2)))
  (assert-string= """[1,2]"""
                  (json:dump-string [1 2]))
  (assert-string= """[1,2]"""
                  (json:dump-string (vector 1 2)))
  (assert-string= """{}"""
                  (json:dump-string (sorted-map)))
  (assert-string= """{"a":1}"""
                  (json:dump-string (sorted-map "a" 1)))
  (assert-string= """{"a":1,"b":2}"""
                  (json:dump-string (sorted-map "a" 1 "b" 2)))
  (assert-string= """{"a":{"b":"c"}}"""
                  (json:dump-string (sorted-map "a" (sorted-map "b" "c")))))

(test "unmarshal"
  (set 'js-val (json:load-string """null"""))
  (assert-nil js-val)
  (set! 'js-val (json:load-string """true"""))
  (assert js-val)
  (assert-string= "true" (to-string js-val))
  (set! 'js-val (json:load-string """false"""))
  (assert-not js-val)
  (assert-not-nil js-val)
  (assert-string= "false" (to-string js-val))
  (set! 'js-val (json:load-string """4.0"""))
  (assert= 4 js-val)
  (set! 'js-val (json:load-string """[1, 2, 3]"""))
  (assert-equal (vector 1 2 3) js-val)
  (set! 'js-val (json:load-string """[]"""))
  (assert-equal (vector) js-val)
  (set! 'js-val (json:load-string """[[]]"""))
  (assert-equal (vector (vector)) js-val)
  (set! 'js-val (json:load-string """{"a":[], "b":null,"c":{}}"""))
  (assert (sorted-map? js-val))
  (assert-equal '("a" "b" "c") (keys js-val))
  (assert-equal (vector) (get js-val "a"))
  (assert-nil (get js-val "b"))
  (assert (sorted-map? (get js-val "c")))
  (assert= 0 (length (keys (get js-val "c")))))

(test "unmarshal-syntax-error"
  (assert-string= "syntax-error"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax-error")])
                    (json:load-string "{false:true}")))
  (assert-string= "ok-json"
                  (handler-bind ([json:syntax-error (lambda (_c _) "syntax-error")])
                    (json:load-string "\"ok-json\""))))

(benchmark-simple "load-object"
  (let ([b (to-bytes """{"test1": 123, "test2": 456, "test3": 789}""")])
    (dotimes (_n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-array"
  (let ([b (to-bytes """["test1", 123, "test2", 456, "test3", 789]""")])
    (dotimes (_n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-nested"
  (let ([b (to-bytes """{"test1": 123, "test2": 456, "test3": {"test1": 123, "test2": 456, "test3": 789}}""")])
    (dotimes (_n 1000)
      (json:load-bytes b))))

(benchmark-simple "load-github"
  (let ([b (to-bytes github-json)])
    (dotimes (_n 1000)
      (json:load-bytes b))))

(set 'benchmark-input-get-nested """
{
  "e0": 12,
  "e1":
  {
    "e0": 34
  },
  "e2":
  [
    {
      "e0": 56
    }
  ],
  "e3":
  [
    {
      "e0":
      {
        "e0": 78
      },
      "e1":
      [
        {
          "e0": 90
        }
      ]
    }
  ]
}
""")

(benchmark-simple "get-nested-baseline"
  (let ([v (json:load-bytes (to-bytes benchmark-input-get-nested))])
    (dotimes (_n 1000)
      (assert-equal 12
                    (thread-first v
                                  (get "e0")))
      (assert-equal 34
                    (thread-first v
                                  (get "e1")
                                  (get "e0")))
      (assert-equal 56
                    (thread-first v
                                  (get "e2")
                                  (nth 0)
                                  (get "e0")))
      (assert-equal 78
                    (thread-first v
                                  (get "e3")
                                  (nth 0)
                                  (get "e0")
                                  (get "e0")))
      (assert-equal 90
                    (thread-first v
                                  (get "e3")
                                  (nth 0)
                                  (get "e1")
                                  (nth 0)
                                  (get "e0")))
      )))

(benchmark-simple "dump-object"
  (let* ([val (sorted-map "test1" 123 "test2" 456 "test3" 789)])
    (dotimes (_n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-array"
  (let* ([val (list "test1" 123 "test2" 456 "test3" 789)])
    (dotimes (_n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-nested"
  (let* ([val (sorted-map "test1" 123 "test2" 456 "test3" (sorted-map "test1" 123 "test2" 456 "test3" 789))])
    (dotimes (_n 1000)
      (json:dump-string val))))

(benchmark-simple "dump-github"
  (let* ([val (json:load-string github-json)])
    (dotimes (_n 1000)
      (json:dump-string val))))

; curl https://api.github.com/repos/luthersystems/elps  
(set 'github-json """
{
  "id": 158678640,
  "node_id": "MDEwOlJlcG9zaXRvcnkxNTg2Nzg2NDA=",
  "name": "elps",
  "full_name": "luthersystems/elps",
  "private": false,
  "owner": {
    "login": "luthersystems",
    "id": 20160060,
    "node_id": "MDEyOk9yZ2FuaXphdGlvbjIwMTYwMDYw",
    "avatar_url": "https://avatars.githubusercontent.com/u/20160060?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/luthersystems",
    "html_url": "https://github.com/luthersystems",
    "followers_url": "https://api.github.com/users/luthersystems/followers",
    "following_url": "https://api.github.com/users/luthersystems/following{/other_user}",
    "gists_url": "https://api.github.com/users/luthersystems/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/luthersystems/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/luthersystems/subscriptions",
    "organizations_url": "https://api.github.com/users/luthersystems/orgs",
    "repos_url": "https://api.github.com/users/luthersystems/repos",
    "events_url": "https://api.github.com/users/luthersystems/events{/privacy}",
    "received_events_url": "https://api.github.com/users/luthersystems/received_events",
    "type": "Organization",
    "site_admin": false
  },
  "html_url": "https://github.com/luthersystems/elps",
  "description": "An embedded lisp interpreter",
  "fork": false,
  "url": "https://api.github.com/repos/luthersystems/elps",
  "forks_url": "https://api.github.com/repos/luthersystems/elps/forks",
  "keys_url": "https://api.github.com/repos/luthersystems/elps/keys{/key_id}",
  "collaborators_url": "https://api.github.com/repos/luthersystems/elps/collaborators{/collaborator}",
  "teams_url": "https://api.github.com/repos/luthersystems/elps/teams",
  "hooks_url": "https://api.github.com/repos/luthersystems/elps/hooks",
  "issue_events_url": "https://api.github.com/repos/luthersystems/elps/issues/events{/number}",
  "events_url": "https://api.github.com/repos/luthersystems/elps/events",
  "assignees_url": "https://api.github.com/repos/luthersystems/elps/assignees{/user}",
  "branches_url": "https://api.github.com/repos/luthersystems/elps/branches{/branch}",
  "tags_url": "https://api.github.com/repos/luthersystems/elps/tags",
  "blobs_url": "https://api.github.com/repos/luthersystems/elps/git/blobs{/sha}",
  "git_tags_url": "https://api.github.com/repos/luthersystems/elps/git/tags{/sha}",
  "git_refs_url": "https://api.github.com/repos/luthersystems/elps/git/refs{/sha}",
  "trees_url": "https://api.github.com/repos/luthersystems/elps/git/trees{/sha}",
  "statuses_url": "https://api.github.com/repos/luthersystems/elps/statuses/{sha}",
  "languages_url": "https://api.github.com/repos/luthersystems/elps/languages",
  "stargazers_url": "https://api.github.com/repos/luthersystems/elps/stargazers",
  "contributors_url": "https://api.github.com/repos/luthersystems/elps/contributors",
  "subscribers_url": "https://api.github.com/repos/luthersystems/elps/subscribers",
  "subscription_url": "https://api.github.com/repos/luthersystems/elps/subscription",
  "commits_url": "https://api.github.com/repos/luthersystems/elps/commits{/sha}",
  "git_commits_url": "https://api.github.com/repos/luthersystems/elps/git/commits{/sha}",
  "comments_url": "https://api.github.com/repos/luthersystems/elps/comments{/number}",
  "issue_comment_url": "https://api.github.com/repos/luthersystems/elps/issues/comments{/number}",
  "contents_url": "https://api.github.com/repos/luthersystems/elps/contents/{+path}",
  "compare_url": "https://api.github.com/repos/luthersystems/elps/compare/{base}...{head}",
  "merges_url": "https://api.github.com/repos/luthersystems/elps/merges",
  "archive_url": "https://api.github.com/repos/luthersystems/elps/{archive_format}{/ref}",
  "downloads_url": "https://api.github.com/repos/luthersystems/elps/downloads",
  "issues_url": "https://api.github.com/repos/luthersystems/elps/issues{/number}",
  "pulls_url": "https://api.github.com/repos/luthersystems/elps/pulls{/number}",
  "milestones_url": "https://api.github.com/repos/luthersystems/elps/milestones{/number}",
  "notifications_url": "https://api.github.com/repos/luthersystems/elps/notifications{?since,all,participating}",
  "labels_url": "https://api.github.com/repos/luthersystems/elps/labels{/name}",
  "releases_url": "https://api.github.com/repos/luthersystems/elps/releases{/id}",
  "deployments_url": "https://api.github.com/repos/luthersystems/elps/deployments",
  "created_at": "2018-11-22T10:01:06Z",
  "updated_at": "2021-05-16T03:55:18Z",
  "pushed_at": "2021-05-16T03:55:15Z",
  "git_url": "git://github.com/luthersystems/elps.git",
  "ssh_url": "git@github.com:luthersystems/elps.git",
  "clone_url": "https://github.com/luthersystems/elps.git",
  "svn_url": "https://github.com/luthersystems/elps",
  "homepage": null,
  "size": 2892,
  "stargazers_count": 13,
  "watchers_count": 13,
  "language": "Go",
  "has_issues": true,
  "has_projects": true,
  "has_downloads": true,
  "has_wiki": true,
  "has_pages": true,
  "forks_count": 7,
  "mirror_url": null,
  "archived": false,
  "disabled": false,
  "open_issues_count": 3,
  "license": {
    "key": "bsd-3-clause",
    "name": "BSD 3-Clause \"New\" or \"Revised\" License",
    "spdx_id": "BSD-3-Clause",
    "url": "https://api.github.com/licenses/bsd-3-clause",
    "node_id": "MDc6TGljZW5zZTU="
  },
  "forks": 7,
  "open_issues": 3,
  "watchers": 13,
  "default_branch": "master",
  "temp_clone_token": null,
  "organization": {
    "login": "luthersystems",
    "id": 20160060,
    "node_id": "MDEyOk9yZ2FuaXphdGlvbjIwMTYwMDYw",
    "avatar_url": "https://avatars.githubusercontent.com/u/20160060?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/luthersystems",
    "html_url": "https://github.com/luthersystems",
    "followers_url": "https://api.github.com/users/luthersystems/followers",
    "following_url": "https://api.github.com/users/luthersystems/following{/other_user}",
    "gists_url": "https://api.github.com/users/luthersystems/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/luthersystems/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/luthersystems/subscriptions",
    "organizations_url": "https://api.github.com/users/luthersystems/orgs",
    "repos_url": "https://api.github.com/users/luthersystems/repos",
    "events_url": "https://api.github.com/users/luthersystems/events{/privacy}",
    "received_events_url": "https://api.github.com/users/luthersystems/received_events",
    "type": "Organization",
    "site_admin": false
  },
  "network_count": 7,
  "subscribers_count": 6
}
""")

; Source: lisp/lisplib/libregexp/libregexp_test.lisp
; Copyright © 2018 The ELPS authors

(use-package 'testing)
(use-package 'regexp)

(test "regexp-compile"
  (set 're (regexp-compile "abc?"))
  (assert-string= "abc?" (regexp-pattern re))
  (set! 're (regexp-compile """abc?\(\)"""))
  (assert-string= "abc?\\(\\)" (regexp-pattern re))
  (set! 're (regexp-compile "abc\n"))
  (assert-string= "abc\n" (regexp-pattern re)))

(test "regexp-match?"
  (defmacro assert-match (patt text)
    (let ([pattsym (gensym)])
      (quasiquote
        (let ([(unquote pattsym) (unquote patt)])
          (assert (funcall 'regexp:regexp-match?
                           (unquote pattsym)
                           (unquote text))
                  "pattern {} does not match text: {}"
                  (quote (unquote pattsym))
                  (quote (unquote text)))))))
  (assert-match "abc?" "ab")
  (assert-match "abc?" "abc")
  (assert-match """^\n*$""" "")
  (assert-match """^\n*$""" "\n")
  (assert-match """^\n*$""" "\n\n")
  (assert-match "^\n*$" "\n\n")
  (assert-match """\s""" "abc\n"))

; Source: lisp/lisplib/libschema/libschema_test.lisp
(use-package 'testing)

; Several tests below reuse a generic name (myint, mystring, x) already bound
; by an earlier, independent test. Each (test ...) body runs in its own
; isolated environment at runtime, so this is not really a rebind -- but the
; set-usage analyzer reads the file as one flat top-level sequence and cannot
; see that isolation, so it reports a false "already bound" positive on the
; later definitions. Tagged ; nolint:set-usage at each site below.

(test "make-validator-string"
  (set 'mystring (s:make-validator "mystring" s:string))
  (set 'x "hello")
  (assert-nil (s:validate mystring x))
  (set 'y 9.0)
  (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
    (assert-equal (s:validate mystring y) "ERROR"))
  (set 'myconditionalstring (s:make-validator "myconditionalstring" s:string (s:in "x" "y" "z")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalstring y)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalstring x)))
  (assert-nil (s:validate mystring "x"))
  (assert-nil (s:validate mystring "y"))
  )

(test "make-validator-regexp"
  (set 'mystring (s:make-validator "mystring" s:string (s:regexp "^Hello"))) ; nolint:set-usage
  (assert-nil (s:validate mystring "Hello mum"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mystring "goodbye mum")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mystring "well hello there")))
  (set 'isodate (s:make-validator "isodate" s:string (s:regexp "^([1-9][0-9]{3})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])?$")))
  (assert-nil (s:validate isodate "2020-04-31"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate isodate "3/4/21")))
  (assert-equal  "ERROR" (handler-bind (('bad-arguments (lambda (&rest _e) "ERROR")))
                           (set 'x (s:make-validator "x" s:string (s:regexp "*"))))) ; nolint:set-usage
  )

(test "make-validator-int"
  (set 'myint (s:make-validator "myint" s:int))
  (assert-nil (s:validate myint 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint 91.3)))
  )

(test "make-validator-bool"
  (set 'mybool (s:make-validator "mybool" s:bool (s:is-true)))
  (assert-nil (s:validate mybool true))
  ; A boolean is the symbol, not a string that spells it.
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mybool "true")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mybool "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybool 'false)))
  )

(test "make-validator-bool-false"
  (set 'myboolf (s:make-validator "myboolf" s:bool (s:is-false)))
  (assert-nil (s:validate myboolf 'false))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myboolf "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolf 'true)))
  )

(test "make-validator-any-truthy"
  (set 'myboolty (s:make-validator "myboolty" s:any (s:is-truthy)))
  (assert-nil (s:validate myboolty true))
  (assert-nil (s:validate myboolty 'true))
  (assert-nil (s:validate myboolty "true"))
  (assert-nil (s:validate myboolty "hello"))
  (assert-nil (s:validate myboolty 100))
  (assert-nil (s:validate myboolty 91.3))
  (assert-nil (s:validate myboolty (vector 3 4 5)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolty 'false)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myboolty 0)))
  )

(test "make-validator-any-falsy"
  (set 'mybooltf (s:make-validator "mybooltf" s:any (s:is-falsy)))
  (assert-nil (s:validate mybooltf false))
  (assert-nil (s:validate mybooltf "false"))
  (assert-nil (s:validate mybooltf ""))
  (assert-nil (s:validate mybooltf 0))
  (assert-nil (s:validate mybooltf -91.3))
  (assert-nil (s:validate mybooltf (vector)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybooltf 'true)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mybooltf 11)))
  )

(test "make-validator-float"
  (set 'myflt (s:make-validator "myflt" s:float))
  (assert-nil (s:validate myflt 4.3))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myflt "error")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myflt 91)))
  )

(test "make-validator-number"
  (set 'mynum (s:make-validator "mynum" s:number))
  (assert-nil (s:validate mynum 4.3))
  (assert-nil (s:validate mynum 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mynum "error")))
  )

(test "make-validator-int-positive"
  (set 'myint (s:make-validator "myint" s:int (s:positive))) ; nolint:set-usage
  (assert-nil (s:validate myint 4))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint -5)))
  )

(test "make-validator-int-negative"
  (set 'myint (s:make-validator "myint" s:int (s:negative))) ; nolint:set-usage
  (assert-nil (s:validate myint -5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 5)))
  )

(test "make-validator-int-constrained"
  (set 'myint (s:make-validator "myint" s:int (s:gt 4) (s:lte 91))) ; nolint:set-usage
  (assert-nil (s:validate myint 5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 93)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 4)))
  )

(test "make-validator-int-constrained2"
  (set 'myint (s:make-validator "myint" s:int (s:gte 4) (s:lt 91))) ; nolint:set-usage
  (assert-nil (s:validate myint 5))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 91)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 3)))
  )

(test "make-validator-int-constrained3"
  (set 'myint (s:make-validator "myint" s:int (s:in 6 7 11))) ; nolint:set-usage
  (assert-nil (s:validate myint 6))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myint "error")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint 8)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myint -6)))
  )

(test "make-validator-array"
  (set 'my4thingarray (s:make-validator "my4thingarray" s:array (s:len 4)))
  (set 'my4to6thingarray (s:make-validator "my4to6thingarray" s:array (s:lengte 4) (s:lenlt 7)))
  (set 'mystringarray (s:make-validator "mystringarray" s:array (s:of s:string)))
  (set 'mystringorfunctionarray (s:make-validator "mystringorfunctionarray" s:array (s:of s:string s:fun)))
  (set 'myint (s:make-validator "myint" s:int (s:gt 11))) ; nolint:set-usage
  (set 'myfancyarray (s:make-validator "myfancyarray" s:array (s:of myint)))
  (assert-nil (s:validate my4thingarray (vector 1 2 3 4)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate my4thingarray (vector 1 2 3 4 5))))
  (assert-nil (s:validate my4to6thingarray (vector 1 2 3 4 5)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate my4thingarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate mystringarray (vector "a" "b" "c")))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mystringarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate mystringorfunctionarray (vector "a" "b" "c" assert-nil)))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mystringorfunctionarray (vector 1 2 3 4 5 6 7))))
  (assert-nil (s:validate myfancyarray (vector 13 87 2222)))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate myfancyarray (vector 13 87 9))))
  )

(test "make-validator-map"
  (set 'mymap (s:make-validator "mymap" s:sorted-map (s:has-key "name" s:string) (s:may-have-key "middle-name" s:string)))
  (set 'mymapdone (s:make-validator "mymapdone" s:sorted-map (s:no-other-keys (s:has-key "name" s:string) (s:may-have-key "middle-name" s:string))))
  (set 'myint (s:make-validator "myint" s:int (s:gt 4))) ; nolint:set-usage
  (set 'mycomplicatedmap (s:make-validator "mycomplicatedmap" s:sorted-map (s:has-key "name" s:string) (s:has-key "id" myint) (s:has-key "innermap" mymap)))
  (set 'myconditionalmap (s:make-validator "myconditionalmap" s:sorted-map (s:has-key "name" s:string) (s:has-key "id" s:int) (s:has-key "writer" s:bool) (s:when "name" (s:in "Reuben" "Sam") "writer" (s:is-true))))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver")))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver" 'middle-name "Wendell")))
  (assert-nil (s:validate mymap (sorted-map 'name "Oliver" 'middle-name "Wendell" 'last-name "Holmes")))
  (assert-nil (s:validate mymapdone (sorted-map 'name "Oliver")))
  (assert-nil (s:validate mymapdone (sorted-map 'name "Oliver" 'middle-name "Wendell")))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate mymapdone (sorted-map 'name "Oliver" 'middle-name "Wendell" 'last-name "Holmes"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mymap (sorted-map 'name "Oliver" 'middle-name 74))))
  (assert-nil (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 6 'innermap (sorted-map 'name "Oliver" 'middle-name "Wendell"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 6 'innermap "hello"))))
  (assert-equal  "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                           (s:validate mycomplicatedmap (sorted-map 'name "Oliver" 'id 3 'innermap (sorted-map 'name "Oliver" 'middle-name "Wendell")))))
  (assert-nil (s:validate myconditionalmap (sorted-map 'name "Reuben" 'id 5 'writer true)))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate myconditionalmap (sorted-map 'name "Reuben" 'id 5 'writer false))))
  )

(test "not"
  (set 'not-test (s:make-validator "not-test" s:string (s:not (s:in "a" "b" "c"))))
  (assert-nil (s:validate not-test "x"))
  (assert-equal  "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                           (s:validate not-test "a")))
  )

(test "lisp-deftype"
  (deftype mystring (s) (to-string s))
  (set 'v (s:make-validator mystring s:string (s:in "a" "b" "c")))
  (assert-nil (s:validate v (new mystring "c")))
  (assert-equal "ERROR" (handler-bind (('failed-constraint (lambda (&rest _e) "ERROR")))
                          (s:validate v (new mystring "x"))))
  (assert-nil (s:validate v (new mystring "c")))
  (deftype mynil ())
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate v (new mynil))))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate v "a"))))

; Issue #325.  s:may-have-key looked its key up as a symbol while s:has-key
; looked the same key up as a string.  lisp.Map is an interface: the built-in
; sorted-map keys on the string value either way, so the asymmetry is
; invisible here, but the map json:load-string returns rejects a non-string
; key outright -- so may-have-key never found anything and silently passed
; whatever it was given.  The key type is now string everywhere.
(test "may-have-key-key-type"
  (set 'optstr (s:make-validator "optstr" s:sorted-map (s:may-have-key "a" s:string)))

  ; string-keyed literal map
  (assert-nil (s:validate optstr (sorted-map "a" "str")))
  (assert-nil (s:validate optstr (sorted-map "z" 0)))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (sorted-map "a" 1))))

  ; symbol-keyed literal map -- a string lookup must still match it
  (assert-nil (s:validate optstr (sorted-map 'a "str")))
  (assert-nil (s:validate optstr (sorted-map 'z 0)))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (sorted-map 'a 1))))

  ; json-decoded map -- the case that silently passed before the fix
  (assert-nil (s:validate optstr (json:load-string "{\"a\": \"str\"}")))
  (assert-nil (s:validate optstr (json:load-string "{\"z\": 0}")))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate optstr (json:load-string "{\"a\": 1}"))))

  ; s:has-key must give the same verdict whenever the key is present
  (set 'reqstr (s:make-validator "reqstr" s:sorted-map (s:has-key "a" s:string)))
  (assert-nil (s:validate reqstr (sorted-map "a" "str")))
  (assert-nil (s:validate reqstr (sorted-map 'a "str")))
  (assert-nil (s:validate reqstr (json:load-string "{\"a\": \"str\"}")))
  (assert-equal "ERROR" (handler-bind (('wrong-type (lambda (&rest _e) "ERROR")))
                          (s:validate reqstr (json:load-string "{\"a\": 1}"))))
  )

; #736: a prefixed library builtin must never write into the caller's
; package, so s:deftype (which bound its validator as a global under the
; caller's own name) was removed. Callers now bind the result of
; s:make-validator themselves with core set -- see e.g.
; make-validator-string above. s:deftype is simply gone; the symbol
; resolves as unbound like any other typo.
(test "deftype-removed"
  (assert-equal (list 'error "unbound symbol: deftype")
                (handler-bind (('error (lambda (&rest e) e)))
                  (s:deftype "removed" s:string))))

; Source: lisp/lisplib/libstring/libstring_test.lisp
; Copyright © 2018 The ELPS authors

(use-package 'string)
(use-package 'testing)

(test "uppercase"
  (assert-string= "" (uppercase ""))
  (assert-string= "ABC" (uppercase "abc")))

(test "lowercase"
  (assert-string= "" (lowercase ""))
  (assert-string= "abc" (lowercase "ABC")))

(test "split"
  (assert-string= "ghi"
                  (nth (split "abc:def:ghi" ":") 2))
  (assert-equal '("abc" "def")
                (split "abc:def" ":"))
  (assert-equal '("abc")
                (split "abc" ":"))
  (assert-equal '("")
                (split "" ":"))
  (assert-equal '("" "")
                (split ":" ":")))

(test "repeat"
  (assert-string= "" (repeat "x" 0))
  (assert-string= "x" (repeat "x" 1))
  (assert-string= "xx" (repeat "x" 2))
  (assert-string= "abcabcabc" (repeat "abc" 3))
  (assert-string= "" (repeat "" 1000)))

(test "trim-space"
  (assert-string= "" (trim-space ""))
  (assert-string= "" (trim-space " "))
  (assert-string= "" (trim-space "\t"))
  (assert-string= "" (trim-space "\n"))
  (assert-string= "" (trim-space "\n\t"))
  (assert-string= "abc" (trim-space " abc"))
  (assert-string= "abc" (trim-space "\n abc"))
  (assert-string= "abc" (trim-space "abc \t"))
  (assert-string= "abc\t def" (trim-space "\tabc\t def\n\n")))

(test "trim"
  (assert-string= "" (trim "" ""))
  (assert-string= "" (trim "" "abc"))
  (assert-string= "abc" (trim "abc" ""))
  (assert-string= "" (trim "abc" "abc"))
  (assert-string= "ab" (trim "abc" "c"))
  (assert-string= "b" (trim "abc" "ca"))
  (assert-string= "a" (trim "abc" "cb")))

(test "trim-left"
  (assert-string= "" (trim-left "" ""))
  (assert-string= "" (trim-left "" "abc"))
  (assert-string= "abc" (trim-left "abc" ""))
  (assert-string= "" (trim-left "abc" "abc"))
  (assert-string= "abc" (trim-left "abc" "c"))
  (assert-string= "bc" (trim-left "abc" "ca"))
  (assert-string= "abc" (trim-left "abc" "cb")))

(test "trim-right"
  (assert-string= "" (trim-right "" ""))
  (assert-string= "" (trim-right "" "abc"))
  (assert-string= "abc" (trim-right "abc" ""))
  (assert-string= "" (trim-right "abc" "abc"))
  (assert-string= "ab" (trim-right "abc" "c"))
  (assert-string= "ab" (trim-right "abc" "ca"))
  (assert-string= "a" (trim-right "abc" "cb")))

(test "has-prefix?"
  (assert (has-prefix? "" ""))
  (assert (has-prefix? "abc" ""))
  (assert-not (has-prefix? "" "a"))
  (assert (has-prefix? "abc" "a"))
  (assert (has-prefix? "abc" "abc"))
  (assert-not (has-prefix? "abc" "abcd"))
  (assert-not (has-prefix? "abc" "b"))
  (assert-not (has-prefix? "abc" "A"))
  (assert (has-prefix? "日本語" "日本"))
  (assert-not (has-prefix? "日本語" "本"))
  (assert (has-prefix? "José" "Jos"))
  (assert-equal 'true (has-prefix? "abc" "ab"))
  (assert-equal 'false (has-prefix? "abc" "bc")))

(test "has-suffix?"
  (assert (has-suffix? "" ""))
  (assert (has-suffix? "abc" ""))
  (assert-not (has-suffix? "" "c"))
  (assert (has-suffix? "abc" "c"))
  (assert (has-suffix? "abc" "abc"))
  (assert-not (has-suffix? "abc" "zabc"))
  (assert-not (has-suffix? "abc" "b"))
  (assert (has-suffix? "日本語" "本語"))
  (assert-not (has-suffix? "日本語" "日"))
  (assert (has-suffix? "José" "é"))
  (assert-equal 'true (has-suffix? "abc" "bc"))
  (assert-equal 'false (has-suffix? "abc" "ab")))

(test "contains?"
  (assert (contains? "" ""))
  (assert (contains? "abc" ""))
  (assert-not (contains? "" "a"))
  (assert (contains? "abc" "a"))
  (assert (contains? "abc" "b"))
  (assert (contains? "abc" "c"))
  (assert (contains? "abc" "abc"))
  (assert-not (contains? "abc" "abcd"))
  (assert-not (contains? "abc" "ac"))
  (assert (contains? "日本語" "本"))
  (assert-not (contains? "日本語" "中"))
  (assert-equal 'true (contains? "abc" "bc"))
  (assert-equal 'false (contains? "abc" "x")))

(test "trim-prefix"
  (assert-string= "" (trim-prefix "" ""))
  (assert-string= "" (trim-prefix "" "a"))
  (assert-string= "abc" (trim-prefix "abc" ""))
  (assert-string= "" (trim-prefix "abc" "abc"))
  (assert-string= "abc" (trim-prefix "abc" "abcd"))
  (assert-string= "bc" (trim-prefix "abc" "a"))
  (assert-string= "abc" (trim-prefix "abc" "b"))
  ; Only one occurrence is removed.
  (assert-string= "abab" (trim-prefix "ababab" "ab"))
  (assert-string= "aa" (trim-prefix "aaa" "a"))
  ; The prefix is a whole string, not a cutset as with trim-left.
  (assert-string= "abc" (trim-prefix "abc" "ba"))
  (assert-string= "c" (trim-left "abc" "ba"))
  (assert-string= "語" (trim-prefix "日本語" "日本"))
  (assert-string= "日本語" (trim-prefix "日本語" "本")))

(test "trim-suffix"
  (assert-string= "" (trim-suffix "" ""))
  (assert-string= "" (trim-suffix "" "a"))
  (assert-string= "abc" (trim-suffix "abc" ""))
  (assert-string= "" (trim-suffix "abc" "abc"))
  (assert-string= "abc" (trim-suffix "abc" "zabc"))
  (assert-string= "ab" (trim-suffix "abc" "c"))
  (assert-string= "abc" (trim-suffix "abc" "b"))
  ; Only one occurrence is removed.
  (assert-string= "abab" (trim-suffix "ababab" "ab"))
  (assert-string= "aa" (trim-suffix "aaa" "a"))
  ; The suffix is a whole string, not a cutset as with trim-right.
  (assert-string= "abc" (trim-suffix "abc" "cb"))
  (assert-string= "a" (trim-right "abc" "cb"))
  (assert-string= "日本" (trim-suffix "日本語" "語"))
  (assert-string= "日本語" (trim-suffix "日本語" "本")))

; Source: lisp/lisplib/libtime/libtime_test.lisp
; Copyright © 2018 The ELPS authors

(use-package 'time)
(use-package 'testing)

(test-let "rfc3339"
  ((fixed "1998-12-13T01:02:03Z"))
  (assert (string= (format-rfc3339 (parse-rfc3339 fixed))
                   fixed)))

(test-let "comparison"
  ((t0 (parse-rfc3339 "2000-01-01T00:00:00Z"))
   (t1 (parse-rfc3339 "2000-01-01T00:00:01Z"))
   (t2 (parse-rfc3339 "2000-01-01T00:00:02Z")))
  (assert (time= t1 t1))
  (assert (time< t0 t1))
  (assert (time> t2 t1))
  (assert (not (time= t0 t1)))
  (assert (not (time< t1 t0)))
  (assert (not (time< t1 t1)))
  (assert (not (time> t1 t1)))
  (assert (not (time> t0 t1))))

(test-let* "duration"
  ((one-second (parse-duration "1s"))
   (_twelve-hours (parse-duration "12h"))
   (complex-dur (parse-duration "1h13m450ms"))
   (epoch-timestamp  "2000-01-01T00:00:00Z")
   (epoch (parse-rfc3339 epoch-timestamp)))
  (assert (= (duration-s one-second) 1))
  (assert (= (duration-ms one-second) 1000))
  (assert (string= (format-rfc3339 (time-add epoch complex-dur))
                   "2000-01-01T01:13:00Z"))
  (assert (string= (format-rfc3339-nano (time-add epoch complex-dur))
                   "2000-01-01T01:13:00.45Z")))

(handler-bind ((problem (lambda (c) (rethrow c)))) (error 'problem "example"))
