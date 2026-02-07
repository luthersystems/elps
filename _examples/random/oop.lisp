;; package 'oop exports a simple object-oriented type system to serve as an
;; example of how an application might make use of ``deftype'' and build a
;; function dispatch system to implement object methods.
(in-package 'oop)

;; method-table is a nested map of type-name -> method-name -> method-implementation
(set 'method-table (sorted-map))

;; type-name returns the type symbol that type-specifier corresponds to.
;; Symbols are returned unaltered and typedef objects will return the symbol of
;; the defined type.
(defun type-name (type-specifier)
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
(defmacro defmethod (type-specifier method-name method-args &rest method-body)
  (cond ((not (symbol? type-specifier))
         (error 'type-error "first argument is not a valid type specifier"))
        ((not (symbol? method-name))
         (error 'type-error "second argument is not a symbol"))
        ((not (list? method-args))
         (error 'type-error "third argument is not a list"))
        ((empty? method-args)
         (error 'type-error "methods must take at least one argument"))
        (:else
          (let* ([type-name (gensym)]
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
  (let* ([name (method-name m)]
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
  (cond ((not (int? n))
         (error 'type-error (format-string "first argument is not an int: {}" (type n))))
        ((not (int? d))
         (error 'type-error (format-string "second argument is not an int: {}" (type d)))))
  ;; TODO:  Reduce by common factors
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
