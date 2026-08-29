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
