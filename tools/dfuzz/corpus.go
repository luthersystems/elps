// Copyright © 2026 The ELPS authors

package main

// SeedCorpus is the hand-written seed set.  Every entry is a SHAPE taken from
// the behaviour the in-tree suites exercise -- quoted literals into mutators,
// repeated calls, slice/append capacity, sorted-map mutation, elpspath on
// nested documents, macros over quoted arguments -- rewritten from scratch
// here so nothing proprietary and nothing from a production-scale phylum is
// carried into a public repository.
//
// The corpus runs first on every invocation, before any generated program.  A
// harness that cannot reproduce the shapes whose defects are already known is
// not trustworthy on the shapes whose defects are not.
//
// Each entry is written so its OBSERVABLE result changes if the literal it
// mutates is damaged: the function is called more than once and every call's
// value is kept.
var SeedCorpus = []string{
	// --- literal mutation, visible only on the second call -----------------
	`(defun f () (stable-sort < '(3 1 2)))
(list (f) (f) (f))`,

	`(defun f () (stable-sort > '(1 2 3 4)))
(list (f) (f))`,

	`(defun f () (append! '(1 2) 9))
(list (f) (f) (f))`,

	`(defun f () (append 'list '(1 2) 9))
(list (f) (f))`,

	`(defun f () (append! (vector 1 2) 9))
(list (f) (f))`,

	`(defun f () (append-bytes! (to-bytes "ab") "c"))
(list (f) (f))`,

	`(defun f () (insert-sorted 'list '(1 3 5) < 4))
(list (f) (f))`,

	`(defun f () (reverse 'list '(1 2 3)))
(list (f) (f))`,

	// --- slice/append capacity aliasing ------------------------------------
	`(defun f ()
  (let ([s (slice 'list '(1 2 3 4) 0 2)])
    (append 'list s 99)
    s))
(list (f) (f))`,

	`(defun f ()
  (let ([v (vector 1 2 3 4)])
    (let ([s (slice 'vector v 0 2)])
      (append! s 99)
      (list s v))))
(list (f) (f))`,

	`(defun f ()
  (let ([s (concat 'list '(1 2) '(3 4))])
    (append! s 5)
    s))
(list (f) (f) '(1 2) '(3 4))`,

	// --- sorted-map mutation ----------------------------------------------
	`(defun f ()
  (let ([m (sorted-map "a" 1 "b" 2)])
    (assoc! m "c" 3)
    (dissoc! m "a")
    m))
(list (f) (f))`,

	`(defun f ()
  (let ([m (sorted-map "a" '(1 2 3))])
    (assoc! m "b" (get m "a"))
    (append! (get m "b") 4)
    m))
(list (f) (f))`,

	// --- macros over quoted arguments, quasiquote splices ------------------
	`(defmacro twice (x) (quasiquote (list (unquote x) (unquote x))))
(defun f () (twice '(1 2)))
(list (f) (f))`,

	`(defmacro cat (&rest xs) (quasiquote (list (unquote-splicing xs) 0)))
(defun f () (cat 1 2 3))
(list (f) (f))`,

	`(defmacro m (x) (quasiquote (append! (quote (1 2)) (unquote x))))
(defun f () (m 3))
(list (f) (f))`,

	`(defun f () (macroexpand-1 '(twice 1)))
(defmacro twice (x) (quasiquote (list (unquote x) (unquote x))))
(list (f) (f))`,

	// --- apply/funcall over a SHARED argument list -------------------------
	`(defun f ()
  (let ([args '(1 2 3)])
    (list (apply (function list) args) (apply (function +) args) args)))
(list (f) (f))`,

	`(defun g (&rest xs) (append! xs 9))
(defun f () (let ([a '(1 2)]) (list (apply (function g) a) a)))
(list (f) (f))`,

	`(defun f () (funcall (lambda (&rest xs) (append! xs 0)) 1 2))
(list (f) (f))`,

	// --- formals and closures shared across calls --------------------------
	`(defun f (&optional a b) (list a b))
(list (f) (f 1) (f 1 2))`,

	`(defun f (&key a b) (list a b))
(list (f) (f :a 1) (f :a 1 :b 2))`,

	`(defun mk () (let ([n 0]) (lambda () (set! n (+ n 1)) n)))
(let ([c (mk)] [d (mk)]) (list (c) (c) (d)))`,

	// --- error shapes: the error-vs-panic surface --------------------------
	`(nth '(1 2 3) 99)`,
	`(aref (vector 1 2) 5)`,
	`(slice 'list '(1 2 3) 2 1)`,
	`(car 1)`,
	`(+ 1 "a")`,
	`(handler-bind ([error (lambda (c &rest r) (list 'caught c))]) (error 'boom "x"))`,
	`(ignore-errors (nth '(1) 99))`,

	// --- deftype / tagged values -------------------------------------------
	`(deftype pt (x y) (sorted-map "x" x "y" y))
(defun f () (new pt 1 2))
(list (f) (f) (user-data (f)))`,

	// --- re-entrancy: same literal through two different call paths --------
	`(defun a (l) (append! l 1))
(defun b (l) (stable-sort < l))
(defun f () (let ([q '(3 1 2)]) (list (b q) (a q) q)))
(list (f) (f))`,

	// --- string/bytes conversions round-tripping literals ------------------
	`(defun f () (to-bytes "abc"))
(list (f) (append-bytes! (f) "d") (f))`,

	`(defun f () (map 'list (lambda (x) (* x 2)) '(1 2 3)))
(list (f) (f))`,

	`(defun f () (foldl (lambda (acc x) (cons x acc)) () '(1 2 3)))
(list (f) (f))`,

	`(defun f () (zip 'list '(1 2) '(3 4)))
(list (f) (f))`,

	`(defun f () (select 'list (function int?) '(1 "a" 2)))
(list (f) (f))`,
}

// ElpspathSeedCorpus is the seed set for the `elpspath` package.  It is kept
// apart from SeedCorpus because elpspath does not exist on origin/main -- see
// enableElpspath in gen.go -- so these seeds are only meaningful when the
// left-hand tree is the adoption commit or later.
var ElpspathSeedCorpus = []string{
	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "a" (sorted-map "b" '(1 2)) "c" (vector 1 2 3))])
    (list (elpspath:?set d "a" "b" 9) d)))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "a" (sorted-map "b" '(1 2)) "c" (vector 1 2 3))])
    (elpspath:?set! d "c" 0 9)
    d))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "items" (vector (sorted-map "n" 1) (sorted-map "n" 2)))])
    (list (elpspath:? d "items" '* "n")
          (elpspath:?del d "items" 0)
          d)))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "l" '(1 2 3 4))])
    (elpspath:?del! d "l" '(range 1 3))
    d))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "l" '(1 2 3))])
    (list (elpspath:?nil d "l" 0) (elpspath:? d "l"))))
(list (f) (f))`,

	`(use-package 'elpspath)
(elpspath:? (sorted-map "a" (vector 1 2)) "a" 99)`,

	`(use-package 'elpspath)
(elpspath:?set! (sorted-map "a" '(1 2)) "a" -99 5)`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "a" '(1 2 3))])
    (list (elpspath:?set d "a" 0 9) d)))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (vector (sorted-map "n" '(1 2)))])
    (list (elpspath:? d 0 "n") (elpspath:? d -1 "n") (elpspath:? d '* "n"))))
(list (f) (f))`,

	`(use-package 'elpspath)
(defun f ()
  (let ([d (sorted-map "v" (vector 1 2 3 4))])
    (list (elpspath:?del d "v" '(range 1 3)) d)))
(list (f) (f))`,
}
