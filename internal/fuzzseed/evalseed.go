// Copyright © 2026 The ELPS authors

package fuzzseed

// Evaluation seeds.
//
// These are separate from the parser corpus for one reason: eval seeds are
// EXECUTED.  A parser seed costs a parse; an eval seed costs a parse plus
// however much interpreter work the program describes, and the mutator
// descends from the seeds it is given, so every generation inherits that
// cost.  Seeding evaluation from the repository's real .lisp sources -- which
// is exactly what the parser targets do, correctly -- would hand the fuzzer a
// full test suite (libtesting drives thousands of assertions) as a starting
// point and throttle every descendant of it.  So the eval corpus is
// hand-written, small, and deliberately does NOT call LispSources.
//
// The corpus is split by expected outcome, because "the program terminated"
// and "the program was stopped by a limit" are different assertions and
// conflating them hides defects in both directions:
//
//   - EvalRunaway: programs that MUST be stopped.  Each one is an unbounded
//     loop, unbounded recursion, or an unbounded allocation.  If one of these
//     ever returns a non-error value, a budget stopped working.
//
//   - EvalTerminating: programs that MUST finish on their own, under the same
//     budget, without an error.  These are the false-positive guard: a limit
//     that fires on a correct, bounded program is as much a defect as a limit
//     that never fires.  An infinite ELPS loop the budget stops is CORRECT
//     behaviour; a 30-element list comprehension that trips a cap is not.
//
//   - EvalErroring: programs that MUST finish on their own AND finish with a
//     specific ordinary, catchable error -- language-contract errors such as
//     the sealed-write guard's modify-literal-error, not budget stops.  They
//     cannot join EvalRunaway (they never test a budget) or EvalTerminating
//     (erroring is their job).

// EvalRunaway returns programs that must NOT run to completion under a
// bounded evaluation budget.  Each entry names the limit that is expected to
// stop it.  The harness asserts only that evaluation ends in an error -- not
// which error -- because several of these are stoppable by more than one
// limit and pinning the specific one would make the corpus brittle against
// legitimate retuning of the budgets.
func EvalRunaway() map[string]string {
	return map[string]string{
		// --- non-terminating tail recursion (MaxTailIterations) ---
		"tail-recursion-forever": `(defun loop () (loop)) (loop)`,
		"mutual-tail-recursion":  `(defun a () (b)) (defun b () (a)) (a)`,
		"tail-loop-with-counter": `(defun countup (n) (countup (+ n 1))) (countup 0)`,

		// --- unbounded NON-tail recursion (MaxHeightPhysical).  This is the
		// shape that overflows the Go goroutine stack, an abort recover()
		// cannot intercept, so the physical-height limit is load-bearing
		// rather than a nicety. ---
		"non-tail-recursion-forever": `(defun rec (n) (+ 1 (rec n))) (rec 0)`,
		"non-tail-mutual":            `(defun p (n) (+ 1 (q n))) (defun q (n) (+ 1 (p n))) (p 0)`,

		// --- unbounded ARGUMENT nesting (MaxEvalNesting).  Same failure
		// mode as the two above -- Go stack exhaustion -- reached by a
		// shape no stack-height limit can see: evalSExprCells evaluates a
		// call's arguments BEFORE pushing the call's frame, so this
		// recurses through the whole evaluator at physical height zero.
		//
		// The nesting is generated during macro expansion from an integer,
		// so the source is constant-size and rdparser's parse-depth limit
		// -- which was the only thing incidentally bounding this -- never
		// sees it.  Measured against the pre-fix tree at stock defaults,
		// the 800000 form aborted the process with "fatal error: stack
		// overflow" (issue #316).  MaxEvalNesting is what stops it now. ---
		"macro-generated-arg-nesting": `(defmacro nest (n) (if (<= n 0) 1 (quasiquote (identity (nest (unquote (- n 1))))))) (nest 800000)`,

		// --- non-recursive infinite loop.  Neither stack limit can see this:
		// it grows no frames and performs no tail calls.  Only a step budget
		// or a context deadline stops it. ---
		//
		// Note the control sequence: `(i 1000000000)`, NOT `([i 1000000000])`.
		// `[` and `(` are interchangeable in ELPS, so the bracketed form reads
		// as `((i 1000000000))` -- a control sequence whose first element is a
		// list rather than a symbol -- and dotimes rejects it as a syntax
		// error before consulting any budget.  Written that way this seed
		// "passes" without ever having started a loop, which is exactly the
		// wrong reason to pass and hid a real defect: the EMPTY-BODY form
		// below never reaches a limit check at all.
		"dotimes-empty-body": `(dotimes (i 1000000000))`,
		"dotimes-with-body":  `(dotimes (i 1000000000) 1)`,

		// --- unbounded macro expansion (MaxMacroExpansionDepth) ---
		"macro-expands-forever": `(defmacro m () (quasiquote (m))) (m)`,

		// --- unbounded allocation.  These describe an enormous result in a
		// handful of bytes, which is the ratio that makes them dangerous:
		// there is no input-size budget that catches them. ---
		"make-sequence-huge": `(make-sequence 0 1000000000)`,
		"map-huge":           `(map 'list to-string (make-sequence 0 100000000))`,
		"concat-huge":        `(concat 'list (make-sequence 0 100000000) '(1))`,

		// --- unbounded allocation via the expr placeholder index.  The
		// formals slice is sized from an integer read straight out of the
		// source text, so 12 bytes of input asks for ~half a billion
		// freshly-constructed symbols.  Regression seed for A1 (issue #320).
		"expr-huge-placeholder":      `#^(%555555591)`,
		"expr-huge-placeholder-call": `#^(list %10485760)`,
	}
}

// EvalTerminating returns programs that must run to completion, without
// error, under the same bounded budget the runaway corpus is evaluated with.
//
// These exist to keep the budgets honest.  Every entry here is a correct,
// bounded ELPS program; if one starts erroring, a limit has been tuned into
// the range of ordinary programs, which is a defect even though the process
// survived.
//
// Note what is NOT here: deeply nested non-tail forms.  Nesting a few thousand
// progn levels reports "physical stack height exceeded maximum", which is the
// physical-height limit doing its job -- that program is unbounded recursion
// from the stack's point of view, so it belongs in EvalRunaway or nowhere.  An
// earlier version of this corpus filed it under EvalTerminating and the
// mistake survived because the terminating assertion only checked that
// evaluation FINISHED, never that it finished without an error.  Both are
// asserted now.
func EvalTerminating() map[string]string {
	return map[string]string{
		// --- trivial / boundary ---
		"empty": ``,
		"whitespace": `
`,
		"nil":     `()`,
		"integer": `42`,
		"string":  `"hello"`,
		"comment": `; nothing but a comment`,

		// --- bounded tail recursion.  Deep enough to exercise the TRO path
		// and the tail-iteration counter, far below the default backstop. ---
		"tail-recursion-bounded": `(defun countdown (n) (if (<= n 0) 'done (countdown (- n 1)))) (countdown 10000)`,

		// --- bounded non-tail recursion.  An ELPS recursion level costs more
		// than one physical frame (measured: `(sum 1000)` reaches height
		// 2001), so this is kept well clear of the harness's 2000-frame
		// budget rather than nominally under it. ---
		"non-tail-recursion-bounded": `(defun sum (n) (if (<= n 0) 0 (+ n (sum (- n 1))))) (sum 500)`,

		// --- ordinary programs: closures, higher-order functions, maps,
		// strings, sequences, error handling ---
		"lambda-apply":      `((lambda (x y) (+ x y)) 1 2)`,
		"let-binding":       `(let ([a 1] [b 2]) (+ a b))`,
		"map-over-sequence": `(map 'list (lambda (x) (* x x)) (make-sequence 0 100))`,
		"foldl-bounded":     `(foldl + 0 (make-sequence 0 1000))`,
		"sorted-map":        `(let ([m (sorted-map 'a 1 'b 2)]) (get m 'a))`,
		"string-ops":        `(string:join (map 'list to-string (make-sequence 0 10)) ",")`,
		"handler-bind":      `(handler-bind ([condition (lambda (c &rest args) 'caught)]) (error 'boom "x"))`,
		"ignore-errors":     `(ignore-errors (error 'boom "x"))`,
		// with-cleanup runs cleanup on both exit paths.  The erroring shapes
		// are wrapped so they belong here rather than in EvalErroring: what
		// is under test is that the operator TERMINATES with the cleanup
		// having run, not the error it lets through.
		"with-cleanup-normal":        `(with-cleanup (2) 1)`,
		"with-cleanup-error":         `(ignore-errors (with-cleanup (1) (error 'boom "x")))`,
		"with-cleanup-cleanup-error": `(ignore-errors (with-cleanup ((error 'boom "x")) 1))`,
		"with-cleanup-multi-body":    `(set 'n 0) (with-cleanup ((set! 'n (+ n 10))) (set! 'n 1) (set! 'n (+ n 1))) n`,
		"with-cleanup-nested":        `(set 'n 0) (ignore-errors (with-cleanup ((set! 'n (+ n 1))) (with-cleanup ((set! 'n (+ n 1))) (error 'boom "x")))) n`,
		"with-cleanup-in-loop":       `(set 'n 0) (dotimes (i 100) (with-cleanup ((set! 'n (+ n 1))) (set! 'n (+ n 1)))) n`,
		"defmacro-bounded":           `(defmacro twice (x) (quasiquote (progn (unquote x) (unquote x)))) (twice 1)`,
		"dotimes-bounded":            `(dotimes (i 1000))`,
		"dotimes-bounded-fn":         `(dotimes (i 1000) (+ i 1))`,

		// --- the expr special operator's legitimate forms.  A1/A2 guard the
		// placeholder index; these pin that the guard did not also break
		// ordinary anonymous functions, including one with more formals than
		// a naive "small" cap would allow. ---
		"expr-short":       `(funcall #^(+ % 1) 1)`,
		"expr-numbered":    `(funcall #^(+ %1 %2) 1 2)`,
		"expr-rest":        `(funcall #^(list %&rest) 1 2 3)`,
		"expr-nine-args":   `(funcall #^(list %1 %2 %3 %4 %5 %6 %7 %8 %9) 1 2 3 4 5 6 7 8 9)`,
		"expr-many-args":   `#^(list %64)`,
		"expr-max-formals": `#^(list %1024)`,

		// --- append with a type specifier: A3's site.  Both the valid bytes
		// form and the valid list form. ---
		"append-bytes-valid": `(append 'bytes (to-bytes "ab") 99)`,
		"append-list-valid":  `(append 'list '(1 2) 3)`,

		// --- the seal guards on program literals (issue #372 machinery,
		// issue #378 semantics; see lisp/seal.go).  The seeds that DRIVE a
		// kernel mutator at a sealed literal live in EvalErroring now: those
		// programs raise the catchable modify-literal-error condition, so
		// they must error.  What belongs here is the other three quarters of
		// the contract, which must keep completing without error:
		//
		//   - the error is catchable through the ordinary condition
		//     machinery (handler-bind by name, ignore-errors),
		//   - (copy lit) is the sanctioned remedy and takes the ordinary
		//     mutable path, and
		//   - the empty carve-out: cdr/rest return the shared sealed empty
		//     list, and the guarded sites accept it rather than turning
		//     (stable-sort < (rest xs)) into a data-dependent error.
		//
		// The eval fuzz harness fingerprints every input's sealed parse
		// before and after evaluation, so these seeds also keep the
		// literal-stays-pristine oracle pointed at the guarded sites. ---
		"sealed-write-caught-by-name":  `(handler-bind ([modify-literal-error (lambda (c &rest args) 'caught)]) (stable-sort > '(9 3 7 1 8)))`,
		"sealed-write-ignored":         `(ignore-errors (append 'vector (slice 'list '(1 2 3) 0 1) 99)) '(1 2 3)`,
		"sealed-write-copy-remedy":     `(stable-sort > (copy '(9 3 7 1 8)))`,
		"sealed-slice-vector-copy":     `(slice 'vector (copy '(1 2 3)) 0 2)`,
		"sealed-append-vector-copy":    `(append 'vector (copy '(1 2 3)) 4)`,
		"sealed-sort-empty-view":       `(stable-sort < (rest '(1)))`,
		"sealed-append-empty-view":     `(append 'vector (cdr '(1)) 99)`,
		"sealed-slice-vector-empty":    `(slice 'vector (rest '(1)) 0 0)`,
		"sealed-slice-vector-empty-of": `(slice 'vector '(1 2 3) 1 1)`,

		// --- debug-print: A4's site.  Both branches must reach the runtime's
		// configured Stderr, which the harness captures. ---
		"debug-print-empty": `(debug-print)`,
		"debug-print-args":  `(debug-print 1 2 3)`,

		// --- the sequence constructors that build their result through
		// lisp.Array, whose unsupplied cells used to be Go nils (#367).  The
		// empty forms are the interesting ones: they are the shapes that ask
		// for storage and then fill none of it.
		//
		// concat is here for a second reason.  Its vector branch no longer
		// allocates the container first and writes through it; it fills a
		// slice and wraps the finished slice, so these rows are what pin that
		// the rewrite still produces the same sequences.
		"concat-vector-empty":  `(concat 'vector)`,
		"concat-vector":        `(concat 'vector (vector 1 2) (vector 3))`,
		"concat-list-empty":    `(concat 'list)`,
		"concat-list":          `(concat 'list '(1 2) '(3))`,
		"concat-vector-mixed":  `(concat 'vector '(1 2) (vector 3))`,
		"map-vector":           `(map 'vector to-string (make-sequence 0 4))`,
		"map-vector-empty":     `(map 'vector to-string ())`,
		"zip-vector":           `(zip 'vector '(1 2) '(3 4))`,
		"reverse-vector-empty": `(reverse 'vector (vector))`,
		"sequence-vector-read": `(let ([v (concat 'vector (vector 1))]) (list (nth v 0) (aref v 0) (equal? v v)))`,
	}
}

// EvalErroring returns programs that must FINISH under the bounded budget --
// no watchdog, no runaway -- and must finish with an ordinary, catchable
// error that is neither a budget stop nor an internal panic.
//
// This is the third leg of the corpus split.  EvalRunaway seeds are stopped
// BY a budget; EvalTerminating seeds must complete without error; these
// seeds terminate almost immediately in a deterministic error condition that
// is part of the language contract.  Filing them under EvalRunaway would
// dilute the "a budget is holding" assertion (they never consult a budget),
// and they cannot live in EvalTerminating because erroring is exactly what
// they must do.
//
// Today the corpus is the sealed-write guard (issue #378): each seed drives
// a kernel mutator at a parsed program literal, or at a backing-sharing view
// of one (cdr / slice), and must raise the catchable modify-literal-error
// condition.  Before issue #378 flipped the policy these were
// EvalTerminating seeds -- the sites copy-on-wrote silently -- and before
// the seal existed each one rewrote the literal in place (stable-sort's
// in-place sort, append 'vector's spare-capacity write: the substrate#378
// class).  The seal invariant they guard still needs fuzz coverage: the
// eval fuzz harness fingerprints every input's sealed parse before and
// after evaluation, so if a guard regresses to an in-place write, these
// seeds fail the very first corpus pass loudly instead of leaving detection
// to the mutator's luck.  TestEvalErroringSeedsError additionally pins that
// each seed's error carries the modify-literal-error condition -- a seed
// erroring for a different reason (a typo, a renamed builtin) is a broken
// seed, not a passing one.
func EvalErroring() map[string]string {
	return map[string]string{
		"sealed-write-stable-sort-literal":       `(set 'q '(9 3 7 1 8)) (stable-sort > q) q`,
		"sealed-write-stable-sort-literal-view":  `(stable-sort < (cdr '(9 3 7 1 8)))`,
		"sealed-write-stable-sort-defun-literal": `(defun s () (let ([q '(3 1 2)]) (stable-sort > q) q)) (s) (s)`,
		"sealed-write-append-vector-slice":       `(set 'lit '(1 2 3)) (append 'vector (slice 'list lit 0 1) 99) lit`,
		"sealed-write-append-vector-cdr":         `(append 'vector (cdr '(1 2 3)) 99)`,
		"sealed-write-slice-vector-literal":      `(slice 'vector '(1 2 3) 0 2)`,
		"sealed-write-append-vector-no-values":   `(append 'vector '(1 2 3))`,
		// with-cleanup does not catch.  Both of these raise the same
		// modify-literal-error the seeds above pin -- once from the body,
		// once from a cleanup form -- so they satisfy this corpus's
		// exact-condition contract while pinning the operator's own: a
		// regression that swallowed either would make the seed COMPLETE,
		// which is precisely what this corpus reports as a removed guard.
		"with-cleanup-does-not-catch-body":    `(with-cleanup (1) (stable-sort > '(9 3 7 1 8)))`,
		"with-cleanup-does-not-catch-cleanup": `(with-cleanup ((stable-sort > '(9 3 7 1 8))) 1)`,
	}
}

// EvalAdversarial returns small hand-written programs aimed at the evaluator's
// sharp edges rather than at a specific budget.  Unlike the two corpora above
// these carry no expected outcome -- an error is a perfectly good answer for
// most of them.  Their job is to be good STARTING POINTS for the mutator:
// each one puts the fuzzer inside a distinct evaluation code path (special
// operators, the package system, type dispatch, the FFI-shaped builtins) that
// random bytes would essentially never reach.
//
// Every entry is small, for the reason documented on Adversarial: seed size
// dominates the cost of a coverage-guided run.
func EvalAdversarial() []string {
	return []string{
		// --- the expr placeholder grammar, which reads an integer straight
		// out of source text and uses it as a slice length.  A2's minimised
		// input is here (#^%-1); the huge ones live in EvalRunaway. ---
		"#^%", "#^%1", "#^%-1", "#^%-0", "#^%+1", "#^%&rest", "#^%&optional",
		"#^(%-1)", "#^(list %-1)", "#^(%1 %2)", "#^(%1 %)", "#^(%&rest %1)",
		"#^%99999999999999999999", "#^(%0)", "#^(% %)",
		"#^(list %1025)", // one past MaxExprFormals

		// --- dotimes, whose Go-level loop is driven by a program-supplied
		// count.  The empty-body form is the one that reached no limit check
		// at all.  `[` and `(` are interchangeable, so the bracketed control
		// sequence reads as a nested list and is a syntax error -- kept here
		// deliberately, as a shape the mutator should be able to reach. ---
		"(dotimes (i 100))", "(dotimes (i 100) 1)", "(dotimes ([i 100]))",
		"(dotimes (i -1))", "(dotimes (i 0))", "(dotimes (i 1 2 3))",

		// --- append / bytes type dispatch.  A3's minimised input is the
		// first of these: the type specifier is validated, the sequence is
		// not. ---
		"(append 'bytes 0)", "(append 'bytes 'x)", "(append 'bytes ())",
		"(append 'bytes \"s\")", "(append 'list 0)", "(append 'vector 0)",
		"(append 0 0)", "(append-bytes 0 0)", "(append! 0 0)",

		// --- arity and type confusion on core builtins ---
		"(+)", "(+ 'a)", "(+ \"a\" 1)", "(/ 1 0)", "(/ 0 0)", "(% 1 0)",
		"(nth () 0)", "(nth '(1) -1)", "(aref (vector 1) -1)",
		"(slice 'list '(1 2) 2 0)", "(car 0)", "(cdr 0)",

		// --- special operators with malformed control structure ---
		"(if)", "(let)", "(let ())", "(let (a))", "(lambda)", "(lambda 0)",
		"(defun)", "(defmacro)", "(quasiquote (unquote))", "(unquote 1)",
		"(handler-bind)", "(handler-bind ())", "(handler-bind (()) 1)",
		"(dotimes)", "(dotimes ())", "(dotimes (i))", "(dotimes (i 'a))",

		// --- the package system ---
		"(in-package 'nope)", "(use-package 'nope)", "(export 'nope)",
		"(in-package)", "nope:sym", "a:b:c",

		// --- error machinery, including the forgery IsInternalPanic must
		// not accept ---
		"(error)", "(error 'x)", "(error 'internal-panic \"forged\")",
		"(ignore-errors (error 'internal-panic \"forged\"))",
		"(handler-bind ([condition (lambda (c &rest r) 'k)]) (error 'internal-panic \"forged\"))",

		// --- allocation-shaped builtins at small sizes.  Small so they are
		// cheap seeds; the mutator's job is to grow the integer. ---
		"(make-sequence 0 4)", "(make-sequence 0 -1)", "(make-sequence 0 10 0)",

		"(concat 'string \"a\" \"b\")", "(reverse 'list '(1 2))",
		"(zip 'list '(1) '(2))",

		// --- string / bytes conversion ---
		"(to-bytes \"a\")", "(to-string (to-bytes \"a\"))",
		"(string 1)", "(format-string \"{}\" 1)", "(format-string \"{\" 1)",

		// --- debug output.  A4's minimised input is the zero-argument form,
		// which wrote to os.Stdout instead of the runtime's Stderr. ---
		"(debug-print)", "(debug-print 1)", "(debug-stack)",

		// --- type confusion at the interpreter's PANICKING accessors
		// (issue #367).  LVal.Bytes, LVal.Map, LVal.FunData and seqCells all
		// panic on a wrong-typed receiver, and each is unreachable from lisp
		// only because every caller checks the type first.  Each line below
		// offers a builtin the wrong type for the accessor it is about to
		// reach, so a caller that drops its check turns one of these into an
		// internal-panic instead of an error.
		//
		// `(append 'bytes 0)` above is the shape that actually shipped: the
		// type SPECIFIER was validated and the sequence was not.  These
		// generalise it across the accessors and give the mutator a starting
		// point inside each one.
		//
		// LVal.Bytes:
		"(concat 'bytes 0)", "(reverse 'bytes 0)", "(to-string 0)",
		"(base64:std-encode 0)", "(json:dump-string (to-bytes \"a\"))",
		// LVal.Map:
		"(get 0 'a)", "(assoc 0 'a 1)", "(assoc! 0 'a 1)", "(keys 0)",
		"(dissoc 0 'a)", "(key? 0 'a)", "(sorted-map 0 1)",
		"(elpspath:? 0 \"a\")", "(elpspath:?set 0 \"a\" 1)",
		// LVal.FunData, reached by anything that takes a function argument:
		"(map 'list 0 '(1))", "(foldl 0 0 '(1))", "(sort 0 '(1))",
		"(apply 0 '())", "(funcall 0)", "(select 'list 0 '(1))",
		// seqCells, whose contract is "the caller guarded with isSeq":
		"(nth 0 0)", "(first 0)", "(rest 0)", "(slice 'list 0 0 1)",
		"(zip 'list 0 0)", "(insert-index 'list 0 0 1)",
		// toFloat, reached once both operands are believed numeric:
		"(< 1 'a)", "(max 1 \"a\")", "(min 'a 1)", "(pow 1 'a)",
		"(make-sequence 0 1 'a)", "(equal? 1 'a)",
		// LVal.CallStack, which panics on a non-error receiver:
		"(ignore-errors (get 0 'a))", "(handler-bind ([condition (lambda (c &rest r) c)]) (get 0 'a))",
	}
}
