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

		// --- non-recursive infinite loop.  Neither stack limit can see this:
		// it grows no frames and performs no tail calls.  Only a step budget
		// or a context deadline stops it. ---
		"dotimes-huge": `(dotimes (i 1000000000))`,
		"foldl-huge":   `(foldl (lambda (acc x) acc) 0 (range 100000000))`,

		// --- unbounded macro expansion (MaxMacroExpansionDepth) ---
		"macro-expands-forever": `(defmacro m () (quasiquote (m))) (m)`,

		// --- unbounded allocation.  These describe an enormous result in a
		// handful of bytes, which is the ratio that makes them dangerous:
		// there is no input-size budget that catches them. ---
		"make-sequence-huge": `(make-sequence 'list 0 1000000000)`,
		"range-huge":         `(range 1000000000)`,
		"concat-huge":        `(concat 'string (make-sequence 'list 0 100000000))`,

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
		"empty":     ``,
		"whitespace": `
`,
		"nil":     `()`,
		"integer": `42`,
		"string":  `"hello"`,
		"comment": `; nothing but a comment`,

		// --- bounded tail recursion.  Deep enough to exercise the TRO path
		// and the tail-iteration counter, far below the default backstop. ---
		"tail-recursion-bounded": `(defun countdown (n) (if (<= n 0) 'done (countdown (- n 1)))) (countdown 10000)`,

		// --- bounded non-tail recursion, well under MaxHeightPhysical ---
		"non-tail-recursion-bounded": `(defun sum (n) (if (<= n 0) 0 (+ n (sum (- n 1))))) (sum 1000)`,

		// --- ordinary programs: closures, higher-order functions, maps,
		// strings, sequences, error handling ---
		"lambda-apply":     `((lambda (x y) (+ x y)) 1 2)`,
		"let-binding":      `(let ([a 1] [b 2]) (+ a b))`,
		"map-over-range":   `(map 'list (lambda (x) (* x x)) (range 100))`,
		"foldl-bounded":    `(foldl + 0 (range 1000))`,
		"sorted-map":       `(let ([m (sorted-map 'a 1 'b 2)]) (get m 'a))`,
		"string-ops":       `(string:join (map 'list string (range 10)) ",")`,
		"handler-bind":     `(handler-bind ([condition (lambda (c &rest args) 'caught)]) (error 'boom "x"))`,
		"ignore-errors":    `(ignore-errors (error 'boom "x"))`,
		"defmacro-bounded": `(defmacro twice (x) (quasiquote (progn (unquote x) (unquote x)))) (twice 1)`,
		"dotimes-bounded":  `(dotimes (i 1000))`,

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

		// --- debug-print: A4's site.  Both branches must reach the runtime's
		// configured Stderr, which the harness captures. ---
		"debug-print-empty": `(debug-print)`,
		"debug-print-args":  `(debug-print 1 2 3)`,
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
		"(make-sequence 'list 0 4)", "(make-sequence 'list 0 -1)",
		"(range 4)", "(range -1)", "(range 0 10 0)",
		"(concat 'string \"a\" \"b\")", "(reverse 'list '(1 2))",
		"(zip 'list '(1) '(2))",

		// --- string / bytes conversion ---
		"(to-bytes \"a\")", "(to-string (to-bytes \"a\"))",
		"(string 1)", "(format-string \"{}\" 1)", "(format-string \"{\" 1)",

		// --- debug output.  A4's minimised input is the zero-argument form,
		// which wrote to os.Stdout instead of the runtime's Stderr. ---
		"(debug-print)", "(debug-print 1)", "(debug-stack)",
	}
}
