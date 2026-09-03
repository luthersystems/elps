// Copyright © 2026 The ELPS authors

package elpstest_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
)

// Each test here is a fork bug that shipped, written as the ForkCheck
// that fails on the tree it shipped in and passes on the tree that fixed
// it.  The program is the shape that reached the bug; the transactions
// are what a caller would run on a fork and observe diverging from a
// cold load.

// Issue #576: `(quasiquote (unquote a))` yields a second header on a's
// sorted-map.  Fork memoised copies per header, so the fork's a and b
// were two maps, and a write through one was invisible through the other
// — on the fork only.  Fixed in #587.
func TestForkCheck_SortedMapAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(set 'both (list a b))
`,
		Tx: []string{
			`(assoc! a "y" 7) (get b "y")`,
			`(dissoc! b "k") (get a "k")`,
			`(assoc! (first both) "z" 1) (list (get (second both) "z") (get a "z"))`,
		},
	})
}

// Issue #576, second payload kind: the same two-header shape over a bytes
// value, which append! grows in place.  Fixed in #587.
func TestForkCheck_BytesAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'a (to-bytes "abc"))
(set 'b (quasiquote (unquote a)))
`,
		Tx: []string{
			`(append! a 7) (length b)`,
			`(append! b 1 2) (length a)`,
		},
	})
}

// countingCloner is a NativeCloner accumulator: the kind of Go payload an
// embedder binds at load time and mutates per transaction.
type countingCloner struct{ clones int }

func (c *countingCloner) CloneNative() interface{} {
	return &countingCloner{clones: c.clones + 1}
}

// Issue #576, third payload kind: two headers over one native payload
// were cloned once per header, so an accumulator the template held once
// became two independent accumulators in the fork.  Fixed in #587.  The
// program cannot express this shape (natives are bound from Go), so
// NewEnv binds it.
func TestForkCheck_NativeAliasAcrossHeaders(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			env, err := elpstest.NewForkCheckEnv()
			if err != nil {
				return nil, err
			}
			a := lisp.Native(&countingCloner{})
			b := *a // a second header over the same payload
			env.PutGlobal(lisp.Symbol("a"), a)
			env.PutGlobal(lisp.Symbol("b"), &b)
			return env, nil
		},
		Tx: []string{`(list a b)`},
	})
}

// Issue #579: a libschema validator minted on the template stopped being
// a validator in a fork, because its credential was the identity of a
// marker cell the fork had copied.  Fixed in #581.  The failing
// validation is included so an error stays an error of the same kind.
func TestForkCheck_SchemaValidatorCredential(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(s:deftype "T" s:int)
(set 'anon (s:make-validator "Anon" s:int (s:gt 1)))
`,
		Tx: []string{
			`(s:validate T 3)`,
			`(s:validate anon 3)`,
			`(s:validate T "nope")`,
			`(s:deftype "U" s:string) (s:validate U "x")`,
			`(s:validate (s:make-validator "Fresh" s:string) "x")`,
		},
	})
}

// Issue #381 (fixed in #581): a fork shared the template's lisp testing
// suite, a Go accumulator held as a native global, so a test registered
// on a fork landed in the template.  The suite is an opaque native to the
// state and isolation oracles (rendered by type; no identity unless it
// is a NativeCloner, which the fix made it).  Parity sees the share
// because registering a name the template's suite already holds is an
// error: both transactions register "one", so on a shared suite the
// second fork to run it fails where the cold environment does not.
func TestForkCheck_TestingSuitePerFork(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `(use-package 'testing)`,
		Tx: []string{
			`(test "one" (assert-equal 1 1))`,
			`(test "one" (assert-equal 2 2))`,
		},
	})
}

// Closure-captured state: the state a fork must copy and a transaction
// mutates through the closure, invisible from the package bindings except
// through the function.  A walker that stopped at the function header
// would pass a fork that shared the captured environment.
func TestForkCheck_ClosureState(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(let ([outer (vector 0)] [box (sorted-map "n" 0)])
  (defun bump! () (append! outer 1) (assoc! box "n" (+ 1 (get box "n"))) ())
  (defun peek () (list (length outer) (get box "n"))))
(set 'shared (sorted-map "k" 1))
(defun share-through-closure () shared)
`,
		Tx: []string{
			`(bump!)`,
			`(bump!) (bump!) (peek)`,
			`(assoc! (share-through-closure) "k" 2) (get shared "k")`,
		},
	})
}

// The shapes the existing fork tests already pin, run through the
// harness so a regression in any of them shows up here with the same
// diagnostics: closures over mutable state, macros, labels mutual
// recursion, nested maps, bytes.
func TestForkCheck_LoadedProgram(t *testing.T) {
	elpstest.RunForkCheck(t, elpstest.ForkCheck{
		Program: `
(set 'counter-box (vector 0))
(defun make-adder (n) (lambda (x) (+ x n)))
(set 'add2 (make-adder 2))
(defmacro with-logging (expr) (quasiquote (progn (unquote expr))))
(defun handler (m) (get-default m "k" (with-logging (add2 40))))
(labels ([even? (n) (if (= n 0) true (odd? (- n 1)))]
         [odd? (n) (if (= n 0) false (even? (- n 1)))])
  (set 'evens even?))
(set 'config (sorted-map "a" 1 "b" (vector 1 2 3) "inner" (sorted-map "n" 0)))
(set 'blob (to-bytes "mutable-bytes"))
(set 'e (list))
`,
		Tx: []string{
			`(handler (sorted-map "x" 1))`,
			`(funcall evens 10)`,
			`(append! counter-box 99) (assoc! config "a" 100) (assoc! (get config "inner") "n" 5) (list counter-box config)`,
			`(set 'fv (append 'vector e 'fork)) (nth fv 0)`,
			`(append! blob 33) (length blob)`,
		},
	})
}
