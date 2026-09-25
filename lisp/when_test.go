package lisp_test

import (
	"context"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWhenUnlessDefaultWhile pins the special operators' semantics against
// the quasiquote macros embedders have historically defined for them.
func TestWhenUnlessDefaultWhile(t *testing.T) {
	tests := elpstest.TestSuite{
		{"when", elpstest.TestSequence{
			{"(when true 1 2 3)", "3", ""},
			{"(when () 1 2 3)", "()", ""},
			{"(when false 1)", "()", ""},
			{"(when 0 1)", "1", ""},
			{"(when true)", "()", ""},
			{"(when ())", "()", ""},
			{"(set 'n 0)", "0", ""},
			{"(when (progn (set! n (+ n 1)) n) (set! n (+ n 10)) (set! n (+ n 100)) n)", "111", ""},
			{"(when () (set! n 0))", "()", ""},
			{"n", "111", ""},
			{"(ignore-errors (progn (when (error 'boom \"x\") 1) 'ok))", "()", ""},
			{"(ignore-errors (progn (when true (error 'boom \"x\") (set! n 0)) 'ok))", "()", ""},
			{"n", "111", ""},
			{"(ignore-errors (progn (when) 'ok))", "()", ""},
		}},
		{"unless", elpstest.TestSequence{
			{"(unless () 1 2 3)", "3", ""},
			{"(unless false 1)", "1", ""},
			{"(unless true 1 2 3)", "()", ""},
			{"(unless 0 1)", "()", ""},
			{"(unless ())", "()", ""},
			{"(ignore-errors (progn (unless (error 'boom \"x\") 1) 'ok))", "()", ""},
			{"(ignore-errors (progn (unless) 'ok))", "()", ""},
		}},
		{"default", elpstest.TestSequence{
			{"(default 1 2)", "1", ""},
			{"(default () 2)", "2", ""},
			{"(default false 2)", "false", ""},
			{"(default 0 2)", "0", ""},
			{`(default "" 2)`, `""`, ""},
			{"(set 'n 0)", "0", ""},
			// value is evaluated exactly once; fallback only when needed
			{"(default (progn (set! n (+ n 1)) n) (set! n 100))", "1", ""},
			{"n", "1", ""},
			{"(default (progn (set! n (+ n 1)) ()) (progn (set! n (+ n 10)) n))", "12", ""},
			{"(ignore-errors (progn (default (error 'boom \"x\") 2) 'ok))", "()", ""},
			{"(ignore-errors (progn (default () (error 'boom \"x\")) 'ok))", "()", ""},
			{"(ignore-errors (progn (default 1) 'ok))", "()", ""},
			{"(ignore-errors (progn (default 1 2 3) 'ok))", "()", ""},
		}},
		{"while", elpstest.TestSequence{
			{"(set 'i 0)", "0", ""},
			{"(set 'acc ())", "()", ""},
			{"(while (< i 5) (set! acc (cons i acc)) (set! i (+ i 1)))", "()", ""},
			{"acc", "'(4 3 2 1 0)", ""},
			{"(while ())", "()", ""},
			{"(while () (error 'boom \"x\"))", "()", ""},
			{"(set! i 0)", "()", ""},
			{"(ignore-errors (progn (while (< i 3) (set! i (+ i 1)) (when (= i 2) (error 'boom \"x\"))) 'ok))", "()", ""},
			{"i", "2", ""},
			{"(ignore-errors (progn (while (error 'boom \"x\")) 'ok))", "()", ""},
			{"(ignore-errors (progn (while) 'ok))", "()", ""},
			// the body shares the enclosing scope
			{"(let ([k 0]) (while (< k 3) (set! k (+ k 1))) k)", "3", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// TestWhileRespectsLimits pins that an iterative while still stops: the
// macro it replaces was a tail-recursive labels loop, bounded by default by
// MaxTailIterations, so the special operator applies the same bound per loop
// entry, and consults the step and context limits once per turn (including
// an empty body, the shape that escaped dotimes in #320).
func TestWhileRespectsLimits(t *testing.T) {
	t.Run("tail iterations default on", func(t *testing.T) {
		env := newLimitTestEnv(t, lisp.WithMaxTailIterations(1000))
		for _, src := range []string{`(while true)`, `(while true 1)`} {
			got := env.LoadString("w.lisp", src)
			require.Equal(t, lisp.LError, got.Type, src)
			assert.Contains(t, got.String(), "tail-call iteration limit exceeded")
			assert.False(t, lisp.IsInternalPanic(got))
		}
		// a bounded loop under the limit completes, and the counter is per
		// entry, so repeated loops each get the full allowance
		require.NoError(t, lisp.GoError(env.LoadString("w.lisp",
			`(set 'i 0) (while (< i 900) (set! i (+ i 1)))
			 (set! i 0) (while (< i 900) (set! i (+ i 1)))`)))
	})
	t.Run("max steps", func(t *testing.T) {
		env := newLimitTestEnv(t, lisp.WithMaxTailIterations(0), lisp.WithMaxSteps(10000))
		got := env.LoadString("w.lisp", `(while true)`)
		require.Equal(t, lisp.LError, got.Type)
		assert.Contains(t, got.String(), "step limit exceeded")
	})
	t.Run("context deadline", func(t *testing.T) {
		env := newLimitTestEnv(t, lisp.WithMaxTailIterations(0))
		ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
		defer cancel()
		got := env.LoadStringContext(ctx, "w.lisp", `(while true)`)
		require.Equal(t, lisp.LError, got.Type)
		assert.Contains(t, got.String(), "context")
	})
}

// TestWhenFamilyUserBindingsTakePrecedence is the compatibility contract.
// Symbols resolve through the current package's own table, and use-package
// copies exported bindings into that table (last import wins, no conflict
// error), so a package that defines or imports its own when/unless/while/
// default keeps using it; only packages that do neither see lisp's operator.
func TestWhenFamilyUserBindingsTakePrecedence(t *testing.T) {
	macros := `
(in-package 'utils)
(export 'when 'unless 'while 'default)
(defmacro when (p &rest body) (quasiquote (list 'utils-when (unquote p))))
(defmacro unless (p &rest body) (quasiquote (list 'utils-unless (unquote p))))
(defmacro while (p &rest body) (quasiquote (list 'utils-while (unquote p))))
(defmacro default (x d) (quasiquote (list 'utils-default (unquote x) (unquote d))))
`
	for _, tc := range []struct{ name, src, want string }{
		{"own package defmacro", macros + `(in-package 'utils) (list (when 1) (unless 2) (while 3) (default 4 5))`,
			`'('('utils-when 1) '('utils-unless 2) '('utils-while 3) '('utils-default 4 5))`},
		{"use-package importer", macros + `(in-package 'svc) (use-package 'utils) (list (when 1) (unless 2) (while 3) (default 4 5))`,
			`'('('utils-when 1) '('utils-unless 2) '('utils-while 3) '('utils-default 4 5))`},
		{"importer can still reach lisp", macros + `(in-package 'svc) (use-package 'utils) (list (lisp:when 1 'op) (lisp:default () 'op))`,
			`'('op 'op)`},
		{"unrelated package sees op", macros + `(in-package 'other) (list (when 1 'op) (unless () 'op) (default () 'op))`,
			`'('op 'op 'op)`},
		{"user defun", `(defun when (x) (list 'fn x)) (when 1)`, `'('fn 1)`},
		{"user set", `(set 'default 42) default`, `42`},
		{"user set then call", `(set 'while (lambda (x) (* x 2))) (while 21)`, `42`},
		{"shirocore-shaped when macro", `
(in-package 'utils)
(export 'when)
(defmacro when (predicate &rest body)
  (quasiquote (if (unquote predicate) (progn (unquote-splicing body)) ())))
(export 'while)
(defmacro while (pred &rest body)
  (let* ([f (gensym)])
    (quasiquote (labels ([(unquote f) () (when (unquote pred) (progn (unquote-splicing body)) ((unquote f)))]) ((unquote f))))))
(export 'default)
(defmacro default (x d)
  (let* ([s (gensym)])
    (quasiquote (let* ([(unquote s) (unquote x)]) (if (nil? (unquote s)) (unquote d) (unquote s))))))
(in-package 'phylum)
(use-package 'utils)
(set 'i 0)
(while (< i 3) (set! i (+ i 1)))
(list (when true 1 2) (default () i) (macroexpand '(when true 1)))`,
			`'(2 3 '(if true (progn 1) ()))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			got := env.LoadString("compat.lisp", tc.src)
			require.NoError(t, lisp.GoError(got))
			assert.Equal(t, tc.want, got.String())
			// the user binding never leaks into package lisp or user
			assert.Equal(t, "'op", env.LoadString("next.lisp",
				`(in-package 'fresh) (when true 'op)`).String())
		})
	}
}

// TestWhenFamilySealed pins that the operators themselves cannot be rebound
// in package lisp, like every other lisp binding.
func TestWhenFamilySealed(t *testing.T) {
	for _, name := range []string{"when", "unless", "while", "default"} {
		env := newLimitTestEnv(t)
		got := env.LoadString("seal.lisp", "(set 'lisp:"+name+" 1)")
		require.Equal(t, lisp.LError, got.Type)
		assert.Contains(t, got.String(), "cannot rebind lisp package binding: "+name)
	}
}

func TestWhenFamilyLispFile(t *testing.T) {
	(&elpstest.Runner{}).RunTestFile(t, "testdata/when_family_test.lisp")
}
