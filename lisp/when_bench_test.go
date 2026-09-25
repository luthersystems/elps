package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// whenMacroSource is the quasiquote defmacro shape embedders have used to
// supply when/unless before they were special operators .  It is
// defined under the name when so that it shadows lisp:when in the user
// package, exactly as a library package exporting its own when does.
const whenMacroSource = `
(defmacro when (predicate &rest body)
  (quasiquote (if (unquote predicate) (progn (unquote-splicing body)) ())))
(defmacro unless (predicate &rest body)
  (quasiquote (if (not (unquote predicate)) (progn (unquote-splicing body)) ())))
(defmacro while (predicate-expression &rest consequent-expressions)
  (let* ([function-binding (gensym)])
    (quasiquote
      (labels ([(unquote function-binding)
                ()
                (when (unquote predicate-expression)
                  (progn
                    (unquote-splicing consequent-expressions))
                  ((unquote function-binding)))])
        ((unquote function-binding))))))
(defmacro default (x d)
  (let* ([tempsymbol (gensym)])
    (quasiquote
      (let* ([(unquote tempsymbol) (unquote x)])
        (if (nil? (unquote tempsymbol))
          (unquote d)
          (unquote tempsymbol))))))
`

func benchWhenEnv(b *testing.B, prelude, fn string) (*lisp.LEnv, *lisp.LVal) {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	if rc := env.LoadString("prelude.lisp", prelude+fn); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	exprs, err := env.Runtime.Reader.Read("call.lisp", strings.NewReader(`(f 1)`))
	if err != nil {
		b.Fatal(err)
	}
	return env, exprs[0]
}

func runWhenBench(b *testing.B, prelude, fn string) {
	env, call := benchWhenEnv(b, prelude, fn)
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		if v := env.Eval(call); v.Type == lisp.LError {
			b.Fatal(v)
		}
	}
}

// BenchmarkWhen compares a truthy when, a falsey unless, and the equivalent
// if, each evaluated inside a function body.  The macro variants re-expand on
// every evaluation; the op variants are the lisp special operators.
func BenchmarkWhen(b *testing.B) {
	whenFn := `(defun f (x) (when x (+ x 1) (+ x 2)))`
	unlessFn := `(defun f (x) (unless (nil? x) (+ x 1) (+ x 2)))`
	b.Run("macro", func(b *testing.B) { runWhenBench(b, whenMacroSource, whenFn) })
	b.Run("op", func(b *testing.B) { runWhenBench(b, "", whenFn) })
	b.Run("unless-macro", func(b *testing.B) { runWhenBench(b, whenMacroSource, unlessFn) })
	b.Run("unless-op", func(b *testing.B) { runWhenBench(b, "", unlessFn) })
	defaultFn := `(defun f (x) (default (if (= x 1) () x) 7))`
	b.Run("default-macro", func(b *testing.B) { runWhenBench(b, whenMacroSource, defaultFn) })
	b.Run("default-op", func(b *testing.B) { runWhenBench(b, "", defaultFn) })
	// ten turns per call, so per-turn cost is ns/op / 10
	whileFn := `(defun f (x) (let ([i 0]) (while (< i 10) (set! i (+ i x))) i))`
	b.Run("while-macro", func(b *testing.B) { runWhenBench(b, whenMacroSource, whileFn) })
	b.Run("while-op", func(b *testing.B) { runWhenBench(b, "", whileFn) })
	b.Run("if", func(b *testing.B) {
		runWhenBench(b, "", `(defun f (x) (if x (progn (+ x 1) (+ x 2)) ()))`)
	})
}
