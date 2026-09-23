package lisp_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// callHeavyProgram is dominated by user function calls: a doubly recursive
// fib, a self tail-recursive loop calling a helper, and a tail loop invoking
// two small anonymous lambdas per iteration.
const callHeavyProgram = `
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun add1 (x) (+ x 1))
(defun countdown (n acc) (if (= n 0) acc (countdown (- n 1) (add1 acc))))
(defun lambda-loop (i acc)
  (if (= i 500)
    acc
    (lambda-loop (+ i 1) ((lambda (a x) (+ a x)) acc ((lambda (x) (* x 2)) i)))))
(defun call-heavy ()
  (+ (fib 20) (countdown 2000 0) (lambda-loop 0 0)))
`

// callHeavyResult is fib(20) + 2000 + sum(2i, i<500).
const callHeavyResult = 6765 + 2000 + 249500

// newCallHeavyEnv loads callHeavyProgram once and returns the env plus the
// parsed (call-heavy) form, so the timed loop measures evaluation only.
// elpstest.RunBenchmark, by contrast, re-parses its source every iteration.
func newCallHeavyEnv(b *testing.B, cfg ...lisp.Config) (*lisp.LEnv, *lisp.LVal) {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env, cfg...); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	if rc := env.LoadString("call-heavy.lisp", callHeavyProgram); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	exprs, err := env.Runtime.Reader.Read("call", strings.NewReader("(call-heavy)"))
	if err != nil {
		b.Fatal(err)
	}
	if len(exprs) != 1 {
		b.Fatalf("expected one form, got %d", len(exprs))
	}
	return env, exprs[0]
}

func runCallHeavy(b *testing.B, env *lisp.LEnv, call *lisp.LVal) {
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		v := env.Eval(call)
		if v.Type != lisp.LInt || v.Int != callHeavyResult {
			b.Fatalf("unexpected result: %v", v)
		}
	}
}

// BenchmarkEvalParsedCallHeavy parses once and evaluates many times. The
// "context" variant installs a cancellable context, as substrate does for
// every transaction, so the per-step context check is exercised.
func BenchmarkEvalParsedCallHeavy(b *testing.B) {
	b.Run("no-context", func(b *testing.B) {
		env, call := newCallHeavyEnv(b)
		runCallHeavy(b, env, call)
	})
	b.Run("context", func(b *testing.B) {
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		env, call := newCallHeavyEnv(b, lisp.WithContext(ctx))
		runCallHeavy(b, env, call)
	})
}
