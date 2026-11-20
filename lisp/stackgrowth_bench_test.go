// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// stackGrowthProgram recurses without tail calls through the forms a phylum
// nests most: if, let, and a builtin (map) calling back into Lisp.
const stackGrowthProgram = `
(defun rec-if (n) (if (= n 0) 0 (+ 1 (rec-if (- n 1)))))
(defun rec-let (n) (if (= n 0) 0 (let ([x (rec-let (- n 1))]) (+ x 1))))
(defun rec-map (n) (if (= n 0) 0 (+ 1 (car (map 'list (lambda (x) (rec-map (- n 1))) '(1))))))
`

// BenchmarkEvalStackGrowthParallel evaluates each recursion at many depths, each on
// a fresh goroutine, as a server runs each request.  A fresh goroutine starts
// with a small stack, and the Go runtime grows it by copying, walking every
// frame on it each time (runtime.morestack, copystack), so the time measures
// the evaluator's Go frames per Lisp call level.  The many depths spread the
// cost over the stack sizes where growth happens.  Other short goroutines
// keep the runtime's adaptive starting stack size small, as a server's many
// small goroutines do.
//
// It is named *Parallel, though it does not call RunParallel, so the
// gated benchmark job (GOMAXPROCS=1, -benchtime=100ms) skips it: its time
// rests on the scheduler and the runtime's adaptive stack size, and spread
// 25-44 ms/op over ten such runs.  The informational parallel job runs it.
func BenchmarkEvalStackGrowthParallel(b *testing.B) {
	env, _ := newCallHeavyEnv(b)
	if rc := env.LoadString("stack-growth.lisp", stackGrowthProgram); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	var calls []*lisp.LVal
	for depth := 3; depth <= 150; depth += 7 {
		for _, f := range []string{"rec-if", "rec-let", "rec-map"} {
			exprs, err := env.Runtime.Reader.Read("call", strings.NewReader(fmt.Sprintf("(%s %d)", f, depth)))
			if err != nil {
				b.Fatal(err)
			}
			calls = append(calls, exprs[0])
		}
	}
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		for _, call := range calls {
			done := make(chan *lisp.LVal)
			go func() { done <- env.Eval(call) }()
			var wg sync.WaitGroup
			for range 20 {
				wg.Add(1)
				go func() { wg.Done() }()
			}
			wg.Wait()
			if v := <-done; v.Type == lisp.LError {
				b.Fatal(v)
			}
		}
	}
}
