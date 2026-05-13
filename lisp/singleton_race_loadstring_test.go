// Copyright © 2025 The ELPS authors

// Realistic singleton-mutation regression test via LoadString.
//
// Demonstrates that the fix holds along the natural production code
// path (parser -> reader -> macroexpand -> stampMacroExpansion) rather
// than only when stampMacroExpansion is invoked directly. Each
// goroutine has its own Runtime/Env, but pre-fix, stampMacroExpansion
// mutated the *process-wide* singletonTrue / singletonFalse LVals, so
// the race fires across runtimes. With the fix, the singletons are
// never written and `-race` stays clean. See issue #274.

package lisp_test

import (
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// A small macro whose expansion body contains literal `true` and
// `false` symbols. stampMacroExpansion recurses through every cell, so
// any literal Bool() in the body would have tripped the pre-fix
// mutation. Multiple literals make the race likelier under stress.
const racyProgram = `
(defmacro racy-mac (x)
  (quasiquote
    (progn
      (if (unquote x) true false)
      (if (unquote x) false true)
      true
      false)))

(defun call-me (z)
  "@trace{ call-me }"
  (racy-mac z))
`

func TestSingletonRaceFromLoadString(t *testing.T) {
	const goroutines = 8
	const iters = 200

	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := range goroutines {
		go func(g int) {
			defer wg.Done()
			for range iters {
				env := lisp.NewEnv(nil)
				if rc := lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader())); rc.Type == lisp.LError {
					t.Errorf("goroutine %d: InitializeUserEnv failed: %v", g, rc)
					return
				}
				env.InPackage(lisp.String(lisp.DefaultUserPackage))
				// Loading parses + macroexpands `racy-mac`, whose body
				// contains literal `true`/`false`. Stamping recurses
				// through them; with the fix, singletons are skipped.
				if rc := env.LoadString("racy.lisp", racyProgram); rc.Type == lisp.LError {
					t.Errorf("goroutine %d: LoadString failed: %v", g, rc)
					return
				}
				if rc := env.LoadString("invoke.lisp", `(call-me true)`); rc.Type == lisp.LError {
					t.Errorf("goroutine %d: call-me failed: %v", g, rc)
					return
				}
			}
		}(g)
	}
	wg.Wait()
}
