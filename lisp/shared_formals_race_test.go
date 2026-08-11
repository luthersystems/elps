// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// Builtin formals are shared, sealed, across every environment in the
// process (registrationFormals in env.go; sealDefaultFormals in
// builtins.go).  That sharing is only sound if NOTHING on the calling
// convention's hot path writes a formals cell: argument binding
// (bind/bindFormalNext), keyword and optional defaulting, &rest
// collection, higher-order builtins that read formals (flip, compose),
// macro expansion and doc rendering must all treat the shared cells as
// read-only.  This test hammers exactly those shapes from many
// goroutines, each with its own Runtime, so that:
//
//   - under -race, any write to a shared formals cell (even a same-value
//     or metadata write) is a reported data race, because every other
//     goroutine is concurrently reading the same cells through bind;
//   - under -tags elpscheck, any write that changes bytes fails the
//     fingerprint verification below (the tables were fingerprinted when
//     sealDefaultFormals sealed them at package init), and any formal
//     symbol leaking through Put/eval into two runtimes panics the
//     ownership checker;
//   - under -race -tags elpscheck, the seal watchdog additionally keeps
//     unsynchronized reads of sampled sealed roots (including the builtin
//     formals tables) live for the whole run.
//
// If the sharing design regresses — a future edit makes the evaluator
// substitute defaults into formals cells in place, stamp metadata onto
// them, or hand them to a mutating builtin — this is the test that goes
// red before an embedder's concurrent runtimes corrupt each other.
func TestSharedFormalsConcurrentCalls(t *testing.T) {
	// Every shape below routes through bind() against a builtin's or
	// macro's SHARED sealed formals: keyword args present and absent
	// (load-string's &key name), &rest through direct call and apply,
	// formals-reading higher-order builtins (flip reads, compose copies),
	// funref aliasing (the function op shares the fun's Cells with a new
	// header),
	// user functions and macros exercising &optional/&rest against
	// builtins, and special ops (let/cond/if) with their own shared
	// formals.
	const program = `
	(load-string "(+ 1 2)" :name "adv-name")
	(load-string "(+ 1 2)")
	(apply list 1 2 '(3 4))
	(funcall (flip cons) '(b) 'a)
	(funcall (compose list reverse) 'list '(1 2 3))
	(set 'adv-f (function list))
	(funcall adv-f 1 2 3)
	(defun adv-fn (a &optional o &rest r) (list a o (apply list r)))
	(adv-fn 1)
	(adv-fn 1 2 3 4 5)
	(defmacro adv-mac (x &rest more) (quasiquote (list (unquote x) (unquote-splicing more))))
	(adv-mac 1 2 3)
	(let ([x 1]) (cond ((= x 1) 'one) (:else 'other)))
	`
	const (
		goroutines = 8
		iterations = 25
	)
	errs := make(chan error, goroutines)
	var wg sync.WaitGroup
	for g := range goroutines {
		wg.Add(1)
		go func() {
			defer wg.Done()
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
				errs <- fmt.Errorf("goroutine %d: InitializeUserEnv: %v", g, rc)
				return
			}
			if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
				errs <- fmt.Errorf("goroutine %d: LoadLibrary: %v", g, rc)
				return
			}
			if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
				errs <- fmt.Errorf("goroutine %d: InPackage: %v", g, rc)
				return
			}
			for i := range iterations {
				name := fmt.Sprintf("adv-%d-%d", g, i)
				if rc := env.LoadString(name, program); rc.Type == lisp.LError {
					errs <- fmt.Errorf("goroutine %d iteration %d: %v", g, i, rc)
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
	// In elpscheck builds this re-fingerprints every sealed root recorded
	// in the process — including the builtin formals tables sealed at
	// package init — so a byte-changing write during the hammering above
	// fails here even if no goroutine happened to observe it.  In default
	// builds it is a documented no-op (the -race teeth above still apply).
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("sealed AST verification after concurrent builtin calls: %v", err)
	}
}
