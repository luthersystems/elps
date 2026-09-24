// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// prewarmDeterminismSource is a small "phylum": two endpoints that touch
// disjoint state, shared helpers, maps with many keys, and a failing path.
const prewarmDeterminismSource = `
(in-package 'lib)
(export 'orders 'users 'place 'lookup 'fail 'trace-me)
(set 'orders (sorted-map "b" 2 "a" 1 "d" 4 "c" 3))
(set 'users (sorted-map "zed" (list 1 2) "amy" (sorted-map "role" "admin") "bob" (to-bytes "x")))
(defun place (k v) (assoc! orders k v) (keys orders))
(defun lookup (k) (get users k))
(defun fail (x) (error 'bad-input (format-string "bad {}" x)))
(defun trace-me (n) (if (<= n 0) (fail n) (trace-me (- n 1))))
(in-package 'user)
(use-package 'lib)
(set 'counter 0)
(defun bump () (set! counter (+ counter 1)) counter)
`

// Histories run on earlier VMs of the template before the compared VM is
// created with VMWithPrewarm; each leaves a different learned hot set.
var prewarmHistories = map[string][]string{
	"none":   nil,
	"orders": {`(place "e" 5)`},
	"users":  {`(lookup "amy")`, `(bump)`},
	"errors": {`(ignore-errors (trace-me 3))`},
	"all":    {`(place "e" 5)`, `(lookup "zed")`, `(ignore-errors (trace-me 2))`, `(bump)`},
}

// Programs whose results, errors and printed output must not depend on the
// VM's history.
var prewarmPrograms = []string{
	`(list (gensym) (gensym) (to-string (gensym)))`,
	`(map 'list (lambda (k) (list k (get lib:orders k))) (keys lib:orders))`,
	`(let ((acc ())) (map () (lambda (k) (set! acc (cons k acc))) (keys lib:users)) acc)`,
	`(keys lib:users)`,
	`(place "aa" 0)`,
	`(list (lookup "amy") (lookup "zed") (lookup "bob") (lookup "nobody"))`,
	`(trace-me 3)`,
	`(handler-bind ((condition (lambda (c &rest args) (list c args)))) (fail 7))`,
	`(debug-print (list lib:orders lib:users)) (debug-print (bump) (bump))`,
	`(json:dump-string (sorted-map "o" lib:orders "u" (lookup "amy")))`,
	`(let ((m (sorted-map))) (assoc! m "self" m) (length (keys m)))`,
}

func runPrewarmProgram(t *testing.T, vm *lisp.LEnv, program string) string {
	t.Helper()
	var out bytes.Buffer
	vm.Runtime.Stderr = &out
	result := vm.LoadString("tx.lisp", program)
	rendered := result.String()
	if result.Type == lisp.LError {
		var trace bytes.Buffer
		if _, err := (*lisp.ErrorVal)(result).WriteTrace(&trace); err != nil {
			t.Fatal(err)
		}
		rendered += "\n" + trace.String()
	}
	return fmt.Sprintf("result=%s\nstderr=%s", rendered, out.String())
}

// TestTemplatePrewarmDeterministicAcrossHistories: peers holding the same
// template but different execution histories must agree. A prewarmed VM's
// learned hot set depends on history; nothing observable may.
func TestTemplatePrewarmDeterministicAcrossHistories(t *testing.T) {
	newTemplate := func() *lisp.Template {
		env := loadTemplateFixture(t, prewarmDeterminismSource)
		tmpl, err := lisp.NewTemplate(env, templateFixturePolicy(), lisp.TemplateWithFrozenPackages("lib"))
		if err != nil {
			t.Fatal(err)
		}
		return tmpl
	}
	for _, program := range prewarmPrograms {
		t.Run(program, func(t *testing.T) {
			cold := runPrewarmProgram(t, forkTemplateFixture(t, newTemplate()), program)
			eagerTmpl, err := lisp.NewTemplate(loadTemplateFixture(t, prewarmDeterminismSource),
				templateFixturePolicy(), lisp.TemplateWithFrozenPackages("lib"), lisp.TemplateWithEagerInstantiation())
			if err != nil {
				t.Fatal(err)
			}
			if eager := runPrewarmProgram(t, forkTemplateFixture(t, eagerTmpl), program); eager != cold {
				t.Fatalf("eager differs from lazy cold:\neager: %s\n cold: %s", eager, cold)
			}
			for name, history := range prewarmHistories {
				tmpl := newTemplate() // a fresh "peer" with its own history
				for _, tx := range history {
					forkTemplateFixture(t, tmpl).LoadString("history.lisp", tx)
				}
				vm, err := tmpl.NewVM(lisp.VMWithPrewarm())
				if err != nil {
					t.Fatal(err)
				}
				if got := runPrewarmProgram(t, vm, program); got != cold {
					t.Errorf("history %q differs from a cold VM:\n got: %s\nwant: %s", name, got, cold)
				}
			}
		})
	}
}
