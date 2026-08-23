// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// This file is the direct contract test for issue #378's policy flip: a
// write through a SEALED value — a parsed program literal, or a view
// sharing its backing — raises the catchable modify-literal-error
// condition instead of the silent copy-on-write the three guarded sites
// (stable-sort, slice 'vector, append 'vector) used to perform.
//
// The flip is census-backed: two elpscheck censuses (issue #378) found
// every firing of the copy-on-write paths came from test machinery built
// to exercise them — zero from stdlib, examples, tooling, or the
// production phylum corpus — so nothing real depended on the silent copy,
// and a silent copy can mask code that believed it owned the value.
//
// Three sides of the contract are pinned:
//
//  1. the positive: each guarded site raises the named condition with the
//     exact message (it names the remedy, so it is API), and the condition
//     flows through the ordinary machinery — handler-bind by name,
//     ignore-errors, a stack trace on the raw error;
//  2. the negative: unsealed runtime values keep the documented in-place
//     semantics, and a (copy ...) of a sealed value mutates freely — copy
//     is the remedy the message names;
//  3. the carve-out: an EMPTY sealed input (the shared sealed empty list
//     that cdr/rest/keys return, an empty literal, a zero-width window) is
//     accepted, because it has no storage to write or alias and erroring
//     would make (stable-sort < (rest xs)) fail only for short xs.

// sealedWriteMessage is the pinned error text.  It names the remedy, and
// downstream suites are entitled to match on it.
const sealedWriteMessage = "cannot modify a program literal; take a (copy ...) first"

// sealedWriteForms drives each guarded site with a literal input, directly
// and through a backing-sharing view.
var sealedWriteForms = []string{
	`(stable-sort < '(3 1 2))`,
	`(stable-sort < (cdr '(9 3 7 1 8)))`,
	`(slice 'vector '(1 2 3) 0 2)`,
	`(slice 'vector (slice 'list '(1 2 3 4) 1 3) 0 1)`,
	`(append 'vector '(1 2 3) 4)`,
	`(append 'vector (slice 'list '(1 2 3) 0 1) 99)`,
	`(append 'vector '(1 2 3))`, // no values: would wrap the literal's backing
}

func TestSealedWriteRaisesCatchableCondition(t *testing.T) {
	for _, src := range sealedWriteForms {
		env := newCowTestEnv(t)

		// The raw error: type, condition, message and a call stack.
		v := env.LoadString("sealed-write.lisp", src)
		if v.Type != lisp.LError {
			t.Errorf("%s: expected an error, got %v", src, v)
			continue
		}
		var ev *lisp.ErrorVal
		if !errors.As(lisp.GoError(v), &ev) {
			t.Errorf("%s: error does not convert to *ErrorVal", src)
			continue
		}
		if cond := ev.Condition(); cond != lisp.CondModifyLiteral {
			t.Errorf("%s: condition = %q, want %q", src, cond, lisp.CondModifyLiteral)
		}
		if msg := ev.ErrorMessage(); msg != sealedWriteMessage {
			t.Errorf("%s: message = %q, want %q", src, msg, sealedWriteMessage)
		}
		if v.CallStack() == nil || v.CallStack().Top() == nil {
			t.Errorf("%s: error carries no call stack; the condition must flow through the normal machinery", src)
		}

		// handler-bind, naming the condition.
		caught := env.LoadString("sealed-write.lisp",
			`(handler-bind ([modify-literal-error (lambda (c &rest args) (list 'caught c (car args)))])
			   `+src+`)`)
		if caught.Type == lisp.LError {
			t.Errorf("%s: handler-bind on modify-literal-error did not catch: %v", src, caught)
		} else if got := caught.String(); got != `'('caught 'modify-literal-error "`+sealedWriteMessage+`")` {
			t.Errorf("%s: handler saw %s", src, got)
		}

		// ignore-errors.
		ignored := env.LoadString("sealed-write.lisp", `(ignore-errors `+src+`)`)
		if ignored.Type == lisp.LError {
			t.Errorf("%s: ignore-errors did not swallow the condition: %v", src, ignored)
		}
	}
}

// TestUnsealedWritesKeepInPlaceSemantics is the negative half: the guard
// keys off provenance, not off the operation, so runtime-constructed values
// and copies of literals keep the documented behaviour unchanged.
func TestUnsealedWritesKeepInPlaceSemantics(t *testing.T) {
	env := newCowTestEnv(t)
	for _, tc := range []struct{ src, want string }{
		// A runtime list is sorted in place: the binding itself reads sorted.
		{`(progn (set 'xs (list 3 1 2)) (stable-sort < xs) xs)`, `'(1 2 3)`},
		// A copy of a literal is the sanctioned remedy and mutates freely.
		{`(stable-sort < (copy '(3 1 2)))`, `'(1 2 3)`},
		{`(slice 'vector (copy '(1 2 3)) 0 2)`, `(vector 1 2)`},
		{`(append 'vector (copy '(1 2 3)) 4)`, `(vector 1 2 3 4)`},
		// The copy is deep: a view sliced out of it appends without error.
		{`(append 'vector (slice 'list (copy '(1 2 3)) 0 1) 99)`, `(vector 1 99)`},
		// Runtime vectors were never guarded.
		{`(let ([v (vector 3 1 2)]) (stable-sort < v) v)`, `(vector 1 2 3)`},
	} {
		v := env.LoadString("unsealed.lisp", tc.src)
		if v.Type == lisp.LError {
			t.Errorf("%s: %v", tc.src, v)
			continue
		}
		if got := v.String(); got != tc.want {
			t.Errorf("%s = %s, want %s", tc.src, got, tc.want)
		}
	}

	// The in-place identity claim, from Go: stable-sort returns the very
	// value it was handed when that value is a mutable runtime list.
	if v := env.LoadString("unsealed.lisp", `(set 'ys (list 2 1))`); v.Type == lisp.LError {
		t.Fatalf("setup: %v", v)
	}
	ys := env.Get(lisp.Symbol("ys"))
	sorted := env.LoadString("unsealed.lisp", `(stable-sort < ys)`)
	if sorted.Type == lisp.LError {
		t.Fatalf("sort: %v", sorted)
	}
	if sorted != ys {
		t.Errorf("stable-sort copied a runtime list: result %p, argument %p", sorted, ys)
	}
}

// TestSealedWriteEmptyCarveOut pins the deliberate exception documented on
// CondModifyLiteral: empty sealed inputs are accepted, so the shared sealed
// empty list that ordinary builtins return cannot turn correct runtime code
// into a data-dependent error, and the results are always fresh mutable
// storage rather than a window onto anything sealed.
func TestSealedWriteEmptyCarveOut(t *testing.T) {
	env := newCowTestEnv(t)
	for _, tc := range []struct{ src, want string }{
		// (rest '(1)) and (cdr '(1)) return the shared sealed empty list.
		{`(stable-sort < (rest '(1)))`, `()`},
		{`(append 'vector (cdr '(1)) 7 8)`, `(vector 7 8)`},
		{`(slice 'vector (rest '(1)) 0 0)`, `(vector)`},
		// An empty literal and an empty window over a non-empty one.
		{`(stable-sort < '())`, `'()`},
		{`(slice 'vector '(1 2 3) 1 1)`, `(vector)`},
		{`(append 'vector (slice 'list '(1 2 3) 0 0) 9)`, `(vector 9)`},
		// The vector handed back owns its (empty) backing: growing it in
		// place must not touch anything sealed, asserted structurally by
		// the elpscheck seal inspector when tagged.
		{`(let ([v (slice 'vector (rest '(1)) 0 0)]) (append! v 9) v)`, `(vector 9)`},
	} {
		v := env.LoadString("empty-carveout.lisp", tc.src)
		if v.Type == lisp.LError {
			t.Errorf("%s: the empty carve-out did not hold: %v", tc.src, v)
			continue
		}
		if got := v.String(); got != tc.want {
			t.Errorf("%s = %s, want %s", tc.src, got, tc.want)
		}
		if v.IsSealed() {
			t.Errorf("%s returned sealed storage; the carve-out must hand back fresh results", tc.src)
		}
	}
}
