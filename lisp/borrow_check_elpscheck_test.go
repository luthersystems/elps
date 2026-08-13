// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// The borrowed-backing detector's own contract, in two halves: it must stay
// SILENT for every legitimate borrow in the tree (no whitelist), and it must
// FIRE for a header minted over sealed backing that never inherits the
// constraint (no blind spot).
//
// The first half is the load-bearing one.  A detector that reports the
// kernel's own cdr/rest/slice is unusable without per-call-site
// suppression, and per-call-site suppression is the hand-maintained
// discipline this mechanism exists to replace.  These rows are what keeps
// that honest: every one of them borrows from a SEALED program literal, and
// nothing in lisp/, lisp/lisplib/ or parser/ carries a suppression of any
// kind.

func borrowProofEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if l := lisp.InitializeUserEnv(env); l.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", l)
	}
	if l := lisplib.LoadLibrary(env); l.Type == lisp.LError {
		t.Fatalf("LoadLibrary: %v", l)
	}
	if l := env.InPackage(lisp.String(lisp.DefaultUserPackage)); l.Type == lisp.LError {
		t.Fatalf("InPackage: %v", l)
	}
	return env
}

func evalBorrowProof(t testing.TB, src string) {
	t.Helper()
	env := borrowProofEnv(t)
	exprs, err := parser.NewReader().Read("borrowproof.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	for _, e := range exprs {
		if r := env.Eval(e); r.Type == lisp.LError {
			t.Fatalf("eval: %v", r)
		}
	}
}

// TestBorrowCheckAcceptsLegitimateBorrows drives every sanctioned borrow
// over a sealed literal and requires the detector to stay silent.
func TestBorrowCheckAcceptsLegitimateBorrows(t *testing.T) {
	defer lisp.DropBorrowNotesForTest()
	for _, tc := range []struct{ name, src string }{
		{"cdr", `(defun lit () '(10 20 30 40))  (cdr (lit))`},
		{"rest", `(defun lit () '(10 20 30 40))  (rest (lit))`},
		{"slice-list", `(defun lit () '(10 20 30 40))  (slice 'list (lit) 0 2)`},
		{"slice-vector", `(defun lit () '(10 20 30 40))  (slice 'vector (lit) 0 2)`},
		{"append-vector", `(defun lit () '(10 20 30 40))  (append 'vector (lit) 999)`},
		{"append-vector-of-cdr", `(defun lit () '(10 20 30 40))  (append 'vector (cdr (lit)) 999)`},
		{"append-list-of-slice", `(defun lit () '(10 20 30 40))  (append 'list (slice 'list (lit) 0 2) 999)`},
		{"elpspath-range-query", `(defun lit () '(10 20 30 40))  (elpspath:? (lit) '(range 0 2))`},
		{"stable-sort-of-slice", `(defun lit () '(30 10 20 40))  (stable-sort < (slice 'list (lit) 0 3))`},
		{"nested-cdr-slice-append", `(defun lit () '(1 2 3 4 5))  (append 'vector (slice 'list (cdr (lit)) 0 2) 9)`},
		{"cond-body-over-literal", `(defun f () (cond ((= 1 1) '(1 2 3) '(4 5 6))))  (f)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			lisp.DropBorrowNotesForTest()
			evalBorrowProof(t, tc.src)
			if err := lisp.VerifySealedASTs(); err != nil {
				t.Fatalf("the detector fired on a LEGITIMATE borrow.  No suppression exists "+
					"anywhere in the tree and none should be needed — this borrow discharges "+
					"structurally, by carrying the source's constraint:\n%v", err)
			}
		})
	}
}

// TestBorrowCheckReportsALaunderedHeader is the other half: a header minted
// over a sealed literal's live cells that never inherits the constraint —
// exactly what rangePath.Get did before #392 — must be reported, with no
// write performed anywhere.  That is the property the corruption-time
// verifier cannot have: it needs the write.
func TestBorrowCheckReportsALaunderedHeader(t *testing.T) {
	defer lisp.DropBorrowNotesForTest()
	lisp.DropBorrowNotesForTest()

	env := borrowProofEnv(t)
	exprs, err := parser.NewReader().Read("borrowproof.lisp",
		strings.NewReader(`'(10 20 30 40)`))
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	lit := env.Eval(exprs[0])
	if lit.Type == lisp.LError {
		t.Fatalf("eval: %v", lit)
	}
	if !lit.IsSealed() {
		t.Fatalf("precondition: the parsed literal is not sealed")
	}

	// The laundering step, spelled the way an embedder spells it, and the
	// way libelpspath spelled it: a raw constructor over someone else's
	// live backing, with no provenance.  Nothing is written through it.
	window := lisp.SExpr(lit.Cells[0:2])
	if window.IsSealed() {
		t.Fatalf("precondition: the raw constructor must not invent a constraint")
	}

	err = lisp.VerifySealedASTs()
	if err == nil {
		t.Fatal("the detector did not report a header minted over sealed backing " +
			"that never inherited the constraint")
	}
	if !strings.Contains(err.Error(), "borrowcheck:") {
		t.Errorf("report does not identify itself as a provenance fault:\n%v", err)
	}
	if !strings.Contains(err.Error(), "borrow_check_elpscheck_test.go") {
		t.Errorf("report does not name the mint site:\n%v", err)
	}
}

// TestBorrowCheckDischargesThroughInheritSeal pins the discharge route that
// makes this detector viable for code OUTSIDE package lisp, where `sealed`
// is unreachable: (*LVal).InheritSeal, already exported and already in the
// tree.  It is the whole reason detector-only needs no new public API.
func TestBorrowCheckDischargesThroughInheritSeal(t *testing.T) {
	defer lisp.DropBorrowNotesForTest()
	lisp.DropBorrowNotesForTest()

	env := borrowProofEnv(t)
	exprs, err := parser.NewReader().Read("borrowproof.lisp",
		strings.NewReader(`'(10 20 30 40)`))
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	lit := env.Eval(exprs[0])
	if lit.Type == lisp.LError {
		t.Fatalf("eval: %v", lit)
	}

	// Construct-then-inherit, the only shape available outside package lisp.
	// The header is unconstrained for the duration of one statement, which
	// is exactly why the check is deferred rather than taken at mint time.
	window := lisp.SExpr(lit.Cells[0:2])
	window.InheritSeal(lit)

	if !window.IsSealed() {
		t.Fatalf("InheritSeal did not carry the constraint")
	}
	if err := lisp.VerifySealedASTs(); err != nil {
		t.Fatalf("a borrow discharged through the exported InheritSeal was still reported:\n%v", err)
	}
}
