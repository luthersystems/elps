// Copyright © 2026 The ELPS authors
//
// Borrowed-backing alias class probes.  Each probe drives a lisp program
// that (a) mints a view over another value's live Go backing storage and
// (b) writes through the view, then asserts the SOURCE value is unchanged.
// On the unfixed tree the source is corrupted.
//
// This file is a measurement harness shared verbatim by every design arm;
// it is never a deliverable.

package aliasprobe_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func probeEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := lisplib.LoadLibrary(env); lerr.Type == lisp.LError {
		t.Fatalf("LoadLibrary: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		t.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// evalProbe runs src and returns the printed value of the final expression.
// A lisp error is returned as "ERROR: <msg>" so a probe can distinguish
// "guarded (refused)" from "guarded (copied)" from "corrupted".
func evalProbe(t testing.TB, src string) string {
	t.Helper()
	env := probeEnv(t)
	var last *lisp.LVal
	exprs, err := parser.NewReader().Read("probe.lisp", strings.NewReader(src))
	if err != nil {
		return "PARSE-ERROR: " + err.Error()
	}
	for _, e := range exprs {
		last = env.Eval(e)
		if last.Type == lisp.LError {
			return "ERROR: " + last.String()
		}
	}
	return last.String()
}

// probe is one alias-class reproduction.
type probe struct {
	name string
	// mode is the failure mode exercised: "spare-cap" writes past the
	// view's length into retained capacity; "in-length" writes inside the
	// view's own length.
	mode string
	src  string
	// want is the value the SOURCE must still hold.  Any other result
	// (including a lisp error) is reported verbatim so an arm that closes
	// the hole by refusing is distinguishable from one that copies.
	want string
}

var aliasProbes = []probe{
	// ---- #392: elpspath range Get launders a sealed literal ------------
	{
		name: "392/append-vector-through-range-view",
		mode: "spare-cap",
		src: `
(defun limits () '(10 20 30 40))
(set 'win (elpspath:? (limits) '(range 0 2)))
(append 'vector win 999)
(limits)`,
		want: `'(10 20 30 40)`,
	},
	{
		// The within-length mode: stable-sort permutes the view's own
		// cells, which ARE the literal's cells.  A three-index slice at
		// the producer changes nothing here.
		name: "392/stable-sort-through-range-view",
		mode: "in-length",
		src: `
(defun limits () '(30 10 20 40))
(set 'win (elpspath:? (limits) '(range 0 3)))
(stable-sort < win)
(limits)`,
		want: `'(30 10 20 40)`,
	},
	{
		// slice 'vector over the laundered view: the kernel's own CoW
		// guard is disarmed because the view lost the flag.
		name: "392/slice-vector-through-range-view",
		mode: "spare-cap",
		src: `
(defun limits () '(10 20 30 40))
(set 'win (elpspath:? (limits) '(range 0 2)))
(append! (slice 'vector win 0 2) 999)
(limits)`,
		want: `'(10 20 30 40)`,
	},

	// ---- #373: bytes ---------------------------------------------------
	{
		name: "373/append-bytes-through-slice",
		mode: "spare-cap",
		src: `
(set 'src (to-bytes "ABCDEF"))
(set 'win (slice 'bytes src 0 2))
(append-bytes win (to-bytes "zz"))
src`,
		want: `#<bytes 65 66 67 68 69 70>`,
	},
	{
		name: "373/append-bytes!-through-slice",
		mode: "spare-cap",
		src: `
(set 'src (to-bytes "ABCDEF"))
(set 'win (slice 'bytes src 0 2))
(append-bytes! win (to-bytes "zz"))
src`,
		want: `#<bytes 65 66 67 68 69 70>`,
	},
	{
		name: "373/append!-byte-through-slice",
		mode: "spare-cap",
		src: `
(set 'src (to-bytes "ABCDEF"))
(set 'win (slice 'bytes src 0 2))
(append! win 122)
src`,
		want: `#<bytes 65 66 67 68 69 70>`,
	},
	{
		name: "373/append-bytes-quote-through-slice",
		mode: "spare-cap",
		src: `
(set 'src (to-bytes "ABCDEF"))
(set 'win (slice 'bytes src 0 2))
(append 'bytes win 122)
src`,
		want: `#<bytes 65 66 67 68 69 70>`,
	},

	// ---- vectors: same class, no seal involved -------------------------
	{
		name: "vector/append!-through-elpspath-range-view",
		mode: "spare-cap",
		src: `
(set 'src (vector 10 20 30 40))
(set 'win (elpspath:? src '(range 0 2)))
(append! win 999)
src`,
		want: `(vector 10 20 30 40)`,
	},
	{
		// DOCUMENTED, not corruption.  src is an ordinary mutable vector
		// carrying no constraint, and a range query over a vector returns
		// a live vector view; sorting the view in place therefore permutes
		// the source's first three elements, which is what a view means.
		// The row is here so that a change to that contract is noticed
		// rather than discovered — it is the control that keeps the rest
		// of the table honest about what "corruption" means.
		name: "vector/stable-sort-through-elpspath-range-view (documented view semantics)",
		mode: "in-length",
		src: `
(set 'src (vector 30 10 20 40))
(set 'win (elpspath:? src '(range 0 3)))
(stable-sort < win)
src`,
		want: `(vector 10 20 30 40)`,
	},

	// ---- controls: sites that ALREADY propagate ------------------------
	{
		name: "control/append-vector-through-slice-list (guarded today)",
		mode: "spare-cap",
		src: `
(defun limits () '(10 20 30 40))
(set 'win (slice 'list (limits) 0 2))
(append 'vector win 999)
(limits)`,
		want: `'(10 20 30 40)`,
	},
	{
		name: "control/stable-sort-on-literal (guarded today)",
		mode: "in-length",
		src: `
(defun limits () '(30 10 20 40))
(stable-sort < (limits))
(limits)`,
		want: `'(30 10 20 40)`,
	},
	{
		name: "control/cdr-then-append-vector (guarded today)",
		mode: "spare-cap",
		src: `
(defun limits () '(10 20 30 40))
(append 'vector (cdr (limits)) 999)
(limits)`,
		want: `'(10 20 30 40)`,
	},
}

// TestAliasClassCensus reports, for every probe, whether the source value
// survived.  It is the arm-comparison table: run with -v and read the
// PASS/FAIL column.
func TestAliasClassCensus(t *testing.T) {
	for _, p := range aliasProbes {
		p := p
		t.Run(p.name, func(t *testing.T) {
			got := evalProbe(t, p.src)
			if got != p.want {
				t.Errorf("[%s] source corrupted or behaviour changed\n  want: %s\n  got:  %s", p.mode, p.want, got)
			}
		})
	}
}
