// Copyright © 2026 The ELPS authors

package astutil

// A call in structure macros share is reported once per place it occurs in
// code, in that place's lexical context, within one budget per query.

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestFindCallsSharedSharedAcrossScopes(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	fn := parseOne(t, `(lambda () x)`)
	fn.Cells[2] = c
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), fn, c})
	sites := FindCalls(root, "emit")
	if len(sites) != 2 {
		t.Fatalf("got %d sites, want 2", len(sites))
	}
}
func TestFindCallsSharedSharedAcrossCodeAndData(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	q := lisp.SExpr([]*lisp.LVal{lisp.Symbol("quote"), c})
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), q, c})
	sites := FindCalls(root, "emit")
	if len(sites) != 1 {
		t.Fatalf("got %d sites, want 1", len(sites))
	}
}
func TestFindCallsSharedSharedAcrossBoundAndFree(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	flet := parseOne(t, `(flet ((emit (x) x)) x)`)
	flet.Cells[2] = c
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), flet, c})
	sites := FindCalls(root, "emit")
	if len(sites) != 1 {
		t.Fatalf("got %d sites, want 1", len(sites))
	}
}
func TestFindCallsSharedRepeatedFunctionOccurrences(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	fn := parseOne(t, `(lambda () x)`)
	fn.Cells[2] = c
	root := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), fn, fn})
	sites := FindCalls(root, "emit")
	if len(sites) != 2 {
		t.Fatalf("got %d sites, want 2", len(sites))
	}
}

func TestFindCallsSharedBoundingAcrossScopes(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	x := c
	for range 8 {
		a := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.Nil(), x})
		b := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.Nil(), x})
		x = lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), a, b})
	}
	sites := FindCalls(x, "emit")
	if len(sites) > maxCallSites {
		t.Fatalf("one shared call produced %d sites; maxCallSites=%d", len(sites), maxCallSites)
	}
}
func TestFindCallsSharedTruncationHidesOutsideHandler(t *testing.T) {
	c := parseOne(t, `(rethrow)`)
	hb := parseOne(t, `(handler-bind ((condition h)) x)`)
	hb.Cells[2] = c
	cells := []*lisp.LVal{lisp.Symbol("progn")}
	for range 64 {
		cells = append(cells, hb)
	}
	cells = append(cells, c)
	sites := FindCalls(lisp.SExpr(cells), "rethrow")
	outside := 0
	for _, s := range sites {
		handled := false
		for _, e := range s.Enclosing {
			if e.Op == "handler-bind" {
				handled = true
			}
		}
		if !handled {
			outside++
		}
	}
	if outside != 1 {
		t.Fatalf("got %d outside-handler sites among %d sites, want 1", outside, len(sites))
	}
}

func TestFindCallSitesReportsOverflow(t *testing.T) {
	c := parseOne(t, `(emit 1)`)
	x := c
	for range 8 {
		a := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.Nil(), x})
		b := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.Nil(), x})
		x = lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), a, b})
	}
	_, complete := FindCallSites(x, "emit")
	if complete {
		t.Fatal("256 distinct places fit in the site budget")
	}
	_, complete = FindCallSites(parseOne(t, `(progn (emit 1) (lambda () (emit 2)))`), "emit")
	if !complete {
		t.Fatal("a small form was reported incomplete")
	}
}
