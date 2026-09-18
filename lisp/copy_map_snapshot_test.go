// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// mapDeletingClone is a native payload whose clone hook removes a SIBLING
// entry from the map the walk is part-way through.  CloneNative is embedder
// code that Copy calls while it holds the map's frame, so it is the same
// hazard walkerMutatingEntries poses to a list, one container over.
type mapDeletingClone struct {
	source *lisp.LVal
	victim string
}

func (p *mapDeletingClone) CloneNative() interface{} {
	p.source.Map().Del(lisp.String(p.victim))
	return &mapDeletingClone{source: p.source, victim: p.victim}
}

// Copy's contract is the entries the map held when Copy entered it: the
// ordered arm walks keys in sorted order and used to read each value back
// out of the SOURCE map at the moment it reached the key, so a hook that
// deleted a later key left a Go nil in the copy -- a value no ELPS program
// can hold, which the renderer then dereferenced.
func TestCopySnapshotsMapValuesAgainstAHostHook(t *testing.T) {
	m := lisp.SortedMap()
	payload := &mapDeletingClone{victim: "b"}
	if lerr := m.MapSet(lisp.String("a"), lisp.Native(payload)); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	if lerr := m.MapSet(lisp.String("b"), lisp.String("sibling")); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	payload.source = m

	cp := m.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	if _, ok := m.Map().Get(lisp.String("b")); ok {
		t.Fatal("anti-vacuity: the hook did not delete the sibling key during the walk")
	}
	got, ok := cp.Map().Get(lisp.String("b"))
	if !ok {
		t.Fatal(`the copy lost key "b"; the map held it when Copy entered it`)
	}
	if got == nil {
		t.Fatal(`the copy holds a Go nil under "b": a deleted source entry must not become one`)
	}
	if got.Str != "sibling" {
		t.Errorf(`the copy holds %v under "b", want "sibling"`, got)
	}
	// Rendering is where a nil child was observed as a crash rather than
	// as a wrong answer, so the copy must render as well as compare.
	if s := cp.String(); !strings.Contains(s, "sibling") {
		t.Errorf("the copy renders as %q, want the entry it held", s)
	}
}

// A renderer must survive a malformed value rather than take the process
// down: every other walk over a nil child reports it, and errorMessage
// already logs and renders <nil> for one.
func TestRenderNilChildDoesNotPanic(t *testing.T) {
	v := lisp.QExpr([]*lisp.LVal{lisp.Int(1), nil, lisp.String("x")})
	s := v.String()
	if !strings.Contains(s, "<nil>") {
		t.Errorf("rendering a nil child produced %q, want a <nil> marker", s)
	}
	if !strings.Contains(s, `"x"`) {
		t.Errorf("rendering stopped at the nil child: %q", s)
	}
}
