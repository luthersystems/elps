// Copyright © 2026 The ELPS authors

package astraw_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestExprsZeroCopy proves the injected accessor hands back the sealed
// expressions themselves — the same nodes on every call, with no copying —
// while Detach (the public escape hatch) hands back different nodes.
func TestExprsZeroCopy(t *testing.T) {
	p, err := lisp.ReadProgram(parser.NewReader(), "raw.lisp",
		strings.NewReader(`(defun f (x) (+ x 1)) (f 41)`))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	if astraw.Exprs == nil {
		t.Fatal("astraw.Exprs was not injected by package lisp")
	}

	raw1 := astraw.Exprs(p)
	raw2 := astraw.Exprs(p)
	if len(raw1) != p.Len() || len(raw2) != p.Len() {
		t.Fatalf("Exprs lengths %d/%d, want %d", len(raw1), len(raw2), p.Len())
	}
	for i := range raw1 {
		if raw1[i] != raw2[i] {
			t.Errorf("expr %d: Exprs returned different nodes across calls — accessor is copying", i)
		}
	}

	det, err := p.Detach()
	if err != nil {
		t.Fatalf("Detach: %v", err)
	}
	for i := range det {
		if det[i] == raw1[i] {
			t.Errorf("expr %d: Detach returned a sealed node instead of a copy", i)
		}
	}
}

// TestExprsEmpty pins the accessor on the zero Program.
func TestExprsEmpty(t *testing.T) {
	if got := astraw.Exprs(lisp.Program{}); len(got) != 0 {
		t.Errorf("Exprs(zero) = %d exprs, want 0", len(got))
	}
}
