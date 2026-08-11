// Copyright © 2026 The ELPS authors

package analysis_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestAnalyzeProgram is the demonstration consumer for the internal/astraw
// zero-copy hook: analysis reads a sealed Program in place and produces the
// same result it produces for raw parser output.
func TestAnalyzeProgram(t *testing.T) {
	const src = `
(defun greet (name) (string:format "hello {}" name))
(defun shout (name) (string:upper (greet name)))
(shout "world")
`
	reader, ok := parser.NewReader().(lisp.LocationReader)
	if !ok {
		t.Fatal("parser.NewReader does not implement LocationReader")
	}

	exprs, err := reader.ReadLocation("greet.lisp", "greet.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := analysis.Analyze(exprs, nil)

	p, err := lisp.ReadLocationProgram(reader, "greet.lisp", "greet.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse program: %v", err)
	}
	got := analysis.AnalyzeProgram(p, nil)

	if len(got.Symbols) == 0 {
		t.Fatal("AnalyzeProgram found no symbols")
	}
	if len(got.Symbols) != len(want.Symbols) {
		t.Errorf("AnalyzeProgram found %d symbols, Analyze found %d", len(got.Symbols), len(want.Symbols))
	}
	names := map[string]bool{}
	for _, sym := range got.Symbols {
		names[sym.Name] = true
	}
	for _, name := range []string{"greet", "shout"} {
		if !names[name] {
			t.Errorf("AnalyzeProgram did not find symbol %q; got %v", name, names)
		}
	}
	if len(got.References) != len(want.References) {
		t.Errorf("AnalyzeProgram found %d references, Analyze found %d",
			len(got.References), len(want.References))
	}
}
