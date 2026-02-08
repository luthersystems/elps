// Copyright © 2024 The ELPS authors

package diagnostic

import (
	"bytes"
	"strings"
	"testing"
)

// testRenderer returns a Renderer with colors disabled and a fake source reader.
func testRenderer(sources map[string]string) *Renderer {
	return &Renderer{
		Color: ColorNever,
		SourceReader: func(name string) ([]byte, error) {
			s, ok := sources[name]
			if !ok {
				return nil, &fakeErr{name}
			}
			return []byte(s), nil
		},
	}
}

type fakeErr struct{ name string }

func (e *fakeErr) Error() string { return "not found: " + e.name }

func TestRenderErrorGolden(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "(set! false 42)",
	})

	d := Diagnostic{
		Severity: SeverityError,
		Message:  "cannot rebind constant: false",
		Spans: []Span{
			{File: "test.lisp", Line: 1, Col: 7, EndCol: 11, Label: "set! target is a language constant"},
		},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	want := "error: cannot rebind constant: false\n" +
		"  --> test.lisp:1:7\n" +
		"   |\n" +
		" 1 |  (set! false 42)\n" +
		"   |        ^^^^^ set! target is a language constant\n" +
		"   |\n"

	if got := buf.String(); got != want {
		t.Errorf("golden output mismatch:\ngot:\n%s\nwant:\n%s", got, want)
	}
}

func TestRenderWarning(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "(set x 1)\n(set x 2)",
	})

	d := Diagnostic{
		Severity: SeverityWarning,
		Message:  "repeated set on symbol: x",
		Spans: []Span{
			{File: "test.lisp", Line: 2, Col: 1, EndCol: 9},
		},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "warning: repeated set on symbol: x")
	assertContains(t, got, "--> test.lisp:2:1")
	assertContains(t, got, "(set x 2)")
}

func TestRenderNote(t *testing.T) {
	t.Parallel()
	r := testRenderer(nil)

	d := Diagnostic{
		Severity: SeverityNote,
		Message:  "defined here",
		Spans:    []Span{{File: "test.lisp", Line: 3, Col: 1}},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "note: defined here")
	assertNotContains(t, got, "error:")
	assertNotContains(t, got, "warning:")
}

func TestRenderNoSource(t *testing.T) {
	t.Parallel()
	r := testRenderer(nil)

	d := Diagnostic{
		Severity: SeverityError,
		Message:  "some error",
		Spans: []Span{
			{File: "<stdin>", Line: 5, Col: 3},
		},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "error: some error")
	assertContains(t, got, "--> <stdin>:5:3")
	// No-source fallback: gutter but no source line or underline
	assertContains(t, got, "   |")
	assertNotContains(t, got, "^")
	assertNotContains(t, got, " 5 |")
}

func TestRenderWithNotes(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "(my-fn 1 2)",
	})

	d := Diagnostic{
		Severity: SeverityError,
		Message:  "unbound symbol: my-fn",
		Spans: []Span{
			{File: "test.lisp", Line: 1, Col: 2, EndCol: 6},
		},
		Notes: []string{
			"in user:my-fn at test.lisp:1:1",
			"called from user:main at main.lisp:10:5",
		},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "= note: in user:my-fn at test.lisp:1:1")
	assertContains(t, got, "= note: called from user:main at main.lisp:10:5")
}

func TestRenderAutoDetectEndCol(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "(defun true () 42)",
	})

	d := Diagnostic{
		Severity: SeverityError,
		Message:  "cannot rebind constant: true",
		Spans: []Span{
			{File: "test.lisp", Line: 1, Col: 8}, // EndCol=0 → auto-detect
		},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	// "true" starts at col 8 and is 4 chars → exactly "^^^^"
	assertContains(t, got, "^^^^")
	assertNotContains(t, got, "^^^^^")
}

func TestRenderMultipleDiagnostics(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "(set x 1)\n(set x 2)\n(if true)",
	})

	diags := []Diagnostic{
		{
			Severity: SeverityWarning,
			Message:  "repeated set on symbol: x",
			Spans:    []Span{{File: "test.lisp", Line: 2, Col: 1, EndCol: 9}},
		},
		{
			Severity: SeverityWarning,
			Message:  "if requires 2-3 arguments",
			Spans:    []Span{{File: "test.lisp", Line: 3, Col: 1, EndCol: 9}},
		},
	}

	var buf bytes.Buffer
	if err := r.RenderAll(&buf, diags); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "repeated set on symbol: x")
	assertContains(t, got, "if requires 2-3 arguments")
	// Both diagnostics should be present
	if strings.Count(got, "warning:") != 2 {
		t.Errorf("expected 2 warning headers, got %d:\n%s", strings.Count(got, "warning:"), got)
	}
}

func TestRenderNoSpans(t *testing.T) {
	t.Parallel()
	r := testRenderer(nil)

	d := Diagnostic{
		Severity: SeverityError,
		Message:  "library error: file not found",
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	assertContains(t, got, "error: library error: file not found")
	// Should be just the header, no arrows or source
	assertNotContains(t, got, "-->")
}

func TestRenderTabExpansion(t *testing.T) {
	t.Parallel()
	r := testRenderer(map[string]string{
		"test.lisp": "\t(set x 1)",
	})

	d := Diagnostic{
		Severity: SeverityWarning,
		Message:  "test",
		Spans:    []Span{{File: "test.lisp", Line: 1, Col: 2, EndCol: 4}},
	}

	var buf bytes.Buffer
	if err := r.Render(&buf, d); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	// Tab should be expanded to 4 spaces in the source display
	assertContains(t, got, "    (set x 1)")
	// Underline should be positioned after the expanded tab
	assertContains(t, got, "    ^^^")
}

func assertContains(t *testing.T, got, want string) {
	t.Helper()
	if !strings.Contains(got, want) {
		t.Errorf("output does not contain %q:\n%s", want, got)
	}
}

func assertNotContains(t *testing.T, got, unwanted string) {
	t.Helper()
	if strings.Contains(got, unwanted) {
		t.Errorf("output unexpectedly contains %q:\n%s", unwanted, got)
	}
}
