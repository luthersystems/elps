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

func TestRenderError(t *testing.T) {
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

	got := buf.String()

	// Verify key structural elements
	assertContains(t, got, "error: cannot rebind constant: false")
	assertContains(t, got, "--> test.lisp:1:7")
	assertContains(t, got, "(set! false 42)")
	assertContains(t, got, "^^^^^")
	assertContains(t, got, "set! target is a language constant")
}

func TestRenderWarning(t *testing.T) {
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

func TestRenderNoSource(t *testing.T) {
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
	// Should have a gutter but no source line
	assertContains(t, got, "|")
	assertNotContains(t, got, "^")
}

func TestRenderNotes(t *testing.T) {
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
	// "true" starts at col 8 and is 4 chars → should produce "^^^^"
	assertContains(t, got, "^^^^")
}

func TestRenderMultipleDiagnostics(t *testing.T) {
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
	// Should have both diagnostics separated by blank line
	parts := strings.Split(got, "\n\n")
	if len(parts) < 2 {
		t.Errorf("expected diagnostics separated by blank line, got:\n%s", got)
	}
	assertContains(t, got, "repeated set on symbol: x")
	assertContains(t, got, "if requires 2-3 arguments")
}

func TestRenderNoSpans(t *testing.T) {
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
