// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"bytes"
	"strings"
	"testing"
)

// This file is the pin for elps#469: writeSpan sized the caret underline in
// BYTES while indenting it in display columns, so on any span containing a
// multi-byte rune the caret run was longer than the text it pointed at and
// spilled over whatever followed.
//
// It is deliberately not a test about an integer.  #469 is two numbers that
// have to agree with each other, and the only place they are observably
// wrong is the rendered two-line block a user reads, so that is what these
// tests compare.  A failure prints the source line and the caret line one
// above the other, which is the form in which the defect is obvious.
//
// BYTES, RUNES, OR CELLS.  Bytes were the bug.  Runes would have fixed the
// reported spillover and still been wrong, because the underline has to line
// up with what a TERMINAL draws and a terminal does not advance one cell per
// rune: an East Asian wide character advances two and a combining mark
// advances none.  Both halves now go through displayWidth, which counts
// cells, so they agree by construction rather than by coincidence -- and the
// CJK rows below are what separates the two candidate fixes.  Under a rune
// count "加算" gets two carets under a four-cell token; under bytes it got
// six.  Four is right.
//
// Rows and tests marked GUARD pass on 95e2e1a as well as here.  They pin
// behaviour the fix must not break; they are not catches.

// renderOneSpan renders a single-span diagnostic over src and returns the
// printed source line and the caret line beneath it, both with the gutter
// stripped.  Colour is off, so the lines contain no escape sequences.
func renderOneSpan(t *testing.T, src string, span Span) (srcLine, caretLine string) {
	t.Helper()
	r := testRenderer(map[string]string{span.File: src})
	var buf bytes.Buffer
	if err := r.Render(&buf, Diagnostic{
		Severity: SeverityError,
		Message:  "probe",
		Spans:    []Span{span},
	}); err != nil {
		t.Fatalf("render: %v", err)
	}
	// Layout is: header, "  --> loc", gutter, source line, caret line,
	// gutter.  Both lines carry the same "%s |  " gutter, so splitting on the
	// first "|" and dropping the two spaces after it leaves the payload.
	var payload []string
	for _, line := range strings.Split(buf.String(), "\n") {
		i := strings.Index(line, "|")
		if i < 0 {
			continue
		}
		payload = append(payload, strings.TrimPrefix(line[i+1:], "  "))
	}
	if len(payload) < 3 {
		t.Fatalf("expected three gutter lines, got %d:\n%s", len(payload), buf.String())
	}
	return payload[1], payload[2]
}

// TestUnderlineCoversExactlyTheSpannedText is the reproduction from #469.
//
// The wanted caret line is written out in full rather than derived, so that
// what the test asserts is a picture of the output and not a restatement of
// the implementation.
func TestUnderlineCoversExactlyTheSpannedText(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name  string
		src   string
		col   int
		want  string // the caret line, verbatim
		guard bool
	}{{
		// GUARD: ASCII, where a byte, a rune and a cell are all the same
		// thing.  This is why the defect survived: every in-tree diagnostic
		// is about a name like this one.
		name: "ascii", src: "(set! abc 1)", col: 7,
		want: "      ^^^", guard: true,
	}, {
		// 2 bytes, 1 rune, 1 cell.  Was "^^".
		name: "two-byte-rune", src: "(set! é 1)", col: 7,
		want: "      ^",
	}, {
		// 6 bytes, 2 runes, 4 cells -- the row that tells cells from runes.
		// Was "^^^^^^", running four columns past the token and underlining
		// " 1)" as well.
		name: "cjk-wide", src: "(f 加算 1)", col: 4,
		want: "   ^^^^",
	}, {
		// 8 bytes, 2 runes, 2 cells (mathematical italic, outside the BMP and
		// NOT wide).  Was "^^^^^^^^".
		name: "astral-narrow", src: "(f 𝛼𝛽 1)", col: 4,
		want: "   ^^",
	}, {
		// e + U+0301 COMBINING ACUTE: 3 bytes, 2 runes, 1 cell.  Was "^^^^";
		// a rune count would say two.
		name: "combining-mark", src: "(f é\u0301 1)", col: 4,
		want: "   ^",
	}, {
		// An emoji is two cells.  Was six carets for six bytes.
		name: "emoji", src: "(f 😀x 1)", col: 4,
		want: "   ^^^",
	}, {
		// GUARD: tabs.  The source line has its tabs rewritten to four
		// spaces before printing, and displayWidth counts them as four, so
		// the indent was already right; this pins that the change did not
		// disturb it.
		name: "tab-indent", src: "\t(set! abc 1)", col: 8,
		want: "          ^^^", guard: true,
	}, {
		// The two together: a CJK name AFTER a tab.  The indent comes from
		// the tab rule and the length from the width rule, which is the
		// combination that has to hold for either to be useful.
		name: "tab-then-cjk", src: "\t(f 加算 1)", col: 5,
		want: "       ^^^^",
	}, {
		// A wide character BEFORE the span: the indent has to skip four
		// cells, not two runes and not six bytes.  Also a catch -- on
		// 95e2e1a displayWidth counted 加算 as two, so the carets started two
		// columns left of the token.
		name: "wide-before-the-span", src: "(加算 abc 1)", col: 9,
		want: "      ^^^",
	}} {
		name := tc.name
		if tc.guard {
			name += "-GUARD"
		}
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			srcLine, caretLine := renderOneSpan(t, tc.src, Span{File: "t.lisp", Line: 1, Col: tc.col})
			if caretLine != tc.want {
				t.Errorf("caret line does not cover the spanned text (#469):\n"+
					"  source: %s\n  got:    %s\n  want:   %s\n"+
					"  (%d carets, wanted %d)",
					srcLine, caretLine, tc.want,
					strings.Count(caretLine, "^"), strings.Count(tc.want, "^"))
			}
		})
	}
}

// TestUnderlineIsMeasuredInTheSameUnitAsTheIndent states the same defect as a
// property rather than as expected text, over every span position in each
// source line: whatever writeSpan chose to underline, the carets must start
// at the spanned text's first cell and run for exactly as many cells as that
// text occupies.
//
// The point of having it as well as the table above is that it cannot be
// satisfied by writing the wrong string into a `want`, and it covers span
// positions nobody thought to tabulate.
func TestUnderlineIsMeasuredInTheSameUnitAsTheIndent(t *testing.T) {
	t.Parallel()
	for _, src := range []string{
		"(set! abc 1)",
		"(f 加算 1)",
		"(set! é 1)",
		"(f 𝛼𝛽 1)",
		"(f é\u0301 1)",
		"\t(f 加算 xyz)",
		"(加算 é 1)",
		"(f 😀 1)",
	} {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			for col := 1; col <= len(src); col++ {
				// Only start a span on a rune boundary; a byte column inside
				// a rune is not a position any producer emits.
				if src[col-1]&0xC0 == 0x80 {
					continue
				}
				span := Span{File: "t.lisp", Line: 1, Col: col}
				srcLine, caretLine := renderOneSpan(t, src, span)

				// The renderer's own auto-detection decides where the span
				// ends; recompute it the same way so the property is about
				// the two MEASUREMENTS agreeing, not about token detection.
				r := &Renderer{}
				endCol := r.detectEndCol(src, col)
				if endCol < col {
					endCol = col
				}
				if endCol > len(src) {
					endCol = len(src)
				}
				wantPad := displayWidth(src[:col-1])
				wantCarets := displayWidth(src[col-1 : endCol])
				if wantCarets < 1 {
					wantCarets = 1
				}

				gotPad := len(caretLine) - len(strings.TrimLeft(caretLine, " "))
				gotCarets := strings.Count(caretLine, "^")
				if gotPad != wantPad || gotCarets != wantCarets {
					t.Errorf("col %d: carets start at cell %d for %d cells, want %d for %d (#469):\n"+
						"  source: %s\n  carets: %s",
						col, gotPad, gotCarets, wantPad, wantCarets, srcLine, caretLine)
				}
				// The consequence, stated directly: the carets must not run
				// past the text they point at.
				if gotPad+gotCarets > displayWidth(src) {
					t.Errorf("col %d: the caret run ends at cell %d, past the end of a %d-cell line (#469):\n"+
						"  source: %s\n  carets: %s",
						col, gotPad+gotCarets, displayWidth(src), srcLine, caretLine)
				}
			}
		})
	}
}

// TestDisplayWidthCountsCells is the unit underneath both tests above.
//
// The wide and combining rows are the ones a rune count gets wrong, and they
// are the reason the fix is "measure both halves in cells" rather than the
// smaller "measure both halves in runes" that would also have stopped the
// reported spillover.
func TestDisplayWidthCountsCells(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		s    string
		want int
	}{
		{"empty", "", 0},
		{"ascii", "abc", 3},
		{"tab", "\t", tabWidth},
		{"tab and ascii", "\tab", tabWidth + 2},
		{"latin-1 accent", "é", 1},
		{"decomposed accent", "e\u0301", 1},
		{"cjk", "加算", 4},
		{"hangul", "한글", 4},
		{"fullwidth latin", "ＡＢ", 4},
		{"halfwidth katakana", "ｱｲ", 2},
		{"astral non-wide", "𝛼𝛽", 2},
		{"emoji", "😀", 2},
		{"ambiguous stays narrow", "→①", 2},
		{"greek stays narrow", "λαβ", 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := displayWidth(tc.s); got != tc.want {
				t.Errorf("displayWidth(%q) = %d, want %d", tc.s, got, tc.want)
			}
		})
	}
}

// TestTabExpansionMatchesTabWidth pins the pair of constants the source line
// and the caret line each read separately.  If they drift, every tab-indented
// diagnostic misaligns and nothing else says so.
func TestTabExpansionMatchesTabWidth(t *testing.T) {
	t.Parallel()
	if len(tabExpansion) != tabWidth || strings.Trim(tabExpansion, " ") != "" {
		t.Errorf("tabExpansion %q is not %d spaces; the source line and the caret line"+
			" are laid out by different code and only agree because these match",
			tabExpansion, tabWidth)
	}
}

// TestSpanEndColIsInclusive pins the convention #469 flags as a latent trap:
// diagnostic.Span.EndCol is INCLUSIVE, while parser/token.Location.EndCol is
// documented (since #463) EXCLUSIVE, and the two fields have the same name.
//
// Nothing bridges them today -- cmd/diagnostic.go and repl/diagnostic.go both
// leave Span.EndCol zero -- so this is not a defect, it is the thing the next
// person to wire an analyser's end position into a Span needs to know.  The
// convention is now written on the field; this makes it executable, so a
// silent switch to exclusive fails here rather than in someone's terminal.
//
// GUARD: passes on 95e2e1a.
func TestSpanEndColIsInclusive(t *testing.T) {
	t.Parallel()
	// "false" occupies byte columns 7..11 of "(set! false 42)".
	const src = "(set! false 42)"
	_, caretLine := renderOneSpan(t, src, Span{File: "t.lisp", Line: 1, Col: 7, EndCol: 11})
	if got := strings.Count(caretLine, "^"); got != len("false") {
		t.Errorf("Col 7 / EndCol 11 underlined %d columns of a %d-column name;"+
			" Span.EndCol is inclusive, and if that changed the doc comment on"+
			" Span needs to change with it\n  source: %s\n  carets: %s",
			got, len("false"), src, caretLine)
	}
}

// TestUnderlineSurvivesOutOfRangeSpans covers positions no in-tree producer
// emits but the exported type permits, since Span is filled in by callers and
// nothing validates it.
//
// On 95e2e1a an EndCol past the end of the line produced a caret run as long
// as the number said -- 500 carets across the terminal for a 15-column line.
// Not a crash, so it is a robustness improvement rather than the defect #469
// is about; it is here because the fix computes a slice from these numbers
// and a slice is the kind of thing that panics.
func TestUnderlineSurvivesOutOfRangeSpans(t *testing.T) {
	t.Parallel()
	const src = "(set! abc 1)"
	for _, span := range []Span{
		{File: "t.lisp", Line: 1, Col: 7, EndCol: 500},
		{File: "t.lisp", Line: 1, Col: 500},
		{File: "t.lisp", Line: 1, Col: 500, EndCol: 900},
		{File: "t.lisp", Line: 1, Col: 900, EndCol: 500},
		{File: "t.lisp", Line: 1, Col: -3, EndCol: -9},
		{File: "t.lisp", Line: 1, Col: len(src), EndCol: len(src)},
	} {
		_, caretLine := renderOneSpan(t, src, span)
		carets := strings.Count(caretLine, "^")
		if carets < 1 {
			t.Errorf("span %+v produced no carets at all", span)
		}
		// One cell past the last is allowed: a position at end-of-line
		// legitimately points just after the final character.  Anything
		// beyond that is the runaway run 95e2e1a produced.
		if pad := len(caretLine) - len(strings.TrimLeft(caretLine, " ")); pad+carets > displayWidth(src)+1 {
			t.Errorf("span %+v underlined %d cells starting at %d, past the end of a %d-cell line:\n  %s",
				span, carets, pad, displayWidth(src), caretLine)
		}
	}
}
