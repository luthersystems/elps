// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/stretchr/testify/assert"
)

// A truncation cuts BYTES out of text whose units are runes, and the two
// diagnostic paths that keep the prefix -- DiagnosticRenderer.Text and the
// trace writer -- handed that prefix to protocol encoders. A cut through the
// middle of a rune made them emit invalid UTF-8; a limit below the marker's
// own length made them emit a piece of the marker, which reads as content.
func TestTruncatedRenderKeepsWholeRunesAndMarkers(t *testing.T) {
	for _, s := range []string{"", "plain text", strings.Repeat("世", 8), "aé世💡" + strings.Repeat("界", 4)} {
		for limit := range len(s) + len(renderTruncatedMark) + 2 {
			got := truncatedRender(s, limit)
			assert.LessOrEqual(t, len(got), limit, "truncation exceeded its limit")
			assert.True(t, utf8.ValidString(got), "truncation split a rune: %q (limit %d)", got, limit)
			if got != "" {
				assert.True(t, strings.HasSuffix(got, renderTruncatedMark),
					"truncation emitted a partial marker: %q (limit %d)", got, limit)
			}
			if limit < len(renderTruncatedMark) {
				assert.Empty(t, got, "a limit too small for the marker must render nothing")
			}
		}
	}
}

func TestDiagnosticRendererTextKeepsWholeRunes(t *testing.T) {
	env := initSafetyTestEnv(t)
	for limit := range 64 {
		r := env.NewRendererWithLimit(context.Background(), limit)
		out := r.Text(strings.Repeat("世", 6), strings.Repeat("x", 10))
		assert.True(t, utf8.ValidString(out), "Text split a rune: %q (limit %d)", out, limit)
		assert.LessOrEqual(t, len(out), limit)
		if i := strings.Index(out, "#<"); i >= 0 {
			assert.Equal(t, renderTruncatedMark, out[i:], "Text emitted a partial marker: %q", out)
		}
	}
}

// diagnosticText is what WriteTrace hands the writer, and a frame name or
// filename is arbitrary text an embedder supplies.
func TestDiagnosticTextKeepsWholeRunes(t *testing.T) {
	for limit := range 64 {
		r := valueRenderer{limit: limit, budget: newRenderBudget(limit, nil)}
		r.traceText(strings.Repeat("世", 20))
		out := r.diagnosticText()
		assert.True(t, utf8.ValidString(out), "trace text split a rune: %q (limit %d)", out, limit)
		assert.LessOrEqual(t, len(out), limit)
	}
}
