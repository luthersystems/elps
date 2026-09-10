// Copyright © 2026 The ELPS authors

package lsp

import (
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// The independent model enumerates complete ASCII symbol tokens, rather than
// scanning outward from the cursor. In particular, a word's end is a hit but
// the next column is not. Non-ASCII bytes remain separators; #641 changes
// allocation, not the existing fallback's symbol alphabet or byte units.
func TestWordPositionBoundaries(t *testing.T) {
	symbol := regexp.MustCompile(`[-a-zA-Z0-9_!?+*/<>=:.#^&]+`)
	for _, content := range []string{
		"", "\n", "\r\n", "hello", "hello\n", "hello\r\n\r\nworld",
		"(pkg:some-name! a)\n\n&rest :key\nlast",
		" é🙂 target\r\n加算 ascii\n\xffbad",
		"a\tb\r(c)[d] 'e\"f\";g\x00h",
	} {
		t.Run(content, func(t *testing.T) {
			lines := strings.Split(content, "\n")
			for line := -1; line <= len(lines); line++ {
				text := ""
				if line >= 0 && line < len(lines) {
					text = lines[line]
				}
				matches := symbol.FindAllStringIndex(text, -1)
				for col := -1; col <= len(text)+1; col++ {
					var start, end int
					var ok bool
					for _, span := range matches {
						if col >= span[0] && col <= span[1] {
							start, end, ok = span[0], span[1], true
							break
						}
					}
					gotStart, gotEnd, gotOK := wordBoundsAtPosition(content, line, col)
					require.Equal(t, start, gotStart, "line %d col %d", line, col)
					require.Equal(t, end, gotEnd, "line %d col %d", line, col)
					require.Equal(t, ok, gotOK, "line %d col %d", line, col)
					require.Equal(t, text[start:end], wordAtPosition(content, line, col), "line %d col %d", line, col)
					var wantRange *protocol.Range
					if ok {
						wantRange = &protocol.Range{
							Start: protocol.Position{Line: safeUint(line), Character: safeUint(start)},
							End:   protocol.Position{Line: safeUint(line), Character: safeUint(end)},
						}
					}
					require.Equal(t, wantRange, wordRangeAtPosition(content, line, col), "line %d col %d", line, col)
				}
			}
		})
	}
}

func TestWordPositionWireEncoding(t *testing.T) {
	const content = "header\r\n é🙂 target\r\n"
	for _, tc := range []struct {
		name string
		enc  positionEncoding
		col  int
	}{
		{"utf16", encodingUTF16, 5},
		{"utf8", encodingUTF8, 8},
	} {
		t.Run(tc.name, func(t *testing.T) {
			s := testServer()
			s.posEncoding.Store(int32(tc.enc))
			line, col := s.cursorAt(&Document{Content: content}, protocol.Position{Line: 1, Character: safeUint(tc.col)})
			require.Equal(t, 1, line)
			require.Equal(t, 8, col)
			require.Equal(t, "target", wordAtPosition(content, line, col))
			require.Equal(t, &protocol.Range{
				Start: protocol.Position{Line: 1, Character: 8},
				End:   protocol.Position{Line: 1, Character: 14},
			}, wordRangeAtPosition(content, line, col))
		})
	}
}

// #641: document line slices must not be allocated per word lookup. Check
// successful results too, so an always-empty fast path cannot satisfy the gate.
func TestWordPositionAllocations(t *testing.T) {
	content := strings.Repeat("target\n", 1<<14)
	for _, line := range []int{0, (1 << 14) - 1} {
		var word string
		allocs := testing.AllocsPerRun(10, func() { word = wordAtPosition(content, line, 3) })
		require.Equal(t, "target", word)
		assert.Zero(t, allocs, "word lookup must not allocate a document-sized line slice")
		var start, end int
		var ok bool
		allocs = testing.AllocsPerRun(10, func() { start, end, ok = wordBoundsAtPosition(content, line, 3) })
		require.Equal(t, 0, start)
		require.Equal(t, 6, end)
		require.True(t, ok)
		assert.Zero(t, allocs, "bounds lookup must not allocate a document-sized line slice")
		var rng *protocol.Range
		allocs = testing.AllocsPerRun(10, func() { rng = wordRangeAtPosition(content, line, 3) })
		require.Equal(t, &protocol.Range{
			Start: protocol.Position{Line: safeUint(line), Character: 0},
			End:   protocol.Position{Line: safeUint(line), Character: 6},
		}, rng)
		assert.LessOrEqual(t, allocs, float64(1), "only the returned range may allocate")
	}
}

// Keep both ordinary files and newline-dense adversarial documents visible.
// First/last-line rows distinguish avoiding a whole-document split from the
// still-linear scan to a late line. Construction is outside the timed region.
func BenchmarkWordPosition(b *testing.B) {
	for _, doc := range []struct {
		name  string
		width int
		lines int
	}{
		{"small", 64, 16},
		{"1MiB-lines64", 64, 1 << 14},
		{"1MiB-lines2", 2, 1 << 19},
	} {
		content := strings.Repeat("x"+strings.Repeat(" ", doc.width-2)+"\n", doc.lines)
		for _, pos := range []struct {
			name string
			line int
		}{
			{"first", 0}, {"last", doc.lines - 1},
		} {
			b.Run(doc.name+"/"+pos.name+"/word", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if wordAtPosition(content, pos.line, 0) != "x" {
						b.Fatal("incorrect word")
					}
				}
			})
			b.Run(doc.name+"/"+pos.name+"/bounds", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					start, end, ok := wordBoundsAtPosition(content, pos.line, 0)
					if start != 0 || end != 1 || !ok {
						b.Fatal("incorrect bounds")
					}
				}
			})
			b.Run(doc.name+"/"+pos.name+"/range", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					rng := wordRangeAtPosition(content, pos.line, 0)
					if rng == nil || rng.Start.Line != safeUint(pos.line) || rng.Start.Character != 0 || rng.End.Line != safeUint(pos.line) || rng.End.Character != 1 {
						b.Fatal("incorrect range")
					}
				}
			})
		}
	}
}
