// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"strings"
	"testing"
)

// BenchmarkWordAtPosition measures the cost of one hover or definition
// lookup, which is what the MCP server pays per request (elps#654).
//
// The two axes are the ones that separate a per-line cost from a
// per-document one:
//
//   - SIZE. A small buffer and a 1 MiB one. An implementation that reads the
//     whole document before finding the line grows with the document; one
//     that walks to the line does not grow with the part it never reads.
//   - WHICH LINE. The first line and the last. Walking to the line is O(bytes
//     before the cursor), so first/last is the best and worst case of the
//     scan, while splitting the document costs the same either way -- the
//     shape of the two rows tells you which implementation is running.
//
// The word is consumed into a package-level sink so the call cannot be
// optimised away, and it is checked once outside the timed loop so a
// benchmark of an always-empty lookup cannot look fast.
func BenchmarkWordAtPosition(b *testing.B) {
	const line = "(defun target (x y) (+ x y))\n"
	col := strings.Index(line, "target")

	small := strings.Repeat(line, 8)
	large := strings.Repeat(line, (1<<20)/len(line)+1)

	docs := []struct {
		name    string
		content string
	}{
		{"small", small},
		{"1MiB", large},
	}
	for _, doc := range docs {
		lines := strings.Count(doc.content, "\n")
		positions := []struct {
			name string
			line int
		}{
			{"first", 0},
			{"last", lines - 1},
		}
		for _, pos := range positions {
			b.Run(doc.name+"/"+pos.name, func(b *testing.B) {
				if got := wordAtPosition(doc.content, pos.line, col); got != "target" {
					b.Fatalf("wordAtPosition = %q, want %q", got, "target")
				}
				b.ReportAllocs()
				b.ResetTimer()
				for b.Loop() {
					wordSink = wordAtPosition(doc.content, pos.line, col)
				}
			})
		}
	}
}

// wordSink keeps the benchmarked result live.
var wordSink string
