// Copyright © 2026 The ELPS authors

package minifier_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/minifier"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// The minifier is the last thing that touches a phylum before it is packaged
// and uploaded, so its output IS the chaincode.  Three properties are asserted,
// each of which the package's own tests already assert for fixed inputs:
//
//   - Determinism.  Two runs over the same source produce byte-identical
//     output and an identical symbol map.  TestMinifySource_DeterministicAnd-
//     ScopeAware asserts exactly this; a non-deterministic minifier makes
//     builds unreproducible and a symbol map useless for decoding a trace.
//
//   - Output parses.  Every test in minifier_test.go compares the output
//     against an expected ELPS program, so producing something the parser
//     rejects is unambiguously a defect.
//
//   - Shape preservation.  Minification renames identifiers; it must not add,
//     drop or re-nest forms.  Comparing the two programs' TYPE SKELETONS
//     catches structural damage without asserting anything about the names,
//     which are exactly what the pass is allowed to change.
//
// Nothing is asserted about which names are chosen, or about semantics beyond
// shape: the renaming rules are scope- and macro-sensitive and the unit tests
// are the right place to pin them.

func parseProgram(src []byte) ([]*lisp.LVal, error) {
	p := rdparser.New(token.NewScanner("fuzz", bytes.NewReader(src)))
	return p.ParseProgram()
}

// skeleton renders a program's structure with every symbol, string and number
// replaced by its type name, so two programs compare equal iff they have the
// same shape.
func skeleton(exprs []*lisp.LVal) string {
	var b strings.Builder
	for _, expr := range exprs {
		writeSkeleton(&b, expr)
		b.WriteByte('\n')
	}
	return b.String()
}

func writeSkeleton(b *strings.Builder, v *lisp.LVal) {
	if v == nil {
		b.WriteString("<nil>")
		return
	}
	if v.IsQuoted() {
		b.WriteByte('\'')
	}
	if len(v.Cells) == 0 {
		b.WriteString(v.Type.String())
		return
	}
	b.WriteString(v.Type.String())
	b.WriteByte('(')
	for i, c := range v.Cells {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeSkeleton(b, c)
	}
	b.WriteByte(')')
}

func FuzzMinifySource(f *testing.F) {
	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		out, symbols, err := minifier.MinifySource(src, "fuzz.lisp", nil)
		if err != nil {
			return
		}

		// Determinism.
		out2, symbols2, err := minifier.MinifySource(src, "fuzz.lisp", nil)
		if err != nil {
			t.Fatalf("second MinifySource of the same source failed: %v", err)
		}
		if !bytes.Equal(out, out2) {
			t.Fatalf("MinifySource is not deterministic\n--- run 1 ---\n%s\n--- run 2 ---\n%s", out, out2)
		}
		if len(symbols.Entries) != len(symbols2.Entries) {
			t.Fatalf("symbol map is not deterministic: %d entries then %d",
				len(symbols.Entries), len(symbols2.Entries))
		}
		for i := range symbols.Entries {
			if symbols.Entries[i] != symbols2.Entries[i] {
				t.Fatalf("symbol map entry %d differs between runs: %+v vs %+v",
					i, symbols.Entries[i], symbols2.Entries[i])
			}
		}

		// Output parses, and has the same shape as the input.
		before, err := parseProgram(src)
		if err != nil {
			t.Fatalf("MinifySource succeeded but the parser rejected its input: %v", err)
		}
		after, err := parseProgram(out)
		if err != nil {
			t.Fatalf("the parser rejected minified output: %v\n--- output ---\n%s", err, out)
		}
		if got, want := skeleton(after), skeleton(before); got != want {
			t.Fatalf("minification changed the program shape\n--- before ---\n%s\n--- after ---\n%s\n--- output ---\n%s",
				want, got, out)
		}

		// Minifying the output again must still parse.  A second pass sees
		// only already-shortened names, which is the state a build pipeline
		// reaches when a bundle is re-processed.
		out3, _, err := minifier.MinifySource(out, "fuzz.lisp", nil)
		if err != nil {
			t.Fatalf("MinifySource rejected its own output: %v\n--- output ---\n%s", err, out)
		}
		if _, err := parseProgram(out3); err != nil {
			t.Fatalf("the parser rejected twice-minified output: %v\n--- output ---\n%s", err, out3)
		}
	})
}
