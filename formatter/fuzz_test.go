// Copyright © 2026 The ELPS authors

package formatter_test

import (
	"bytes"

	"strings"
	"testing"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// The formatter is the one place in this repository where a round-trip
// property is worth more than a panic check, because `elps fmt` REWRITES the
// user's source in place.  Two invariants are asserted, both of which the
// package's own tests already assert for fixed inputs -- this generalises them
// to arbitrary input:
//
//   - AST preservation.  Parsing the formatted text must yield the same
//     program as parsing the original.  formatter_test.go asserts exactly this
//     via roundTripEqual (comparing LVal.String() of each top-level form) for
//     the repository's .lisp files; a formatter that changes the AST silently
//     changes program semantics.
//
//   - Idempotence.  Format(Format(x)) == Format(x).  Asserted for every case
//     in runFormatTests and again in TestRepoFileRoundTrip / TestEdgeIdempotency.
//     A non-idempotent formatter makes `elps fmt` produce diff churn forever.
//
// Neither is asserted for input the formatter rejects: Format returns the
// parse error unchanged and there is nothing to compare.

// programString renders a parsed program the way formatter_test.go's
// roundTripEqual does -- LVal.String() per top-level form -- so the fuzz
// target and the table tests agree on what "the same AST" means.
//
// It goes through ParseProgram rather than a bare Parse loop because
// ParseProgram is what lisp.Reader.Read calls, and it is the only entry point
// that consumes a leading hash-bang.  A Parse loop reports `#!/usr/bin/env
// elps` as a parse failure that the real reader accepts.
func programString(t *testing.T, src []byte) (string, bool) {
	t.Helper()
	p := rdparser.New(token.NewScanner("fuzz", bytes.NewReader(src)))
	exprs, err := p.ParseProgram()
	if err != nil {
		return "", false
	}
	parts := make([]string, 0, len(exprs))
	for _, expr := range exprs {
		if expr == nil || expr.Type == lisp.LError {
			return "", false
		}
		parts = append(parts, expr.String())
	}
	return strings.Join(parts, "\n"), true
}

func FuzzFormat(f *testing.F) {
	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		formatted, err := formatter.Format(src, nil)
		if err != nil {
			// Rejecting input is a legitimate outcome; the contract is only
			// that a rejection comes with no output.
			if formatted != nil {
				t.Fatalf("Format returned %d bytes alongside error %v", len(formatted), err)
			}
			return
		}

		// Idempotence.
		again, err := formatter.Format(formatted, nil)
		if err != nil {
			t.Fatalf("Format rejected its own output: %v\n--- output ---\n%s", err, formatted)
		}
		if !bytes.Equal(formatted, again) {
			t.Fatalf("Format is not idempotent\n--- pass 1 ---\n%q\n--- pass 2 ---\n%q",
				formatted, again)
		}

		// AST preservation.  The original must still parse -- Format only
		// succeeds when the format-preserving parser accepted the input --
		// so a failure to re-parse either side is itself a defect.
		before, ok := programString(t, src)
		if !ok {
			t.Fatalf("Format succeeded but the standard parser rejected the input")
		}
		after, ok := programString(t, formatted)
		if !ok {
			t.Fatalf("the standard parser rejected formatted output\n--- output ---\n%s", formatted)
		}
		if before != after {
			t.Fatalf("Format changed the AST\n--- before ---\n%s\n--- after ---\n%s\n--- output ---\n%s",
				before, after, formatted)
		}
	})
}

// FuzzFormatCompact covers the second printer path.  Compact mode is what the
// minifier emits through, so it is the one that ends up in shipped phylum
// bundles; it strips comments and collapses whitespace, and only the AST-
// preservation half of the contract applies to it in the same terms.
func FuzzFormatCompact(f *testing.F) {
	for _, src := range fuzzseed.All() {
		f.Add(src)
	}
	f.Fuzz(func(t *testing.T, src []byte) {
		cfg := formatter.DefaultConfig()
		cfg.Compact = true
		cfg.StripComments = true

		formatted, err := formatter.Format(src, cfg)
		if err != nil {
			return
		}
		again, err := formatter.Format(formatted, cfg)
		if err != nil {
			t.Fatalf("compact Format rejected its own output: %v\n--- output ---\n%s", err, formatted)
		}
		if !bytes.Equal(formatted, again) {
			t.Fatalf("compact Format is not idempotent\n--- pass 1 ---\n%q\n--- pass 2 ---\n%q",
				formatted, again)
		}
		before, ok := programString(t, src)
		if !ok {
			t.Fatalf("compact Format succeeded but the standard parser rejected the input")
		}
		after, ok := programString(t, formatted)
		if !ok {
			t.Fatalf("the standard parser rejected compact output\n--- output ---\n%s", formatted)
		}
		if before != after {
			t.Fatalf("compact Format changed the AST\n--- before ---\n%s\n--- after ---\n%s\n--- output ---\n%s",
				before, after, formatted)
		}
	})
}
