// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"
)

// stepCapHint sizes the step slice selectorPaths appends into. It matters
// for two different reasons, and the two tests below are one per reason.
//
// It must be an UPPER BOUND, or the scan grows and the presizing buys
// nothing. That is the correctness-shaped half, and it is asserted here on a
// table and again in FuzzParseSelector on everything the fuzzer accepts.
//
// It must also be TIGHT on the shapes callers actually write, or the
// presizing trades growth allocations for wasted bytes and the B/op the
// benchmark gate watches goes up instead of down. That half cannot be
// fuzzed -- "tight" is a claim about real selectors, not about all strings
// -- so it is a table, and the table is the same practical shapes
// BenchmarkParseSelector measures.

// TestStepCapHintBoundsStepCount pins the upper-bound half over a corpus
// chosen for the ways the count could go WRONG rather than for coverage of
// the grammar: quoted keys carrying a "." or a "[", whitespace inside
// brackets, the "?" suffix, and the ".[x]" preprocessing case, which strips
// a leading dot after the hint is computed.
func TestStepCapHintBoundsStepCount(t *testing.T) {
	t.Parallel()
	selectors := []string{
		".", ".a", ".a.b", ".a.b.c", ".hello", "._private",
		`.["$private"]`, `.["a"]`, `.["a"]["b"]`, `.[""]`,
		// A quoted key holding a step-starting byte: the count
		// over-estimates here, which is allowed, but it must never do the
		// reverse.
		`.["a.b"]`, `.["a[b"]`, `.["...."]`, `.["[[[["]`, `.["a.b"].c[0]`,
		`.["a\"b"]`, `.["\"\n"]`, `.["]"]`, `.["["]`,
		".[0]", ".[-1]", ".[23]", ".[]", ".[][]", ".[][][]",
		".[1:3]", ".[:2]", ".[1:]", ".[:]", ".[-2:]", ".[-3:-1]",
		".a[0].b", ".a[].b", ".items[1:3].id", ".[0].a[]",
		".a?", ".[0]?", `.["a"]?`,
		".[ 1 : 2 ]", ". a . b",
		"." + strings.TrimSuffix(strings.Repeat("k.", 100), "."),
		"." + strings.Repeat("[]", 50),
	}
	for _, sel := range selectors {
		t.Run(sel, func(t *testing.T) {
			steps, err := selectorPaths(sel)
			if err != nil {
				t.Fatalf("selectorPaths(%q): %v", sel, err)
			}
			hint := stepCapHint(selectorBody(strings.TrimSpace(sel)))
			if hint < len(steps) {
				t.Fatalf("stepCapHint(%q) = %d, below the %d steps it names",
					sel, hint, len(steps))
			}
			// The presizing only pays if the scan then never grows. cap
			// is the direct evidence: append leaves it alone while it has
			// room, so a cap above the hint means a growth happened.
			if cap(steps) > hint {
				t.Fatalf("selectorPaths(%q) grew its step slice: cap %d exceeds hint %d",
					sel, cap(steps), hint)
			}
		})
	}
}

// TestStepCapHintIsTightOnPracticalSelectors is the other half. An upper
// bound alone is satisfied by len(selector), which would allocate eight
// bytes of unused capacity per selector byte; what keeps B/op down is that
// the hint is EXACT on the shapes callers write.
//
// These are BenchmarkParseSelector's practical arm verbatim, so a change
// that loosens the hint fails here with the reason rather than arriving as
// an unexplained byte regression in CI.
func TestStepCapHintIsTightOnPracticalSelectors(t *testing.T) {
	t.Parallel()
	for _, sel := range []string{
		".a.b.c", ".items[0].id", `.["first name"].address.city`,
		".items[1:3]", ".items[1:]",
		"." + strings.TrimSuffix(strings.Repeat("k.", 100), "."),
	} {
		t.Run(sel, func(t *testing.T) {
			steps, err := selectorPaths(sel)
			if err != nil {
				t.Fatalf("selectorPaths(%q): %v", sel, err)
			}
			if hint := stepCapHint(selectorBody(sel)); hint != len(steps) {
				t.Fatalf("stepCapHint(%q) = %d for %d steps; the hint is no longer exact "+
					"on a practical selector, which costs B/op on every parse",
					sel, hint, len(steps))
			}
		})
	}
}
