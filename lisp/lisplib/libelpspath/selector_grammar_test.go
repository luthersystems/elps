// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"strings"
	"testing"
)

// TestSelectorGrammarPathologies is the deterministic half of the grammar's
// panic coverage: strings that are ALMOST selectors.
//
// FuzzParseSelector explores; this enumerates. The shapes below are the ones
// a fuzzer reaches only by luck because they need several specific bytes in
// order -- runs of dots, dots INSIDE brackets, unbalanced and nested
// brackets, a quoted key ending in backslashes, stacked "?" suffixes, colons
// where a range wants one and where it does not. `..[.]path` is the shape
// that prompted them.
//
// Three properties per selector, and the second and third are what make this
// more than a smoke test:
//
//   - no panic, from either entry point;
//   - if ACCEPTED, the printed path parses back -- the round-trip invariant
//     issue #566 broke, checked here on inputs chosen to stress it rather
//     than on inputs chosen to be valid;
//   - ParseSelector and SelectorSteps AGREE about acceptance. They share
//     selectorPaths, so they can only diverge in pathToStep, which is
//     exactly where a grammar form added without a step spelling would
//     land.
//
// None of these is expected to be accepted-and-interesting; the point is
// that the failure is an error every time.
func TestSelectorGrammarPathologies(t *testing.T) {
	t.Parallel()

	selectors := []string{
		// Runs of dots, and dots where a step is expected.
		`..[.]path`, `.[.]`, `..`, `...`, `....`, `..a`, `.a..b`, `.a.`, `.`,
		`. `, ` .`, `.. .`, `.[.].[.]`, `.[..]`, `.[...]`, `.[.a]`, `.a[.]`,
		`.[]a`, `.[]. `, strings.Repeat(".", 100), strings.Repeat("..", 50),
		"." + strings.Repeat("[.]", 30), "." + strings.Repeat(".a", 50),

		// Brackets: unbalanced, nested, and stacked.
		`.[`, `.]`, `.[]`, `.[][]`, `.[[]]`, `.[[`, `.]]`, `.[]]`, `.[[]`,
		`.[ [ ] ]`, `.[0`, `.0]`, strings.Repeat(".[", 60), strings.Repeat(".]", 60),

		// Quoted keys, including the escape-grammar cases scanStringLiteral
		// exists for (see ParseSelector's note on issue #566).
		`.["a"`, `."a"`, `.[""]`, `.[" "]`, `.["]`, `.[\"]`, `.["\"]`,
		`.["\\"]`, `.["\\\\"]`, `.["\n"]`, `.["\t"]`, `.["\x41"]`, `.["A"]`,
		`.["a"]["b"]["c"]`, `.["a"].["b"]`, `.[".a"]`, `.["[0]"]`, `.["]"]["["]`,
		"." + strings.Repeat(`["a"]`, 50),
		`.["` + strings.Repeat(`\`, 60) + `"]`,
		`.["` + strings.Repeat(`.`, 200) + `"]`,

		// Ranges: too many colons, no colon, signs, and the int extremes.
		`.[:]`, `.[::]`, `.[:::]`, `.[1:2:3]`, `.[-]`, `.[--]`, `.[+1]`, `.[1-]`,
		`.[ : ]`, `.[1 : ]`, `.[: 2]`, `.[1:2`, `.1:2]`, `.[0:0:0]`,
		`.[99999999999999999999]`, `.[-99999999999999999999]`,
		`.[9223372036854775807:-9223372036854775808]`,
		"." + strings.Repeat("[:]", 40),

		// The "?" suffix, which the parser accepts and discards.
		`.a?`, `.a??`, `.?`, `.??`, `.[0]?`, `.[0]??`, `.[]?`, `?`, `.a?.b?`,
		"." + strings.Repeat("?", 100),

		// Whitespace and control bytes, where the scan trims between rounds.
		".\t.a", ".\n", ".\r", ".\x00", ".a\x00b", ". \t\n [0]", "\t.a",

		// Non-ASCII, which the bare-key rule excludes and the quoted form
		// admits.
		`.café`, `.日本`, `.["café"]`, `.á`, ` .a`, `.["日本"]`,

		// Not selectors at all.
		``, `a`, `[0]`, `0`, `-`, `]`,
	}

	for _, sel := range selectors {
		t.Run(fmt.Sprintf("%q", sel), func(t *testing.T) {
			var path Path
			var err error
			mustNotPanic(t, "ParseSelector", func() { path, err = ParseSelector(sel) })

			var stepsErr error
			mustNotPanic(t, "SelectorSteps", func() { _, stepsErr = SelectorSteps(sel) })

			if (err == nil) != (stepsErr == nil) {
				t.Fatalf("ParseSelector and SelectorSteps disagree: %v vs %v", err, stepsErr)
			}
			if err != nil {
				return
			}
			printed := path.String()
			if _, err := ParseSelector(printed); err != nil {
				t.Fatalf("accepted, but its printed form %q does not parse back: %v",
					printed, err)
			}
		})
	}
}
