// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"
)

// The three selector regexps, pinned at the level parseArray, parseArrayKey
// and parseDotKey actually read them: the CAPTURE GROUP INDICES.
//
// Those indices are load-bearing and invisible at the call site. parseArray
// reads match[1] as the from, match[3] as the to and never reads match[2] at
// all -- match[2] is the whole ":to" clause and exists only because the two
// range bounds have to be optional together. Inserting a group anywhere in
// reArray silently renumbers the two that are read, and the failure is not a
// parse error: "[0:1]" would still parse, just as some other path. The array
// tests below assert the group COUNT (5) as well as the values for exactly
// that reason.
//
// The one case worth reading twice is "iterator": "[]" and "[:]" produce the
// same three captures, and parseArray separates them by looking for ":" in
// match[0], not in any group. That is why the raw match cannot be dropped.
//
// Ported with the parser from luthersystems/substrate (issue #564).

// TestArrRegexp tests the array regular expression.
func TestArrRegexp(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name   string
		In     string
		From   string
		To     string
		Option string
	}{
		{
			Name: "simple",
			In:   `[0]`,
			From: "0",
		},
		{
			Name:   "simple opt",
			In:     `[0]?`,
			From:   "0",
			Option: "?",
		},
		{
			Name: "simple neg",
			In:   `[-1]`,
			From: "-1",
		},
		{
			Name: "range",
			In:   `[0:1]`,
			From: "0",
			To:   "1",
		},
		{
			Name: "implicit start",
			In:   `[:1]`,
			From: "",
			To:   "1",
		},
		{
			Name: "all implicit",
			In:   `[:]`,
			From: "",
			To:   "",
		},
		{
			Name: "implicit end",
			In:   `[1:]`,
			From: "1",
			To:   "",
		},
		{
			Name: "space after",
			In:   `[0:1] `,
			From: "0",
			To:   "1",
		},
		{
			Name: "space from",
			In:   `[ 0 :1] `,
			From: "0",
			To:   "1",
		},
		{
			Name: "space to",
			In:   `[0: 1 ] `,
			From: "0",
			To:   "1",
		},
		{
			Name: "iterator",
			In:   `[]`,
			From: "",
			To:   "",
		},
		{
			// -0 is not 0 to strconv.Atoi's caller by inspection, but it
			// is by value, and the corpus depends on it: ".[-0]" is the
			// FIRST element, not the last. The regexp has to hand the
			// sign through rather than reject it.
			Name: "neg zero",
			In:   `[-0]`,
			From: "-0",
		},
		{
			Name: "range both negative",
			In:   `[-2:-1]`,
			From: "-2",
			To:   "-1",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			matches := reArray.FindAllStringSubmatch(tc.In, -1)
			if len(matches) != 1 {
				t.Fatalf("len(matches) == %d != 1", len(matches))
			}
			if len(matches[0]) != 5 {
				t.Fatalf("len(mmatches[0]) == %d != 5", len(matches[0]))
			}
			if matches[0][1] != tc.From {
				t.Fatalf("len(mmatches[0][1]) == %s != %s", matches[0][1], tc.From)
			}
			if matches[0][3] != tc.To {
				t.Fatalf("len(mmatches[0][3]) == %s != %s", matches[0][3], tc.To)
			}
			if matches[0][4] != tc.Option {
				t.Fatalf("len(mmatches[0][4]) == %s != %s", matches[0][4], tc.Option)
			}
		})
	}
}

// TestArrKeyRegexp tests the key access using array notation.
func TestArrKeyRegexp(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name   string
		In     string
		Key    string
		Option string
	}{
		{
			Name: "simple",
			In:   `["0"]`,
			Key:  `"0"`,
		},
		{
			Name: "simple space",
			In:   `[  "a1"  ]`,
			Key:  `"a1"`,
		},
		{
			Name:   "simple opt",
			In:     `["0"]?`,
			Key:    `"0"`,
			Option: "?",
		},
		{
			Name: "empty key",
			In:   `[""]`,
			Key:  `""`,
		},
		{
			// The capture keeps the Go escapes intact for
			// strconv.Unquote, which is the inverse of the %q
			// dotPath.String() renders a key with. The greedy body
			// (?:\"|[^"])* matches the interior quote and backtracks to
			// the final one.
			Name: "escaped quote and newline",
			In:   `["\"\n"]`,
			Key:  `"\"\n"`,
		},
		{
			Name: "key containing a bracket",
			In:   `["a]b"]`,
			Key:  `"a]b"`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			matches := reArrayKey.FindAllStringSubmatch(tc.In, -1)
			if len(matches) != 1 {
				t.Fatalf("len(matches) == %d != 1", len(matches))
			}
			if len(matches[0]) != 3 {
				t.Fatalf("len(mmatches[0]) == %d != 3", len(matches[0]))
			}
			if matches[0][1] != tc.Key {
				t.Fatalf("len(mmatches[0][1]) == %s != %s", matches[0][1], tc.Key)
			}
			if matches[0][2] != tc.Option {
				t.Fatalf("len(mmatches[0][2]) == %s != %s", matches[0][2], tc.Option)
			}
		})
	}
}

// TestDotKeyRegexp tests key access with dot notation.
func TestDotKeyRegexp(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name   string
		In     string
		Key    string
		Option string
	}{
		{
			Name: "simple",
			In:   `.wut`,
			Key:  "wut",
		},
		{
			Name: "simple space",
			In:   `.  wut`,
			Key:  "wut",
		},
		{
			Name:   "simple opt",
			In:     `.wut?`,
			Key:    "wut",
			Option: "?",
		},
		{
			Name: "leading underscore",
			In:   `._private`,
			Key:  "_private",
		},
		{
			// The identifier rule stops at the next selector's dot, so a
			// chain does not collapse into one key.
			Name: "stops at the next dot",
			In:   `.a.b`,
			Key:  "a",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			matches := reDotKey.FindAllStringSubmatch(tc.In, -1)
			if len(matches) != 1 {
				t.Fatalf("len(matches) == %d != 1", len(matches))
			}
			if len(matches[0]) != 3 {
				t.Fatalf("len(mmatches[0]) == %d != 3", len(matches[0]))
			}
			if matches[0][1] != tc.Key {
				t.Fatalf("len(mmatches[0][1]) == %s != %s", matches[0][1], tc.Key)
			}
			if matches[0][2] != tc.Option {
				t.Fatalf("len(mmatches[0][2]) == %s != %s", matches[0][2], tc.Option)
			}
		})
	}
}

// TestSelectorRegexpsDoNotOverlap pins the assumption that makes the fixed
// order of `parsers` safe.
//
// parseArray runs before parseArrayKey, and both anchor on "[". If reArray
// could match the head of a quoted-key selector, the key would never be
// reached and `.["0"]` would parse as something else -- or, worse, as
// nothing, since a round that consumes no input aborts the parse. It cannot,
// because every one of reArray's groups is optional but the closing "]" is
// not, and a quoted key does not present one until after the string.
//
// The converse is asserted too: neither bracket regexp may match a bare dot
// key, which is what lets parseDotKey run last.
func TestSelectorRegexpsDoNotOverlap(t *testing.T) {
	t.Parallel()
	arrayMustNotMatch := []string{`["0"]`, `["a1"]`, `[""]`, `.foo`, `["a]b"]`}
	for _, in := range arrayMustNotMatch {
		if reArray.MatchString(in) {
			t.Errorf("reArray must not match %q -- parseArray runs first and would consume it", in)
		}
	}
	keyMustNotMatch := []string{`[0]`, `[]`, `[1:3]`, `[:]`, `.foo`}
	for _, in := range keyMustNotMatch {
		if reArrayKey.MatchString(in) {
			t.Errorf("reArrayKey must not match %q", in)
		}
	}
	dotMustNotMatch := []string{`[0]`, `["a"]`, `.0`, `.$private`, `.`}
	for _, in := range dotMustNotMatch {
		if reDotKey.MatchString(in) {
			t.Errorf("reDotKey must not match %q", in)
		}
	}
}
