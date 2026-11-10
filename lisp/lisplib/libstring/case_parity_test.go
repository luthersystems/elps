// Copyright © 2026 The ELPS authors

package libstring

import (
	"math/rand"
	"strings"
	"testing"
	"unicode"

	"github.com/luthersystems/elps/lisp"
)

// caseOracle is string:lowercase or string:uppercase as it was before the
// ASCII path: the Unicode conversion alone.
func caseOracle(env *lisp.LEnv, s string, upper bool) *lisp.LVal {
	if upper {
		return convertCase(env, s, unicode.ToUpper)
	}
	return convertCase(env, s, unicode.ToLower)
}

func caseBuiltin(env *lisp.LEnv, s string, upper bool) *lisp.LVal {
	args := lisp.QExpr([]*lisp.LVal{lisp.String(s)})
	if upper {
		return builtinUpper(env, args)
	}
	return builtinLower(env, args)
}

// TestConvertCaseASCIIMatchesConvertCase pins that string:lowercase and
// string:uppercase return, for every input, what the Unicode conversion
// returns: the same bytes, or the same error with the same text.  The ASCII
// path must be invisible.  The inputs are fixed special cases and generated
// strings: ASCII only, ASCII with one non-ASCII rune, invalid UTF-8 and
// arbitrary bytes, each under a large and a small allocation limit.
func TestConvertCaseASCIIMatchesConvertCase(t *testing.T) {
	fixed := []string{
		"", "a", "A", "z", "Z", "@", "[", "`", "{", "\x00", "\x7f",
		"abc", "ABC", "aBc", "123", "hello, WORLD 42!", "already lower", "ALREADY UPPER",
		// Letters whose mapping changes length or leaves ASCII.
		"ß", "ẞ", "İ", "ı", "I", "i", "ſ", "K", "Å", "ǅ", "ǆ", "Ǆ",
		"ΌΣΟΣ", "όσος", "ς", "Σ", "ﬁ", "straße", "STRASSE",
		"Ⱥ", "ⱥ", "ɐ", "Ɐ",
		// ASCII around one non-ASCII rune.
		"abcé", "éABC", "ABCİdef", "abc DEF",
		// Invalid UTF-8.
		"\xff", "a\xffB", "\xc3", "ab\xc3", "\xed\xa0\x80", "A\xf0\x9f\x98", "\x80abc", "ABC\x80",
		string([]byte{0xef, 0xbf, 0xbd}), "a�B",
	}
	limits := []int{0, 1, 2, 3, 4, 7, 8, 16, 64}
	check := func(s string) {
		t.Helper()
		for _, limit := range limits {
			for _, upper := range []bool{false, true} {
				env := lisp.NewEnv(nil)
				env.Runtime.MaxAlloc = limit
				want := caseOracle(env, s, upper)
				got := caseBuiltin(env, s, upper)
				if got.Type != want.Type {
					t.Fatalf("upper=%v limit=%d %q: got %v, want %v", upper, limit, s, got, want)
				}
				if want.Type == lisp.LError {
					if g, w := lisp.GoError(got).Error(), lisp.GoError(want).Error(); g != w {
						t.Fatalf("upper=%v limit=%d %q: error %q, want %q", upper, limit, s, g, w)
					}
					continue
				}
				if got.Str != want.Str {
					t.Fatalf("upper=%v limit=%d %q: got %q, want %q", upper, limit, s, got.Str, want.Str)
				}
			}
		}
	}
	for _, s := range fixed {
		check(s)
		check(s + s)
		check("ascii " + s + " ASCII")
	}
	// Every ASCII byte alone and between letters.
	for c := range 128 {
		check(string(rune(c)))
		check("aZ" + string(rune(c)) + "Az")
	}
	r := rand.New(rand.NewSource(1)) //nolint:gosec // G404: a fixed seed makes the test repeatable
	runes := []rune("aAzZ09 _-éÉßİıσςΣ �😀")
	for range 20000 {
		n := r.Intn(40)
		var sb strings.Builder
		switch r.Intn(4) {
		case 0: // ASCII only
			for range n {
				sb.WriteByte(byte(r.Intn(128))) //nolint:gosec // G115: 0 to 127
			}
		case 1: // ASCII letters with one rune from the list
			at := r.Intn(n + 1)
			for i := range n + 1 {
				if i == at {
					sb.WriteRune(runes[r.Intn(len(runes))])
				} else {
					sb.WriteByte("abcXYZ 09"[r.Intn(9)])
				}
			}
		case 2: // runes from the list
			for range n {
				sb.WriteRune(runes[r.Intn(len(runes))])
			}
		default: // arbitrary bytes, mostly invalid UTF-8
			for range n {
				sb.WriteByte(byte(r.Intn(256))) //nolint:gosec // G115: 0 to 255
			}
		}
		check(sb.String())
	}
}

// TestConvertCaseKeepsAnUnchangedString pins that a string with no letter to
// change comes back as the same string, even past the allocation limit: the
// conversion allocates nothing for it.
func TestConvertCaseKeepsAnUnchangedString(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.MaxAlloc = 4
	for _, s := range []string{"already lower 123", "déjà vu, already lower"} {
		got := caseBuiltin(env, s, false)
		if got.Type != lisp.LString || got.Str != s {
			t.Fatalf("lowercase(%q) = %v", s, got)
		}
	}
	got := caseBuiltin(env, "Not lower", false)
	if got.Type != lisp.LError {
		t.Fatalf("lowercase past the limit = %v, want an error", got)
	}
}
