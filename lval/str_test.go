// Copyright © 2026 The ELPS authors

package lval

import (
	"fmt"
	"strconv"
	"testing"
	"unicode/utf16"
	"unicode/utf8"
)

// strByReencode is how str checked a string before luthersystems/elps#987:
// decode it leniently, write it again with appendString and compare the
// bytes.  It is slow, but plainly the rule, so the one-pass check in str
// is tested against it.
func strByReencode(d *decoder) (string, error) {
	start := d.i
	d.i++ // "
	var out []byte
	for {
		if d.i >= len(d.b) {
			return "", d.errorf("unterminated string")
		}
		c := d.b[d.i]
		switch c {
		case '"':
			d.i++
			enc, err := appendString(nil, string(out))
			if err != nil {
				return "", d.errorf("string is not valid UTF-8")
			}
			if string(enc) != string(d.b[start:d.i]) {
				return "", d.errorf("non-canonical string")
			}
			return string(out), nil
		case '\\':
			if d.i+1 >= len(d.b) {
				return "", d.errorf("bad escape")
			}
			e := d.b[d.i+1]
			d.i += 2
			switch e {
			case '"', '\\', '/':
				out = append(out, e)
			case 'b':
				out = append(out, '\b')
			case 'f':
				out = append(out, '\f')
			case 'n':
				out = append(out, '\n')
			case 'r':
				out = append(out, '\r')
			case 't':
				out = append(out, '\t')
			case 'u':
				r, ok := d.hex4()
				if !ok {
					return "", d.errorf("bad \\u escape")
				}
				if utf16.IsSurrogate(r) {
					if !d.lit(`\u`) {
						return "", d.errorf("lone surrogate")
					}
					r2, ok := d.hex4()
					if !ok {
						return "", d.errorf("bad \\u escape")
					}
					r = utf16.DecodeRune(r, r2)
				}
				out = utf8.AppendRune(out, r)
			default:
				return "", d.errorf("bad escape")
			}
		default:
			out = append(out, c)
			d.i++
		}
	}
}

// strSeeds covers each rule str checks: every escape appendString writes
// and the spellings it does not, raw characters it escapes, surrogates,
// and invalid UTF-8 next to other faults.
var strSeeds = []string{
	`""`, `"abc"`, `"~$x"`, jsonQuote("h" + runeText(0xe9) + "llo " + runeText(0x2603) + " " + runeText(0x1d11e)),
	`"\""`, `"\\"`, `"\/"`, `"/"`, `"\n\r\t\b\f"`,
	jsonQuote(uEsc("000a")), jsonQuote(uEsc("0008")), jsonQuote(uEsc("0000")), jsonQuote(uEsc("001f")), jsonQuote(uEsc("001F")), jsonQuote(uEsc("0041")),
	jsonQuote(uEsc("003c") + uEsc("003e") + uEsc("0026")), jsonQuote(uEsc("003C")), `"<"`, `">"`, `"&"`,
	jsonQuote(runeText(0x2028)), jsonQuote(runeText(0x2029)), jsonQuote(uEsc("2028") + uEsc("2029")), jsonQuote(uEsc("202a")), jsonQuote(uEsc("00e9")),
	jsonQuote(uEsc("d834") + uEsc("dd1e")), jsonQuote(uEsc("D834") + uEsc("DD1E")), jsonQuote(uEsc("d834")), jsonQuote(uEsc("d834") + "x"),
	jsonQuote(uEsc("d834") + uEsc("0041")), jsonQuote(uEsc("dd1e") + uEsc("d834")),
	"\"\x01\"", "\"\x7f\"", "\"a\x00b\"",
	"\"\xff\"", "\"\xc3\"", "\"\xe2\x80\"", "\"\xff\\/\"", "\"\xff<\"", "\"\xed\xa0\x80\"",
	`"\x"`, jsonQuote(uEsc("12")), jsonQuote(uEsc("zzzz")), `"abc`, `"\`, `"a\"`, jsonQuote(uEsc("d834") + uEsc("12")),
	`"a\nb<c"`, jsonQuote(runeText(0xfffd)), jsonQuote(uEsc("fffd")),
}

// uEsc returns the JSON escape of four hex digits, built at run time so
// that no tool on the way to this file reads it as an escape of its own.
func uEsc(hex string) string { return "\\" + "u" + hex }

// runeText returns the rune as text, so that the source holds no raw non-ASCII
// character (gosec G116 refuses bidirectional and separator runes).
func runeText(c rune) string { return string(c) }

// jsonQuote quotes s as a JSON string with no escaping of its own.
func jsonQuote(s string) string { return `"` + s + `"` }

func checkStrMatchesReencode(t *testing.T, in []byte) {
	got := decoder{b: in}
	want := decoder{b: in}
	gs, gerr := got.str()
	ws, werr := strByReencode(&want)
	if (gerr == nil) != (werr == nil) || gs != ws || got.i != want.i ||
		(gerr != nil && gerr.Error() != werr.Error()) {
		t.Fatalf("str(%q) = %q, %v (at %d)\nre-encode check: %q, %v (at %d)",
			in, gs, gerr, got.i, ws, werr, want.i)
	}
}

// TestStrMatchesReencode: the one-pass check in str accepts, refuses and
// reads each string as writing it again and comparing did.
func TestStrMatchesReencode(t *testing.T) {
	for _, s := range strSeeds {
		checkStrMatchesReencode(t, []byte(s))
	}
	// Every rune escaped in every way the reader accepts.
	for r := rune(0); r < 0x3000; r++ {
		if utf16.IsSurrogate(r) {
			continue
		}
		for _, esc := range []string{"\\u%04x", "\\u%04X"} {
			checkStrMatchesReencode(t, []byte(`"`+fmt.Sprintf(esc, r)+`"`))
		}
		checkStrMatchesReencode(t, utf8.AppendRune([]byte(`"`), r)) // unterminated
		checkStrMatchesReencode(t, append(utf8.AppendRune([]byte(`"`), r), '"'))
	}
}

// TestParseIntMatchesStrconv: parseInt's fast path agrees with the
// strconv round trip it skips.
func TestParseIntMatchesStrconv(t *testing.T) {
	for _, s := range intSeeds {
		checkParseIntMatchesStrconv(t, s)
	}
}

func checkParseIntMatchesStrconv(t *testing.T, s string) {
	n, form := parseInt(s)
	wn, wform := parseIntByStrconv(s)
	if n != wn || form != wform {
		t.Fatalf("parseInt(%q) = %d, %d; strconv says %d, %d", s, n, form, wn, wform)
	}
}

var intSeeds = []string{
	"0", "-0", "00", "01", "-01", "+1", "1", "-1", "", "-", "1-", "1a",
	"999999999999999999", "-999999999999999999", "1000000000000000000",
	"9223372036854775807", "-9223372036854775808", "9223372036854775808",
	"0009", "12345", "-12345", "1_000", " 1",
}

// parseIntByStrconv is parseInt without its fast path.
func parseIntByStrconv(text string) (int64, intText) {
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		return 0, notInt
	}
	if strconv.FormatInt(n, 10) != text {
		return n, nonCanonicalInt
	}
	return n, canonicalInt
}
