// Copyright © 2026 The ELPS authors

package libjson

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// Pin the plain encoder's exact escape set, including characters JCS leaves
// literal. Prefixing the rune avoids the separate leading-tilde type escape.
func TestTypedStringEscapesMatchPlain(t *testing.T) {
	escapes := map[rune]string{
		'"': `\"`, '\\': `\\`, '\b': `\b`, '\f': `\f`, '\n': `\n`, '\r': `\r`, '\t': `\t`,
		'<': `\u003c`, '>': `\u003e`, '&': `\u0026`, '\u2028': `\u2028`, '\u2029': `\u2029`,
	}
	runes := []rune{'\u2028', '\u2029', '\u0080', 'é', '\u0301', '世', '\ue000', '\uffff', '😀'}
	for r := rune(0); r <= 0x7f; r++ {
		runes = append(runes, r)
	}
	for _, r := range runes {
		t.Run(fmt.Sprintf("U+%04X", r), func(t *testing.T) {
			written, ok := escapes[r]
			if !ok {
				written = string(r)
				if r < 0x20 {
					written = fmt.Sprintf(`\u%04x`, r)
				}
			}
			for _, prefix := range []string{"x", "x\n"} {
				s := prefix + string(r) + "end"
				want := `"` + strings.ReplaceAll(prefix, "\n", `\n`) + written + `end"`
				plain, err := Dump(lisp.String(s), false)
				require.NoError(t, err)
				require.Equal(t, want, string(plain))
				typed, err := DumpTyped(lisp.String(s))
				require.NoError(t, err)
				require.Equal(t, plain, typed)
				v, err := LoadTyped(plain)
				require.NoError(t, err)
				require.Equal(t, s, v.Str)
				m := tsmap(t, lisp.String(s), lisp.Int(1))
				plain, err = Dump(m, false)
				require.NoError(t, err)
				typed, err = DumpTyped(m)
				require.NoError(t, err)
				require.Equal(t, plain, typed)
				_, err = LoadTyped(plain)
				require.NoError(t, err)
			}
		})
	}
}

func TestTypedRequiresPlainEscapes(t *testing.T) {
	for _, text := range []string{
		"<", ">", "&", "\u2028", "\u2029",
		`\u003C`, `\u003E`, `\u002F`, `\u002f`, `\u0022`, `\u005c`,
		`\u0008`, `\u0009`, `\u000a`, `\u000c`, `\u000d`, `\u001F`,
		`\u007f`, `\u0080`, `\u00e9`, `\ufffd`, `\ud83d\ude00`, `\/`,
		`\u202`, `\u202a`, `\u0027`,
	} {
		// Exercise both the unescaped fast path and the path after an escape,
		// plus map keys, symbol/keyword names and tagged type names.
		for _, prefix := range []string{"", `x\n`} {
			s := prefix + text
			for _, doc := range []string{
				`"` + s + `"`, `{"` + s + `":1}`,
				`"~$` + s + `"`, `"~:` + s + `"`, `"~~` + s + `"`,
				`["~#tagged",["` + s + `",1]]`,
			} {
				t.Run(doc, func(t *testing.T) {
					_, err := LoadTyped([]byte(doc))
					require.Error(t, err)
				})
			}
		}
	}
}

func TestTypedEscapesInNamesAndTags(t *testing.T) {
	name := "x<>&\u2028\u2029é😀"
	escaped := `x\u003c\u003e\u0026\u2028\u2029é😀`
	for _, tc := range []struct {
		v    *lisp.LVal
		want string
	}{
		{lisp.Symbol(name), `"~$` + escaped + `"`},
		{lisp.Symbol(":" + name), `"~:` + escaped + `"`},
		{lisp.String("~" + name), `"~~` + escaped + `"`},
		{tsmap(t, lisp.Symbol(name), lisp.Int(1)), `{"~$` + escaped + `":1}`},
		{tsmap(t, lisp.Symbol(":"+name), lisp.Int(1)), `{"~:` + escaped + `":1}`},
		{ttagged(name, lisp.Int(1)), `["~#tagged",["` + escaped + `",1]]`},
	} {
		b, err := DumpTyped(tc.v)
		require.NoError(t, err)
		require.Equal(t, tc.want, string(b))
		v, err := LoadTyped([]byte(tc.want))
		require.NoError(t, err)
		b, err = DumpTyped(v)
		require.NoError(t, err)
		require.Equal(t, tc.want, string(b))
	}
}

// Sorting uses member text before JSON escaping; a literal '<' precedes 'Z'
// even though its written Unicode escape begins with a backslash.
func TestTypedKeyOrderBeforeJSONEscaping(t *testing.T) {
	m := tsmap(t, lisp.String("Z"), lisp.Int(3), lisp.String("<"), lisp.Int(2),
		lisp.String("&"), lisp.Int(1), lisp.String("\\"), lisp.Int(4),
		lisp.String("a"), lisp.Int(5), lisp.String("\u2028"), lisp.Int(6))
	want := `{"\u0026":1,"\u003c":2,"Z":3,"\\":4,"a":5,"\u2028":6}`
	b, err := DumpTyped(m)
	require.NoError(t, err)
	require.Equal(t, want, string(b))
	plain, err := Dump(m, false)
	require.NoError(t, err)
	require.Equal(t, plain, b)
	_, err = LoadTyped(b)
	require.NoError(t, err)
	_, err = LoadTyped([]byte(`{"Z":3,"\u003c":2}`))
	require.ErrorContains(t, err, "members out of order")
}
