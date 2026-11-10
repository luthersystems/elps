// Copyright © 2024 The ELPS authors

package parser

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

// TestSignedRadixLiteralRefused pins luthersystems/elps#943. A sign joined to
// a #x or #o literal used to read as the symbol - (or +) and then the
// positive integer, with no error. The reader refuses the joined form and
// names it. The forms with a delimiter keep their meaning.
func TestSignedRadixLiteralRefused(t *testing.T) {
	for _, opts := range [][]ReaderOption{nil, {WithFormatPreserving()}} {
		// A literal that is already malformed keeps its own diagnostic.
		for _, tc := range [][2]string{
			{"-#x10:foo", `invalid-hex-literal: invalid numeric literal "#x10:foo" (separate values with whitespace)`},
			{"-#xZ", `invalid-hex-literal: invalid hexadecimal literal character: 'Z'`},
			{"+#o8", `invalid-octal-literal: invalid octal literal character: '8'`},
			{"#x-10", `invalid-hex-literal: invalid hexadecimal literal character: '-'`},
			{"#o+10", `invalid-octal-literal: invalid octal literal character: '+'`},
		} {
			t.Run(tc[0], func(t *testing.T) {
				_, err := NewReader(opts...).Read("number.lisp", strings.NewReader(tc[0]))
				require.ErrorContains(t, err, tc[1])
			})
		}
		for _, literal := range []string{
			"-#x10", "+#x10", "-#X10", "+#X10", "-#o17", "+#o17", "-#O17", "+#O17",
			"--#x10", "-#xFF",
		} {
			t.Run(literal, func(t *testing.T) {
				hash := strings.Index(literal, "#")
				joined := literal[hash-1:]
				for _, src := range []string{literal, "(list " + literal + ")", "'(" + literal + ")"} {
					_, err := NewReader(opts...).Read("number.lisp", strings.NewReader(src))
					require.Error(t, err, src)
					require.ErrorContains(t, err, "invalid numeric literal \""+joined+"\"", src)
					require.ErrorContains(t, err, "a sign cannot be joined to a "+joined[1:3]+" literal", src)
				}
			})
		}
		for _, tc := range [][2]string{
			{"(- #x10)", "(- 16)"},
			{"(+ #x10)", "(+ 16)"},
			{"(- #o17)", "(- 15)"},
			{"'(-\n#x10)", "'(- 16)"},
			{"'(- ; c\n#x10)", "'(- 16)"},
			{"'(a-#x10)", "'(a- 16)"},
			{"'(a#x10)", "'(a 16)"},
			{"'(+-#x10)", "'(+- 16)"},
			{"'(-+#x10)", "'(-+ 16)"},
			{"'(a:+#x10)", "'(a:+ 16)"},
			{"'(-1 #x10 -a + #x10)", "'(-1 16 -a + 16)"},
			{"'(-#'f)", "'(- (lisp:function f))"},
			{"'(+#'f)", "'(+ (lisp:function f))"},
		} {
			t.Run(tc[0], func(t *testing.T) {
				exprs, err := NewReader(opts...).Read("number.lisp", strings.NewReader(tc[0]))
				require.NoError(t, err)
				require.Len(t, exprs, 1)
				require.Equal(t, tc[1], exprs[0].String())
			})
		}
	}
}
