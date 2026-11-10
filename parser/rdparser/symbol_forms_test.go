// Copyright © 2024 The ELPS authors

package rdparser_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// TestSymbolColonForms pins the result of ParseSymbol for every position a
// ':' can take in a symbol token. ParseSymbol has a path for a token with no
// ':' that does not split it (luthersystems/elps#944). The reader runs in
// chaincode, so the value, the source location and the error text of each
// form must stay the same.
func TestSymbolColonForms(t *testing.T) {
	type want struct {
		str string // the printed symbol, or "" for an error
		err string // the complete error text
	}
	for src, w := range map[string]want{
		"a":      {str: "a"},
		"abc-d?": {str: "abc-d?"},
		"+":      {str: "+"},
		"-":      {str: "-"},
		"-a":     {str: "-a"},
		"--":     {str: "--"},
		"a:b":    {str: "a:b"},
		"a:+1":   {str: "a:+1"},
		"-a:b":   {str: "-a:b"},
		"a:-":    {str: "a:-"},
		"--:f":   {str: "--:f"},
		":kw":    {str: ":kw"},
		":1":     {str: ":1"},
		":-":     {str: ":-"},
		"-:kw":   {str: "-:kw"},
		":":      {err: `test:2:3: invalid-symbol: invalid symbol ":"`},
		"a:":     {err: `test:2:3: invalid-symbol: invalid symbol "a:"`},
		"-a:":    {err: `test:2:3: invalid-symbol: invalid symbol "-a:"`},
		"::":     {err: `test:2:3: invalid-symbol: invalid symbol "::"`},
		"a::":    {err: `test:2:3: invalid-symbol: invalid symbol "a::"`},
		"::a":    {err: `test:2:3: invalid-symbol: invalid symbol "::a"`},
		"a::b":   {err: `test:2:3: invalid-symbol: invalid symbol "a::b"`},
		"a:b:c":  {err: `test:2:3: invalid-symbol: invalid symbol "a:b:c"`},
		"a:b:":   {err: `test:2:3: invalid-symbol: invalid symbol "a:b:"`},
		":a:b":   {err: `test:2:3: invalid-symbol: invalid symbol ":a:b"`},
		"a:1":    {err: `test:2:3: invalid-symbol: invalid symbol "a:1": "1" does not read back as a symbol`},
		"a:-1":   {err: `test:2:3: invalid-symbol: invalid symbol "a:-1": "-1" does not read back as a symbol`},
	} {
		t.Run(src, func(t *testing.T) {
			for _, formatting := range []bool{false, true} {
				// The symbol is on the second line, so its location is not
				// the start of the input.
				s := token.NewScanner("test", strings.NewReader("\n ("+src+" z)"))
				var p *rdparser.Parser
				if formatting {
					p = rdparser.NewFormatting(s)
				} else {
					p = rdparser.New(s)
				}
				exprs, err := p.ParseProgram()
				if w.err != "" {
					require.Error(t, err)
					require.Equal(t, w.err, err.Error())
					continue
				}
				require.NoError(t, err)
				require.Len(t, exprs, 1)
				list := exprs[0]
				require.Len(t, list.Cells, 2)
				sym := list.Cells[0]
				require.Equal(t, lisp.LSymbol, sym.Type)
				require.Equal(t, w.str, sym.Str)
				loc, ok := sym.Source()
				require.True(t, ok)
				require.Equal(t, 2, loc.Line)
				require.Equal(t, 3, loc.Col)
				require.Equal(t, 3, loc.Pos)
			}
		})
	}
}
