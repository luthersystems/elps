// Copyright © 2018 The ELPS authors

package rdparser_test

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

func parseOne(t *testing.T, src string) (*lisp.LVal, error) {
	t.Helper()
	exprs, err := rdparser.New(token.NewScanner("test", bytes.NewReader([]byte(src)))).ParseProgram()
	if err != nil {
		return nil, err
	}
	require.Len(t, exprs, 1, "expected exactly one expression from %q", src)
	return exprs[0], nil
}

// TestFunRefOperandMustReadBackAsSymbol pins the #' round-trip contract.
//
// #' is shorthand: the reader desugars "#'f" to (lisp:function f) and the
// printer writes it back in that longhand. So the operand has to be a name the
// reader gives back as a symbol -- otherwise printing changes the program.
//
// "#'0" was rejected earlier by requiring a symbol-START rune, but '-' and '+'
// ARE symbol-start runes, so the sign-prefixed forms sailed past: "#'-1"
// produced a symbol named "-1" whose longhand "(lisp:function -1)" reads back
// as an INT. `elps fmt` silently rewrote a function reference as a number.
// Found by FuzzFormatCompact on "#'-0".
func TestFunRefOperandMustReadBackAsSymbol(t *testing.T) {
	t.Parallel()

	// Names that do not survive the round trip must be refused outright.
	// "-1abc" is the one that matters most: it is not a number by
	// strconv.ParseInt or ParseFloat, so any check short of re-reading it
	// lets it through -- and it still comes back as an INT.
	for _, src := range []string{"#'-0", "#'-1", "#'1.5", "#'-1abc", "#'-1e3", "#'0"} {
		t.Run("rejected/"+src, func(t *testing.T) {
			t.Parallel()
			_, err := parseOne(t, src)
			assert.Error(t, err, "%q must be refused: its longhand does not read back as a symbol", src)
		})
	}

	// ...and every name that DOES survive must still be accepted. "--" and
	// "-a" are the guard against over-tightening: they lex as NEGATIVE and
	// only become symbols once ParseNegative merges them, so a token-level
	// check rejects them wrongly.
	for _, src := range []string{"#'-", "#'+", "#'--", "#'-a", "#'+0", "#'x", "#'a:b", "#'*foo*", "#'<=", "#'set!", "#'->"} {
		t.Run("accepted/"+src, func(t *testing.T) {
			t.Parallel()
			expr, err := parseOne(t, src)
			require.NoError(t, err, "%q is a valid function reference", src)
			require.Len(t, expr.Cells, 2)
			inner := expr.Cells[1]
			require.Equal(t, lisp.LSymbol, inner.Type)

			// The actual contract: re-reading the printed longhand yields the
			// same symbol, not a number.
			rt, err := parseOne(t, expr.String())
			require.NoError(t, err, "longhand %q must re-parse", expr.String())
			require.Len(t, rt.Cells, 2)
			assert.Equal(t, inner.Type, rt.Cells[1].Type,
				"round trip changed the operand type for %q (longhand %q)", src, expr.String())
			assert.Equal(t, inner.Str, rt.Cells[1].Str,
				"round trip changed the operand name for %q", src)
		})
	}
}
