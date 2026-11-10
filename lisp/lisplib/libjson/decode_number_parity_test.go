// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
)

// TestDirectDecoderNumbersMatchIndirect pins the value the direct decoder
// returns for a number, in each number mode, to the value of the original
// two-step decode.  The direct decoder converts a number without a heap
// string and takes a short int without a parser, so each spelling below goes
// through a different branch.
func TestDirectDecoderNumbersMatchIndirect(t *testing.T) {
	numbers := []string{
		"0", "-0", "1", "-1", "9", "10", "-10", "421", "123456789", "999999999999999",
		"1000000000000000", "9007199254740992", "9007199254740993", "-9007199254740993",
		"9223372036854775807", "-9223372036854775808", "9223372036854775808",
		"9223372036854776000", "100000000000000000000", "100000000000000000000000",
		"1.5", "-1.5", "0.0", "-0.0", "1e5", "1E5", "1e+5", "1e-5", "1.25e10",
		"1e308", "1e309", "-1e309", "1e-400", "0.1", "123456789012345678901234567890.5",
	}
	for _, opts := range []libjson.LoadOpts{
		{},
		{StringNumbers: true},
		{ExactIntegers: true},
		{StringNumbers: true, ExactIntegers: true},
	} {
		for _, n := range numbers {
			for _, doc := range []string{n, "[" + n + "," + n + "]", `{"a":` + n + "}", " " + n + " "} {
				want := libjson.LoadIndirectForTest([]byte(doc), opts)
				got := libjson.LoadWith([]byte(doc), opts)
				if !assert.Equal(t, want.Type, got.Type, "%s %+v: %v, want %v", doc, opts, got, want) {
					continue
				}
				if want.Type == lisp.LError {
					assert.Equal(t, want.Str, got.Str, "%s %+v", doc, opts)
					assert.Equal(t, lisp.GoError(want).Error(), lisp.GoError(got).Error(), "%s %+v", doc, opts)
					continue
				}
				// String renders an int and a float of one value alike, so
				// compare the types of the leaves too.
				assert.Equal(t, want.String(), got.String(), "%s %+v", doc, opts)
				assert.Equal(t, leafTypes(want), leafTypes(got), "%s %+v", doc, opts)
			}
		}
	}
}

// leafTypes lists the types of the numbers in a decoded document.
func leafTypes(v *lisp.LVal) []lisp.LType {
	switch v.Type {
	case lisp.LArray:
		var out []lisp.LType
		for _, c := range v.Cells[1].Cells {
			out = append(out, leafTypes(c)...)
		}
		return out
	case lisp.LSortMap:
		var out []lisp.LType
		for _, p := range v.MapEntries().Cells {
			out = append(out, leafTypes(p.Cells[1])...)
		}
		return out
	default:
		return []lisp.LType{v.Type}
	}
}
