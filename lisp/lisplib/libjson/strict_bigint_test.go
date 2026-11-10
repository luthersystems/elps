// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestStrictLoadIntegerTextPastInt64 pins that :strict true judges integer
// text that does not fit an int by the float it loads as
// (luthersystems/elps#943).
//
// Strict load accepts only the bytes the plain encoder writes.  Integer text
// past int64 loads as a float, so it is canonical only when it is the text
// the encoder writes for that float.  The encoder writes a float below 1e21
// as plain digits, in the shortest form that reads back as the same float,
// and a larger one with an exponent.  So
// "100000000000000000000" (1e20) is canonical and
// "100000000000000000000000" (1e23, written "1e+23") is not.
func TestStrictLoadIntegerTextPastInt64(t *testing.T) {
	for _, tc := range []struct {
		text string
		ok   bool
	}{
		{"9223372036854775807", true},
		{"-9223372036854775808", true},
		{"9223372036854776000", true},    // dump writes this for 2^63
		{"9223372036854775808", false},   // 2^63 exactly: not what dump writes
		{"100000000000000000000", true},  // 1e20
		{"-100000000000000000000", true}, // -1e20
		{"9223372036854775809", false},
		{"-9223372036854775809", false},
		{"100000000000000000000000", false}, // dump writes 1e+23
		{"-100000000000000000000000", false},
		{"1000000000000000000000", false}, // 1e21, dump writes 1e+21
		{"1e+23", true},
	} {
		for _, opts := range []libjson.LoadOpts{
			{Strict: true},
			{Strict: true, StringNumbers: true},
			{Strict: true, ExactIntegers: true},
		} {
			for _, doc := range []string{tc.text, "[" + tc.text + "]", `{"a":` + tc.text + "}"} {
				got := libjson.LoadWith([]byte(doc), opts)
				if !tc.ok {
					if assert.Equal(t, lisp.LError, got.Type, "%s %+v: %v", doc, opts, got) && opts.ExactIntegers {
						// Exact integers keep their own error condition.
						assert.Equal(t, "json:integer-range-error", got.Str, "%s", doc)
					}
					continue
				}
				if !assert.NotEqual(t, lisp.LError, got.Type, "%s %+v: %v", doc, opts, got) {
					continue
				}
				if opts.ExactIntegers {
					// With exact integers, dump writes back what strict
					// load accepts.  Without them an int loads as a float.
					out, err := libjson.Dump(got, false)
					require.NoError(t, err)
					assert.Equal(t, doc, string(out), "%+v", opts)
				}
			}
		}
	}
}

// TestNonStrictLoadIntegerTextPastInt64 pins that the default load still
// takes integer text of any size as a float.
func TestNonStrictLoadIntegerTextPastInt64(t *testing.T) {
	got := libjson.LoadWith([]byte("100000000000000000000000"), libjson.LoadOpts{})
	require.Equal(t, lisp.LFloat, got.Type, "%v", got)
	assert.InDelta(t, 1e23, got.Float, 0)
	got = libjson.LoadWith([]byte("9223372036854775809"), libjson.LoadOpts{})
	require.Equal(t, lisp.LFloat, got.Type, "%v", got)
}
