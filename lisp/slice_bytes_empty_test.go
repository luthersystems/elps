// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestSliceToBytesEmptyStaysNil pins the distinction libjson's encoder draws
// between a nil byte slice and an empty non-nil one: a nil []byte serializes
// as JSON null and an empty non-nil one as "", a documented v1.13.0
// compatibility contract (encodeBytes in lisp/lisplib/libjson/encode.go).
//
// `slice` coercing a list to bytes must therefore keep producing nil when the
// window is empty.  Pre-sizing the destination with make([]byte, 0, n) turns
// every empty window into a non-nil zero-length slice, which silently rewrote
// (json:dump-string (slice 'bytes '() 0 0)) from "null" to "\"\"" -- and did
// the same for an empty window taken out of a non-empty input, out of a
// vector, for those values nested inside maps and lists, and for
// json:dump-bytes.
//
// The environment here loads the standard library, which the TestSuite runner
// does not, because the contract this pins lives in libjson.
func TestSliceToBytesEmptyStaysNil(t *testing.T) {
	tests := []struct {
		expr string
		want string
	}{
		// Every empty list-to-bytes coercion encodes as JSON null.
		{`(json:dump-string (slice 'bytes '() 0 0))`, `"null"`},
		{`(json:dump-string (slice 'bytes (vector) 0 0))`, `"null"`},
		// An empty window taken out of a non-empty input: the cell count
		// of the sliced view is what must drive the decision, not the
		// cell count of the argument.
		{`(json:dump-string (slice 'bytes '(1 2 3) 1 1))`, `"null"`},
		{`(json:dump-string (slice 'bytes (vector 1 2) 2 2))`, `"null"`},
		// Nested, where the encoder reaches the value through a container
		// rather than at the top level.
		{`(json:dump-string (sorted-map "b" (slice 'bytes '() 0 0)))`, `"{\"b\":null}"`},
		{`(json:dump-string (list (slice 'bytes '(7) 0 0)))`, `"[null]"`},
		// json:dump-bytes takes the same encoder path.
		{`(to-string (json:dump-bytes (slice 'bytes '() 0 0)))`, `"null"`},

		// Non-empty coercions are unchanged.
		{`(json:dump-string (slice 'bytes '(104 105) 0 2))`, `"\"aGk=\""`},
		{`(json:dump-string (slice 'string '(104 105) 0 2))`, `"\"hi\""`},

		// The empty coercions themselves still behave.  The 'string arm is
		// kept symmetric with 'bytes even though string(nil) and
		// string([]byte{}) are both "".
		{`(slice 'string '() 0 0)`, `""`},
		{`(slice 'bytes '() 0 0)`, `#<bytes>`},
		{`(length (slice 'bytes '() 0 0))`, `0`},
		{`(to-string (slice 'bytes '() 0 0))`, `""`},
		{`(append 'bytes (slice 'bytes '() 0 0) 1)`, `#<bytes 1>`},
		// A bytes input with an empty window never went through the list
		// path and was never affected; it is here so the two stay
		// comparable.
		{`(json:dump-string (slice 'bytes "" 0 0))`, `"\"\""`},
	}
	env := newLimitTestEnv(t)
	for _, test := range tests {
		t.Run(test.expr, func(t *testing.T) {
			res := env.LoadString("test", test.expr)
			require.NotEqual(t, lisp.LError, res.Type, "%v", res)
			assert.Equal(t, test.want, res.String())
		})
	}
}
