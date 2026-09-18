// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// MaxAlloc caps the data a program may build. It is not a diagnostic budget,
// and an embedder who sets it to cap COLLECTION SIZES (its documented second
// meaning, "bytes or elements") has not asked for error messages of that many
// bytes. With a small one every message came back as the bare truncation
// marker -- including the allocation error that explains the limit, which is
// the one message the operator needs to read.
func TestSmallMaxAllocStillRendersDiagnostics(t *testing.T) {
	for _, tc := range []struct{ expr, want string }{
		{`(length (make-sequence 0 33))`, "allocation size 33 exceeds maximum (32)"},
		{`(length (concat 'string (string:repeat "a" 32) "b"))`, "allocation size 33 exceeds maximum (32)"},
		{`(length (string:repeat "a" 33))`, "repeat would exceed maximum allocation size (32 bytes)"},
	} {
		expr := tc.expr
		t.Run(expr, func(t *testing.T) {
			env := newLimitTestEnv(t, lisp.WithMaxAlloc(32))
			v := env.LoadString("max-alloc.lisp", expr)
			require.Equal(t, lisp.LError, v.Type, "expression must fail under the limit")
			e := (*lisp.ErrorVal)(v)
			for name, got := range map[string]string{
				"ErrorMessage": e.ErrorMessage(),
				"Error":        e.Error(),
			} {
				assert.NotEqual(t, "#<truncated>", got, "%s returned only the truncation marker", name)
				assert.Contains(t, got, tc.want, "%s must describe the failure", name)
			}
			var buf bytes.Buffer
			_, err := e.WriteTrace(&buf)
			require.NoError(t, err)
			assert.Contains(t, buf.String(), tc.want)
			assert.Contains(t, buf.String(), "Stack Trace", "the trace must survive the limit too")
		})
	}
}

// The data side of MaxAlloc is untouched: the limit still rejects the
// allocation that tripped it, and still admits one exactly at the limit.
func TestSmallMaxAllocStillCapsData(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxAlloc(32))
	ok := env.LoadString("max-alloc.lisp", `(length (make-sequence 0 32))`)
	require.Equal(t, lisp.LInt, ok.Type, "a value at the limit must still be allowed: %v", ok)
	assert.Equal(t, 32, ok.Int)
	bad := env.LoadString("max-alloc.lisp", `(length (make-sequence 0 33))`)
	assert.Equal(t, lisp.LError, bad.Type)
	// A VALUE rendered for the program stays under MaxAlloc: only the text
	// describing a failure gets the floor.
	big := env.LoadString("max-alloc.lisp", `(format-string "{}" (make-sequence 0 20))`)
	assert.Equal(t, lisp.LError, big.Type, "format-string must still honour MaxAlloc: %v", big)
}
