// Copyright © 2026 The ELPS authors

package libelpspath_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// allocLimit is the allocation cap the tests below run under.
const allocLimit = 10000

// allocEnv is iterEnv under allocLimit.
func allocEnv(t *testing.T, setup string) *lisp.LEnv {
	t.Helper()
	env := iterEnv(t, 0, "")
	lisp.WithMaxAlloc(allocLimit)(env)
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	return env
}

// A range splice and a nested iterator build a sequence whose length the
// program controls.  Both made it with no allocation check, so a program
// could hold a sequence far past Runtime.MaxAlloc: a self-splice doubles a
// vector on each call for almost no steps (luthersystems/elps#935).  Each
// now makes the check concat makes, on the length of the sequence it builds.
func TestSequenceBuildsObeyTheAllocationLimit(t *testing.T) {
	const big = `(set 'big (make-sequence 0 6000))
(set 'vbig (map 'vector identity big))`
	const tooBig = "exceeds maximum (10000)"

	for _, tc := range []struct {
		name, setup, expr string
	}{
		{"set! range doubles a vector",
			`(set 'a (vector 1))`,
			`(dotimes (i 24) (elpspath:?set! a '(range 0 0) a))`},
		{"set! range",
			big, `(elpspath:?set! vbig '(range 0 0) vbig)`},
		{"set range",
			big, `(elpspath:?set vbig '(range 0 0) vbig)`},
		{"set range of a list",
			big, `(elpspath:?set big '(range 0 0) big)`},
		// The iterator treats an element's error as that element's failure.
		// The limit is not one: it stops the operation.
		{"set! range under an iterator",
			big, `(elpspath:?set! (vector vbig) '* '(range 0 0) vbig)`},
		{"set range under an iterator",
			big, `(elpspath:?set (vector vbig) '* '(range 0 0) vbig)`},
		{"nested iterators",
			`(set 'row (map 'vector identity (make-sequence 0 200)))
(set 'v (map 'vector (lambda (i) row) (make-sequence 0 200)))`,
			`(elpspath:? v '* '*)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := allocEnv(t, tc.setup)
			got := env.LoadString("test.lisp", tc.expr)
			require.Equal(t, lisp.LError, got.Type, "the operation built its result")
			assert.Contains(t, got.String(), tooBig)
		})
	}

	// At the limit, and for a result no longer than the source, the
	// operations work as before: the check is on the result's length.
	for _, tc := range []struct {
		name, setup, expr, want string
	}{
		{"set! range, same length",
			big, `(length (elpspath:?set! vbig '(range 0 6000) vbig))`, "6000"},
		{"set! range, result at the limit",
			big, `(length (elpspath:?set! vbig '(range 0 2000) vbig))`, "10000"},
		{"nil! range",
			big, `(length (elpspath:?nil! vbig '(range 0 6000)))`, "6000"},
		{"nested iterators at the limit",
			`(set 'row (map 'vector identity (make-sequence 0 100)))
(set 'v (map 'vector (lambda (i) row) (make-sequence 0 100)))`,
			`(length (elpspath:? v '* '*))`, "10000"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := allocEnv(t, tc.setup)
			got := env.LoadString("test.lisp", tc.expr)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
}
