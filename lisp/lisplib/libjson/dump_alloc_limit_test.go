// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"context"
	"strings"
	"sync/atomic"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// allocLimitEnv returns a stdlib environment with cfg applied, so a test can
// lower the allocation cap.
func allocLimitEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	for _, c := range cfg {
		require.NoError(t, lisp.GoError(c(env)))
	}
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

func mustLoad(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("test", src)
	require.NotEqual(t, lisp.LError, v.Type, "%s: %v", src, v)
	return v
}

var dumpBuiltins = []string{"dump-string", "dump-bytes", "dump-message"}

// json:dump-* used to build its output without consulting Runtime.MaxAlloc,
// while format-string refused the very same value.  A list can hold many
// references to one value, so a program could make the encoder emit
// (references x size) bytes and take the host down.
func TestJSONDumpHonoursMaxAlloc(t *testing.T) {
	env := allocLimitEnv(t, lisp.WithMaxAlloc(1024))
	// Each piece is under the cap; the rendered document is not.
	mustLoad(t, env, `(set 'l (let* ((s (string:repeat "a" 512))) (list s s s s)))`)

	fs := env.LoadString("test", `(format-string "{}" l)`)
	require.Equal(t, lisp.LError, fs.Type, "format-string must refuse: %v", fs)
	require.Contains(t, lisp.GoError(fs).Error(), "allocation size exceeds maximum (1024)")

	for _, fn := range dumpBuiltins {
		t.Run(fn, func(t *testing.T) {
			res := env.LoadString("test", `(json:`+fn+` l)`)
			require.Equal(t, lisp.LError, res.Type, "json:%s built a document over the 1024-byte cap", fn)
			// The same ordinary error format-string raises, named for the
			// json builtin.
			assert.Equal(t, "error", res.Str)
			assert.Contains(t, lisp.GoError(res).Error(), "json:"+fn+": allocation size exceeds maximum (1024)")
		})
	}
}

func TestJSONDumpMaxAllocEdges(t *testing.T) {
	env := allocLimitEnv(t, lisp.WithMaxAlloc(1024))
	tests := []struct {
		name, expr string
		wantErr    bool
	}{
		// A document of exactly the cap is allowed; one byte more is not.
		{"string at the cap", `(json:dump-string (string:repeat "a" 1022))`, false},
		{"string one over the cap", `(json:dump-string (string:repeat "a" 1023))`, true},
		// Escaping amplifies an under-cap string 6x ("\u0000" per byte).
		{"escaping amplification", `(json:dump-string (string:repeat "\x00" 1000))`, true},
		// Base64 amplifies bytes 4/3.
		{"bytes amplification", `(json:dump-string (to-bytes (string:repeat "a" 900)))`, true},
		{"bytes under the cap", `(json:dump-string (to-bytes (string:repeat "a" 600)))`, false},
		// Many small leaves: the cap is checked between values, not only on
		// large ones.
		{"many small values", `(json:dump-string (map 'list (lambda (i) i) (make-sequence 0 600)))`, true},
		{"map values", `(json:dump-string (let ((m (sorted-map))) (dotimes (i 200) (assoc! m (to-string i) "xxxx")) m))`, true},
		// Deeper than the encoder's shallow pass: the second pass is
		// bounded too.
		{"deeply nested", `(json:dump-string (let ((x (string:repeat "a" 600))) (dotimes (i 70) (set! x (list x "bbbbbbbbbb"))) x))`, true},
		{"deeply nested and small", `(json:dump-string (let ((x 1)) (dotimes (i 100) (set! x (list x))) x))`, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			res := env.LoadString("test", tc.expr)
			if tc.wantErr {
				require.Equal(t, lisp.LError, res.Type, "%s", tc.expr)
				assert.Contains(t, lisp.GoError(res).Error(), "allocation size exceeds maximum (1024)")
				return
			}
			require.NotEqual(t, lisp.LError, res.Type, "%v", res)
			assert.LessOrEqual(t, len(res.Str), 1024)
		})
	}

	// The exact-cap document is the full document, not a truncation.
	res := mustLoad(t, env, `(json:dump-string (string:repeat "a" 1022))`)
	assert.Equal(t, `"`+strings.Repeat("a", 1022)+`"`, res.Str)

	// An ordinary error, so handler-bind and ignore-errors catch it.
	res = mustLoad(t, env, `(handler-bind ((condition (lambda (c &rest _) "caught")))
	                          (json:dump-string (string:repeat "a" 2000)))`)
	assert.Equal(t, `"caught"`, res.String())
}

// The motivating case: shared references make a document exponential in the
// number of steps that build it.  Under the default cap the dump fails instead
// of returning a 100 MB string.  TestEncodeBudgetStopsEarly pins that it stops
// near the cap rather than building the document first.
func TestJSONDumpSharedReferencesUnderDefaultCap(t *testing.T) {
	env := allocLimitEnv(t)
	mustLoad(t, env, `(set 'x (let ((x "a")) (dotimes (i 24) (set! x (list x x))) x))`)
	for _, fn := range dumpBuiltins {
		t.Run(fn, func(t *testing.T) {
			res := env.LoadString("test", `(json:`+fn+` x)`)
			require.Equal(t, lisp.LError, res.Type)
			assert.Contains(t, lisp.GoError(res).Error(), "allocation size exceeds maximum")
		})
	}
}

// cancelAfterCtx reports cancellation from its Err method once Err has been
// called more than n times.  It lets a test cancel the context from inside a
// builtin's own work without depending on timing: the evaluator polls Err a
// handful of times before the builtin starts, and an encoder that polled
// nothing would finish the document with the context never cancelled.
type cancelAfterCtx struct {
	context.Context
	done  chan struct{}
	calls atomic.Int64
	n     int64
}

// Done is never closed but is not nil: a nil Done channel declares a context
// that can never be cancelled, which the encoder is entitled not to poll.
func (c *cancelAfterCtx) Done() <-chan struct{} { return c.done }

func (c *cancelAfterCtx) Err() error {
	if c.calls.Add(1) > c.n {
		return context.Canceled
	}
	return nil
}

// A cancelled evaluation context stops an encode that is still under the byte
// cap, and raises the evaluator's context-cancelled condition.
func TestJSONDumpObservesContextCancellation(t *testing.T) {
	// A cap high enough that only the context can stop the encode.  The
	// document is 2^22 ints, about 16 MB, so an encoder that ignored the
	// context would finish it and fail the test rather than run away.
	env := allocLimitEnv(t, lisp.WithMaxAlloc(1<<30))
	mustLoad(t, env, `(set 'x (let ((x 1)) (dotimes (i 22) (set! x (list x x))) x))`)
	for _, fn := range dumpBuiltins {
		t.Run(fn, func(t *testing.T) {
			// The evaluator polls Err only a handful of times before the
			// builtin runs; 100 leaves a wide margin, and the encoder polls
			// about 4000 times on this document.
			ctx := &cancelAfterCtx{Context: context.Background(), done: make(chan struct{}), n: 100}
			res := env.LoadStringContext(ctx, "test", `(json:`+fn+` x)`)
			require.Equal(t, lisp.LError, res.Type, "the encoder did not observe the cancelled context")
			assert.Equal(t, lisp.CondContextCancelled, res.Str, "%v", res)
		})
	}
}

// The Go-level Dump has no runtime to read a cap from and stays unbounded.
func TestGoDumpIsNotCapped(t *testing.T) {
	big := lisp.String(strings.Repeat("a", lisp.DefaultMaxAlloc))
	b, err := libjson.Dump(big, false)
	require.NoError(t, err)
	assert.Len(t, b, lisp.DefaultMaxAlloc+2)
}
