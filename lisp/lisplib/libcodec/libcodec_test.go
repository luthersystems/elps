// Copyright © 2026 The ELPS authors

package libcodec_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCodecBuiltins(t *testing.T) {
	tests := elpstest.TestSuite{
		{"serialize bytes", elpstest.TestSequence{
			{`(codec:encode 1)`, `#<bytes 1 1 2>`, ""},
			{`(codec:encode '(a :b "c"))`, `#<bytes 1 8 3 6 1 97 7 1 98 4 1 99>`, ""},
		}},
		{"round trip", elpstest.TestSequence{
			{`(codec:decode (codec:encode '(1 2.5 "s" sym :kw (nested))))`, `'(1 2.5 "s" sym :kw '(nested))`, ""},
			{`(let ((v (vector 1 (sorted-map "b" 2 :a 1)))) (equal? v (codec:decode (codec:encode v))))`, `true`, ""},
			{`(codec:decode (codec:encode (to-bytes "hi")))`, `#<bytes 104 105>`, ""},
			{`(equal? (codec:encode (sorted-map 1 'x "k" 'y)) (codec:encode (sorted-map "k" 'y 1 'x)))`, `true`, ""},
			{`(deftype point (x y) (list x y))`, `'user:point`, ""},
			{`(user-data (codec:decode (codec:encode (new point 1 2))))`, `'(1 2)`, ""},
			{`(type (codec:decode (codec:encode (new point 1 2))))`, `'user:point`, ""},
		}},
		{"fresh", elpstest.TestSequence{
			{`(set 'b (codec:encode (sorted-map "k" 1)))`, `#<bytes 1 10 1 4 1 107 1 2>`, ""},
			{`(set 'x (codec:decode b))`, `(sorted-map "k" 1)`, ""},
			{`(set 'y (codec:decode b))`, `(sorted-map "k" 1)`, ""},
			{`(assoc! x "k" 9)`, `(sorted-map "k" 9)`, ""},
			{`y`, `(sorted-map "k" 1)`, ""},
		}},
		{"rejects", elpstest.TestSequence{
			{`(codec:encode (lambda () 1))`, `test:1:1: codec:encode: canonical codec: cannot encode a function`, ""},
			{`(codec:decode (to-bytes "x"))`, `test:1:1: codec:decode: canonical codec: unsupported format version`, ""},
			{`(codec:decode "x")`, `test:1:1: codec:decode: argument is not bytes: 'string`, ""},
		}},
	}
	for _, seq := range tests {
		t.Run(seq.Name, func(t *testing.T) {
			env := newLimitTestEnv(t)
			for i, x := range seq.TestSequence {
				v := env.LoadString("test", x.Expr)
				assert.Equal(t, x.Result, v.String(), "expr %d: %s", i, x.Expr)
			}
		})
	}
}

// codec:encode charges one step per started KiB of output, codec:decode one per
// started KiB of input, before decoding.
func TestCodecSteps(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 2000)) ""))`)))
	_, base := stepsOf(t, env, `(identity s)`)
	_, ser := stepsOf(t, env, `(codec:encode s)`)
	// 1 version + 1 tag + 2 length bytes + 2000 = 2004 bytes: 2 started KiB.
	assert.Equal(t, int64(2), ser-base)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 'b (codec:encode s))`)))
	_, de := stepsOf(t, env, `(codec:decode b)`)
	assert.Equal(t, int64(2), de-base)
}

func TestCodecHonorsMaxAlloc(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxAlloc(100))
	v := env.LoadString("t", `(codec:encode (string:join (map 'list (lambda (i) "x") (make-sequence 0 99)) ""))`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "limit exceeded")
}

func newLimitTestEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
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

func stepsOf(t *testing.T, env *lisp.LEnv, src string) (*lisp.LVal, int64) {
	t.Helper()
	v := env.LoadStringContext(context.Background(), "test", src)
	return v, env.Runtime.Steps()
}

// The builtins are not in the core lisp package: a phylum or program that
// defines its own serialize keeps it (luthersystems/elps#747).
func TestCodecNotInCorePackage(t *testing.T) {
	env := newLimitTestEnv(t)
	for _, name := range []string{"serialize", "deserialize", "encode", "decode"} {
		v := env.LoadString("t", "(lisp:"+name+" 1)")
		assert.Equal(t, lisp.LError, v.Type, "lisp:%s should not exist", name)
	}
}
