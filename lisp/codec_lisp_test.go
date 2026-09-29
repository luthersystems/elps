// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSerializeBuiltins(t *testing.T) {
	tests := elpstest.TestSuite{
		{"serialize bytes", elpstest.TestSequence{
			{`(serialize 1)`, `#<bytes 1 1 2>`, ""},
			{`(serialize '(a :b "c"))`, `#<bytes 1 8 3 6 1 97 7 1 98 4 1 99>`, ""},
		}},
		{"round trip", elpstest.TestSequence{
			{`(deserialize (serialize '(1 2.5 "s" sym :kw (nested))))`, `'(1 2.5 "s" sym :kw '(nested))`, ""},
			{`(let ((v (vector 1 (sorted-map "b" 2 :a 1)))) (equal? v (deserialize (serialize v))))`, `true`, ""},
			{`(deserialize (serialize (to-bytes "hi")))`, `#<bytes 104 105>`, ""},
			{`(equal? (serialize (sorted-map 1 'x "k" 'y)) (serialize (sorted-map "k" 'y 1 'x)))`, `true`, ""},
			{`(deftype point (x y) (list x y))`, `'user:point`, ""},
			{`(user-data (deserialize (serialize (new point 1 2))))`, `'(1 2)`, ""},
			{`(type (deserialize (serialize (new point 1 2))))`, `'user:point`, ""},
		}},
		{"fresh", elpstest.TestSequence{
			{`(set 'b (serialize (sorted-map "k" 1)))`, `#<bytes 1 10 1 4 1 107 1 2>`, ""},
			{`(set 'x (deserialize b))`, `(sorted-map "k" 1)`, ""},
			{`(set 'y (deserialize b))`, `(sorted-map "k" 1)`, ""},
			{`(assoc! x "k" 9)`, `(sorted-map "k" 9)`, ""},
			{`y`, `(sorted-map "k" 1)`, ""},
		}},
		{"rejects", elpstest.TestSequence{
			{`(serialize (lambda () 1))`, `test:1:1: lisp:serialize: canonical codec: cannot encode a function`, ""},
			{`(deserialize (to-bytes "x"))`, `test:1:1: lisp:deserialize: canonical codec: unsupported format version`, ""},
			{`(deserialize "x")`, `test:1:1: lisp:deserialize: argument is not bytes: 'string`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}

// serialize charges one step per started KiB of output, deserialize one per
// started KiB of input, before decoding.
func TestSerializeSteps(t *testing.T) {
	env := newLimitTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 2000)) ""))`)))
	_, base := stepsOf(t, env, `(identity s)`)
	_, ser := stepsOf(t, env, `(serialize s)`)
	// 1 version + 1 tag + 2 length bytes + 2000 = 2004 bytes: 2 started KiB.
	assert.Equal(t, int64(2), ser-base)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 'b (serialize s))`)))
	_, de := stepsOf(t, env, `(deserialize b)`)
	assert.Equal(t, int64(2), de-base)
}

func TestSerializeHonorsMaxAlloc(t *testing.T) {
	env := newLimitTestEnv(t, lisp.WithMaxAlloc(100))
	v := env.LoadString("t", `(serialize (string:join (map 'list (lambda (i) "x") (make-sequence 0 99)) ""))`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "limit exceeded")
}
