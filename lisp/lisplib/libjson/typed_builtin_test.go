// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newTypedTestEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
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

func TestTypedBuiltins(t *testing.T) {
	for _, seq := range []struct {
		name  string
		steps [][2]string
	}{
		{"dump", [][2]string{
			{`(to-string (json:dump-bytes 1 :typed true))`, `"1"`},
			{`(to-string (json:dump-bytes '(a :b "c" 1.0) :typed true))`, `"[\"~#list\",[\"~$a\",\"~:b\",\"c\",\"~d1\"]]"`},
			{`(to-string (json:dump-bytes (sorted-map 'amount 125000 "k" (vector true false)) :typed true))`, `"{\"k\":[true,false],\"~$amount\":125000}"`},
			{`(to-string (json:dump-bytes () :typed true))`, `"null"`},
			{`(to-string (json:dump-bytes (vector) :typed true))`, `"[]"`},
		}},
		{"load sequences", [][2]string{
			{`(json:load-string "null" :typed true)`, `()`},
			{`(type (json:load-string "[]" :typed true))`, `'array`},
			{`(equal? (vector 1 2) (json:load-string "[1,2]" :typed true))`, `true`},
			{`(json:load-string "[\"~#list\",[1,2]]" :typed true)`, `'(1 2)`},
		}},
		{"plain escaping compatibility", [][2]string{
			{`(equal? (json:dump-bytes "<>&" :typed true) (json:dump-bytes "<>&"))`, `true`},
			{`(json:load-string (json:dump-string "<>&") :typed true)`, `"<>&"`},
			{`(equal? (sorted-map "<" (vector 1 0.5 "&>")) (json:load-string (json:dump-string (sorted-map "<" (vector 1 0.5 "&>"))) :typed true))`, `true`},
		}},
		{"round trip", [][2]string{
			{`(json:load-bytes (json:dump-bytes '(1 2.5 "s" sym :kw (nested)) :typed true) :typed true)`, `'(1 2.5 "s" sym :kw '(nested))`},
			{`(let ((v (vector 1 (sorted-map "b" 2 :a 1 7 'x)))) (equal? v (json:load-bytes (json:dump-bytes v :typed true) :typed true)))`, `true`},
			{`(json:load-bytes (json:dump-bytes (to-bytes "hi") :typed true) :typed true)`, `#<bytes 104 105>`},
			{`(float? (json:load-string (to-string (json:dump-bytes 1.0 :typed true)) :typed true))`, `true`},
			{`(equal? (json:dump-bytes (sorted-map 1 'x "k" 'y) :typed true) (json:dump-bytes (sorted-map "k" 'y 1 'x) :typed true))`, `true`},
			{`(deftype point (x y) (list x y))`, `'user:point`},
			{`(user-data (json:load-bytes (json:dump-bytes (new point 1 2) :typed true) :typed true))`, `'(1 2)`},
			{`(type (json:load-bytes (json:dump-bytes (new point 1 2) :typed true) :typed true))`, `'user:point`},
		}},
		{"fresh", [][2]string{
			{`(set 'b (json:dump-bytes (sorted-map "k" 1) :typed true))`, `#<bytes 123 34 107 34 58 49 125>`},
			{`(set 'x (json:load-bytes b :typed true))`, `(sorted-map "k" 1)`},
			{`(set 'y (json:load-bytes b :typed true))`, `(sorted-map "k" 1)`},
			{`(assoc! x "k" 9)`, `(sorted-map "k" 9)`},
			{`y`, `(sorted-map "k" 1)`},
		}},
		{"rejects", [][2]string{
			{`(json:dump-bytes (lambda () 1) :typed true)`, `test:1:1: json:dump-bytes: typed json: cannot encode a function`},
			{`(json:load-string "{\"a\": 1}" :typed true)`, `test:1:1: json:load-string: json: non-canonical whitespace`},
			{`(json:load-string 1 :typed true)`, `test:1:1: json:load-string: argument is not a string: int`},
			{`(json:load-string "[\"~#unknown\",[1,2]]" :typed true)`, `test:1:1: json:load-string: typed json: unknown tag`},
			{`(json:load-string "[\"~#list\",[]]" :typed true)`, `test:1:1: json:load-string: typed json: empty list must be null`},
		}},
	} {
		t.Run(seq.name, func(t *testing.T) {
			env := newTypedTestEnv(t)
			for i, x := range seq.steps {
				v := env.LoadString("test", x[0])
				assert.Equal(t, x[1], v.String(), "expr %d: %s", i, x[0])
			}
		})
	}
}

func typedSteps(t *testing.T, env *lisp.LEnv, src string) int64 {
	t.Helper()
	v := env.LoadStringContext(context.Background(), "test", src)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	return env.Runtime.Steps()
}

// Typed dumping charges one step per started KiB of output, typed loading one
// per started KiB of input, before decoding.
func TestTypedSteps(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 2000)) ""))`)))
	base := typedSteps(t, env, `(identity s)`)
	// Evaluating the keyword and its value costs two steps; the builtin
	// itself charges two for the 2002-byte document.
	assert.Equal(t, int64(2), typedSteps(t, env, `(json:dump-bytes s :typed true)`)-base-2)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 'b (json:dump-bytes s :typed true))`)))
	assert.Equal(t, int64(2), typedSteps(t, env, `(json:load-bytes b :typed true)`)-base-2)
}

func TestTypedHonorsMaxAlloc(t *testing.T) {
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(100))
	v := env.LoadString("t", `(json:dump-bytes (string:join (map 'list (lambda (i) "x") (make-sequence 0 99)) "") :typed true)`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "limit exceeded")
}

// A step budget stops typed dumping with the budget's own condition while it
// encodes.
func TestTypedDumpStopsAtStepBudget(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup",
		`(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 1000)) ""))
		 (set 'v (map 'list (lambda (i) s) (make-sequence 0 200)))`)))
	env.Runtime.SetStepBudget(50)
	got := env.LoadStringContext(context.Background(), "t", `(json:dump-bytes v :typed true)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.Contains(t, got.String(), "step")
	assert.NotContains(t, got.String(), "typed json")
}
