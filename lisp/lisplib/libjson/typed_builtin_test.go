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
			{`(to-string (json:dump-typed 1))`, `"1"`},
			{`(to-string (json:dump-typed '(a :b "c" 1.0)))`, `"[\"~#list\",[\"~$a\",\"~:b\",\"c\",1.0]]"`},
			{`(to-string (json:dump-typed (sorted-map 'amount 125000 "k" (vector true false))))`, `"{\"k\":[true,false],\"~$amount\":125000}"`},
			{`(to-string (json:dump-typed ()))`, `"null"`},
			{`(to-string (json:dump-typed (vector)))`, `"[]"`},
		}},
		{"load sequences", [][2]string{
			{`(json:load-typed "null")`, `()`},
			{`(type (json:load-typed "[]"))`, `'array`},
			{`(equal? (vector 1 2) (json:load-typed "[1,2]"))`, `true`},
			{`(json:load-typed "[\"~#list\",[1,2]]")`, `'(1 2)`},
		}},
		{"plain escaping compatibility", [][2]string{
			{`(equal? (json:dump-typed "<>&") (json:dump-bytes "<>&"))`, `true`},
			{`(json:load-typed (json:dump-string "<>&"))`, `"<>&"`},
			{`(equal? (sorted-map "<" (vector 1 0.5 "&>")) (json:load-typed (json:dump-string (sorted-map "<" (vector 1 0.5 "&>")))))`, `true`},
		}},
		{"round trip", [][2]string{
			{`(json:load-typed (json:dump-typed '(1 2.5 "s" sym :kw (nested))))`, `'(1 2.5 "s" sym :kw '(nested))`},
			{`(let ((v (vector 1 (sorted-map "b" 2 :a 1 7 'x)))) (equal? v (json:load-typed (json:dump-typed v))))`, `true`},
			{`(json:load-typed (json:dump-typed (to-bytes "hi")))`, `#<bytes 104 105>`},
			{`(float? (json:load-typed (to-string (json:dump-typed 1.0))))`, `true`},
			{`(equal? (json:dump-typed (sorted-map 1 'x "k" 'y)) (json:dump-typed (sorted-map "k" 'y 1 'x)))`, `true`},
			{`(deftype point (x y) (list x y))`, `'user:point`},
			{`(user-data (json:load-typed (json:dump-typed (new point 1 2))))`, `'(1 2)`},
			{`(type (json:load-typed (json:dump-typed (new point 1 2))))`, `'user:point`},
		}},
		{"fresh", [][2]string{
			{`(set 'b (json:dump-typed (sorted-map "k" 1)))`, `#<bytes 123 34 107 34 58 49 125>`},
			{`(set 'x (json:load-typed b))`, `(sorted-map "k" 1)`},
			{`(set 'y (json:load-typed b))`, `(sorted-map "k" 1)`},
			{`(assoc! x "k" 9)`, `(sorted-map "k" 9)`},
			{`y`, `(sorted-map "k" 1)`},
		}},
		{"rejects", [][2]string{
			{`(json:dump-typed (lambda () 1))`, `test:1:1: json:dump-typed: typed json: cannot encode a function`},
			{`(json:load-typed "{\"a\": 1}")`, `test:1:1: json:load-typed: typed json: offset 5: invalid value`},
			{`(json:load-typed 1)`, `test:1:1: json:load-typed: argument is not bytes or a string: 'int`},
			{`(json:load-typed "[\"~#vector\",[1,2]]")`, `test:1:1: json:load-typed: typed json: offset 1: unknown tag`},
			{`(json:load-typed "[\"~#list\",[]]")`, `test:1:1: json:load-typed: typed json: offset 12: empty list must be null`},
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

// dump-typed charges one step per started KiB of output, load-typed one
// per started KiB of input, before decoding.
func TestTypedSteps(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 2000)) ""))`)))
	base := typedSteps(t, env, `(identity s)`)
	assert.Equal(t, int64(2), typedSteps(t, env, `(json:dump-typed s)`)-base) // 2002 bytes
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(set 'b (json:dump-typed s))`)))
	assert.Equal(t, int64(2), typedSteps(t, env, `(json:load-typed b)`)-base)
}

func TestTypedHonorsMaxAlloc(t *testing.T) {
	env := newTypedTestEnv(t, lisp.WithMaxAlloc(100))
	v := env.LoadString("t", `(json:dump-typed (string:join (map 'list (lambda (i) "x") (make-sequence 0 99)) ""))`)
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "limit exceeded")
}

// A step budget stops dump-typed with the budget's own condition while it
// encodes.
func TestTypedDumpStopsAtStepBudget(t *testing.T) {
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup",
		`(set 's (string:join (map 'list (lambda (i) "x") (make-sequence 0 1000)) ""))
		 (set 'v (map 'list (lambda (i) s) (make-sequence 0 200)))`)))
	env.Runtime.SetStepBudget(50)
	got := env.LoadStringContext(context.Background(), "t", `(json:dump-typed v)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.Contains(t, got.String(), "step")
	assert.NotContains(t, got.String(), "typed json")
}
