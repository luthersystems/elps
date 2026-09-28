// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestDumpIntKeys pins json:dump for sorted-maps with int keys (#733): an
// int key is written as its decimal string, int keys first in numeric order
// and then the string and symbol keys, and a map where a stringified int key
// collides with a string key is refused rather than written with a
// duplicate member.  Decoding is unchanged: JSON object keys are always
// strings, so json:load never produces an int key.
func TestDumpIntKeys(t *testing.T) {
	for _, tc := range []struct {
		expr, want string
	}{
		{`(json:dump-string (sorted-map 2 "b" 10 "c" "a" 1 'z 0))`, `"{\"2\":\"b\",\"10\":\"c\",\"a\":1,\"z\":0}"`},
		{`(json:dump-string (sorted-map -1 (sorted-map 1 2)))`, `"{\"-1\":{\"1\":2}}"`},
		{`(json:dump-string (sorted-map 1 "int" "2" "string"))`, `"{\"1\":\"int\",\"2\":\"string\"}"`},
		{`(json:dump-string (sorted-map 9223372036854775807 1 -9223372036854775808 2))`, `"{\"-9223372036854775808\":2,\"9223372036854775807\":1}"`},
		{`(json:dump-string (vector (sorted-map 3 (vector (sorted-map 4 5)))))`, `"[{\"3\":[{\"4\":5}]}]"`},
		{`(to-string (json:dump-bytes (sorted-map 1 2)))`, `"{\"1\":2}"`},
		// Decoding keeps string keys.
		{`(json:load-string "{\"1\":2,\"10\":3,\"a\":4}")`, `(sorted-map "1" 2 "10" 3 "a" 4)`},
		{`(map 'list type (keys (json:load-string "{\"1\":2}")))`, `'('string)`},
		{`(get (json:load-string "{\"1\":2}") "1")`, `2`},
		// A copy of a decoded map is a stock map and so takes int keys.
		{`(assoc (json:load-string "{\"a\":2}") 1 "int")`, `(sorted-map 1 "int" "a" 2)`},
		// Round trip: the int key comes back as its string.
		{`(json:load-string (json:dump-string (sorted-map 1 2)))`, `(sorted-map "1" 2)`},
	} {
		t.Run(tc.expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			got := env.LoadString("intkey.lisp", tc.expr)
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
}

func TestDumpIntKeyCollision(t *testing.T) {
	for _, expr := range []string{
		`(json:dump-string (sorted-map 1 "int" "1" "string"))`,
		`(json:dump-string (assoc (sorted-map -7 "int") sym "symbol"))`,
		`(json:dump-string (sorted-map "outer" (sorted-map 1 "int" "1" "string")))`,
		`(json:dump-string (assoc (json:load-string "{\"1\":2}") 1 "int"))`,
		`(json:dump-bytes (sorted-map 1 "int" "1" "string"))`,
	} {
		t.Run(expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			env.PutGlobal(lisp.Symbol("sym"), lisp.Quote(lisp.Symbol("-7")))
			got := env.LoadString("intkey.lisp", expr)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			require.False(t, lisp.IsInternalPanic(got), "%v", got)
			msg := env.Render(got)
			assert.True(t, strings.Contains(msg, "collides with string key"), msg)
		})
	}
}

// TestDecodedMapKeepsStringKeyPolicy pins that a map json:load returns keeps
// its own key policy: it holds string keys only, so an in-place int write
// is refused, exactly as before int keys existed.
func TestDecodedMapKeepsStringKeyPolicy(t *testing.T) {
	env, err := (&elpstest.Runner{}).NewEnv(t)
	require.NoError(t, err)
	got := env.LoadString("intkey.lisp", `(assoc! (json:load-string "{\"a\":1}") 1 2)`)
	require.Equal(t, lisp.LError, got.Type, "%v", got)
	assert.Contains(t, env.Render(got), "sorted-map decoded from json cannot hold key with type 'int")
}
