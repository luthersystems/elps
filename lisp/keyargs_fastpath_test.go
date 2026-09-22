// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func keyArgsEnv(t testing.TB) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	require.NotEqual(t, lisp.LError, lisplib.LoadLibrary(env).Type)
	require.NotEqual(t, lisp.LError, env.InPackage(lisp.String(lisp.DefaultUserPackage)).Type)
	return env
}

// TestKeyArgsNoKeywordsSupplied pins that a call supplying no keyword
// arguments binds every key formal to nil and reports the same errors as
// before the no-keywords fast path, and that supplied keywords still bind,
// the later duplicate still wins, and an unknown keyword still names the
// first unrecognised one in call order.
func TestKeyArgsNoKeywordsSupplied(t *testing.T) {
	env := keyArgsEnv(t)
	for _, tc := range []struct{ src, want string }{
		{`((lambda (&key a b) (list a b)))`, `'(() ())`},
		{`((lambda (x &key a) (list x a)) 1)`, `'(1 ())`},
		{`((lambda (&key a b) (list a b)) :b 2)`, `'(() 2)`},
		{`((lambda (&key a) a) :a 1 :a 2)`, `2`},
	} {
		v := env.LoadString("keyargs", tc.src)
		require.NotEqual(t, lisp.LError, v.Type, tc.src)
		assert.Equal(t, tc.want, v.String(), tc.src)
	}
	for _, tc := range []struct{ src, want string }{
		{`((lambda (&key a) a) :a)`, "odd number of keyword arguments"},
		{`((lambda (&key a) a) :z 1 :y 2)`, "unrecognized keyword argument: z"},
	} {
		v := env.LoadString("keyargs", tc.src)
		require.Equal(t, lisp.LError, v.Type, tc.src)
		assert.Contains(t, v.String(), tc.want, tc.src)
	}
}
