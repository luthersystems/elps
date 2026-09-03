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

// LEnv.Get used to hand back a FunRef copy of every function binding it
// resolved -- a fresh 112-byte LVal header per lookup -- purely so the
// caller's spelling of the name landed in LVal.Str, where GetFunName reads
// it as a fallback for error messages and stack traces.  The head symbol of
// every evaluated s-expression goes through that path, so it was one of the
// interpreter's highest frequency allocations.
//
// These tests pin both halves of the fix: the binding itself comes back when
// its name already matches (no copy), and an alias still gets the copy, so
// the names rendered in diagnostics are unchanged.

func funRefTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadLibrary(env).IsNil())
	require.True(t, env.InPackage(lisp.String(lisp.DefaultUserPackage)).IsNil())
	return env
}

func funRefLoad(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("get_funref_test.lisp", src)
	require.NotNil(t, v)
	require.NotEqual(t, lisp.LError, v.Type, "eval %q: %v", src, v)
	return v
}

// TestGetReturnsBoundFunctionWhenNameMatches: a lookup whose symbol matches
// the name the binding was defined under returns the binding itself, not a
// copy.  This is the allocation the change removes; it fails before it.
func TestGetReturnsBoundFunctionWhenNameMatches(t *testing.T) {
	env := funRefTestEnv(t)
	funRefLoad(t, env, `(defun f (x) x)`)

	stored, ok := env.Runtime.Package.Symbol("f")
	require.True(t, ok, "f is not bound in the user package")
	require.Equal(t, lisp.LFun, stored.Type)
	require.Equal(t, "f", stored.Str, "defun must stamp the definition name onto the fresh lambda")

	got := env.Get(lisp.Symbol("f"))
	require.Equal(t, lisp.LFun, got.Type)
	assert.Same(t, stored, got, "Get must return the binding itself when the name already matches")
	assert.Equal(t, "f", got.Str)
}

// TestGetReturnsBoundBuiltinWhenNameMatches: the same holds for the builtins
// registered by AddBuiltins, which are now born carrying their own name.
func TestGetReturnsBoundBuiltinWhenNameMatches(t *testing.T) {
	env := funRefTestEnv(t)

	got := env.Get(lisp.Symbol("car"))
	require.Equal(t, lisp.LFun, got.Type)
	assert.Equal(t, "car", got.Str)

	lispPkg := env.Runtime.Registry.Package(lisp.DefaultLangPackage)
	require.NotNil(t, lispPkg, "the lisp package is not registered")
	stored, ok := lispPkg.Symbol("car")
	require.True(t, ok, "car is not bound in the lisp package")
	assert.Same(t, stored, got, "Get must return the registered builtin itself")

	// A qualified lookup spells the symbol differently from the binding, so
	// it still takes the copy path and still reports the qualified name.
	qualified := env.Get(lisp.Symbol("lisp:car"))
	require.Equal(t, lisp.LFun, qualified.Type)
	assert.Equal(t, "lisp:car", qualified.Str)
	assert.NotSame(t, stored, qualified)
}

// TestGetCopiesFunctionForAlias: a second name for the same function still
// gets a renamed copy, and the original binding is untouched.
func TestGetCopiesFunctionForAlias(t *testing.T) {
	env := funRefTestEnv(t)
	funRefLoad(t, env, `(defun f (x) x)`)
	funRefLoad(t, env, `(set 'g f)`)

	stored, ok := env.Runtime.Package.Symbol("f")
	require.True(t, ok)

	alias := env.Get(lisp.Symbol("g"))
	require.Equal(t, lisp.LFun, alias.Type)
	assert.Equal(t, "g", alias.Str, "an alias lookup must still be renamed")
	assert.NotSame(t, stored, alias, "an alias lookup must not hand back the binding")

	// Renaming the alias must not have renamed the binding.
	again := env.Get(lisp.Symbol("f"))
	assert.Equal(t, "f", again.Str)
	assert.Same(t, stored, again)

	// The alias is callable and behaves like the original.
	res := funRefLoad(t, env, `(g 1)`)
	assert.Equal(t, "1", res.String())
}

// TestAliasErrorMessagesAreUnchanged: the rendered error for calling an
// aliased user function, and an aliased builtin, with the wrong number of
// arguments.  The expected strings were captured on the parent commit, so a
// mismatch means the change altered a diagnostic.
func TestAliasErrorMessagesAreUnchanged(t *testing.T) {
	for _, test := range []struct {
		name  string
		setup []string
		call  string
		want  string
	}{
		{
			name:  "user function alias",
			setup: []string{`(defun f (x) x)`, `(set 'g f)`},
			call:  `(g)`,
			want:  aliasUserFunErrorText,
		},
		{
			name:  "builtin alias",
			setup: []string{`(set 'first car)`},
			call:  `(first)`,
			want:  aliasBuiltinErrorText,
		},
		{
			// The case the FunRef copy actually decides: a lambda bound
			// locally by flet never reaches the package's funNames table,
			// so its name in the message comes from the Str the copy
			// stamps.  The two cases above are decided by funNames and
			// would render the same with the copy path deleted.
			name: "flet-bound lambda",
			call: `(flet ([myf (a b) a]) (myf 1))`,
			want: aliasFletErrorText,
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			env := funRefTestEnv(t)
			for _, src := range test.setup {
				funRefLoad(t, env, src)
			}
			v := env.LoadString("get_funref_test.lisp", test.call)
			require.Equal(t, lisp.LError, v.Type, "expected an error from %s", test.call)
			assert.Equal(t, test.want, v.String())
		})
	}
}

// The expected renderings above, captured verbatim on the parent commit --
// they must not change.
const (
	aliasUserFunErrorText = "get_funref_test.lisp:1:1: g: invalid number of arguments: 0"
	aliasBuiltinErrorText = "get_funref_test.lisp:1:1: lisp:car: invalid number of arguments: 0"
	aliasFletErrorText    = "get_funref_test.lisp:1:23: myf: invalid number of arguments: 1"
)
