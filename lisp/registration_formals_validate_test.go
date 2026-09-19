// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// A formal argument name that can never be bound -- a keyword or one of the
// boolean constants -- is refused where the function is DEFINED, and the
// binder does not look at formals again (issue #666).  For a lisp definition
// that place is LEnv.Lambda, which reports an error value; for a
// host-registered definition it is registration, which panics like every
// other mistake an embedder can only make from Go (issue #367).

type badFormalsDef struct {
	formals *lisp.LVal
	name    string
}

func (d *badFormalsDef) Name() string        { return d.name }
func (d *badFormalsDef) Formals() *lisp.LVal { return d.formals }
func (d *badFormalsDef) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return lisp.Nil()
}

func testEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	return env
}

func TestRegistrationRejectsUnbindableFormals(t *testing.T) {
	tests := []struct {
		formals *lisp.LVal
		name    string
		defName string
		message string
	}{
		{
			name:    "keyword",
			defName: "bad-keyword",
			formals: lisp.Formals(":key"),
			message: "builtin bad-keyword cannot be registered: function formal argument list contains a keyword: :key",
		},
		{
			name:    "constant",
			defName: "bad-constant",
			formals: lisp.Formals("x", "true"),
			message: "builtin bad-constant cannot be registered: function formal argument list contains the constant true",
		},
		{
			name:    "non-symbol",
			defName: "bad-nonsymbol",
			formals: lisp.QExpr([]*lisp.LVal{lisp.Int(1)}),
			message: "builtin bad-nonsymbol cannot be registered: first argument contains a non-symbol: int",
		},
		{
			name:    "not-a-list",
			defName: "bad-notalist",
			formals: lisp.Int(1),
			message: "builtin bad-notalist cannot be registered: formals is not a list of symbols: int",
		},
		{
			name:    "nil",
			defName: "bad-nil",
			formals: nil,
			message: "builtin bad-nil cannot be registered: formals is not a list of symbols: <nil>",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			env := testEnv(t)
			def := &badFormalsDef{name: test.defName, formals: test.formals}
			assert.PanicsWithValue(t, test.message, func() {
				env.AddBuiltins(true, def)
			})
			// The same definition offered as a special operator or a macro is
			// refused by the same rule, with the kind named.
			assert.Panics(t, func() { testEnv(t).AddSpecialOps(true, def) })
			assert.Panics(t, func() { testEnv(t).AddMacros(true, def) })
		})
	}
}

// The public embedding path reports the same mistake, as an error rather than
// a panic, before anything is registered.
func TestElpsutilValidateRejectsUnbindableFormals(t *testing.T) {
	env := testEnv(t)
	def := elpsutil.Function("bad", lisp.Formals(":key"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		return lisp.Nil()
	})
	lerr := elpsutil.Load(env, elpsutil.PackageLoader(&badFormalsPackage{def}))
	require.Equal(t, lisp.LError, lerr.Type)
	assert.Contains(t, lerr.String(), "function formal argument list contains a keyword: :key")
}

type badFormalsPackage struct {
	def *elpsutil.Builtin
}

func (p *badFormalsPackage) PackageName() string { return "bad-formals-test" }
func (p *badFormalsPackage) Builtins() []lisp.LBuiltinDef {
	return []lisp.LBuiltinDef{p.def}
}

// A valid registration is unaffected, and the registered function still runs.
func TestRegistrationAcceptsValidFormals(t *testing.T) {
	env := testEnv(t)
	def := &badFormalsDef{name: "good-formals", formals: lisp.Formals("a", "&optional", "b")}
	require.NotPanics(t, func() { env.AddBuiltins(true, def) })
	v := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("good-formals"), lisp.Int(1)}))
	assert.NotEqual(t, lisp.LError, v.Type, "%v", v)
}
