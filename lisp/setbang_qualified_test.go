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

func setBangQualifiedEnv(t *testing.T, cfg ...lisp.Config) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, cfg...)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

// set creates/overwrites a package-qualified binding and a qualified symbol
// reads it, but set! used to look the qualified name up verbatim ("a:x") in
// the CURRENT package's table, so it reported "symbol not bound" for a
// binding that exists -- even for the current package's own qualified name.
func TestSetBangResolvesQualifiedSymbols(t *testing.T) {
	env := setBangQualifiedEnv(t)
	res := env.LoadString("test", `(progn
	  (in-package 'sbq-pkg) (set 'x 1) (export 'x) (in-package 'user)
	  (set! sbq-pkg:x 2)
	  sbq-pkg:x)`)
	require.NotEqual(t, lisp.LError, res.Type, "set! on a qualified bound symbol failed: %v", res)
	assert.Equal(t, "2", res.String())

	res = env.LoadString("test", `(progn (set 'y 1) (set! user:y 2) y)`)
	require.NotEqual(t, lisp.LError, res.Type, "set! user:y failed: %v", res)
	assert.Equal(t, "2", res.String())

	// Qualified access reaches every symbol of a package, exported or not,
	// for set (and reads) alike; set! matches set.
	res = env.LoadString("test", `(progn
	  (in-package 'sbq-priv) (set 'hidden 1) (in-package 'user)
	  (set 'sbq-priv:hidden 2)
	  (set! sbq-priv:hidden (+ sbq-priv:hidden 1))
	  sbq-priv:hidden)`)
	require.NotEqual(t, lisp.LError, res.Type, "set! on a non-exported qualified symbol failed: %v", res)
	assert.Equal(t, "3", res.String())

	// An import is a copy: set! through the qualified name rebinds the
	// source package, as set does, and leaves the importer's copy alone.
	res = env.LoadString("test", `(progn
	  (in-package 'sbq-src) (export 'v) (set 'v 1)
	  (in-package 'sbq-dst) (use-package 'sbq-src)
	  (set! sbq-src:v 2)
	  (let ((r (list v sbq-src:v))) (in-package 'user) r))`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
	assert.Equal(t, "'(1 2)", res.String())

	// Functions bound through a qualified set! stay callable by that name.
	res = env.LoadString("test", `(progn
	  (in-package 'sbq-fn) (defun f () 1) (in-package 'user)
	  (set! sbq-fn:f (lambda () 2))
	  (sbq-fn:f))`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
	assert.Equal(t, "2", res.String())
}

// The package branch of set! refuses what set refuses, with the same
// messages, and additionally refuses to create a binding.
func TestSetBangQualifiedErrors(t *testing.T) {
	for _, tc := range []struct{ source, want string }{
		// Unbound in an existing package: set! still never creates.
		{`(set! sbq-none:nosuch 1)`, "unknown package: \"sbq-none\""},
		{`(progn (in-package 'sbq-e) (in-package 'user) (set! sbq-e:nosuch 1))`,
			"symbol not bound: sbq-e:nosuch (set! only mutates existing bindings; use set to create new ones)"},
		{`(set! user:nosuch 1)`, "symbol not bound: user:nosuch"},
		// The lisp seal, qualified from any package; set says the same.
		{`(set! lisp:car 1)`, "cannot rebind lisp package binding: car"},
		{`(set 'lisp:car 1)`, "cannot rebind lisp package binding: car"},
		{`(set! lisp:nosuch 1)`, "cannot rebind lisp package binding: nosuch"},
		// Constants, qualified or not.
		{`(set! user:true 1)`, "cannot rebind constant: true"},
		// A local declared with a qualified spelling is never the target of a
		// qualified set!, just as it is never the value of a qualified read.
		{`((lambda (user:q) (set! user:q 5)) 3)`, "symbol not bound: user:q"},
		{`(let ((lisp:car 5)) (set! lisp:car 6))`, "cannot rebind lisp package binding: car"},
		// Keywords are not qualified names; their set! error is unchanged.
		{`(set! :kw 1)`, "symbol not bound: :kw"},
	} {
		t.Run(tc.source, func(t *testing.T) {
			env := setBangQualifiedEnv(t)
			res := env.LoadString("test", tc.source)
			require.Equal(t, lisp.LError, res.Type, "got %v", res)
			assert.Contains(t, res.String(), tc.want)
			assert.False(t, lisp.IsInternalPanic(res))
		})
	}
	// The trusted Go Update resolves a qualified key in its package too.
	env0 := setBangQualifiedEnv(t)
	require.NotEqual(t, lisp.LError, env0.LoadString("setup", `(set 'y 1)`).Type)
	require.NotEqual(t, lisp.LError, env0.Update(lisp.Symbol("user:y"), lisp.Int(5)).Type)
	assert.Equal(t, "5", env0.LoadString("test", `y`).String())

	// The failed writes left the sealed binding intact.
	env := setBangQualifiedEnv(t)
	res := env.LoadString("test", `(progn (set! lisp:car 1) (car '(7)))`)
	require.Equal(t, lisp.LError, res.Type)
	res = env.LoadString("test", `(lisp:car '(7))`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
	assert.Equal(t, "7", res.String())
}

// Unqualified set! and the lexical chain are untouched; a qualified name
// skips the chain exactly as a qualified read does (docs/lang.md#scope).
func TestSetBangQualifiedScope(t *testing.T) {
	for _, tc := range []struct{ source, want string }{
		// Unqualified shadowing is unchanged.
		{`(progn (set 'x 1) (let ((x 5)) (set! x 6) (list x user:x)))`, "'(6 1)"},
		{`(progn (set 'x 1) (let ((x 5)) (set! user:x 6) (list x user:x)))`, "'(5 6)"},
		{`(progn (set 'x 1) ((lambda (x) (set! user:x 7) (list x user:x)) 3))`, "'(3 7)"},
		// A local spelled user:x is accepted for compatibility but cannot be
		// read through user:x; set! user:x now writes the package too.
		{`(progn (set 'x 1) (let ((user:x 5)) (set! user:x 9) user:x))`, "9"},
		// Unqualified set! on a package binding in a sealed current package
		// is unaffected (user is not sealed).
		{`(progn (set 'z 1) (set! z 2) z)`, "2"},
	} {
		t.Run(tc.source, func(t *testing.T) {
			res := setBangQualifiedEnv(t).LoadString("test", tc.source)
			require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
			assert.Equal(t, tc.want, res.String())
		})
	}
	// Legacy keyword formals keep their lexical set! (issue #686).
	res := setBangQualifiedEnv(t, lisp.WithLegacyKeywordFormals(true)).
		LoadString("test", `(let ((:k 1)) (set! :k 2))`)
	require.NotEqual(t, lisp.LError, res.Type, "got %v", res)
}

// A qualified set! costs exactly what the unqualified one does: resolution
// happens inside the one evaluation step of the set! form.
func TestSetBangQualifiedStepCount(t *testing.T) {
	steps := func(src string) int64 {
		env := setBangQualifiedEnv(t, lisp.WithMaxSteps(1<<40))
		require.NotEqual(t, lisp.LError, env.LoadString("setup", `(set 'y 1)`).Type)
		before := env.Runtime.TotalSteps()
		res := env.LoadString("test", src)
		require.NotEqual(t, lisp.LError, res.Type, "%s: %v", src, res)
		return env.Runtime.TotalSteps() - before
	}
	unqualified := steps(`(set! y 2)`)
	// Pinned to the count before qualified set! worked, so the change
	// cannot move metering for existing programs.
	assert.Equal(t, int64(3), unqualified)
	assert.Equal(t, unqualified, steps(`(set! user:y 2)`))
	assert.Equal(t, steps(`(let ((q 0)) (set! q 1) (set! y 2))`),
		steps(`(let ((q 0)) (set! q 1) (set! user:y 2))`))
}

// A frozen template package is rebound per VM by a qualified set!, and the
// write stays private to that VM.
func TestSetBangQualifiedFrozenPackage(t *testing.T) {
	_, tmpl := frozenTemplate(t)
	vm, sibling := frozenVM(t, tmpl), frozenVM(t, tmpl)
	assert.Equal(t, "2", frozenEval(t, vm, `(progn (set! frozen-lib:counter 2) frozen-lib:counter)`))
	assert.Equal(t, "1", frozenEval(t, sibling, `frozen-lib:counter`))
	assert.Equal(t, "1", frozenEval(t, frozenVM(t, tmpl), `frozen-lib:counter`))
	// The frozen, sealed lisp package still refuses.
	res := vm.LoadString("test", `(set! lisp:car 1)`)
	require.Equal(t, lisp.LError, res.Type)
	assert.Contains(t, res.String(), "cannot rebind lisp package binding: car")
}
