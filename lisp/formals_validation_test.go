// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// A keyword or a constant cannot name a parameter: binding one is refused by
// Put, so such a function could never be called.  The docstring promises the
// error is raised at CREATION, which is where a reader of the definition can
// act on it -- not on the first call, arbitrarily far away.
func TestFormalsRejectKeywordAndConstantAtCreation(t *testing.T) {
	for _, tc := range []struct{ source, want string }{
		{`(lambda (:x) 1)`, "keyword"},
		{`(lambda (a &optional :x) 1)`, "keyword"},
		{`(lambda (a &rest :x) 1)`, "keyword"},
		{`(lambda (true) 1)`, "constant"},
		{`(lambda (false) 1)`, "constant"},
		{`(defun f (:x) 1)`, "keyword"},
		{`(defun f (true) 1)`, "constant"},
		{`(defmacro m (:x) 1)`, "keyword"},
		{`(labels ((f (:x) 1)) (f 1))`, "keyword"},
		{`(flet ((f (true) 1)) 1)`, "constant"},
	} {
		t.Run(tc.source, func(t *testing.T) {
			env := newGoMacroEnv(t)
			got := env.LoadString("formals.lisp", tc.source)
			require.Equal(t, lisp.LError, got.Type, "got %v", got)
			assert.Contains(t, got.String(), tc.want)
		})
	}
}

// Valid formals, including the markers and a package-qualified-looking name,
// keep working.
func TestFormalsAcceptOrdinaryNames(t *testing.T) {
	for _, tc := range []struct{ source, result string }{
		{`((lambda (a &optional b) a) 1)`, "1"},
		{`((lambda (&rest xs) xs) 1 2)`, "'(1 2)"},
		{`((lambda (&key k) k) :k 3)`, "3"},
		{`((lambda (truthy falsey) truthy) 1 2)`, "1"},
	} {
		t.Run(tc.source, func(t *testing.T) {
			env := newGoMacroEnv(t)
			got := env.LoadString("formals.lisp", tc.source)
			require.NotEqual(t, lisp.LError, got.Type, "got %v", got)
			assert.Equal(t, tc.result, got.String())
		})
	}
}

// A host-registered function's formals run through the same check when the
// function is bound, so a Go embedder learns of the mistake on the call
// rather than getting a binding error from deep inside the binder.
func TestFormalsRejectKeywordFromHostRegistration(t *testing.T) {
	env := newGoMacroEnv(t)
	env.AddBuiltins(true, elpsutil.Function("host-keyword", lisp.Formals(":x"),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() }))
	got := env.LoadString("host.lisp", `(host-keyword 1)`)
	require.Equal(t, lisp.LError, got.Type, "got %v", got)
	assert.Contains(t, got.String(), "keyword")
}
