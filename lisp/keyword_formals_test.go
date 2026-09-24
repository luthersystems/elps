// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// newKeywordFormalsEnv builds a user environment, optionally with the
// WithLegacyKeywordFormals override (issue #686).
func newKeywordFormalsEnv(t *testing.T, legacy bool) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithLegacyKeywordFormals(legacy))))
	return env
}

// Through v1.61.0 a keyword could name a parameter or a lexical binding.  It
// is a POSITIONAL parameter, not an &key one, so `(defun f (:a a :b b) ...)`
// called as `(f :a 1 :b 2)` binds :a to :a, a to 1, and so on.  The keyword is
// bound, but reading it still yields the keyword (a keyword evaluates to
// itself); the one place the binding shows is that set! on it succeeds.  Every
// expectation below was recorded from a v1.61.0 build, and the override must
// reproduce each of them.
var legacyKeywordFormalCases = []struct{ source, want string }{
	{`(defun f (:a a :b b) (list a b)) (f :a 1 :b 2)`, "'(1 2)"},
	{`(defun f (:a a :b b) (list a b :a :b)) (f :a 1 :b 2)`, "'(1 2 :a :b)"},
	// Positional: the keyword slots take whatever is passed there.
	{`(defun f (:a a :b b) (list a b :a :b)) (f 7 1 8 2)`, "'(1 2 :a :b)"},
	{`(defun g (x &optional :o) (list x :o)) (g 1 5)`, "'(1 :o)"},
	{`(defun g (x &optional :o) (list x :o)) (g 1)`, "'(1 :o)"},
	{`(defun h (x &rest :r) (list x :r)) (h 1 5)`, "'(1 :r)"},
	{`(defun k (&key :kk) :kk) (k)`, ":kk"},
	{`(defmacro m (:a b) b) (m :a 4)`, "4"},
	{`(labels ((f (:a a) a)) (f :a 3))`, "3"},
	// The keyword IS bound: set! on it succeeds, and reading it is unchanged.
	{`((lambda (:a) (set! :a 9)) 1)`, "()"},
	{`((lambda (:a) (set! :a 9) :a) 1)`, ":a"},
	{`((lambda (:a) (lambda () :a)) 1)`, "(lambda () :a)"},
	{`(let ((:k 1)) :k)`, ":k"},
	{`(let* ((:k 1)) :k)`, ":k"},
	{`(let ((:k 1)) (set! :k 2))`, "()"},
	{`(dotimes (:k 2) 1)`, "()"},
	{`(labels ((:k () 1)) 5)`, "5"},
}

// What v1.61.0 refused, the override still refuses.
var legacyKeywordFormalErrors = []struct{ source, want string }{
	// With no binding in scope set! on a keyword is an unbound symbol.
	{`((lambda () (set! :a 9)))`, "symbol not bound: :a"},
	// An &key parameter named by a keyword has no keyword that selects it.
	{`(defun k (&key :kk) :kk) (k :kk 3)`, "unrecognized keyword argument"},
	// Global bindings of keywords were never allowed.
	{`(set ':a 1)`, "value cannot be assigned to a keyword: :a"},
	{`((lambda (:a) (set ':a 9)) 1)`, "value cannot be assigned to a keyword: :a"},
}

func TestLegacyKeywordFormalsOverride(t *testing.T) {
	for _, tc := range legacyKeywordFormalCases {
		t.Run(tc.source, func(t *testing.T) {
			got := newKeywordFormalsEnv(t, true).LoadString("keyword-formals.lisp", tc.source)
			require.NotEqual(t, lisp.LError, got.Type, "got %v", got)
			assert.Equal(t, tc.want, got.String())
		})
	}
	for _, tc := range legacyKeywordFormalErrors {
		t.Run(tc.source, func(t *testing.T) {
			got := newKeywordFormalsEnv(t, true).LoadString("keyword-formals.lisp", tc.source)
			require.Equal(t, lisp.LError, got.Type, "got %v", got)
			assert.Contains(t, got.String(), tc.want)
			assert.False(t, lisp.IsInternalPanic(got))
		})
	}
}

// By default a keyword formal is refused where the function is defined, and
// the message says what to write instead and that the override exists.
func TestKeywordFormalsRefusedByDefault(t *testing.T) {
	env := newKeywordFormalsEnv(t, false)
	got := env.LoadString("keyword-formals.lisp", `(defun f (:a a :b b) (list a b))`)
	require.Equal(t, lisp.LError, got.Type, "got %v", got)
	assert.Contains(t, got.String(), "function formal argument list contains a keyword: :a")
	assert.Contains(t, got.String(), "use &key for keyword arguments")
	assert.Contains(t, got.String(), "lisp.WithLegacyKeywordFormals")
	got = env.LoadString("keyword-formals.lisp", `(let ((:k 1)) :k)`)
	require.Equal(t, lisp.LError, got.Type, "got %v", got)
	assert.Contains(t, got.String(), "value cannot be assigned to a keyword: :k")
}

// The override belongs to one runtime.
func TestLegacyKeywordFormalsDoNotLeakAcrossEnvs(t *testing.T) {
	const src = `(defun f (:a a) a) (f :a 1)`
	legacy := newKeywordFormalsEnv(t, true)
	plain := newKeywordFormalsEnv(t, false)
	assert.Equal(t, "1", legacy.LoadString("a.lisp", src).String())
	assert.Equal(t, lisp.LError, plain.LoadString("b.lisp", src).Type)
	// A later default env is unaffected too.
	assert.Equal(t, lisp.LError, newKeywordFormalsEnv(t, false).LoadString("c.lisp", src).Type)
}

// A template published from a legacy environment carries the setting into
// every VM it mints, both for functions defined before publication and for
// definitions evaluated in the VM; a default template does not.
func TestLegacyKeywordFormalsCarriedByTemplate(t *testing.T) {
	for _, legacy := range []bool{true, false} {
		env := newKeywordFormalsEnv(t, legacy)
		if legacy {
			require.NoError(t, lisp.GoError(env.LoadString("pre.lisp", `(defun pre (:a a) a)`)))
		}
		tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
		require.NoError(t, err)
		for range 2 {
			vm, err := tmpl.NewVM()
			require.NoError(t, err)
			assert.Equal(t, legacy, vm.Runtime.LegacyKeywordFormals)
			got := vm.LoadString("vm.lisp", `(defun f (:a a :b b) (list a b)) (f :a 1 :b 2)`)
			if !legacy {
				assert.Equal(t, lisp.LError, got.Type)
				continue
			}
			assert.Equal(t, "'(1 2)", got.String())
			assert.Equal(t, "3", vm.LoadString("vm.lisp", `(pre :a 3)`).String())
		}
	}
}
