package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLispPackageSeal(t *testing.T) {
	for _, library := range []bool{false, true} {
		for _, tc := range []struct{ source, name string }{
			{`(set 'lisp:if 1)`, "if"},
			{`(set! 'lisp:if 1)`, "if"},
			{`(defun lisp:car (x) x)`, "car"},
			{`(defmacro lisp:quasiquote (x) x)`, "quasiquote"},
			{`(set 'target 'lisp:lambda) (set target 1)`, "lambda"},
			{`(in-package 'lisp) (set 'if 1)`, "if"},
			{`(in-package 'lisp) (set! 'if 1)`, "if"},
			{`(in-package 'lisp) (defun car (x) x)`, "car"},
			{`(in-package 'lisp) (defmacro quasiquote (x) x)`, "quasiquote"},
			{`(in-package 'lisp) (export 'if) (set 'if 1)`, "if"},
			{`(in-package 'lisp) (set 'new-name 1)`, "new-name"},
			{`(in-package 'lisp) (export 'new-name) (set 'new-name 1)`, "new-name"},
			{`(in-package 'replacement) (export 'if) (set 'if 1) (in-package 'lisp) (use-package 'replacement)`, "if"},
		} {
			t.Run(tc.source+map[bool]string{false: "/core", true: "/library"}[library], func(t *testing.T) {
				env := templateTestEnv(t)
				if library {
					require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
				}
				got := env.LoadString("seal.lisp", tc.source)
				assert.Equal(t, lisp.LError, got.Type, "got %v", got)
				assert.Contains(t, got.String(), "cannot rebind lisp package binding: "+tc.name)
				assert.Equal(t, "'a", env.LoadString("next.lisp", `(in-package 'mypkg) (use-package 'lisp) (if true 'a 'b)`).String())
			})
		}
	}
}

func TestLispPackageSealUserShadowing(t *testing.T) {
	for _, source := range []string{
		`(set 'if 1) if`, `(set! 'if 1) if`, `(set 'lambda 1) lambda`,
		`(set 'quote 1) quote`, `(defun car (x) x) (car 1)`,
		`(defmacro car (x) x) (car 1)`,
	} {
		t.Run(source, func(t *testing.T) {
			env := templateTestEnv(t)
			assert.Equal(t, "1", env.LoadString("shadow.lisp", source).String())
			assert.Equal(t, "'a", env.LoadString("next.lisp", `(in-package 'mypkg) (if true 'a 'b)`).String())
		})
	}
}

func TestLispPackageSealTemplateAndAdmission(t *testing.T) {
	env := templateTestEnv(t)
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	for _, candidate := range []*lisp.LEnv{vm, lisp.NewEnv(nil)} {
		if candidate != vm {
			candidate.Runtime.Reader = env.Runtime.Reader
			require.True(t, candidate.Runtime.Registry.AddPackage(env.Runtime.Registry.Package("lisp")))
			require.NoError(t, lisp.GoError(candidate.InPackage(lisp.Symbol("lisp"))))
		}
		got := candidate.LoadString("seal.lisp", `(set 'lisp:if 1)`)
		if got.Type != lisp.LError || !strings.Contains(got.String(), "cannot rebind lisp package binding: if") {
			t.Errorf("copied package lost seal: %v", got)
		}
	}
}
