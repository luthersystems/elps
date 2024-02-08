// Copyright © 2021 The ELPS authors

package libhelp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDocstring(t *testing.T) {
	env := lisp.NewEnv(nil)
	reader := parser.NewReader()
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil())
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil())

	// check docs for builtin function, operator, and macro
	builtinSymbols := []string{"map", "lambda", "defun"}
	for _, name := range builtinSymbols {
		fun := env.Get(lisp.Symbol(name))
		if assert.Equal(t, lisp.LFun, fun.Type) {
			assert.NotEqual(t, "", fun.Docstring())
		}
	}

	rc = env.LoadString("test.lisp", `
	(defun const-string1 () "abc")
	(defun const-string2 () "abc" "")
	`)
	require.True(t, rc.IsNil())

	lisp1 := env.Get(lisp.Symbol("const-string1"))
	if assert.Equal(t, lisp.LFun, lisp1.Type) {
		assert.Equal(t, "", lisp1.Docstring())
	}
	lisp2 := env.Get(lisp.Symbol("const-string2"))
	if assert.Equal(t, lisp.LFun, lisp2.Type) {
		assert.Equal(t, "abc", lisp2.Docstring())
	}
}
