// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestTypedBuiltinsKeepTailCalls runs 200000 tail calls through funcall,
// apply, and loops whose tail position is a builtin now decoded by
// lisp.Func1/Func2 or ArgReader (string, time, base64).  None of those
// builtins calls a function, so the typed decoders cannot sit between a
// caller and its tail call; this pins that the loops still run in constant
// stack and that no marker value escapes as a result.
func TestTypedBuiltinsKeepTailCalls(t *testing.T) {
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	for src, want := range map[string]string{
		`(defun f (n) (if (= n 0) "done" (funcall f (- n 1)))) (f 200000)`:                                `"done"`,
		`(defun f (n) (if (= n 0) "done" (apply f (list (- n 1))))) (f 200000)`:                           `"done"`,
		`(defun f (n) (if (= n 0) (string:trim-space " a ") (f (- n 1)))) (f 200000)`:                     `"a"`,
		`(defun f (n s) (if (= n 0) s (f (- n 1) (string:trim s "x")))) (f 200000 "xax")`:                 `"a"`,
		`(defun f (n) (if (= n 0) (string:has-prefix? "ab" "a") (funcall f (- n 1)))) (f 200000)`:         `true`,
		`(defun f (n) (if (= n 0) (time:duration-ns (time:parse-duration "1s")) (f (- n 1)))) (f 200000)`: `1000000000`,
		`(defun f (n) (if (= n 0) (to-string (base64:encode "a")) (apply f (list (- n 1))))) (f 200000)`:  `"YQ=="`,
	} {
		v := env.LoadString("tail", src)
		require.NotEqual(t, lisp.LError, v.Type, "%s: %v", src, v)
		assert.NotContains(t, []lisp.LType{lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand}, v.Type, src)
		assert.Equal(t, want, v.String(), src)
	}
}
