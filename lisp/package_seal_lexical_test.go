// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The core package's seal protects package bindings.  A lexical binding is
// not one: it belongs to the let, lambda or handler frame that introduced it,
// so writing to it can never reach the sealed namespace -- even when the name
// happens to shadow a core symbol and even when the code runs in package lisp.
func TestLispPackageSealAllowsLexicalUpdate(t *testing.T) {
	for _, tc := range []struct{ source, result string }{
		{`(in-package 'lisp) (let ((x 1)) (set! x 2) x)`, "2"},
		{`(in-package 'lisp) (let ((x 1)) (set! 'x 2) x)`, "2"},
		{`(in-package 'lisp) (let ((car 1)) (set! car 2) car)`, "2"},
		{`(in-package 'lisp) ((lambda (x) (set! x 5) x) 1)`, "5"},
		{`(in-package 'lisp) (let* ((x 1) (y 2)) (let ((x 3)) (set! x 4)) x)`, "1"},
		{`(let ((car 1)) (set! car 2) car)`, "2"},
	} {
		t.Run(tc.source, func(t *testing.T) {
			env := templateTestEnv(t)
			got := env.LoadString("lexical.lisp", tc.source)
			require.NotEqual(t, lisp.LError, got.Type, "got %v", got)
			assert.Equal(t, tc.result, got.String())
		})
	}
}

// The package half of the same write is still refused: only a write that
// actually reaches a sealed package's symbols trips the seal.
func TestLispPackageSealStillRefusesPackageUpdate(t *testing.T) {
	for _, source := range []string{
		`(in-package 'lisp) (set! 'if 1)`,
		`(in-package 'lisp) (set! if 1)`,
		`(set! 'lisp:if 1)`,
		`(in-package 'lisp) (let ((x 1)) (set! 'lisp:if 1))`,
		`(in-package 'lisp) (let ((if 1)) (set! 'lisp:if 2))`,
	} {
		t.Run(source, func(t *testing.T) {
			env := templateTestEnv(t)
			got := env.LoadString("seal.lisp", source)
			require.Equal(t, lisp.LError, got.Type, "got %v", got)
			assert.Contains(t, got.String(), "cannot rebind lisp package binding: if")
			assert.Equal(t, "'a", env.LoadString("next.lisp",
				`(in-package 'mypkg) (use-package 'lisp) (if true 'a 'b)`).String())
		})
	}
}
