// Copyright © 2026 The ELPS authors

package astutil_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestPackageForms(t *testing.T) {
	p := rdparser.New(token.NewScanner("forms.lisp", strings.NewReader(`
 (let ([k (progn (defun initializer () 1) 1)])
  (defun helper (x) (+ k x))
  (export 'helper))
 (labels ([local () (defmacro nested () 1)]) (local))
 '(progn (defun data () 1))
 (quote (defun explicit-data () 1))
 (quasiquote (defun template () 1))`)))
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	var heads []string
	for _, form := range astutil.PackageForms(exprs) {
		heads = append(heads, astutil.HeadSymbol(form))
	}
	require.Equal(t, []string{"let", "defun", "defun", "export", "labels", "defmacro"}, heads)
}
