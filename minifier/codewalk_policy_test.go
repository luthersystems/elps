// Copyright © 2026 The ELPS authors

package minifier

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestScanProgramSymbolsPreservesSyntacticPolicy(t *testing.T) {
	source := `(in-package 'p)
(defun f) (defmacro m) (deftype ty) (set 'v 1)
(lisp:export 'f 'm 'ty 'v)
(lisp:defun qualified ()) (pkg:defun library ())
(let ((x 1)) (defun nested ()))
(quote (defun data ()))
(quasiquote (unquote (defun hole ())))`
	exprs, err := rdparser.New(token.NewScanner("syntax.lisp", strings.NewReader(source))).ParseProgram()
	require.NoError(t, err)
	symbols := scanProgramSymbols(exprs, nil)
	globals, exports, packages := symbols.globals, symbols.exports, symbols.packages
	want := map[string]analysis.SymbolKind{
		"f": analysis.SymFunction, "m": analysis.SymMacro, "ty": analysis.SymType,
		"v": analysis.SymVariable, "nested": analysis.SymFunction,
	}
	got := make(map[string]analysis.SymbolKind)
	for _, sym := range globals {
		require.Equal(t, "p", sym.Package)
		require.NotNil(t, sym.Source)
		got[sym.Name] = sym.Kind
	}
	require.Equal(t, want, got)
	delete(want, "nested")
	got = make(map[string]analysis.SymbolKind)
	for _, sym := range exports["p"] {
		got[sym.Name] = sym.Kind
	}
	require.Equal(t, want, got)
	require.Equal(t, map[string]bool{"user": true, "p": true}, packages)
}
