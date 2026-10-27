// Copyright © 2026 The ELPS authors

package lint

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const recordOrder = `(set 'order
  (s:make-record "order"
    (s:has-key "lines" s:sorted-map)
    (s:method add (sku n)
      (s:set! this 'lines (assoc (this 'lines) sku n)))))
`

func TestRecordSetOutsideMethod(t *testing.T) {
	diags := lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder+`
(defun bump (o) (s:set! o 'lines (sorted-map)))`)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 7, "s:set! outside a method")

	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder))
	// A lambda in a method still has this.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder+`
(s:defmethod order clear () (map () (lambda (k) (s:set! this 'lines (sorted-map))) '(1)))`))
	// Quoted data is not code.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, `(set 'x '(s:set! o 'a 1))`))
	// A let binding's value is code.
	assertHasDiag(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, `(let ([x (s:set! o 'a 1)]) x)`), "s:set! outside")
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod,
		`(defun bump (o) (s:set! o 'lines 1)) ; nolint:record-set-outside-method`))
}

func TestRecordTypeBinding(t *testing.T) {
	for _, src := range []string{
		recordOrder,
		// A closure around the type keeps private helpers.
		`(set 'pricer (labels ([cents (d) (* 100 d)])
  (s:make-record "pricer" (s:has-key "d" s:int) (s:method c () (cents (this 'd))))))`,
		`(set 'order (let ([v s:int]) (s:make-record "order" (s:has-key "n" v))))`,
		`(defconst order (s:make-record "order" (s:has-key "n" s:int)))`,
	} {
		assertNoDiags(t, lintCheck(t, AnalyzerRecordTypeBinding, src))
	}
	for _, src := range []string{
		`(s:make-record "loose" (s:has-key "n" s:int))`,
		`(defun mk () (s:make-record "local" (s:has-key "n" s:int)))`,
		`(set 'mk (lambda () (s:make-record "local" (s:has-key "n" s:int))))`,
		`(let () (set 'order (s:make-record "nested" (s:has-key "n" s:int))))`,
		`(set 'm (labels ([mk () (s:make-record "in-fn" (s:has-key "n" s:int))]) (mk)))`,
		`(set 'm (let ([f (lambda () (s:make-record "in-let" (s:has-key "n" s:int)))]) (f)))`,
	} {
		diags := lintCheck(t, AnalyzerRecordTypeBinding, src)
		assert.Len(t, diags, 1, src)
		assertHasDiag(t, diags, "does not reach a top-level set")
	}
}

func TestRecordFieldMutation(t *testing.T) {
	diags := lintCheck(t, AnalyzerRecordFieldMutation, `(set 'order
  (s:make-record "order"
    (s:has-key "lines" s:sorted-map)
    (s:method add (sku n) (assoc! (this 'lines) sku n))))
(s:defmethod order grow (x) (lisp:append! (this 'items) x))`)
	assert.Len(t, diags, 2)
	assertDiagOnLine(t, diags, 4, "assoc! changes field lines")
	assertDiagOnLine(t, diags, 5, "append! changes field items")

	// A copy, or a write outside a method, is not reported here.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordFieldMutation, recordOrder))
	assertNoDiags(t, lintCheck(t, AnalyzerRecordFieldMutation, `(defun f (this) (assoc! (this 'lines) "a" 1))`))
}

// TestRecordWorkspaceNames lints record types with workspace analysis.  The
// CLI expands s:make-record and s:defmethod, so this and a method's formals
// are bound, and an unused this (which the file never wrote) is not
// reported.
func TestRecordWorkspaceNames(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "order.lisp")
	require.NoError(t, os.WriteFile(file, []byte(`(in-package 'shop)
(set 'order
  (s:make-record "order"
    (s:has-key "lines" s:sorted-map)
    (s:method add (sku n)
      (s:set! this 'lines (assoc (this 'lines) sku n)))
    (s:method zero () 0)))
(s:defmethod order size () (length (keys (this 'lines))))
(s:defmethod order bad (x) (+ x missing))
`), 0o600))
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol, AnalyzerUnusedVariable}}
	diags, err := l.LintFiles(&LintConfig{Workspace: dir}, []string{file})
	require.NoError(t, err)
	require.Len(t, diags, 1, "%v", diags)
	assertDiagOnLine(t, diags, 9, "undefined symbol: missing")
}

// TestRecordWorkspaceNamesWithStdlibExports lints with precomputed stdlib
// exports and no env, as an embedder does: the record macros still expand.
func TestRecordWorkspaceNamesWithStdlibExports(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "order.lisp")
	require.NoError(t, os.WriteFile(file, []byte(`(set 'order
  (s:make-record "order" (s:has-key "n" s:int) (s:method twice () (* 2 (this 'n)))))
`), 0o600))
	env := newStdlibEnv()
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol}}
	diags, err := l.LintFiles(&LintConfig{
		Workspace:     dir,
		StdlibExports: analysis.ExtractPackageExports(env.Runtime.Registry),
	}, []string{file})
	require.NoError(t, err)
	assertNoDiags(t, diags)
}
