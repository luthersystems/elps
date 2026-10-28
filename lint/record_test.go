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
    (s:method add (o sku n)
      (s:set-field! o 'lines (assoc (o 'lines) sku n)))))
`

func TestRecordSetOutsideMethod(t *testing.T) {
	diags := lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder+`
(defun bump (o) (s:set-field! o 'lines (sorted-map)))`)
	assert.Len(t, diags, 1)
	assertDiagOnLine(t, diags, 7, "s:set-field! outside a method")

	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder))
	// A lambda in a method still sees the receiver.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, recordOrder+`
(s:defmethod order clear (o) (map () (lambda (k) (s:set-field! o 'lines (sorted-map))) '(1)))`))
	// Quoted data is not code.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, `(set 'x '(s:set-field! o 'a 1))`))
	// A let binding's value is code.
	assertHasDiag(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, `(let ([x (s:set-field! o 'a 1)]) x)`), "s:set-field! outside")
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod,
		`(defun bump (o) (s:set-field! o 'lines 1)) ; nolint:record-set-outside-method`))
}

func TestRecordTypeBinding(t *testing.T) {
	for _, src := range []string{
		recordOrder,
		// A closure around the type keeps private helpers.
		`(set 'pricer (labels ([cents (d) (* 100 d)])
  (s:make-record "pricer" (s:has-key "d" s:int) (s:method c (o) (cents (o 'd))))))`,
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
    (s:method add (o sku n) (assoc! (o 'lines) sku n))))
(s:defmethod order grow (o x) (lisp:append! (o 'items) x))`)
	assert.Len(t, diags, 2)
	assertDiagOnLine(t, diags, 4, "assoc! changes field lines")
	assertDiagOnLine(t, diags, 5, "append! changes field items")

	// A copy, or a write outside a method, is not reported here.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordFieldMutation, recordOrder))
	assertNoDiags(t, lintCheck(t, AnalyzerRecordFieldMutation, `(defun f (o) (assoc! (o 'lines) "a" 1))`))
}

// TestRecordWorkspaceNames lints record types with workspace analysis.  The
// CLI expands s:make-record and s:defmethod, so a method's receiver and
// formals are bound.  An unused receiver is an unused parameter, as any
// other: name it with a leading _.
func TestRecordWorkspaceNames(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "order.lisp")
	require.NoError(t, os.WriteFile(file, []byte(`(in-package 'shop)
(set 'order
  (s:make-record "order"
    (s:has-key "lines" s:sorted-map)
    (s:method add (o sku n)
      (s:set-field! o 'lines (assoc (o 'lines) sku n)))
    (s:method zero (_o) 0)))
(s:defmethod order size (o) (length (keys (o 'lines))))
(s:defmethod order bad (_o x) (+ x missing))
(set 'sized (s:make-interface "sized" (size ())))
(set 'both (s:make-interface "both" :embed (sized nope) (pay (amount))))
`), 0o600))
	l := &Linter{Analyzers: []*Analyzer{AnalyzerUndefinedSymbol, AnalyzerUnusedVariable}}
	diags, err := l.LintFiles(&LintConfig{Workspace: dir}, []string{file})
	require.NoError(t, err)
	require.Len(t, diags, 2, "%v", diags)
	assertDiagOnLine(t, diags, 9, "undefined symbol: missing")
	// :embed names are evaluated, so an unknown one is undefined; the
	// method specs are data.
	assertDiagOnLine(t, diags, 11, "undefined symbol: nope")
}

// TestRecordWorkspaceNamesWithStdlibExports lints with precomputed stdlib
// exports and no env, as an embedder does: the record macros still expand.
func TestRecordWorkspaceNamesWithStdlibExports(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "order.lisp")
	require.NoError(t, os.WriteFile(file, []byte(`(set 'order
  (s:make-record "order" (s:has-key "n" s:int) (s:method twice (o) (* 2 (o 'n)))))
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

func TestRecordUnknownMember(t *testing.T) {
	diags := lintCheck(t, AnalyzerRecordUnknownMember, `(set 'order
  (s:make-record "order"
    (s:has-key "lines" s:sorted-map)
    (s:method add (o sku n) (s:set-field! o 'lnies (assoc (o 'lines) sku n)))
    (s:method size (o) (length (o 'lines)))
    (s:method twice (o) (* 2 (o 'szie)))))
(s:defmethod order count (o) (o 'size))
(s:defmethod order clear (o) (s:set-field! o 'size 0))`)
	assert.Len(t, diags, 3, "%v", diags)
	assertDiagOnLine(t, diags, 4, "s:set-field! names lnies, which is not a field")
	assertDiagOnLine(t, diags, 6, "szie is not a field or method")
	assertDiagOnLine(t, diags, 8, "s:set-field! names size, which is not a field")

	// The check follows the receiver the method names.
	assertDiagOnLine(t, lintCheck(t, AnalyzerRecordUnknownMember, recordOrder+`
(s:defmethod order size (self) (length (self 'lnies)))`), 7, "lnies is not a field or method")

	// A method from a s:defmethod in the file is a member.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordUnknownMember, recordOrder+`
(s:defmethod order size (o) (length (keys (o 'lines))))
(s:defmethod order big (o) (> (o 'size) 10))`))
	// A field form that is not a literal s:has-key: the fields are unknown.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordUnknownMember, `(set 'f (s:has-key "a" s:int))
(set 'r (s:make-record "r" f (s:method m (o) (o 'b))))`))
	// A type from another file: the shape is unknown.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordUnknownMember, `(s:defmethod other m (o) (o 'whatever))`))
	// :implements and its list are an option, not a field form, so the
	// shape stays known.
	assertDiagOnLine(t, lintCheck(t, AnalyzerRecordUnknownMember, `(set 'p (s:make-interface "p" (n ())))
(set 'r (s:make-record "r" :implements (p) (s:has-key "a" s:int)
  (s:method n (o) (o 'b))))`), 3, "b is not a field or method")
}

func TestRecordCheckWrites(t *testing.T) {
	src := `(set 'counter
  (s:make-record "counter"
    (s:has-key "n" s:int)
    (s:has-key "seen" s:sorted-map)
    :check (lambda (c)
             (s:set-field! c 'n 0)
             (assoc! (c 'seen) "k" 1)
             (when (< (c 'n) 0) (error 'negative (c 'n))))))`
	diags := lintCheck(t, AnalyzerRecordCheckWrites, src)
	assert.Len(t, diags, 2, "%v", diags)
	assertDiagOnLine(t, diags, 6, "s:set-field! in a :check function")
	assertDiagOnLine(t, diags, 7, "assoc! changes field seen in a :check function")
	// The s:set-field! is reported once, not also as outside a method.
	assertNoDiags(t, lintCheck(t, AnalyzerRecordSetOutsideMethod, src))

	assertNoDiags(t, lintCheck(t, AnalyzerRecordCheckWrites, `(set 'counter
  (s:make-record "counter" (s:has-key "n" s:int)
    :check (lambda (c) (when (< (c 'n) 0) (error 'negative (c 'n))))))`))
}

func TestRecordMethodArity(t *testing.T) {
	src := `(set 'order
  (s:make-record "order"
    (s:has-key "total" s:int)
    (s:method add (o n &optional note) (s:set-field! o 'total (+ (o 'total) n)))
    (s:method all (o &rest ns) (o 'total))
    (s:method twice (o) (o 'add 1 2 3) (o 'add) (o 'total 1) (o 'all 1 2 3) (o 'add 1 "x"))))
(s:defmethod order pay (o amount) (o 'pay) (o 'pay 1))`
	diags := lintCheck(t, AnalyzerRecordMethodArity, src)
	assert.Len(t, diags, 4, "%v", diags)
	assertDiagOnLine(t, diags, 6, "method add accepts at most 2 argument(s), got 3")
	assertDiagOnLine(t, diags, 6, "method add needs at least 1 argument(s), got 0")
	assertDiagOnLine(t, diags, 6, "field total takes no arguments, got 1")
	assertDiagOnLine(t, diags, 7, "method pay needs at least 1 argument(s), got 0")
}

// TestRecordMethodArityTyped reads typed formals, [name type] (#905), as
// their names.
func TestRecordMethodArityTyped(t *testing.T) {
	src := `(set 'acct
  (s:make-record "acct"
    (s:has-key "bal" s:int)
    (s:method dep (o [amount s:int] &optional [memo s:string]) amount)
    (s:method bad (o) (o 'dep 1 2 3) (o 'dep) (o 'dep 1 "m"))))
(s:defmethod acct w (o [amt s:int]) (o 'w) (o 'w 1))`
	diags := lintCheck(t, AnalyzerRecordMethodArity, src)
	assert.Len(t, diags, 3, "%v", diags)
	assertDiagOnLine(t, diags, 5, "method dep accepts at most 2 argument(s), got 3")
	assertDiagOnLine(t, diags, 5, "method dep needs at least 1 argument(s), got 0")
	assertDiagOnLine(t, diags, 6, "method w needs at least 1 argument(s), got 0")
}

func TestRecordDefmethodType(t *testing.T) {
	src := `(set 'order (s:make-record "order" (s:has-key "n" s:int)))
(set 'label "order")
(defun make-type () order)
(s:defmethod order ok (_o) 1)
(s:defmethod 'order quoted (_o) 1)
(s:defmethod "order" str (_o) 1)
(s:defmethod label lit (_o) 1)
(s:defmethod make-type fn (_o) 1)
(s:defmethod (make-type) call (_o) 1)
(s:defmethod other-file-type far (_o) 1)`
	diags := lintCheckSemantic(t, AnalyzerRecordDefmethodType, src)
	assert.Len(t, diags, 4, "%v", diags)
	assertDiagOnLine(t, diags, 5, "a quoted value")
	assertDiagOnLine(t, diags, 6, "a literal")
	assertDiagOnLine(t, diags, 7, "label is bound to a literal, not to a record type")
	assertDiagOnLine(t, diags, 8, "make-type is a function, not a record type")
}

func TestRecordPlainSet(t *testing.T) {
	src := `(set 'order
  (s:make-record "order"
    (s:has-key "n" s:int)
    (s:method reset (o) (set! o 0) (set 'o 1) (s:set-field! o 'n 0))
    (s:method bump (o) (lisp:set! o 'n 1) nil)))
(s:defmethod order other (o x) (set! x 1) (s:set-field! o 'n x))`
	diags := lintCheck(t, AnalyzerRecordPlainSet, src)
	assert.Len(t, diags, 3, "%v", diags)
	assertDiagOnLine(t, diags, 4, "set! rebinds the local name o")
	assertDiagOnLine(t, diags, 4, "set binds the package name o")
	assertDiagOnLine(t, diags, 5, "lisp:set! does not change a record field")
}

// TestRecordShadowedReceiver checks that a nested function whose formal has
// the receiver's name is not read as the receiver.
func TestRecordShadowedReceiver(t *testing.T) {
	src := `(set 'order
  (s:make-record "order"
    (s:has-key "total" s:int)
    (s:method merge (o others)
      (map 'list (lambda (o) (assoc! (o 'lines) "k" 1) (o 'total 1) (set! o 2)) others))))`
	for _, a := range []*Analyzer{AnalyzerRecordUnknownMember, AnalyzerRecordMethodArity, AnalyzerRecordFieldMutation, AnalyzerRecordPlainSet} {
		assertNoDiags(t, lintCheck(t, a, src))
	}
}

// TestRecordSyntaxExpanderOnly checks that the linter expands the record
// macros without switching the analysis into expander mode.
func TestRecordSyntaxExpanderOnly(t *testing.T) {
	cfg, err := BuildAnalysisConfig(&LintConfig{})
	require.NoError(t, err)
	assert.Nil(t, cfg.MacroExpander)
	assert.NotNil(t, cfg.SyntaxExpander)
}
