// Copyright © 2026 The ELPS authors

package lint

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// rethrow-context walks code with the shared code walker, so it knows which
// parts of each form are code.
func TestRethrowContext_CodeWalker(t *testing.T) {
	for _, src := range []string{
		`'(foo (rethrow))`,                         // quoted data
		`(quote (foo (rethrow)))`,                  // quote form
		`(quasiquote (foo (rethrow)))`,             // template, not code
		`(flet ((rethrow () 1)) (rethrow))`,        // local function shadows it
		`(let ((error-stack list)) (error-stack))`, // local variable shadows it
		`(help rethrow)`,                           // data argument
	} {
		assertNoDiags(t, lintCheck(t, AnalyzerRethrowContext, src))
	}
	// The unquoted hole of a quasiquote is code.
	require.Len(t, lintCheck(t, AnalyzerRethrowContext, `(quasiquote (a (unquote (rethrow))))`), 1)
	// A qualified handler-bind is a handler-bind.
	assertNoDiags(t, lintCheck(t, AnalyzerRethrowContext,
		`(lisp:handler-bind ((condition (lambda (c &rest a) (rethrow)))) (error 'x "y"))`))
	// The body of a flet is not a function body.
	require.Len(t, lintCheck(t, AnalyzerRethrowContext, `(flet ((f () 1)) (error-stack))`), 1)
	// A flet function body is.
	assertNoDiags(t, lintCheck(t, AnalyzerRethrowContext, `(flet ((f () (error-stack))) (f))`))
}

func newRethrowEnv(t *testing.T, defs string) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	require.NotEqual(t, lisp.LError, lisplib.LoadLibrary(env).Type)
	require.NotEqual(t, lisp.LError, env.InPackage(lisp.String(lisp.DefaultUserPackage)).Type)
	res := env.LoadString("defs", defs)
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)
	return env
}

// With a macro expander, rethrow-context sees through user macros, including
// ones nested in the arguments of other calls: the analyzer expands the whole
// form, not just its head.
func TestRethrowContext_SeesThroughMacros(t *testing.T) {
	env := newRethrowEnv(t, `
(defmacro with-rethrow-handler (&rest body)
  (quasiquote
    (handler-bind ((condition (lambda (c &rest a) (unquote-splicing body))))
      (error 'boom "x"))))
(defmacro rethrow-now () (quote (rethrow)))
`)
	exp := &analysis.EnvMacroExpander{Env: env}
	l := &Linter{Analyzers: []*Analyzer{AnalyzerRethrowContext}}
	run := func(src string) []Diagnostic {
		t.Helper()
		diags, err := l.LintFileWithAnalysis([]byte(src), "test.lisp", &analysis.Config{MacroExpander: exp})
		require.NoError(t, err)
		return diags
	}

	// rethrow inside a macro that expands to handler-bind: fine.
	assertNoDiags(t, run(`(with-rethrow-handler (debug-print "x") (rethrow))`))
	// Nested inside an ordinary call, still expanded.
	assertNoDiags(t, run(`(list 1 (with-rethrow-handler (rethrow)))`))
	// Without the expander the same source is a false positive.
	require.Len(t, lintCheck(t, AnalyzerRethrowContext, `(with-rethrow-handler (rethrow))`), 1)

	// A rethrow the macro itself synthesizes has no location in this file
	// and is not reported against it; one written here is.
	assertNoDiags(t, run(`(rethrow-now)`))
	diags := run("(progn\n  (rethrow))")
	require.Len(t, diags, 1)
	assert.Equal(t, 2, diags[0].Pos.Line)
}

// A macro whose template emits a bare rethrow is reported at the template,
// once, whether or not an expander is configured; error-stack in a template
// keeps the defmacro function-body exemption.
func TestRethrowContext_MacroTemplates(t *testing.T) {
	src := "(defmacro rethrow-now ()\n  (quote (rethrow)))\n(defmacro rq () (quasiquote (progn (rethrow))))\n(defmacro es () (quote (error-stack)))\n(defmacro ok () (quasiquote (handler-bind ((condition (lambda (c &rest a) (rethrow)))) 1)))\n(rethrow-now)\n(rethrow-now)"
	diags := lintCheck(t, AnalyzerRethrowContext, src)
	require.Len(t, diags, 2)
	assert.Equal(t, 2, diags[0].Pos.Line)
	assert.Equal(t, 3, diags[1].Pos.Line)

	env := newRethrowEnv(t, strings.Join(strings.Split(src, "\n")[:5], "\n"))
	l := &Linter{Analyzers: []*Analyzer{AnalyzerRethrowContext}}
	got, err := l.LintFileWithAnalysis([]byte(src), "test.lisp",
		&analysis.Config{MacroExpander: &analysis.EnvMacroExpander{Env: env}})
	require.NoError(t, err)
	require.Len(t, got, 2, "%v", got)
}

// The test runner, not a handler, calls a test body.
func TestRethrowContext_TestBodyIsNotAHandlerFunction(t *testing.T) {
	require.Len(t, lintCheck(t, AnalyzerRethrowContext, `(test "x" (error-stack))`), 1)
}

// A macro that never finishes expanding does not hide the rest of the form.
func TestRethrowContext_NonTerminatingMacroKeepsWalking(t *testing.T) {
	env := newRethrowEnv(t, `(defmacro loop-forever () '(loop-forever))`)
	l := &Linter{Analyzers: []*Analyzer{AnalyzerRethrowContext}}
	diags, err := l.LintFileWithAnalysis([]byte("(progn (loop-forever)\n  (rethrow))"), "test.lisp",
		&analysis.Config{MacroExpander: &analysis.EnvMacroExpander{Env: env}})
	require.NoError(t, err)
	require.Len(t, diags, 1)
	assert.Equal(t, 2, diags[0].Pos.Line)
}
