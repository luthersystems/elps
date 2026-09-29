// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func lintNamed(t *testing.T, analyzers []*Analyzer, filename, source string) []Diagnostic {
	t.Helper()
	l := &Linter{Analyzers: analyzers}
	diags, err := l.LintFile([]byte(source), filename)
	require.NoError(t, err)
	return diags
}

func TestTestOutsideTestFile_Positive(t *testing.T) {
	for _, src := range []string{
		`(test "t" (assert= 1 1))`,
		`(lisp:test "t" 1)`,
		`(testing:test "t" 1)`,
		`(test-let "t" ((x 1)) x)`,
		`(test-let* "t" ((x 1)) x)`,
		`(benchmark "b" (n) n)`,
		`(benchmark-simple "b" 1)`,
		`(testing:benchmark-simple "b" 1)`,
		`(defun f () (test "nested" 1))`,
	} {
		for _, file := range []string{"main.lisp", "lib/util.lisp", "common_testhelpers.lisp"} {
			t.Run(file+" "+src, func(t *testing.T) {
				diags := lintNamed(t, []*Analyzer{AnalyzerTestOutsideTestFile}, file, src)
				require.Len(t, diags, 1, "%v", diags)
				assert.Equal(t, SeverityError, diags[0].Severity)
				assert.Contains(t, diags[0].Message, "registers a test outside a _test.lisp file")
				if file == "common_testhelpers.lisp" {
					assert.Contains(t, diags[0].Notes[0], "*_testhelpers.lisp")
				}
			})
		}
	}
	// Line placement.
	diags := lintNamed(t, []*Analyzer{AnalyzerTestOutsideTestFile}, "main.lisp", "(defun f () 1)\n(test \"t\" (f))")
	assertDiagOnLine(t, diags, 2, "registers a test")
}

func TestTestOutsideTestFile_Negative(t *testing.T) {
	check := func(file, src string) {
		t.Helper()
		assertNoDiags(t, lintNamed(t, []*Analyzer{AnalyzerTestOutsideTestFile}, file, src))
	}
	// Test files may register tests.
	check("math_test.lisp", `(test "t" 1)`)
	check("dir/math_test.lisp", `(testing:benchmark-simple "b" 1)`)
	// Input without a .lisp file name (stdin) is not checked.
	check("<stdin>", `(test "t" 1)`)
	check("", `(test "t" 1)`)
	// Quoted data and quasiquote templates are not calls.
	check("main.lisp", `'(test "t" 1)`)
	check("main.lisp", `(quasiquote (test "t" (unquote x)))`)
	// A file's own definition of the name is not the core form.
	check("main.lisp", `(defun test (x) (* 2 x)) (test 4) ; nolint:builtin-shadowing`)
	// Other packages' forms of the same name, and lookalikes.
	check("main.lisp", `(other:test "t" 1)`)
	check("main.lisp", `(my-test "t" 1)`)
	check("main.lisp", `(testing:assert= 1 1)`)
}

func TestTestOutsideTestFile_Nolint(t *testing.T) {
	diags := lintNamed(t, DefaultAnalyzers(), "main.lisp", `(test "t" 1) ; nolint:test-outside-test-file`)
	for _, d := range diags {
		assert.NotEqual(t, "test-outside-test-file", d.Analyzer)
		assert.NotEqual(t, "unused-nolint", d.Analyzer, d.Message)
	}
	diags = lintNamed(t, DefaultAnalyzers(), "main.lisp", `(test "t" 1)`)
	var found bool
	for _, d := range diags {
		found = found || d.Analyzer == "test-outside-test-file"
	}
	assert.True(t, found, "reported by the default analyzers")
}
