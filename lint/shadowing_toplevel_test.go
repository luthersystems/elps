// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #736: a top-level defun, defmacro or set that shadows a core lisp
// name is a warning -- the same severity as a local binding that hides a
// callable -- including the names that joined lisp in #736 (help, test,
// benchmark, test-let, test-let*, benchmark-simple).  builtin-shadowing
// reports it; "; nolint:shadowing" suppresses it, like local shadowing.

func lintAllDefault(t *testing.T, source string) []Diagnostic {
	t.Helper()
	l := &Linter{Analyzers: DefaultAnalyzers()}
	diags, err := l.LintFileWithAnalysis([]byte(source), "test.lisp", nil)
	require.NoError(t, err)
	return diags
}

func shadowDiags(diags []Diagnostic) []Diagnostic {
	var out []Diagnostic
	for _, d := range diags {
		if d.Analyzer == "builtin-shadowing" || d.Analyzer == "shadowing" || d.Analyzer == "unused-nolint" {
			out = append(out, d)
		}
	}
	return out
}

func TestTopLevelShadowingOfCoreNames(t *testing.T) {
	for _, src := range []string{
		`(defun test () 1)`,
		`(defun help (x) x)`,
		`(defmacro benchmark (x) x)`,
		`(set 'test-let 1)`,
		`(defun benchmark-simple () 1)`,
		`(defun test-let* () 1)`,
		`(in-package 'app) (defun test (x) x)`,
		`(defun map (f l) l)`,
	} {
		t.Run(src, func(t *testing.T) {
			diags := shadowDiags(lintAllDefault(t, src))
			require.Len(t, diags, 1, "%v", diags)
			assert.Equal(t, "builtin-shadowing", diags[0].Analyzer)
			assert.Equal(t, SeverityWarning, diags[0].Severity)
			assert.Contains(t, diags[0].Message, "shadows lisp")
		})
	}
}

func TestTopLevelShadowingNolint(t *testing.T) {
	for _, src := range []string{
		`(defun test () 1) ; nolint:shadowing`,
		`(defun test () 1) ; nolint:builtin-shadowing`,
		`(defun test () 1) ; nolint`,
		`(set 'help 1) ; nolint:shadowing`,
		`(let ((car 1)) car) ; nolint:shadowing`,
	} {
		t.Run(src, func(t *testing.T) {
			assert.Empty(t, shadowDiags(lintAllDefault(t, src)))
		})
	}
	// Syntactic runs (no --workspace) honour the family name too.
	for _, src := range []string{
		`(defun test () 1) ; nolint:shadowing`,
		`(defun help (x) x) ; nolint:shadowing`,
	} {
		t.Run("syntactic "+src, func(t *testing.T) {
			assert.Empty(t, shadowDiags(lintSource(t, src)))
		})
	}
	// The family name does not suppress anything else.
	diags := lintAllDefault(t, "(defun test () 1) ; nolint:set-usage")
	var names []string
	for _, d := range diags {
		names = append(names, d.Analyzer)
	}
	assert.Contains(t, names, "builtin-shadowing")
}

func TestTopLevelShadowingNegative(t *testing.T) {
	for _, src := range []string{
		`(defun my-test () 1)`,
		`(defun f () (test "x" 1))`,
		`(set 'lisp:help 1) ; nolint:lisp-package-seal`,
	} {
		t.Run(src, func(t *testing.T) {
			for _, d := range shadowDiags(lintAllDefault(t, src)) {
				t.Errorf("unexpected %s: %s", d.Analyzer, d.Message)
			}
		})
	}
}

func TestDirectiveSuppresses(t *testing.T) {
	assert.True(t, directiveSuppresses("shadowing", "shadowing"))
	assert.True(t, directiveSuppresses("shadowing", "builtin-shadowing"))
	assert.True(t, directiveSuppresses("builtin-shadowing", "builtin-shadowing"))
	assert.False(t, directiveSuppresses("builtin-shadowing", "shadowing"))
	assert.False(t, directiveSuppresses("shadowing", "set-usage"))
}
