// Copyright © 2026 The ELPS authors

package formatter_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/formatter"
	"github.com/stretchr/testify/require"
)

// TestCommentInColumnZeroIsPreserved pins the rule in printer.go's
// commentIndent: a comment written in column 0 stays in column 0, even inside
// an indented form.
//
// The rule exists because a flush-left comment can be column-aligned against
// the line ABOVE it, in which case its column carries meaning that
// re-indenting destroys. The case that forced it is
// editors/vscode/test/grammar/*.lisp, whose TextMate syntax-test assertions
// look like
//
//	(defun f (x &optional y)
//	;           ^^^^^^^^^ variable.parameter.elps
//
// where the caret column IS the assertion. `elps fmt` indented those comments
// by the form's indent, silently changing which characters they asserted
// about while appearing to succeed. TestGrammarFixturesAreFormatFixedPoints
// below is the end-to-end guard on the real files.
//
// Every case comes in a pair: the column-0 input and the same shape written
// with the comment indented. The indented half is the one that proves the
// rule keys off the comment's ORIGINAL column instead of just always writing
// column 0 -- without it, a bug that flattened every comment to the left
// margin would pass.
func TestCommentInColumnZeroIsPreserved(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "defun body/column 0 stays",
			input:    "(defun f (x)\n; c\n(+ x 1))\n",
			expected: "(defun f (x)\n; c\n  (+ x 1))\n",
		},
		{
			name:     "defun body/indented stays indented",
			input:    "(defun f (x)\n  ; c\n(+ x 1))\n",
			expected: "(defun f (x)\n  ; c\n  (+ x 1))\n",
		},
		{
			name:     "nested form/column 0 stays",
			input:    "(g (h\n; c\nx))\n",
			expected: "(g (h\n; c\n     x))\n",
		},
		{
			name:     "nested form/indented is re-indented to the form",
			input:    "(g (h\n ; c\nx))\n",
			expected: "(g (h\n     ; c\n     x))\n",
		},
		{
			// The alignment case itself, reduced. The carets must still line
			// up under `&optional` after formatting.
			name:     "alignment assertion under a defun signature",
			input:    "(defun f (x &optional y)\n;           ^^^^^^^^^ variable.parameter.elps\nx)\n",
			expected: "(defun f (x &optional y)\n;           ^^^^^^^^^ variable.parameter.elps\n  x)\n",
		},
		{
			// A top-level comment was already written at column 0, so this is
			// a control: it must be unaffected by the rule.
			name:     "top level is unchanged",
			input:    "; c\n(f x)\n",
			expected: "; c\n(f x)\n",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			out, err := formatter.Format([]byte(tt.input), formatter.DefaultConfig())
			require.NoError(t, err)
			require.Equal(t, tt.expected, string(out))

			// Formatting is a fixed point: a second pass must not move it
			// again. A rule that reads the ORIGINAL column has to survive
			// being fed its own output, where the column is now whatever it
			// just wrote.
			out2, err := formatter.Format(out, formatter.DefaultConfig())
			require.NoError(t, err)
			require.Equal(t, string(out), string(out2), "second format pass changed the output")
		})
	}
}

// TestGrammarFixturesAreFormatFixedPoints is the end-to-end guard, and the
// reason it is a test rather than a comment: editors/vscode/test/grammar/*.lisp
// are fixtures for vscode-tmgrammar-test, where `; <-` and `; ^^^` comments
// are POSITIONAL assertions about the line above. Any reflow of those lines
// changes what the fixture asserts, so the formatter must leave these files
// byte-identical.
//
// CI runs `elps fmt -l --exclude 'grammar'`, which skips these files, so
// nothing else in the build would notice a regression here -- the exclusion
// hides it. That is exactly why this guard reads the real files instead of
// trusting the CLI's exclusion list.
func TestGrammarFixturesAreFormatFixedPoints(t *testing.T) {
	dir := filepath.Join("..", "editors", "vscode", "test", "grammar")
	entries, err := filepath.Glob(filepath.Join(dir, "*.lisp"))
	require.NoError(t, err)
	require.NotEmpty(t, entries, "no grammar fixtures found at %s -- did they move?", dir)

	for _, path := range entries {
		t.Run(filepath.Base(path), func(t *testing.T) {
			// #nosec G304 -- path comes from a Glob over a fixed directory
			// literal in this test, not from input.
			src, err := os.ReadFile(path)
			require.NoError(t, err)

			out, err := formatter.Format(src, formatter.DefaultConfig())
			require.NoError(t, err)
			require.Equal(t, string(src), string(out),
				"elps fmt would rewrite %s; its `; <-` / `; ^^^` comments are "+
					"positional assertions about the line above, so any reflow "+
					"changes what the grammar test asserts", path)
		})
	}
}
