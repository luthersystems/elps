package analysis

import (
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// sameSourceFile decides whether a reference's text lives in the file being
// indexed. ExtractFileRefs pairs File (the URI rename edits) with Source (the
// range rename edits), so a wrong "true" here aims an edit at the wrong file.
// These cases pin each branch against the two call sites that exist.
func TestSameSourceFile(t *testing.T) {
	t.Run("identical is the LSP call site's fast path", func(t *testing.T) {
		// lsp/server.go passes one filePath to both AnalyzeFile (which sets
		// Source.File) and ExtractFileRefs, so ordinary nodes compare equal
		// without ever reaching filepath.Abs.
		assert.True(t, sameSourceFile("/ws/a.lisp", "/ws/a.lisp"))
		assert.True(t, sameSourceFile("a.lisp", "a.lisp"))
	})

	t.Run("Abs carries the workspace-scan call site", func(t *testing.T) {
		// workspace.go analyses with a possibly-relative path but extracts
		// against its absolute form. Without normalisation every reference
		// from a relative path would be dropped and rename would silently
		// miss those files -- so this is load-bearing, not cosmetic.
		abs, err := filepath.Abs("a.lisp")
		require.NoError(t, err)
		assert.True(t, sameSourceFile("a.lisp", abs))
		assert.True(t, sameSourceFile(abs, "a.lisp"))
		assert.True(t, sameSourceFile("./a.lisp", "a.lisp"))
		assert.True(t, sameSourceFile("b/../a.lisp", "a.lisp"))
	})

	t.Run("distinct files are rejected", func(t *testing.T) {
		assert.False(t, sameSourceFile("a.lisp", "b.lisp"))
		assert.False(t, sameSourceFile("/ws/a.lisp", "/other/a.lisp"))
		// The macro-expansion case this guard was added for: a node spliced
		// in from the macro's defining file carries that file's location.
		assert.False(t, sameSourceFile("macros.lisp", "consumer.lisp"))
	})

	t.Run("empty source is deliberately permissive", func(t *testing.T) {
		// FAIL-OPEN, and the only branch that can produce a false positive.
		// Nodes without a recorded file are attributed to the file being
		// indexed; failing closed instead would drop them and break rename
		// for ordinary code. The cost is that if macro expansion ever yields
		// a node with an EMPTY Source.File rather than the defining file's
		// path, this guard cannot see it and rename could aim at the wrong
		// file. Anything that starts populating Source.File differently
		// should revisit this case first.
		assert.True(t, sameSourceFile("", "consumer.lisp"))
		assert.True(t, sameSourceFile("", "/anywhere/at/all.lisp"))
	})

	t.Run("known limits, recorded rather than asserted as correct", func(t *testing.T) {
		// Both are false negatives -- they drop a real reference, so rename
		// under-applies rather than corrupting an unrelated file. Neither is
		// reachable from the current call sites, because both paths derive
		// from the same string. They would become reachable if a caller ever
		// compared an editor-supplied path against an independently resolved
		// one.
		assert.False(t, sameSourceFile("/tmp/a.lisp", "/private/tmp/a.lisp"),
			"symlinked paths to one file are not resolved")
		assert.False(t, sameSourceFile("A.lisp", "a.lisp"),
			"comparison is case-sensitive regardless of the filesystem")
	})
}
