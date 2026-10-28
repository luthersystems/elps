// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWorkspaceRecordMethods checks that the workspace index carries the
// s:defmethod methods of other files into a document's analysis.  The
// record lint checks read them there, so a method added in another file
// is not an unknown member in the editor.
func TestWorkspaceRecordMethods(t *testing.T) {
	dir := t.TempDir()
	src := "(set 'order (s:make-record \"order\" (s:method big (o) (> (o 'size) 1))))\n"
	require.NoError(t, os.WriteFile(filepath.Join(dir, "order.lisp"), []byte(src), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "methods.lisp"), []byte(`(s:defmethod order size (o) 0)
`), 0o600))

	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()

	doc := openDoc(s, pathToURI(filepath.Join(dir, "order.lisp")), src)
	s.ensureAnalysis(doc)
	require.NotNil(t, doc.analysis)
	assert.Contains(t, doc.analysis.RecordMethods["order"], "size")
}
