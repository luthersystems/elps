// Copyright © 2026 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestPrescanWorkspace_RecordMethods checks that the prescan collects the
// s:defmethod calls of every file, so a method added in one file is known
// in another.
func TestPrescanWorkspace_RecordMethods(t *testing.T) {
	dir := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(dir, "a.lisp"), []byte(`(set 'order (s:make-record "order"))
(s:defmethod order size (o) 0)
(s:defmethod order clash (o x) x)
`), 0o600))
	require.NoError(t, os.WriteFile(filepath.Join(dir, "b.lisp"), []byte(`(let ([k 1])
  (s:defmethod order pay (o [amount s:int] &optional memo) (+ k amount)))
(s:defmethod order clash (o) 0)
'(s:defmethod order quoted (o) 0)
(s:defmethod (type-of x) dynamic (o) 0)
`), 0o600))

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	assert.Len(t, prescan.RecordMethodsByFile, 2)
	methods := prescan.RecordMethodsByFile.Except("")["order"]
	require.NotNil(t, methods)
	assert.Len(t, methods, 3, "%v", methods)

	require.NotNil(t, methods["size"])
	assert.Equal(t, 0, methods["size"].MinArity())
	assert.Equal(t, 0, methods["size"].MaxArity())

	// A typed formal counts as its name.
	require.NotNil(t, methods["pay"])
	assert.Equal(t, 1, methods["pay"].MinArity())
	assert.Equal(t, 2, methods["pay"].MaxArity())

	// Two arities for one method: the check cannot know which one runs.
	sig, ok := methods["clash"]
	assert.True(t, ok)
	assert.Nil(t, sig)

	// Quoted data and a type that is not a name add no method.
	assert.NotContains(t, methods, "quoted")
	assert.Len(t, prescan.RecordMethodsByFile.Except(""), 1)
}

// TestRecordMethodsQualifiedType checks that a package-qualified type name
// is keyed by its name: (s:defmethod shop:order ...) adds to order.
func TestRecordMethodsQualifiedType(t *testing.T) {
	m := ScanRecordMethods(parsePreamble(t, `(s:defmethod shop:order size (o) 0)`))
	assert.Contains(t, m["order"], "size")
	assert.NotContains(t, m, "shop:order")
	assert.Equal(t, "order", RecordTypeKey("shop:order"))
	assert.Equal(t, "order", RecordTypeKey("order"))
}

// TestPrescanWorkspace_RecordMethodsLoadScopeOnly checks that the prescan
// collects only methods a production load adds.  A method from a test
// file, or from a s:defmethod that runs at call time, is not part of the
// type a template publishes, so it must not make a member known.
func TestPrescanWorkspace_RecordMethodsLoadScopeOnly(t *testing.T) {
	dir := t.TempDir()
	for name, src := range map[string]string{
		"order.lisp":             "(s:defmethod order size (o) 0)\n(defun later () (s:defmethod order late (o) 0))\n",
		"order_test.lisp":        "(s:defmethod order from-test (o) 0)\n",
		"order_testhelpers.lisp": "(s:defmethod order from-helper (o) 0)\n",
	} {
		require.NoError(t, os.WriteFile(filepath.Join(dir, name), []byte(src), 0o600))
	}
	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	methods := prescan.RecordMethodsByFile.Except("")["order"]
	assert.Contains(t, methods, "size")
	for _, name := range []string{"late", "from-test", "from-helper"} {
		assert.NotContains(t, methods, name)
	}
	assert.Len(t, prescan.RecordMethodsByFile, 1)

	assert.Nil(t, ExtractFileRecordMethods([]byte("(s:defmethod order x (o) 0)"), "order_test.lisp"))
	assert.Nil(t, ExtractFileRecordMethods([]byte("(defun f () 1)"), "plain.lisp"))
	assert.Contains(t, ExtractFileRecordMethods([]byte("(s:defmethod order x (o) 0)"), "order.lisp")["order"], "x")
}

// TestWorkspaceRecordMethodsExceptOwnFile checks that the analysis of a
// file leaves out that file's own workspace entries: they come from the
// file on disk, and its text declares its methods itself.
func TestWorkspaceRecordMethodsExceptOwnFile(t *testing.T) {
	dir := t.TempDir()
	own := filepath.Join(dir, "order.lisp")
	other := filepath.Join(dir, "methods.lisp")
	byFile := RecordMethodsByFile{}.
		With(own, RecordMethods{"order": {"old-name": nil}}).
		With(other, RecordMethods{"order": {"size": nil}})

	res := parseAndAnalyzeWithConfig(t, `(+ 1 2)`, &Config{RecordMethodsByFile: byFile, Filename: own})
	methods := res.WorkspaceRecordMethods()["order"]
	assert.Contains(t, methods, "size")
	assert.NotContains(t, methods, "old-name")

	// With no workspace data there are no workspace methods.
	assert.Nil(t, parseAndAnalyzeWithConfig(t, `(+ 1 2)`, &Config{}).WorkspaceRecordMethods())
	assert.Nil(t, RecordMethods(nil).Merge(nil))

	// With copies: the map a Config shares does not change.
	next := byFile.With(other, nil)
	assert.Len(t, byFile, 2)
	assert.Len(t, next, 1)
}
