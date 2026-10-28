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
	methods := prescan.RecordMethods["order"]
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
	assert.Len(t, prescan.RecordMethods, 1)
}

// TestRecordMethodsReachResult checks that Analyze copies the workspace
// methods to the result, where the lint checks read them.
func TestRecordMethodsReachResult(t *testing.T) {
	m := RecordMethods(nil).Merge(ScanRecordMethods(parsePreamble(t, `(s:defmethod order size (o) 0)`)))
	res := parseAndAnalyzeWithConfig(t, `(+ 1 2)`, &Config{RecordMethods: m})
	assert.Contains(t, res.RecordMethods["order"], "size")

	assert.Nil(t, RecordMethods(nil).Merge(nil))
}
