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
	assert.Equal(t, 2, prescan.RecordMethodsByFile.Len())
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
// collects only methods a production load adds.  A method from a
// s:defmethod that runs at call time is not part of the type a template
// publishes, so it must not make a member known.  A method from a test
// file is known only to the analysis of a test file.
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
	methods := prescan.RecordMethodsByFile.Except(filepath.Join(dir, "other.lisp"))["order"]
	assert.Contains(t, methods, "size")
	for _, name := range []string{"late", "from-test", "from-helper"} {
		assert.NotContains(t, methods, name)
	}

	// A test file knows the methods of the other test files.
	inTest := prescan.RecordMethodsByFile.Except(filepath.Join(dir, "order_test.lisp"))["order"]
	assert.Contains(t, inTest, "size")
	assert.Contains(t, inTest, "from-helper")
	assert.NotContains(t, inTest, "from-test", "a file's own entries are left out")
	assert.NotContains(t, inTest, "late")

	assert.Contains(t, ExtractFileRecordMethods([]byte("(s:defmethod order x (o) 0)"), "order_test.lisp")["order"], "x")
	assert.Nil(t, ExtractFileRecordMethods([]byte("(defun f () 1)"), "plain.lisp"))
	assert.Contains(t, ExtractFileRecordMethods([]byte("(s:defmethod order x (o) 0)"), "order.lisp")["order"], "x")
}

// TestRecordDefmethodsScope checks where RecordDefmethods says each
// s:defmethod runs.  The arguments of a function call run at load; a
// function body or a macro call defers them, and the innermost such form
// names the finding.
func TestRecordDefmethodsScope(t *testing.T) {
	for _, tc := range []struct {
		src, want string
	}{
		{"(s:defmethod order m (o) 0)", ""},
		{"(set 'zz (list (s:defmethod order m (o) 0)))", ""},
		{"(export (s:defmethod order m (o) 0))", ""},
		{"(let ([k 1]) (s:defmethod order m (o) k))", ""},
		{"(defconst y (s:defmethod order m (o) 0))", ""},
		{"(unknown-fn (s:defmethod order m (o) 0))", ""},
		{"(defun f () (s:defmethod order m (o) 0))", "defun"},
		{"(defmacro defep (&rest body) body)\n(defep (s:defmethod order m (o) 0))", "defep"},
		{"(get-default (s:defmethod order m (o) 0))", "get-default"},
		{"(get-default (lambda () (s:defmethod order m (o) 0)))", "lambda"},
		{"(defun f () (get-default (s:defmethod order m (o) 0)))", "get-default"},
		{"(s:defmethod order a (o) (s:defmethod order m (p) 0))", recordDefmethod},
		// Test bodies run after the load.
		{`(test "t" (s:defmethod order m (o) 0))`, "test"},
		{`(testing:test "t" (s:defmethod order m (o) 0))`, "testing:test"},
		{`(benchmark-simple "b" (s:defmethod order m (o) 0))`, "benchmark-simple"},
		// A test form name the file defines as a function is a function;
		// one it defines as a macro is a macro (luthersystems/elps#908).
		{"(defun test (n x) x)\n(test \"t\" (s:defmethod order m (o) 0))", ""},
		{"(set 'test-let (lambda (n b x) x))\n(test-let \"t\" () (s:defmethod order m (o) 0))", ""},
		{"(defmacro test (n &rest body) (quasiquote (progn)))\n(test \"t\" (s:defmethod order m (o) 0))", "test"},
		{"(defmacro benchmark-simple (n &rest b) ())\n(benchmark-simple \"b\" (s:defmethod order m (o) 0))", "benchmark-simple"},
		// A formal named test does not define the name for the file.
		{"(defun keep-if (test xs) xs)\n(test \"t\" (s:defmethod order m (o) 0))", "test"},
		// A local function of the name is that function.
		{`(flet ([test (n x) x]) (test "t" (s:defmethod order m (o) 0)))`, ""},
	} {
		exprs := parsePreamble(t, tc.src)
		isMacro := RecordMacroTest(nil, exprs)
		var got []RecordDefmethod
		for _, top := range exprs {
			got = append(got, RecordDefmethods(top, isMacro)...)
		}
		require.NotEmpty(t, got, tc.src)
		last := got[len(got)-1]
		assert.Equal(t, "m", last.Call.Cells[2].Str, tc.src)
		assert.Equal(t, tc.want, last.NotLoadScope, tc.src)
	}
	// With no isMacro, a test form name is the test form, and nothing else
	// defers a call but a function body.
	for _, tc := range []struct{ src, want string }{
		{`(test "t" (s:defmethod order m (o) 0))`, "test"},
		{`(lisp:test "t" (s:defmethod order m (o) 0))`, "test"},
		{`(test-let "t" ([k 1]) (s:defmethod order m (o) k))`, "test-let"},
		{`(benchmark "b" (n) (s:defmethod order m (o) n))`, "benchmark"},
		{`(flet ([test (n x) x]) (test "t" (s:defmethod order m (o) 0)))`, ""},
		{`(get-default (s:defmethod order m (o) 0))`, ""},
	} {
		got := RecordDefmethods(parsePreamble(t, tc.src)[0], nil)
		require.Len(t, got, 1, tc.src)
		assert.Equal(t, tc.want, got[0].NotLoadScope, tc.src)
	}
	// A function call's arguments run at load, so the prescan keeps the
	// method.
	m := ScanRecordMethods(parsePreamble(t, "(set 'zz (list (s:defmethod order m (o) 0)))"))
	assert.Contains(t, m["order"], "m")
}

// TestRecordMacroTestScope checks that the analysis scope decides what is
// a macro when it knows the head.
func TestRecordMacroTestScope(t *testing.T) {
	res := parseAndAnalyzeWithConfig(t, "(defmacro ep (&rest body) body)\n(defun list2 (&rest x) x)", &Config{})
	isMacro := RecordMacroTest(res.RootScope, nil)
	assert.True(t, isMacro("ep"))
	assert.False(t, isMacro("list2"))
	assert.False(t, isMacro("list"))
	assert.True(t, isMacro("defconst"))
	assert.True(t, isMacro("s:defmethod"))
	assert.False(t, isMacro("not-defined"))
}

// TestWorkspaceRecordMethodsExceptOwnFile checks that the analysis of a
// file leaves out that file's own workspace entries: they come from the
// file on disk, and its text declares its methods itself.
func TestWorkspaceRecordMethodsExceptOwnFile(t *testing.T) {
	dir := t.TempDir()
	own := filepath.Join(dir, "order.lisp")
	other := filepath.Join(dir, "methods.lisp")
	third := filepath.Join(dir, "third.lisp")
	sig := func(src string) *Signature {
		return RecordMethodSignature(parsePreamble(t, src)[0])
	}
	var byFile *RecordMethodsByFile
	byFile = byFile.
		With(own, RecordMethods{"order": {"old-name": nil, "shared": sig("(o a)")}, "own-only": {"x": nil}}).
		With(other, RecordMethods{"order": {"size": nil, "shared": sig("(o a b)")}}).
		With(third, RecordMethods{"order": {"shared": sig("(o a b)")}})

	res := parseAndAnalyzeWithConfig(t, `(+ 1 2)`, &Config{RecordMethodsByFile: byFile, Filename: own})
	all := res.WorkspaceRecordMethods()
	methods := all["order"]
	assert.Contains(t, methods, "size")
	assert.NotContains(t, methods, "old-name")
	assert.NotContains(t, all, "own-only")
	// The method the file shares is rebuilt from the other files: their
	// arities agree, so its signature is known again.
	require.NotNil(t, methods["shared"])
	assert.Equal(t, 2, methods["shared"].MinArity())

	// In the merge of every file, the arities disagree.
	merged := byFile.Except("")["order"]
	assert.Contains(t, merged, "shared")
	assert.Nil(t, merged["shared"])
	assert.Contains(t, merged, "old-name")

	// A file with no entries shares the merge.
	assert.Equal(t, merged, byFile.Except(filepath.Join(dir, "none.lisp"))["order"])

	// With no workspace data there are no workspace methods.
	assert.Nil(t, parseAndAnalyzeWithConfig(t, `(+ 1 2)`, &Config{}).WorkspaceRecordMethods())
	assert.Nil(t, RecordMethods(nil).Merge(nil))

	// With copies: the value a Config shares does not change.
	next := byFile.With(other, nil)
	assert.Equal(t, 3, byFile.Len())
	assert.Equal(t, 2, next.Len())
	assert.Contains(t, byFile.File(other)["order"], "size")
	assert.Nil(t, next.File(other))
}
