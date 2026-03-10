// Copyright © 2024 The ELPS authors

package perf

import (
	"path/filepath"
	"runtime"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testdataDir(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	require.True(t, ok)
	return filepath.Join(filepath.Dir(file), "testdata")
}

func TestAnalyzeFiles_Basic(t *testing.T) {
	dir := testdataDir(t)
	files := []string{filepath.Join(dir, "basic.lisp")}
	result, err := AnalyzeFiles(files, nil)
	require.NoError(t, err)
	require.NotNil(t, result)

	// Should find PERF003 for expensive calls in loops
	var perf003 []Issue
	for _, issue := range result.Issues {
		if issue.Rule == PERF003 {
			perf003 = append(perf003, issue)
		}
	}
	assert.NotEmpty(t, perf003, "expected PERF003 issues in basic.lisp")
}

func TestAnalyzeFiles_Recursive(t *testing.T) {
	dir := testdataDir(t)
	files := []string{filepath.Join(dir, "recursive.lisp")}
	result, err := AnalyzeFiles(files, nil)
	require.NoError(t, err)

	var perf004 []Issue
	for _, issue := range result.Issues {
		if issue.Rule == PERF004 {
			perf004 = append(perf004, issue)
		}
	}
	// Should detect ping/pong cycle and factorial self-recursion
	assert.GreaterOrEqual(t, len(perf004), 2, "expected at least 2 PERF004 issues")
}

func TestAnalyzeFiles_Suppressed(t *testing.T) {
	dir := testdataDir(t)
	files := []string{filepath.Join(dir, "suppressed.lisp")}
	result, err := AnalyzeFiles(files, nil)
	require.NoError(t, err)

	for _, issue := range result.Issues {
		assert.NotEqual(t, "noisy-but-ok", issue.Function,
			"suppressed function should not produce issues")
	}
}

func TestAnalyzeFiles_MultiFile(t *testing.T) {
	dir := testdataDir(t)
	files := []string{
		filepath.Join(dir, "basic.lisp"),
		filepath.Join(dir, "recursive.lisp"),
	}
	result, err := AnalyzeFiles(files, nil)
	require.NoError(t, err)

	// Should see functions from both files
	assert.NotEmpty(t, result.Graph.Functions)
	assert.True(t, len(result.Graph.Functions) >= 5,
		"expected functions from both files, got %d", len(result.Graph.Functions))
}
