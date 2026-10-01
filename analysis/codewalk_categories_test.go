// Copyright © 2026 The ELPS authors

package analysis

import (
	"encoding/json"
	"os"
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestCodeWalkScopeCategoriesGolden(t *testing.T) {
	sources := []string{
		`(let ((x outer) (y (lambda () x))) (flet ((f (p) x)) (labels ((g () (f x))) (g))))`,
		`(macrolet ((m () x)) (m x)) (test "name" (lambda () x)) (test-let "name" ((x outer)) x)`,
		`(quote (lambda (x) x)) (quasiquote (lambda (x) (unquote (lambda (y) y)) (quasiquote (unquote x))))`,
	}
	names := []string{"defun", "defmacro", "deftype", "test-let", "test-let*"}
	for _, op := range lisp.DefaultSpecialOps() {
		names = append(names, op.Name())
	}
	sort.Strings(names)
	for _, name := range names {
		for _, prefix := range []string{"", "lisp:"} {
			for _, args := range []string{"", " x", " () (lambda () body)", " bad (lambda (x) x)", " ((f () (lambda () body)) (1 () malformed)) (lambda () tail)"} {
				sources = append(sources, "("+prefix+name+args+")")
			}
		}
	}
	var snapshots []string
	for _, source := range sources {
		snapshots = append(snapshots, source+"\n"+analysisSnapshot(parseAndAnalyzeWithConfig(t, source, &Config{})))
	}
	data, err := json.MarshalIndent(snapshots, "", "  ")
	require.NoError(t, err)
	data = append(data, '\n')
	path := "testdata/codewalk-categories.golden.json"
	if os.Getenv("ELPS_UPDATE_CODEWALK_GOLDEN") == "1" {
		require.NoError(t, os.WriteFile(path, data, 0o600))
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	require.Equal(t, string(want), string(data))
}
