// Copyright © 2026 The ELPS authors

package lint

import (
	"encoding/json"
	"os"
	"sort"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestLambdaListOccurrencesGolden(t *testing.T) {
	sources := []string{
		`(lambda () (lambda (x x) x)) (defun f () (lambda (&rest) 1))`,
		`(flet ((1 (x x) (lambda (&rest) 1)) (bad malformed (lambda (&key) 1)) (empty ())) (lambda (:x) 1))`,
		`(labels ((1 (&rest) (lambda (&key) 1)) [bracket (x x) (lambda (&optional) 1)]))`,
		`(macrolet ((m (x x) (lambda (&rest) 1)) (bad malformed (lambda (&key) 1))))`,
		`(lambda bad (lambda (&rest) 1)) (defun 1 (x x) (lambda (&key) 1))`,
		`(quote (lambda (x x) 1)) '(lambda (x x) 1) (lisp:quote (lambda (&rest) 1))`,
		`(quasiquote (unquote (lambda (x x) 1)) (quasiquote (unquote (lambda (&rest) 1))))`,
		`(defmacro m () (lambda (x x) 1) (quasiquote (unquote (lambda (x x) 1))))`,
		`(defun lambda (x y) x) (lambda (x x) 1) (lisp:lambda (x x) 1)`,
		`(let ((lambda list)) (lambda (x x) 1) (lisp:lambda (x x) 1))`,
		`(lambda (flet) (flet ((f (x x) 1)) 1)) (other:lambda (x x) 1)`,
		`(let ([entry (lambda (&rest) 1) (kf :x 1 :x 2)]) (kf :y 1 :y 2))`,
		`(lisp:handler-bind (['condition (lambda (&rest) 1)]) (lambda (&key) 1))`,
		`(cond [(lambda (&rest) 1) (lambda (&key) 1)] bad ())`,
	}
	names := []string{"defun", "defmacro", "deftype", "test-let", "test-let*"}
	for _, op := range lisp.DefaultSpecialOps() {
		names = append(names, op.Name())
	}
	sort.Strings(names)
	for _, name := range names {
		for _, prefix := range []string{"", "lisp:", "other:"} {
			for _, args := range []string{"", " ()", " (x x) (lambda (&rest) 1)", " f (x x) (lambda (&key) 1)", " ((1 (x x) (lambda (&rest) 1)) [f () (kf :x 1 :x 2)]) (lambda (&key) 1)"} {
				sources = append(sources, "("+prefix+name+args+")")
			}
		}
	}
	type snapshot struct {
		Source      string
		Calls       []string
		Diagnostics []Diagnostic
	}
	var snapshots []snapshot
	for _, source := range sources {
		s := snapshot{Source: source}
		walkLambdaListCalls(parseTestSource(t, source), func(v *lisp.LVal) { s.Calls = append(s.Calls, v.String()) })
		for _, analyzer := range []*Analyzer{AnalyzerLambdaList, AnalyzerDuplicateBinding, AnalyzerDuplicateKeyword} {
			s.Diagnostics = append(s.Diagnostics, lintCheck(t, analyzer, source)...)
		}
		snapshots = append(snapshots, s)
	}
	data, err := json.MarshalIndent(snapshots, "", "  ")
	require.NoError(t, err)
	data = append(data, '\n')
	path := "testdata/codewalk-occurrences.golden.json"
	if os.Getenv("ELPS_UPDATE_CODEWALK_GOLDEN") == "1" {
		require.NoError(t, os.MkdirAll("testdata", 0o750))
		require.NoError(t, os.WriteFile(path, data, 0o600))
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	require.Equal(t, string(want), string(data))
}
