// Copyright © 2024 The ELPS authors

package perf

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSolve_PERF003_ExpensiveInLoop(t *testing.T) {
	src := `(defun process (items) (map 'list (lambda (item) (db-put item)) items))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var perf003 []Issue
	for _, issue := range issues {
		if issue.Rule == PERF003 {
			perf003 = append(perf003, issue)
		}
	}
	require.NotEmpty(t, perf003, "expected PERF003 issue for expensive call in loop")
	assert.Contains(t, perf003[0].Message, "db-put")
	assert.NotEmpty(t, perf003[0].Fingerprint, "issue should have a fingerprint")
	assert.NotEmpty(t, perf003[0].Trace, "issue should have a trace")
}

func TestSolve_PERF004_MutualRecursion(t *testing.T) {
	src := `
(defun ping (n) (if (= n 0) true (pong (- n 1))))
(defun pong (n) (if (= n 0) true (ping (- n 1))))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	solved, issues := Solve(graph, cfg)

	// Both functions should be marked as in-cycle
	for _, sf := range solved {
		if sf.Name == "ping" || sf.Name == "pong" {
			assert.True(t, sf.InCycle, "%s should be in cycle", sf.Name)
		}
	}

	var perf004 []Issue
	for _, issue := range issues {
		if issue.Rule == PERF004 {
			perf004 = append(perf004, issue)
		}
	}
	require.NotEmpty(t, perf004, "expected PERF004 issue for recursive cycle")
	assert.NotEmpty(t, perf004[0].Trace, "cycle issue should have a trace")
}

func TestSolve_PERF004_SelfRecursion(t *testing.T) {
	src := `(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var perf004 []Issue
	for _, issue := range issues {
		if issue.Rule == PERF004 {
			perf004 = append(perf004, issue)
		}
	}
	require.NotEmpty(t, perf004, "expected PERF004 issue for self-recursion")
	assert.Contains(t, perf004[0].Message, "factorial")
}

func TestSolve_Suppressed(t *testing.T) {
	src := `
;; elps-analyze-disable
(defun hot-path (items)
  (map 'list (lambda (item) (db-put item)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	// Should have no issues because function is suppressed
	for _, issue := range issues {
		assert.NotEqual(t, "hot-path", issue.Function,
			"suppressed function should not generate issues")
	}
}

func TestSolve_ScalingPropagation(t *testing.T) {
	src := `
(defun inner () (concat 'string "x"))
(defun outer (items) (map 'list (lambda (x) (inner)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	solved, _ := Solve(graph, cfg)

	var outerSolved *SolvedFunction
	for _, sf := range solved {
		if sf.Name == "outer" {
			outerSolved = sf
			break
		}
	}
	require.NotNil(t, outerSolved)
	// outer calls inner inside a loop, so scaling order should be >= 1
	assert.GreaterOrEqual(t, outerSolved.ScalingOrder, 1)
}

func TestSolve_RuleFilter(t *testing.T) {
	// This source triggers both PERF003 and PERF004
	src := `
(defun recursive-expensive (items)
  (map 'list (lambda (item) (db-put item)) items)
  (recursive-expensive items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	cfg.Rules = []string{"PERF003"} // Only run PERF003
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	for _, issue := range issues {
		assert.Equal(t, PERF003, issue.Rule,
			"only PERF003 should fire when rules are filtered")
	}
}

func TestSolve_FingerprintStability(t *testing.T) {
	src := `(defun process (items) (map 'list (lambda (item) (db-put item)) items))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()

	// Run twice — fingerprints should be identical
	summaries1 := ScanFile(exprs, "test.lisp", cfg)
	_, issues1 := Solve(BuildGraph(summaries1), cfg)

	summaries2 := ScanFile(exprs, "test.lisp", cfg)
	_, issues2 := Solve(BuildGraph(summaries2), cfg)

	require.Equal(t, len(issues1), len(issues2))
	for i := range issues1 {
		assert.Equal(t, issues1[i].Fingerprint, issues2[i].Fingerprint,
			"fingerprints should be stable across runs")
	}
}

func TestSolve_UNKNOWN001_FuncallApply(t *testing.T) {
	src := `
(defun dispatch (f x) (funcall f x))
(defun run-all (f args) (apply f args))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var unknown []Issue
	for _, issue := range issues {
		if issue.Rule == UNKNOWN001 {
			unknown = append(unknown, issue)
		}
	}
	assert.Len(t, unknown, 2, "expected UNKNOWN001 for funcall and apply")
}
