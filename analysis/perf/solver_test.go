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
	// Only reported once per cycle (on alphabetically first member)
	require.Len(t, perf004, 1, "expected exactly one PERF004 issue per cycle")
	assert.Equal(t, "ping", perf004[0].Function, "should be reported on alphabetically first member")
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
	// First verify the source WOULD produce issues without suppression
	unsuppressedSrc := `
(defun hot-path (items)
  (map 'list (lambda (item) (db-put item)) items))
`
	exprs := parseSource(t, unsuppressedSrc)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, unsuppressedIssues := Solve(graph, cfg)
	var hotPathIssues int
	for _, issue := range unsuppressedIssues {
		if issue.Function == "hot-path" {
			hotPathIssues++
		}
	}
	require.Greater(t, hotPathIssues, 0, "unsuppressed source must produce issues for this test to be valid")

	// Now verify suppression prevents those issues
	suppressedSrc := `
;; elps-analyze-disable
(defun hot-path (items)
  (map 'list (lambda (item) (db-put item)) items))
`
	exprs = parseSource(t, suppressedSrc)
	summaries = ScanFile(exprs, "test.lisp", cfg)
	graph = BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

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
	// outer calls inner inside one loop: scaling = inner(0) + 1 = 1
	assert.Equal(t, 1, outerSolved.ScalingOrder)
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

	require.NotEmpty(t, issues, "expected at least one issue with PERF003 filter")
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

func TestSolve_PERF001_HotPath(t *testing.T) {
	// Build a call chain: hot -> warm -> db-put (expensive).
	// With low MaxScore threshold, hot should trigger PERF001.
	src := `
(defun warm () (db-put "x"))
(defun hot () (warm))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	cfg.MaxScore = 10 // Low threshold so PERF001 fires
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var perf001 []Issue
	for _, issue := range issues {
		if issue.Rule == PERF001 {
			perf001 = append(perf001, issue)
		}
	}
	require.NotEmpty(t, perf001, "expected PERF001 issue for hot path")
	assert.Equal(t, SeverityWarning, perf001[0].Severity)
	assert.NotEmpty(t, perf001[0].Trace, "PERF001 should have a hot path trace")
	assert.Contains(t, perf001[0].Message, "hot path")
}

func TestSolve_PERF002_ScalingWarning(t *testing.T) {
	// leaf(0) called inside inner's map → inner scaling=1.
	// inner called inside outer's map → outer scaling=2.
	// Default MaxAcceptableOrder=2 → warning.
	src := `
(defun leaf () (concat 'string "x"))
(defun inner (items) (map 'list (lambda (x) (leaf)) items))
(defun outer (items) (map 'list (lambda (x) (inner x)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var perf002 []Issue
	for _, issue := range issues {
		if issue.Rule == PERF002 {
			perf002 = append(perf002, issue)
		}
	}
	require.NotEmpty(t, perf002, "expected PERF002 issue for O(N^2) scaling")
	// At O(N^2), which equals MaxAcceptableOrder, should be a warning
	assert.Equal(t, SeverityWarning, perf002[0].Severity)
	assert.Contains(t, perf002[0].Message, "scaling risk")
	assert.NotEmpty(t, perf002[0].Trace, "PERF002 should have a scaling trace")
}

func TestSolve_PERF002_ScalingError(t *testing.T) {
	// leaf(0) → inner(1) → mid(2) → top(3).
	// Default ScalingErrorThreshold=3 → error.
	src := `
(defun leaf () (concat 'string "x"))
(defun inner (items) (map 'list (lambda (x) (leaf)) items))
(defun mid (items) (map 'list (lambda (x) (inner x)) items))
(defun top (items) (map 'list (lambda (x) (mid x)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)
	graph := BuildGraph(summaries)
	_, issues := Solve(graph, cfg)

	var perf002Errors []Issue
	for _, issue := range issues {
		if issue.Rule == PERF002 && issue.Severity == SeverityError {
			perf002Errors = append(perf002Errors, issue)
		}
	}
	require.NotEmpty(t, perf002Errors, "expected PERF002 error for O(N^3) scaling")
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
