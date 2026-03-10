// Copyright © 2024 The ELPS authors

package perf

import (
	"bytes"
	"fmt"
	"os"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Result holds the complete output of a performance analysis run.
type Result struct {
	// Solved contains the propagated scores for each function.
	Solved []*SolvedFunction
	// Issues contains all performance diagnostics.
	Issues []Issue
	// Graph is the call graph used for analysis.
	Graph *CallGraph
}

// Analyze runs the full 3-pass pipeline on pre-parsed expressions from
// a single file.
func Analyze(exprs []*lisp.LVal, filename string, cfg *Config) *Result {
	if cfg == nil {
		cfg = DefaultConfig()
	}
	summaries := ScanFile(exprs, filename, cfg)
	graph := BuildGraph(summaries)
	solved, issues := Solve(graph, cfg)
	return &Result{
		Solved: solved,
		Issues: issues,
		Graph:  graph,
	}
}

// AnalyzeFiles runs the full pipeline across multiple source files,
// building a combined call graph.
func AnalyzeFiles(files []string, cfg *Config) (*Result, error) {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	var allSummaries []*FunctionSummary
	for _, path := range files {
		data, err := os.ReadFile(path) //nolint:gosec // analyzer reads user-specified files
		if err != nil {
			return nil, fmt.Errorf("%s: %w", path, err)
		}

		s := token.NewScanner(path, bytes.NewReader(data))
		p := rdparser.NewFormatting(s)
		exprs, err := p.ParseProgram()
		if err != nil {
			return nil, fmt.Errorf("%s: %w", path, err)
		}

		summaries := ScanFile(exprs, path, cfg)
		allSummaries = append(allSummaries, summaries...)
	}

	graph := BuildGraph(allSummaries)
	solved, issues := Solve(graph, cfg)
	return &Result{
		Solved: solved,
		Issues: issues,
		Graph:  graph,
	}, nil
}
