// Copyright © 2024 The ELPS authors

package perf

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFormatMermaid_BasicGraph(t *testing.T) {
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"main":     {Name: "main"},
				"process":  {Name: "process"},
				"save":     {Name: "save"},
			},
			Edges: []CallEdge{
				{Caller: "main", Callee: "process"},
				{Caller: "process", Callee: "save"},
			},
		},
		Solved: []*SolvedFunction{
			{Name: "main", TotalScore: 100, ScalingOrder: 0},
			{Name: "process", TotalScore: 50, ScalingOrder: 1},
			{Name: "save", TotalScore: 10, ScalingOrder: 0},
		},
	}

	var buf bytes.Buffer
	err := FormatMermaid(&buf, result)
	require.NoError(t, err)

	output := buf.String()
	assert.Contains(t, output, "flowchart TD")
	assert.Contains(t, output, "main")
	assert.Contains(t, output, "process")
	assert.Contains(t, output, "save")
	assert.Contains(t, output, "-->")
	assert.Contains(t, output, "score=100")
	assert.Contains(t, output, "O(N)")
}

func TestFormatMermaid_LoopAnnotations(t *testing.T) {
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"outer": {Name: "outer"},
				"inner": {Name: "inner"},
			},
			Edges: []CallEdge{
				{
					Caller:  "outer",
					Callee:  "inner",
					Context: CallContext{LoopDepth: 2, InLoop: true},
				},
			},
		},
	}

	var buf bytes.Buffer
	err := FormatMermaid(&buf, result)
	require.NoError(t, err)

	output := buf.String()
	assert.Contains(t, output, "loop depth=2")
}

func TestFormatMermaid_CycleHighlight(t *testing.T) {
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"ping": {Name: "ping"},
				"pong": {Name: "pong"},
			},
			Edges: []CallEdge{
				{Caller: "ping", Callee: "pong"},
				{Caller: "pong", Callee: "ping"},
			},
		},
		Issues: []Issue{
			{
				Rule:     PERF004,
				Severity: SeverityWarning,
				Message:  "recursive cycle: ping -> pong",
				Function: "ping",
				Details:  []string{"ping", "pong"},
			},
		},
	}

	var buf bytes.Buffer
	err := FormatMermaid(&buf, result)
	require.NoError(t, err)

	output := buf.String()
	// Cycle edges should use dashed arrow syntax.
	assert.Contains(t, output, "-.->")
}

func TestFormatMermaid_ExpensiveNodes(t *testing.T) {
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"caller": {Name: "caller"},
				"db-put": {Name: "db-put"},
			},
			Edges: []CallEdge{
				{Caller: "caller", Callee: "db-put", Context: CallContext{InLoop: true, LoopDepth: 1}, IsExpensive: true},
			},
		},
		Issues: []Issue{
			{
				Rule:     PERF003,
				Severity: SeverityWarning,
				Message:  `expensive call "db-put" inside loop (depth 1)`,
				Function: "caller",
				Source:   &token.Location{File: "test.lisp", Line: 5, Col: 3},
				File:     "test.lisp",
				Trace: []TraceEntry{
					{Function: "caller", Note: "calls db-put in loop"},
					{Function: "db-put", Note: "expensive"},
				},
			},
		},
	}

	var buf bytes.Buffer
	err := FormatMermaid(&buf, result)
	require.NoError(t, err)

	output := buf.String()
	// Expensive nodes get hexagon shape and fill style.
	assert.Contains(t, output, "expensive")
	assert.Contains(t, output, "fill:#f96")
}

func TestFormatMermaid_EmptyGraph(t *testing.T) {
	// nil result
	var buf bytes.Buffer
	err := FormatMermaid(&buf, nil)
	require.NoError(t, err)
	assert.Contains(t, buf.String(), "flowchart TD")

	// empty result
	buf.Reset()
	err = FormatMermaid(&buf, &Result{})
	require.NoError(t, err)
	assert.Contains(t, buf.String(), "flowchart TD")

	// result with empty graph
	buf.Reset()
	err = FormatMermaid(&buf, &Result{Graph: &CallGraph{
		Functions: map[string]*FunctionSummary{},
	}})
	require.NoError(t, err)
	assert.Contains(t, buf.String(), "flowchart TD")
}
