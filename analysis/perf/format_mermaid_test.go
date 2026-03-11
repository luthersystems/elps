// Copyright © 2024 The ELPS authors

package perf

import (
	"bytes"
	"errors"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFormatMermaid_BasicGraph(t *testing.T) {
	t.Parallel()
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"main":    {Name: "main"},
				"process": {Name: "process"},
				"save":    {Name: "save"},
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

	lines := strings.Split(strings.TrimSpace(buf.String()), "\n")

	// First line must be the flowchart header.
	require.True(t, len(lines) > 0)
	assert.Equal(t, "flowchart TD", lines[0])

	output := buf.String()

	// Verify node labels are tied to correct scores and scaling.
	assert.Contains(t, output, `"main\nscore=100 O(1)"`)
	assert.Contains(t, output, `"process\nscore=50 O(N)"`)
	assert.Contains(t, output, `"save\nscore=10 O(1)"`)

	// Verify both edges exist (node IDs are stable sorted: main=n0, process=n1, save=n2).
	assert.Contains(t, output, "n0 --> n1")
	assert.Contains(t, output, "n1 --> n2")
}

func TestFormatMermaid_LoopAnnotations(t *testing.T) {
	t.Parallel()
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
	// Verify full Mermaid edge annotation syntax with pipe delimiters.
	assert.Contains(t, output, `-->|"loop depth=2"|`)
}

func TestFormatMermaid_CycleHighlight(t *testing.T) {
	t.Parallel()
	// Mixed graph: ping<->pong cycle + ping->helper non-cycle edge.
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"helper": {Name: "helper"},
				"ping":   {Name: "ping"},
				"pong":   {Name: "pong"},
			},
			Edges: []CallEdge{
				{Caller: "ping", Callee: "pong"},
				{Caller: "pong", Callee: "ping"},
				{Caller: "ping", Callee: "helper"},
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

	// Cycle edges should use dashed arrows.
	assert.Contains(t, output, "-.->")

	// Non-cycle edge (ping->helper) should use solid arrow.
	// Sorted: helper=n0, ping=n1, pong=n2
	assert.Contains(t, output, "n1 --> n0") // ping -> helper (solid)
	assert.Contains(t, output, "n1 -.-> n2") // ping -> pong (dashed cycle)
	assert.Contains(t, output, "n2 -.-> n1") // pong -> ping (dashed cycle)
}

func TestFormatMermaid_CycleWithLoopAnnotation(t *testing.T) {
	t.Parallel()
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{
				"a": {Name: "a"},
				"b": {Name: "b"},
			},
			Edges: []CallEdge{
				{Caller: "a", Callee: "b", Context: CallContext{InLoop: true, LoopDepth: 1}},
				{Caller: "b", Callee: "a"},
			},
		},
		Issues: []Issue{
			{
				Rule:    PERF004,
				Details: []string{"a", "b"},
			},
		},
	}

	var buf bytes.Buffer
	err := FormatMermaid(&buf, result)
	require.NoError(t, err)

	output := buf.String()
	// Cycle edge in loop should get dashed arrow WITH annotation.
	assert.Contains(t, output, `-.->|"loop depth=1"|`)
}

func TestFormatMermaid_ExpensiveNodes(t *testing.T) {
	t.Parallel()
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
	// Expensive nodes use hexagon shape syntax {{}}.
	assert.Contains(t, output, "{{")
	// Regular nodes use bracket syntax [].
	assert.Contains(t, output, `["caller"`)
	// Expensive node label includes warning.
	assert.Contains(t, output, "expensive")
	// Style directive applied.
	assert.Contains(t, output, "fill:#f96")
}

func TestFormatMermaid_EmptyGraph(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name   string
		result *Result
	}{
		{"nil result", nil},
		{"empty result", &Result{}},
		{"empty graph", &Result{Graph: &CallGraph{Functions: map[string]*FunctionSummary{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			err := FormatMermaid(&buf, tc.result)
			require.NoError(t, err)
			output := strings.TrimSpace(buf.String())
			// Output should be exactly the header with no nodes or edges.
			assert.Equal(t, "flowchart TD", output)
		})
	}
}

func TestScalingString(t *testing.T) {
	t.Parallel()
	tests := []struct {
		order int
		want  string
	}{
		{0, "O(1)"},
		{1, "O(N)"},
		{2, "O(N²)"},
		{3, "O(N³)"},
		{4, "O(N^4)"},
		{10, "O(N^10)"},
	}
	for _, tt := range tests {
		assert.Equal(t, tt.want, scalingString(tt.order))
	}
}

func TestFormatMermaid_WriterError(t *testing.T) {
	t.Parallel()
	errBoom := errors.New("boom")
	fw := &failWriter{failAfter: 5, err: errBoom}
	result := &Result{
		Graph: &CallGraph{
			Functions: map[string]*FunctionSummary{"a": {Name: "a"}},
			Edges:     []CallEdge{{Caller: "a", Callee: "a"}},
		},
	}
	err := FormatMermaid(fw, result)
	assert.ErrorIs(t, err, errBoom)
}

// failWriter returns an error after failAfter bytes have been written.
type failWriter struct {
	written   int
	failAfter int
	err       error
}

func (fw *failWriter) Write(p []byte) (int, error) {
	if fw.written+len(p) > fw.failAfter {
		return 0, fw.err
	}
	fw.written += len(p)
	return len(p), nil
}
