// Copyright © 2024 The ELPS authors

package perf

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestBuildGraph_Basic(t *testing.T) {
	summaries := []*FunctionSummary{
		{
			Name: "foo",
			File: "a.lisp",
			Calls: []CallEdge{
				{Caller: "foo", Callee: "bar"},
			},
		},
		{
			Name: "bar",
			File: "a.lisp",
			Calls: []CallEdge{
				{Caller: "bar", Callee: "baz"},
			},
		},
	}

	graph := BuildGraph(summaries)
	require.NotNil(t, graph)
	assert.Len(t, graph.Functions, 2)
	assert.Contains(t, graph.Functions, "foo")
	assert.Contains(t, graph.Functions, "bar")
	assert.Len(t, graph.Edges, 2)
}

func TestBuildGraph_Empty(t *testing.T) {
	graph := BuildGraph(nil)
	require.NotNil(t, graph)
	assert.Empty(t, graph.Functions)
	assert.Empty(t, graph.Edges)
}

func TestBuildGraph_DuplicateName(t *testing.T) {
	// When two files define the same function, last one wins
	summaries := []*FunctionSummary{
		{Name: "dup", File: "a.lisp", LocalCost: 1},
		{Name: "dup", File: "b.lisp", LocalCost: 2},
	}

	graph := BuildGraph(summaries)
	require.Len(t, graph.Functions, 1)
	assert.Equal(t, "b.lisp", graph.Functions["dup"].File)
	assert.Equal(t, 2, graph.Functions["dup"].LocalCost)
}
