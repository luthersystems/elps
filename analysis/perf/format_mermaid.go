// Copyright © 2024 The ELPS authors

package perf

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

// FormatMermaid writes a Mermaid flowchart of the call graph from result.
// Nodes are labeled with function name, score, and scaling order.
// Edges show loop context annotations and cycle edges are styled distinctly.
func FormatMermaid(w io.Writer, result *Result) error {
	if result == nil || result.Graph == nil {
		fmt.Fprintln(w, "flowchart TD") //nolint:errcheck
		return nil
	}

	graph := result.Graph
	solved := buildSolvedIndex(result.Solved)
	cycleEdges := buildCycleEdgeSet(result.Issues)
	expensiveFuncs := buildExpensiveSet(result.Issues)

	// Collect all node names from edges for stable ordering.
	nodeSet := make(map[string]bool)
	for _, edge := range graph.Edges {
		nodeSet[edge.Caller] = true
		nodeSet[edge.Callee] = true
	}
	// Also include functions from the graph that may have no edges.
	for name := range graph.Functions {
		nodeSet[name] = true
	}

	nodes := make([]string, 0, len(nodeSet))
	for name := range nodeSet {
		nodes = append(nodes, name)
	}
	sort.Strings(nodes)

	// Assign stable IDs to nodes (Mermaid IDs can't contain special chars).
	nodeID := make(map[string]string, len(nodes))
	for i, name := range nodes {
		nodeID[name] = fmt.Sprintf("n%d", i)
	}

	ew := &errWriter{w: w}
	ew.printf("flowchart TD\n")

	// Emit node definitions.
	for _, name := range nodes {
		id := nodeID[name]
		label := buildNodeLabel(name, solved[name], expensiveFuncs[name])
		if expensiveFuncs[name] {
			// Hexagon shape for expensive functions.
			ew.printf("    %s{{%q}}\n", id, label)
		} else {
			ew.printf("    %s[%q]\n", id, label)
		}
	}

	// Emit edges.
	for _, edge := range graph.Edges {
		fromID := nodeID[edge.Caller]
		toID := nodeID[edge.Callee]
		edgeKey := edge.Caller + " -> " + edge.Callee
		if cycleEdges[edgeKey] {
			// Dashed edge for cycle.
			if edge.Context.InLoop {
				ew.printf("    %s -.->|%q| %s\n", fromID, fmt.Sprintf("loop depth=%d", edge.Context.LoopDepth), toID)
			} else {
				ew.printf("    %s -.-> %s\n", fromID, toID)
			}
		} else if edge.Context.InLoop {
			ew.printf("    %s -->|%q| %s\n", fromID, fmt.Sprintf("loop depth=%d", edge.Context.LoopDepth), toID)
		} else {
			ew.printf("    %s --> %s\n", fromID, toID)
		}
	}

	// Emit styles for expensive functions.
	for _, name := range nodes {
		if expensiveFuncs[name] {
			ew.printf("    style %s fill:#f96\n", nodeID[name])
		}
	}

	return ew.err
}

func buildNodeLabel(name string, sf *SolvedFunction, expensive bool) string {
	var parts []string
	parts = append(parts, name)
	if sf != nil {
		parts = append(parts, fmt.Sprintf("score=%d %s", sf.TotalScore, scalingString(sf.ScalingOrder)))
	}
	if expensive {
		parts = append(parts, "⚠️ expensive")
	}
	return strings.Join(parts, "\n")
}

func scalingString(order int) string {
	switch order {
	case 0:
		return "O(1)"
	case 1:
		return "O(N)"
	case 2:
		return "O(N²)"
	case 3:
		return "O(N³)"
	default:
		return fmt.Sprintf("O(N^%d)", order)
	}
}

func buildSolvedIndex(solved []*SolvedFunction) map[string]*SolvedFunction {
	m := make(map[string]*SolvedFunction, len(solved))
	for _, sf := range solved {
		m[sf.Name] = sf
	}
	return m
}

func buildCycleEdgeSet(issues []Issue) map[string]bool {
	set := make(map[string]bool)
	for _, issue := range issues {
		if issue.Rule != PERF004 {
			continue
		}
		// Cycle members are listed in Details; create edges between consecutive members.
		for i := 0; i < len(issue.Details)-1; i++ {
			set[issue.Details[i]+" -> "+issue.Details[i+1]] = true
		}
		// Close the cycle.
		if len(issue.Details) > 1 {
			set[issue.Details[len(issue.Details)-1]+" -> "+issue.Details[0]] = true
		}
	}
	return set
}

func buildExpensiveSet(issues []Issue) map[string]bool {
	set := make(map[string]bool)
	for _, issue := range issues {
		if issue.Rule != PERF003 {
			continue
		}
		// The trace's last entry is typically the expensive callee.
		if len(issue.Trace) > 0 {
			set[issue.Trace[len(issue.Trace)-1].Function] = true
		}
	}
	return set
}

// errWriter wraps an io.Writer and captures the first error.
type errWriter struct {
	w   io.Writer
	err error
}

func (ew *errWriter) printf(format string, args ...any) {
	if ew.err != nil {
		return
	}
	_, ew.err = fmt.Fprintf(ew.w, format, args...)
}
