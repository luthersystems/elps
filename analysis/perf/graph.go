// Copyright © 2024 The ELPS authors

package perf

// BuildGraph performs Pass 2: aggregates per-function summaries into a
// CallGraph. Summaries from multiple files are merged by function name.
func BuildGraph(summaries []*FunctionSummary) *CallGraph {
	g := &CallGraph{
		Functions: make(map[string]*FunctionSummary, len(summaries)),
	}
	for _, s := range summaries {
		g.Functions[s.Name] = s
		g.Edges = append(g.Edges, s.Calls...)
	}
	return g
}
