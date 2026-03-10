// Copyright © 2024 The ELPS authors

package perf

import (
	"fmt"
	"sort"
	"strings"
)

// Solve performs Pass 3: cycle detection, topological sort, score and
// scaling propagation, and issue generation. It returns the solved
// function data and any performance issues found.
func Solve(graph *CallGraph, cfg *Config) ([]*SolvedFunction, []Issue) {
	// Build adjacency list
	adj := make(map[string][]string)
	for _, edge := range graph.Edges {
		if _, ok := graph.Functions[edge.Callee]; ok {
			adj[edge.Caller] = append(adj[edge.Caller], edge.Callee)
		}
	}

	// Detect cycles using Tarjan's algorithm
	cycles := findCycles(adj, graph.Functions)
	cycleMembers := make(map[string][]string) // func -> cycle it belongs to
	for _, cycle := range cycles {
		for _, name := range cycle {
			cycleMembers[name] = cycle
		}
	}

	// Topological sort (ignoring back edges from cycles)
	order := topoSort(adj, graph.Functions, cycleMembers)

	// Propagate scores in reverse topological order (leaves first)
	solved := make(map[string]*SolvedFunction, len(graph.Functions))
	for _, name := range order {
		fn := graph.Functions[name]
		if fn == nil {
			continue
		}
		sf := &SolvedFunction{
			Name:      name,
			Source:    fn.Source,
			File:     fn.File,
			LocalCost: fn.LocalCost,
			TotalScore: fn.LocalCost,
			InCycle:   cycleMembers[name] != nil,
		}
		solved[name] = sf
	}

	// Propagate in reverse order (callees before callers)
	for i := len(order) - 1; i >= 0; i-- {
		name := order[i]
		sf := solved[name]
		if sf == nil {
			continue
		}
		fn := graph.Functions[name]
		if fn == nil {
			continue
		}

		for _, edge := range fn.Calls {
			callee := solved[edge.Callee]
			if callee == nil {
				continue
			}
			// Propagate cost with loop amplification
			contribution := callee.TotalScore
			for range edge.Context.LoopDepth {
				contribution *= cfg.LoopMultiplier
			}
			sf.TotalScore += contribution

			// Propagate scaling order
			calleeOrder := callee.ScalingOrder + edge.Context.LoopDepth
			if calleeOrder > sf.ScalingOrder {
				sf.ScalingOrder = calleeOrder
			}
		}

		// Cap recursive function scaling
		if sf.InCycle && sf.ScalingOrder > cfg.MaxRecursionOrder {
			sf.ScalingOrder = cfg.MaxRecursionOrder
		}
	}

	// Generate issues
	var issues []Issue

	for _, name := range order {
		sf := solved[name]
		fn := graph.Functions[name]
		if sf == nil || fn == nil || fn.Suppressed {
			continue
		}

		// PERF001: Hot path
		if sf.TotalScore > cfg.MaxScore {
			issues = append(issues, Issue{
				Rule:     PERF001,
				Severity: SeverityWarning,
				Message:  fmt.Sprintf("hot path: total score %d exceeds threshold %d", sf.TotalScore, cfg.MaxScore),
				Function: name,
				Source:   fn.Source,
				File:    fn.File,
			})
		}

		// PERF002: Scaling risk
		if sf.ScalingOrder >= cfg.ScalingErrorThreshold {
			issues = append(issues, Issue{
				Rule:     PERF002,
				Severity: SeverityError,
				Message:  fmt.Sprintf("scaling risk: O(N^%d) complexity", sf.ScalingOrder),
				Function: name,
				Source:   fn.Source,
				File:    fn.File,
			})
		} else if sf.ScalingOrder >= cfg.MaxAcceptableOrder {
			issues = append(issues, Issue{
				Rule:     PERF002,
				Severity: SeverityWarning,
				Message:  fmt.Sprintf("scaling risk: O(N^%d) complexity", sf.ScalingOrder),
				Function: name,
				Source:   fn.Source,
				File:    fn.File,
			})
		}

		// PERF003: Expensive call in loop
		for _, edge := range fn.Calls {
			if edge.IsExpensive && edge.Context.InLoop {
				details := []string{
					fmt.Sprintf("expensive function %q called at loop depth %d", edge.Callee, edge.Context.LoopDepth),
				}
				issues = append(issues, Issue{
					Rule:     PERF003,
					Severity: SeverityWarning,
					Message:  fmt.Sprintf("expensive call %q inside loop (depth %d)", edge.Callee, edge.Context.LoopDepth),
					Function: name,
					Source:   edge.Source,
					File:    fn.File,
					Details: details,
				})
			}
		}

		// PERF004: Recursive cycle
		if sf.InCycle {
			cycle := cycleMembers[name]
			// Only report once per cycle (on the first member alphabetically)
			sorted := make([]string, len(cycle))
			copy(sorted, cycle)
			sort.Strings(sorted)
			if sorted[0] == name {
				issues = append(issues, Issue{
					Rule:     PERF004,
					Severity: SeverityWarning,
					Message:  fmt.Sprintf("recursive cycle: %s", strings.Join(sorted, " -> ")),
					Function: name,
					Source:   fn.Source,
					File:    fn.File,
					Details: sorted,
				})
			}
		}

		// UNKNOWN001: Dynamic dispatch
		for _, edge := range fn.Calls {
			if edge.Callee == "<dynamic>" {
				issues = append(issues, Issue{
					Rule:     UNKNOWN001,
					Severity: SeverityInfo,
					Message:  "dynamic dispatch: callee cannot be statically resolved",
					Function: name,
					Source:   edge.Source,
					File:    fn.File,
				})
			}
		}
	}

	// Sort issues by file, then line
	sort.Slice(issues, func(i, j int) bool {
		if issues[i].File != issues[j].File {
			return issues[i].File < issues[j].File
		}
		li, lj := 0, 0
		if issues[i].Source != nil {
			li = issues[i].Source.Line
		}
		if issues[j].Source != nil {
			lj = issues[j].Source.Line
		}
		return li < lj
	})

	var solvedList []*SolvedFunction
	for _, name := range order {
		if sf := solved[name]; sf != nil {
			solvedList = append(solvedList, sf)
		}
	}
	return solvedList, issues
}

// findCycles uses Tarjan's algorithm to find strongly connected components
// with more than one member (i.e., recursive cycles).
func findCycles(adj map[string][]string, funcs map[string]*FunctionSummary) [][]string {
	var (
		index    int
		stack    []string
		onStack  = make(map[string]bool)
		indices  = make(map[string]int)
		lowlinks = make(map[string]int)
		visited  = make(map[string]bool)
		cycles   [][]string
	)

	var strongconnect func(v string)
	strongconnect = func(v string) {
		indices[v] = index
		lowlinks[v] = index
		index++
		visited[v] = true
		stack = append(stack, v)
		onStack[v] = true

		for _, w := range adj[v] {
			if !visited[w] {
				strongconnect(w)
				if lowlinks[w] < lowlinks[v] {
					lowlinks[v] = lowlinks[w]
				}
			} else if onStack[w] {
				if indices[w] < lowlinks[v] {
					lowlinks[v] = indices[w]
				}
			}
		}

		// Root of SCC
		if lowlinks[v] == indices[v] {
			var scc []string
			for {
				w := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				onStack[w] = false
				scc = append(scc, w)
				if w == v {
					break
				}
			}
			// Only report cycles (SCCs with > 1 member) or self-loops
			if len(scc) > 1 {
				cycles = append(cycles, scc)
			} else if len(scc) == 1 {
				// Check for self-loop
				for _, w := range adj[scc[0]] {
					if w == scc[0] {
						cycles = append(cycles, scc)
						break
					}
				}
			}
		}
	}

	for name := range funcs {
		if !visited[name] {
			strongconnect(name)
		}
	}
	return cycles
}

// topoSort returns a topological ordering of functions, with callers
// before callees. Cycle edges are ignored.
func topoSort(adj map[string][]string, funcs map[string]*FunctionSummary, cycleMembers map[string][]string) []string {
	visited := make(map[string]bool)
	inProgress := make(map[string]bool)
	var order []string

	var visit func(string)
	visit = func(name string) {
		if visited[name] {
			return
		}
		if inProgress[name] {
			return // cycle — skip back edge
		}
		inProgress[name] = true

		for _, callee := range adj[name] {
			// Skip back edges within the same cycle to avoid infinite recursion
			if cycleMembers[name] != nil && cycleMembers[callee] != nil {
				sameCycle := false
				for _, m := range cycleMembers[name] {
					if m == callee {
						sameCycle = true
						break
					}
				}
				if sameCycle {
					continue
				}
			}
			visit(callee)
		}

		visited[name] = true
		delete(inProgress, name)
		order = append(order, name)
	}

	// Sort function names for deterministic output
	names := make([]string, 0, len(funcs))
	for name := range funcs {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		visit(name)
	}

	// Reverse: callers first
	for i, j := 0, len(order)-1; i < j; i, j = i+1, j-1 {
		order[i], order[j] = order[j], order[i]
	}
	return order
}
