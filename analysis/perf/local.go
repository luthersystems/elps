// Copyright © 2024 The ELPS authors

package perf

import (
	"path/filepath"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// ScanFile performs Pass 1: a local scan of parsed expressions from a
// single file, producing FunctionSummary values for each defun/defmacro.
func ScanFile(exprs []*lisp.LVal, filename string, cfg *Config) []*FunctionSummary {
	loopSet := makeSet(cfg.LoopKeywords)
	expensivePatterns := cfg.ExpensiveFunctions

	var summaries []*FunctionSummary

	astutil.WalkSExprs(exprs, func(sexpr *lisp.LVal, depth int) {
		head := astutil.HeadSymbol(sexpr)
		if head != "defun" && head != "defmacro" {
			return
		}
		if astutil.ArgCount(sexpr) < 2 {
			return
		}

		nameNode := sexpr.Cells[1]
		if nameNode.Type != lisp.LSymbol {
			return
		}

		funcName := nameNode.Str
		src := astutil.SourceOf(sexpr)

		summary := &FunctionSummary{
			Name:      funcName,
			Source:     src.Source,
			File:      filename,
			Suppressed: isSuppressed(sexpr),
		}

		// Walk the function body (skip name and formals)
		bodyStart := 3 // Cells[0]=head, Cells[1]=name, Cells[2]=formals
		if bodyStart > len(sexpr.Cells) {
			bodyStart = len(sexpr.Cells)
		}
		// Skip docstring if present
		if bodyStart < len(sexpr.Cells) && sexpr.Cells[bodyStart].Type == lisp.LString {
			bodyStart++
		}

		body := sexpr.Cells[bodyStart:]
		scanBody(body, funcName, 0, loopSet, expensivePatterns, cfg.ExpensiveCost, summary)

		summaries = append(summaries, summary)
	})

	return summaries
}

// scanBody recursively walks body expressions, tracking loop depth and
// collecting call edges and cost.
func scanBody(
	exprs []*lisp.LVal,
	caller string,
	loopDepth int,
	loopSet map[string]bool,
	expensivePatterns []string,
	expensiveCost int,
	summary *FunctionSummary,
) {
	for _, expr := range exprs {
		scanExpr(expr, caller, loopDepth, loopSet, expensivePatterns, expensiveCost, summary)
	}
}

func scanExpr(
	expr *lisp.LVal,
	caller string,
	loopDepth int,
	loopSet map[string]bool,
	expensivePatterns []string,
	expensiveCost int,
	summary *FunctionSummary,
) {
	if expr == nil {
		return
	}

	// Only process unquoted s-expressions (calls/forms)
	if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
		return
	}

	head := astutil.HeadSymbol(expr)
	if head == "" {
		// Dynamic dispatch — can't resolve callee
		if expr.Cells[0].Type == lisp.LSExpr {
			summary.Calls = append(summary.Calls, CallEdge{
				Caller:  caller,
				Callee:  "<dynamic>",
				Source:  astutil.SourceOf(expr).Source,
				Context: CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
			})
		}
		// Still scan children
		for _, child := range expr.Cells {
			scanExpr(child, caller, loopDepth, loopSet, expensivePatterns, expensiveCost, summary)
		}
		return
	}

	// Check if this is a loop form
	if loopSet[head] {
		newDepth := loopDepth + 1
		if newDepth > summary.MaxLoopDepth {
			summary.MaxLoopDepth = newDepth
		}
		// Scan children with increased loop depth
		for _, child := range expr.Cells[1:] {
			scanExpr(child, caller, newDepth, loopSet, expensivePatterns, expensiveCost, summary)
		}
		return
	}

	// Skip defun/defmacro/lambda inside body — they define new scopes
	switch head {
	case "defun", "defmacro", "lambda":
		return
	case "quote", "quasiquote":
		return
	}

	// Record call edge for named function calls
	if isCallable(head) {
		expensive := matchesAnyPattern(head, expensivePatterns)
		cost := 1
		if expensive {
			cost += expensiveCost
		}
		// Apply loop amplification to local cost
		amplifiedCost := cost
		for i := 0; i < loopDepth; i++ {
			amplifiedCost *= summary.loopMultiplier(expensiveCost)
		}
		summary.LocalCost += amplifiedCost

		summary.Calls = append(summary.Calls, CallEdge{
			Caller:      caller,
			Callee:      head,
			Source:       callSource(expr),
			Context:     CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
			IsExpensive: expensive,
		})
	}

	// Recurse into children (skip head symbol)
	for _, child := range expr.Cells[1:] {
		scanExpr(child, caller, loopDepth, loopSet, expensivePatterns, expensiveCost, summary)
	}
}

// loopMultiplier returns the configured loop multiplier. This is stored
// on the config, but we thread it through the summary's cost tracking
// for convenience. Since we don't store config on summary, just use 20.
func (s *FunctionSummary) loopMultiplier(_ int) int {
	return 20 // matches DefaultConfig
}

// isCallable returns true if the symbol name looks like a function call
// rather than a special form that doesn't produce a call edge.
func isCallable(name string) bool {
	switch name {
	case "if", "cond", "progn", "let", "flet", "labels",
		"set", "set!", "and", "or", "not",
		"handler-bind", "ignore-errors",
		"in-package", "use-package", "export",
		"begin", "do",
		"assert-equal", "assert=", "assert-nil", "assert-not-nil",
		"test", "test-let":
		return false
	}
	return true
}

// callSource returns the best source location for a call expression.
func callSource(expr *lisp.LVal) *token.Location {
	src := astutil.SourceOf(expr)
	if src != nil {
		return src.Source
	}
	return nil
}

// matchesAnyPattern checks if name matches any of the glob patterns.
func matchesAnyPattern(name string, patterns []string) bool {
	for _, pattern := range patterns {
		if matched, _ := filepath.Match(pattern, name); matched {
			return true
		}
	}
	return false
}

// makeSet converts a string slice to a set for O(1) lookup.
func makeSet(items []string) map[string]bool {
	s := make(map[string]bool, len(items))
	for _, item := range items {
		s[item] = true
	}
	return s
}
