// Copyright © 2024 The ELPS authors

package perf

import (
	"path/filepath"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// scanContext bundles the immutable configuration threaded through the scan.
type scanContext struct {
	loopSet           map[string]bool
	expensivePatterns []string
	expensiveCost     int
	loopMultiplier    int
	functionCosts     map[string]int
}

// ScanFile performs Pass 1: a local scan of parsed expressions from a
// single file, producing FunctionSummary values for each defun/defmacro.
func ScanFile(exprs []*lisp.LVal, filename string, cfg *Config) []*FunctionSummary {
	ctx := &scanContext{
		loopSet:           makeSet(cfg.LoopKeywords),
		expensivePatterns: cfg.ExpensiveFunctions,
		expensiveCost:     cfg.ExpensiveCost,
		loopMultiplier:    cfg.LoopMultiplier,
		functionCosts:     cfg.FunctionCosts,
	}

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
			Name:       funcName,
			Source:     src.Source,
			File:       filename,
			Suppressed: isSuppressed(sexpr, cfg.SuppressionPrefix),
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
		scanBody(body, funcName, 0, ctx, summary)

		summaries = append(summaries, summary)
	})

	return summaries
}

// scanBody recursively walks body expressions, tracking loop depth and
// collecting call edges and cost.
func scanBody(exprs []*lisp.LVal, caller string, loopDepth int, ctx *scanContext, summary *FunctionSummary) {
	for _, expr := range exprs {
		scanExpr(expr, caller, loopDepth, ctx, summary)
	}
}

func scanExpr(expr *lisp.LVal, caller string, loopDepth int, ctx *scanContext, summary *FunctionSummary) {
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
				Source:   astutil.SourceOf(expr).Source,
				Context: CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
			})
		}
		// Still scan children
		for _, child := range expr.Cells {
			scanExpr(child, caller, loopDepth, ctx, summary)
		}
		return
	}

	// Check if this is a loop form
	if ctx.loopSet[head] {
		newDepth := loopDepth + 1
		if newDepth > summary.MaxLoopDepth {
			summary.MaxLoopDepth = newDepth
		}
		// Scan children with increased loop depth
		for _, child := range expr.Cells[1:] {
			scanExpr(child, caller, newDepth, ctx, summary)
		}
		return
	}

	// Skip nested defun/defmacro — they define separate top-level functions
	// that will be scanned independently.
	switch head {
	case "defun", "defmacro":
		return
	case "quote", "quasiquote":
		return
	case "lambda":
		// Lambda bodies execute inline (often as callbacks to map/foldl),
		// so scan the body at the current loop depth. Skip formals (Cells[1]).
		bodyStart := 2 // Cells[0]="lambda", Cells[1]=formals
		if bodyStart < len(expr.Cells) {
			scanBody(expr.Cells[bodyStart:], caller, loopDepth, ctx, summary)
		}
		return
	case "funcall", "apply":
		// Dynamic dispatch — callee is a runtime value.
		summary.Calls = append(summary.Calls, CallEdge{
			Caller:  caller,
			Callee:  "<dynamic>",
			Source:   callSource(expr),
			Context: CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
		})
		// Scan arguments (skip head + function arg)
		for _, child := range expr.Cells[2:] {
			scanExpr(child, caller, loopDepth, ctx, summary)
		}
		return
	}

	// Record call edge for named function calls (skip special forms)
	if isCallable(head) {
		expensive := matchesAnyPattern(head, ctx.expensivePatterns)
		cost := 1
		if override, ok := ctx.functionCosts[head]; ok {
			cost = override
		}
		if expensive {
			cost += ctx.expensiveCost
		}
		// Apply loop amplification to local cost
		amplifiedCost := cost
		for range loopDepth {
			amplifiedCost *= ctx.loopMultiplier
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
		scanExpr(child, caller, loopDepth, ctx, summary)
	}
}

// isCallable returns true if the symbol name looks like a function call
// rather than a special form that doesn't produce a call edge.
func isCallable(name string) bool {
	switch name {
	case // special operators
		"if", "cond", "progn", "let", "let*", "flet", "labels", "macrolet",
		"set", "set!", "and", "or",
		"handler-bind", "ignore-errors",
		"in-package", "use-package", "export",
		"dotimes",
		"thread-first", "thread-last",
		"assert", "qualified-symbol",
		"function", "expr",
		// macros
		"defun", "defmacro", "deftype", "defconst",
		"curry-function", "get-default", "trace",
		// testing forms (not real calls)
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
