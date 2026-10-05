// Copyright © 2024 The ELPS authors

package perf

import (
	"path/filepath"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
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

	visit := func(sexpr, _ *lisp.LVal, op string, _ int) bool {
		if op == codewalk.OpQuasiquote {
			return false
		}
		if sexpr.Type != lisp.LSExpr || sexpr.IsQuoted() || len(sexpr.Cells) == 0 {
			return true
		}
		head := astutil.HeadSymbol(sexpr)
		if head != op || (op != codewalk.OpDefun && op != codewalk.OpDefmacro) {
			return true
		}
		if astutil.ArgCount(sexpr) < 2 {
			return true
		}

		nameNode := sexpr.Cells[1]
		if nameNode.Type != lisp.LSymbol {
			return true
		}

		funcName := nameNode.Str
		src := astutil.SourceOf(sexpr)

		summary := &FunctionSummary{
			Name:   funcName,
			Source: astutil.SourceLoc(src),
			File:   filename,
		}
		applySuppression(summary, parseSuppression(sexpr, cfg.SuppressionPrefix))

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
		scanBody(body, funcName, bodyScan{loopDepth: 0, ctx: ctx, summary: summary})

		summaries = append(summaries, summary)
		return true
	}
	for _, expr := range exprs {
		codewalk.Syntax(expr, codewalk.SyntaxContext{Parent: nil, Depth: 0}, visit)
	}
	return summaries
}

// bodyScan holds loop context and the function summary.
type bodyScan struct {
	// loopDepth is the enclosing loop depth.
	loopDepth int
	// ctx contains traversal context.
	ctx *scanContext
	// summary receives the function summary.
	summary *FunctionSummary
}

// scanBody recursively walks body expressions, tracking loop depth and
// collecting call edges and cost.
func scanBody(exprs []*lisp.LVal, caller string, opts bodyScan) {
	loopDepth, ctx, summary := opts.loopDepth, opts.ctx, opts.summary

	s := bodyScanner{caller: caller, loopDepth: loopDepth, ctx: ctx, summary: summary}
	for _, expr := range exprs {
		s.walk(expr, loopDepth)
	}
}

type bodyScanner struct {
	ctx       *scanContext
	summary   *FunctionSummary
	caller    string
	loopDepth int
}

func (s *bodyScanner) walk(expr *lisp.LVal, loopDepth int) {
	previous := s.loopDepth
	s.loopDepth = loopDepth
	codewalk.Syntax(expr, codewalk.SyntaxContext{Parent: nil, Depth: 0}, s.scanExpr)
	s.loopDepth = previous
}

func (s *bodyScanner) scanExpr(expr, _ *lisp.LVal, op string, _ int) bool {
	caller, loopDepth, ctx, summary := s.caller, s.loopDepth, s.ctx, s.summary
	// Only process unquoted s-expressions (calls/forms)
	if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
		return false
	}

	head := astutil.HeadSymbol(expr)
	if head == "" {
		// Dynamic dispatch — can't resolve callee
		if expr.Cells[0].Type == lisp.LSExpr {
			summary.Calls = append(summary.Calls, CallEdge{
				Caller:  caller,
				Callee:  "<dynamic>",
				Source:  astutil.SourceLoc(astutil.SourceOf(expr)),
				Context: CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
			})
		}
		// Still scan children
		for _, child := range expr.Cells {
			s.walk(child, loopDepth)
		}
		return false
	}

	// Check if this is a loop form
	if ctx.loopSet[head] {
		newDepth := loopDepth + 1
		if newDepth > summary.MaxLoopDepth {
			summary.MaxLoopDepth = newDepth
		}
		// Scan children with increased loop depth
		for _, child := range expr.Cells[1:] {
			s.walk(child, newDepth)
		}
		return false
	}

	// Skip nested defun/defmacro — they define separate top-level functions
	// that will be scanned independently.
	// Only bare spellings were structural in the original cost scan.
	if head != op {
		op = ""
	}
	switch op {
	case codewalk.OpDefun, codewalk.OpDefmacro:
		return false
	case codewalk.OpQuote, codewalk.OpQuasiquote:
		return false
	case codewalk.OpLambda:
		// Lambda bodies execute inline (often as callbacks to map/foldl),
		// so scan the body at the current loop depth. Skip formals (Cells[1]).
		bodyStart := 2 // Cells[0]=head, Cells[1]=formals
		if bodyStart < len(expr.Cells) {
			for _, child := range expr.Cells[bodyStart:] {
				s.walk(child, loopDepth)
			}
		}
		return false
	}
	if head == "funcall" || head == "apply" {
		// Dynamic dispatch — callee is a runtime value.
		summary.Calls = append(summary.Calls, CallEdge{
			Caller:  caller,
			Callee:  "<dynamic>",
			Source:  callSource(expr),
			Context: CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
		})
		// Scan arguments (skip head + function arg)
		for _, child := range expr.Cells[2:] {
			s.walk(child, loopDepth)
		}
		return false
	}

	// Record call edge for named function calls (skip special forms)
	if callableOperator(head, op) {
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
			Source:      callSource(expr),
			Context:     CallContext{LoopDepth: loopDepth, InLoop: loopDepth > 0},
			IsExpensive: expensive,
		})
	}

	return true // raw children preserve the cost scan's structural-list quirks
}

// isCallable preserves the standalone classification used by drift probes.
// Production scans already have the walker's Op and use callableOperator.
func isCallable(name string) bool {
	head := lisp.LVal{Type: lisp.LSymbol, Str: name}
	cells := [1]*lisp.LVal{&head}
	form := lisp.LVal{Type: lisp.LSExpr, Cells: cells[:]}
	callable := true
	codewalk.Syntax(&form, codewalk.SyntaxContext{Parent: nil, Depth: 0}, func(_, _ *lisp.LVal, op string, _ int) bool {
		callable = callableOperator(name, op)
		return false
	})
	return callable
}

func callableOperator(name, op string) bool {
	if op == codewalk.OpUnquote || op == codewalk.OpUnquoteSplicing {
		return true // template markers are ordinary calls outside a template
	}
	if name == op && op != "" {
		// These forms are intercepted by scanExpr, which also redirects or
		// stops traversal. Historically isCallable itself returned true.
		return op == codewalk.OpLambda || op == codewalk.OpQuote || op == codewalk.OpQuasiquote
	}
	// Package-writing calls and macros outside the walker's structural
	// registry do not contribute call edges either.
	switch name {
	case "set", "in-package", "use-package", "export", "defconst",
		"curry-function", "get-default", "trace", "benchmark-simple":
		return false
	}
	return true
}

// callSource returns the best source location for a call expression.
func callSource(expr *lisp.LVal) *token.Location {
	return astutil.SourceLoc(astutil.SourceOf(expr))
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
