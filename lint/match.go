// Copyright © 2026 The ELPS authors

package lint

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// match-unreachable (luthersystems/elps#905): match tries its clauses in
// order, so a clause after one whose pattern matches every value, with no
// :when guard, never runs.

// AnalyzerMatchUnreachable reports a match clause after a catch-all clause.
var AnalyzerMatchUnreachable = &Analyzer{
	Name:     "match-unreachable",
	Severity: SeverityWarning,
	Doc: "Report a match clause that follows a clause matching every value.\n\n" +
		"match tries its clauses in order. A pattern of _ or a bare name, with no :when guard, matches every value, " +
		"so the clauses after it never run. Move the catch-all clause last.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, _ int) {
			if head := HeadSymbol(sexpr); (head != "match" && head != "lisp:match") || len(sexpr.Cells) < 3 {
				return
			}
			clauses := sexpr.Cells[2:]
			for i, c := range clauses[:len(clauses)-1] {
				if !isMatchCatchAll(c) {
					continue
				}
				next := clauses[i+1]
				pass.Report(Diagnostic{
					Message: "match clause never runs: the clause before it matches every value",
					Pos:     posFromSource(astutil.SourceLoc(SourceOf(next))),
					EndPos:  endPosFromNode(SourceOf(next)),
					Notes:   []string{"move the catch-all clause last, or add :when to it"},
				})
				return
			}
		})
		return nil
	},
}

// isMatchCatchAll reports whether clause c matches every value: its
// pattern is _, :else or a name to bind, and it has no :when guard.  It
// follows the symbol rules of lisp's classifyPattern: a keyword, true,
// false, an &-name or a qualified name is not a name to bind.
func isMatchCatchAll(c *lisp.LVal) bool {
	if c.Type != lisp.LSExpr || c.IsQuoted() || len(c.Cells) == 0 {
		return false
	}
	p := c.Cells[0]
	if p.Type != lisp.LSymbol || p.IsQuoted() {
		return false
	}
	if len(c.Cells) > 1 && c.Cells[1].IsSymbol(":when") {
		return false
	}
	switch {
	case p.Str == "_" || p.Str == ":else":
		return true
	case p.Str == "" || p.Str == "true" || p.Str == "false" || strings.HasPrefix(p.Str, "&") || strings.Contains(p.Str, ":"):
		return false
	}
	return true
}
