// Copyright © 2026 The ELPS authors

package lint

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// match-unreachable (luthersystems/elps#905, #915): match tries its
// clauses in order, so a clause never runs when it follows a clause, with
// no :when guard, whose pattern matches every value or is the same
// pattern.

// AnalyzerMatchUnreachable reports a match clause that never runs: a
// clause after a catch-all clause, and a clause whose pattern repeats the
// pattern of an earlier clause with no :when guard.
var AnalyzerMatchUnreachable = &Analyzer{
	Name:     "match-unreachable",
	Severity: SeverityWarning,
	Doc: "Report a match clause that never runs.\n\n" +
		"match tries its clauses in order. A pattern of _ or a bare name, with no :when guard, matches every value, " +
		"so the clauses after it never run. Move the catch-all clause last. " +
		"A clause whose pattern repeats the pattern of an earlier clause with no :when guard never runs either. " +
		"Two patterns are the same when they differ only in the names they bind; _ must be the same, and each type must be the same symbol. " +
		"A pattern whose type is an expression other than a symbol is not compared, because the expression can give a new type each time.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, _ int) {
			if head := HeadSymbol(sexpr); (head != "match" && head != "lisp:match") || len(sexpr.Cells) < 3 {
				return
			}
			checkMatchClauses(pass, sexpr.Cells[2:])
		})
		return nil
	},
}

// checkMatchClauses reports the first clause after a catch-all clause, and
// each clause whose pattern key (matchPatternKey) is the key of an earlier
// clause with no :when guard.  It builds one key for each clause, so its
// cost is linear in the size of the patterns.
func checkMatchClauses(pass *Pass, clauses []*lisp.LVal) {
	var seen map[string]bool
	for i, c := range clauses {
		if i > 0 && isMatchCatchAll(clauses[i-1]) {
			reportMatchClause(pass, c, "match clause never runs: the clause before it matches every value",
				"move the catch-all clause last, or add :when to it")
			return
		}
		key, ok := matchPatternKey(c)
		if !ok {
			continue
		}
		if seen[key] {
			reportMatchClause(pass, c, "match clause never runs: an earlier clause with no :when guard has the same pattern",
				"delete this clause, or change its pattern")
			continue
		}
		if len(c.Cells) > 1 && c.Cells[1].IsSymbol(":when") {
			continue // a guarded clause can fall through to a repeat
		}
		if seen == nil {
			seen = make(map[string]bool)
		}
		seen[key] = true
	}
}

// reportMatchClause reports the match clause c.
func reportMatchClause(pass *Pass, c *lisp.LVal, msg, note string) {
	pass.Report(Diagnostic{
		Message: msg,
		Pos:     posFromSource(astutil.SourceLoc(SourceOf(c))),
		EndPos:  endPosFromNode(SourceOf(c)),
		Notes:   []string{note},
	})
}

// matchPatternKey returns a key for the pattern of the match clause c: two
// patterns with the same key match the same values.  Each name the
// pattern binds becomes one placeholder, so (list x 1) and (list y 1)
// have the same key.  _ stays _, so (list _ 1) has its own key.  Every
// other part of the pattern, a type expression too, is its source text.
// The second result is false for a clause that is not a list, for a bad
// pattern and for a macro template, whose shape is known only after
// expansion.  It is also false when a type in the pattern is an
// expression other than a symbol: match evaluates the expression each
// time it tries the pattern, so the same text can give a different type.
// A clause is a list or a bracket list, as match reads it.
func matchPatternKey(c *lisp.LVal) (string, bool) {
	if c.Type != lisp.LSExpr || len(c.Cells) == 0 {
		return "", false
	}
	p := c.Cells[0]
	if hasUnquote(p) {
		return "", false
	}
	names, err := lisp.MatchPatternNames(p)
	if err != nil {
		return "", false
	}
	// The names are the pattern's own cells, so a cell is a bound name
	// exactly when it is in this set.  A symbol in a type expression with
	// the same text is a different cell.
	bound := make(map[*lisp.LVal]bool, len(names))
	for _, n := range names {
		bound[n] = true
	}
	var b strings.Builder
	if !writePatternKey(&b, p, bound) {
		return "", false
	}
	return b.String(), true
}

// writePatternKey writes the key of the pattern p to b.  A bound name is
// the byte 0, which no source text holds.  It follows the pattern shapes
// of lisp's classifyPattern, whose rules MatchPatternNames has checked.
// It returns false for a type that is not a symbol.
func writePatternKey(b *strings.Builder, p *lisp.LVal, bound map[*lisp.LVal]bool) bool {
	if bound[p] {
		b.WriteByte(0)
		return true
	}
	if p.Type != lisp.LSExpr || len(p.Cells) == 0 {
		b.WriteString(p.String())
		return true
	}
	if p.IsQuoted() { // [name T]
		if !isTypeSymbol(p.Cells[1]) {
			return false
		}
		b.WriteString("'(")
		writePatternKey(b, p.Cells[0], bound)
		b.WriteByte(' ')
		b.WriteString(p.Cells[1].String())
		b.WriteByte(')')
		return true
	}
	head := p.Cells[0]
	switch strings.TrimPrefix(head.Str, "lisp:") {
	case "quote":
		b.WriteString(p.String()) // a datum binds no name
		return true
	case "list", "vector", "sorted-map":
	default: // (T :field p ...)
		if !isTypeSymbol(head) {
			return false
		}
	}
	b.WriteByte('(')
	b.WriteString(head.String())
	for _, q := range p.Cells[1:] {
		b.WriteByte(' ')
		if !writePatternKey(b, q, bound) {
			return false
		}
	}
	b.WriteByte(')')
	return true
}

// isTypeSymbol reports whether the type expression t of a pattern is a
// symbol, qualified or not, which gives the same type each time match
// tries the pattern.
func isTypeSymbol(t *lisp.LVal) bool {
	return t.Type == lisp.LSymbol && !t.IsQuoted()
}

// isMatchCatchAll reports whether clause c matches every value: its
// pattern is _, :else or a name to bind, and it has no :when guard.  It
// follows the symbol rules of lisp's classifyPattern: a keyword, true,
// false, an &-name or a qualified name is not a name to bind.
func isMatchCatchAll(c *lisp.LVal) bool {
	if c.Type != lisp.LSExpr || len(c.Cells) == 0 {
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
