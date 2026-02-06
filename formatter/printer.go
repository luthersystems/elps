// Copyright © 2024 The ELPS authors

package formatter

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

type printer struct {
	buf   bytes.Buffer
	cfg   *Config
	col   int  // current column (0-indexed)
	atBOL bool // at beginning of line (nothing written on current line)
	first bool // first top-level form (for blank line suppression)
}

func newPrinter(cfg *Config) *printer {
	return &printer{
		cfg:   cfg,
		atBOL: true,
		first: true,
	}
}

// writeTopLevel writes a sequence of top-level expressions.
func (p *printer) writeTopLevel(exprs []*lisp.LVal, trailingComments []*token.Token) {
	for i, expr := range exprs {
		if i > 0 {
			for j := 0; j < p.blankLinesBefore(expr); j++ {
				p.newline()
			}
		}
		p.writeLeadingComments(expr, 0)
		p.writeBlankLinesAfterComments(expr)
		p.writeExpr(expr, 0)
		p.writeTrailingComment(expr)
		p.newline()
		p.first = false
	}

	// Trailing comments
	for _, c := range trailingComments {
		if !p.first {
			blankLines := 0
			if c.PrecedingNewlines > 1 {
				blankLines = c.PrecedingNewlines - 1
				if blankLines > p.cfg.MaxBlankLines {
					blankLines = p.cfg.MaxBlankLines
				}
			}
			for j := 0; j < blankLines; j++ {
				p.newline()
			}
		}
		p.writeIndent(0)
		p.writeString(c.Text)
		p.newline()
		p.first = false
	}
}

// writeLeadingComments writes comments that precede a node.
func (p *printer) writeLeadingComments(v *lisp.LVal, indent int) {
	if v.Meta == nil || len(v.Meta.LeadingComments) == 0 {
		return
	}
	for i, c := range v.Meta.LeadingComments {
		if i > 0 && c.PrecedingNewlines > 1 {
			blankLines := c.PrecedingNewlines - 1
			if blankLines > p.cfg.MaxBlankLines {
				blankLines = p.cfg.MaxBlankLines
			}
			for j := 0; j < blankLines; j++ {
				p.newline()
			}
		}
		p.writeIndent(indent)
		p.writeString(c.Text)
		p.newline()
		p.first = false
	}
}

// writeBlankLinesAfterComments emits blank lines between the last leading
// comment and the expression, if any were present in the source.
func (p *printer) writeBlankLinesAfterComments(v *lisp.LVal) {
	if v.Meta == nil {
		return
	}
	n := v.Meta.BlankLinesAfterComments
	if n > p.cfg.MaxBlankLines {
		n = p.cfg.MaxBlankLines
	}
	for i := 0; i < n; i++ {
		p.newline()
	}
}

// writeTrailingComment writes an inline comment that appeared on the same line
// as the expression, after the expression. Preserves original spacing for
// column-aligned comments.
func (p *printer) writeTrailingComment(v *lisp.LVal) {
	if v.Meta != nil && v.Meta.TrailingComment != nil {
		spaces := 1
		if v.Meta.TrailingComment.PrecedingSpaces > 1 {
			spaces = v.Meta.TrailingComment.PrecedingSpaces
		}
		for i := 0; i < spaces; i++ {
			p.buf.WriteByte(' ')
		}
		p.col += spaces
		p.writeString(v.Meta.TrailingComment.Text)
	}
}

// writeInnerTrailingComments writes comments that appeared between the last
// child and the closing bracket of an s-expression or list.
func (p *printer) writeInnerTrailingComments(v *lisp.LVal, indent int) {
	if v.Meta == nil || len(v.Meta.InnerTrailingComments) == 0 {
		return
	}
	for i, c := range v.Meta.InnerTrailingComments {
		p.newline()
		if i > 0 && c.PrecedingNewlines > 1 {
			blankLines := c.PrecedingNewlines - 1
			if blankLines > p.cfg.MaxBlankLines {
				blankLines = p.cfg.MaxBlankLines
			}
			for j := 0; j < blankLines; j++ {
				p.newline()
			}
		}
		p.writeIndent(indent)
		p.writeString(c.Text)
	}
}

// writeExpr dispatches to the appropriate printer for a node type.
func (p *printer) writeExpr(v *lisp.LVal, indent int) {
	// Handle quoting prefix for non-LQuote types
	if v.Quoted && v.Type != lisp.LQuote && v.Type != lisp.LSExpr {
		p.writeString("'")
	}
	switch v.Type {
	case lisp.LInt:
		p.writeAtom(v)
	case lisp.LFloat:
		p.writeAtom(v)
	case lisp.LString:
		p.writeStringLiteral(v)
	case lisp.LSymbol:
		p.writeString(v.Str)
	case lisp.LSExpr:
		if v.Quoted {
			// [] brackets in source are parsed as QExpr (Quoted: true), but
			// the bracket itself serves as the quote — no ' prefix needed.
			if v.Meta != nil && v.Meta.BracketType == '[' {
				p.writeListInner(v, indent)
			} else {
				p.writeString("'")
				p.writeListInner(v, indent)
			}
		} else {
			p.writeSExpr(v, indent)
		}
	case lisp.LQuote:
		p.writeQuote(v, indent)
	default:
		p.writeString(v.String())
	}
}

// writeAtom writes an integer or float, using original text if available.
func (p *printer) writeAtom(v *lisp.LVal) {
	if v.Meta != nil && v.Meta.OriginalText != "" {
		p.writeString(v.Meta.OriginalText)
		return
	}
	switch v.Type {
	case lisp.LInt:
		p.writeString(strconv.Itoa(v.Int))
	case lisp.LFloat:
		p.writeString(strconv.FormatFloat(v.Float, 'g', -1, 64))
	}
}

// writeStringLiteral writes a string, using original text if available.
func (p *printer) writeStringLiteral(v *lisp.LVal) {
	if v.Meta != nil && v.Meta.OriginalText != "" {
		p.writeString(v.Meta.OriginalText)
		return
	}
	p.writeString(fmt.Sprintf("%q", v.Str))
}

// writeQuote writes a quoted value with ' prefix.
func (p *printer) writeQuote(v *lisp.LVal, indent int) {
	if v.Type == lisp.LQuote {
		p.writeString("'")
		p.writeQuote(v.Cells[0], indent)
		return
	}
	// Single level of quoting (v.Quoted == true)
	p.writeString("'")
	p.writeExpr(v, indent)
}

// hasNewlineBefore returns true if the node was on a new line in the source.
func hasNewlineBefore(v *lisp.LVal) bool {
	if v.Meta != nil {
		return v.Meta.NewlineBefore || len(v.Meta.LeadingComments) > 0
	}
	return false
}

// blankLinesBefore returns the number of blank lines before a node,
// clamped to the configured maximum.
func (p *printer) blankLinesBefore(v *lisp.LVal) int {
	if v.Meta == nil {
		return 0
	}
	n := v.Meta.BlankLinesBefore
	if n == 0 && len(v.Meta.LeadingComments) > 0 {
		first := v.Meta.LeadingComments[0]
		if first.PrecedingNewlines > 1 {
			n = first.PrecedingNewlines - 1
		}
	}
	if n > p.cfg.MaxBlankLines {
		n = p.cfg.MaxBlankLines
	}
	return n
}

// writeSExpr writes an unquoted s-expression (function call, special form, etc).
func (p *printer) writeSExpr(v *lisp.LVal, indent int) {
	if len(v.Cells) == 0 {
		bracket := bracketOpen(v)
		p.writeString(string(bracket))
		p.writeString(string(bracketClose(bracket)))
		return
	}

	// Check for special prefix forms that should be rendered as shorthand
	if p.tryPrefixForm(v, indent) {
		return
	}

	bracket := bracketOpen(v)
	p.writeString(string(bracket))
	bracketCol := p.col - 1 // column of the opening bracket

	// Write the first child (head).
	// For data lists (non-symbol head), preserve first-child-on-new-line.
	isCall := v.Cells[0].Type == lisp.LSymbol
	if !isCall && hasNewlineBefore(v.Cells[0]) {
		p.newline()
		p.writeIndent(bracketCol + 1)
	}
	p.writeExpr(v.Cells[0], bracketCol+1)
	p.writeTrailingComment(v.Cells[0])
	firstArgCol := p.col + 1 // column where the first arg would go (after space)

	// Look up indent rule based on head symbol
	rule := &IndentRule{Style: IndentAlign}
	if isCall {
		headSym := v.Cells[0].Str
		if idx := strings.LastIndex(headSym, ":"); idx >= 0 {
			headSym = headSym[idx+1:]
		}
		rule = p.cfg.RuleFor(headSym)
	} else {
		// Not a function call — it's a data list like a cond clause or
		// binding pair. Align subsequent elements just inside the bracket.
		firstArgCol = bracketCol + 1
	}

	// When the first argument wraps to a new line, fall back to body indent
	// to avoid rightward drift from long form names. For IndentAlign only;
	// IndentSpecial handles newline-wrapping per-child in computeChildIndent.
	if isCall && len(v.Cells) > 1 && hasNewlineBefore(v.Cells[1]) {
		if rule.Style == IndentAlign {
			rule = &IndentRule{Style: IndentBody}
		}
	}

	// Write remaining children
	for i := 1; i < len(v.Cells); i++ {
		child := v.Cells[i]
		onNewLine := hasNewlineBefore(child)
		childIndent := p.computeChildIndent(rule, i, firstArgCol, bracketCol, onNewLine)

		if onNewLine {
			p.newline()
			for j := 0; j < p.blankLinesBefore(child); j++ {
				p.newline()
			}
			if child.Meta != nil && len(child.Meta.LeadingComments) > 0 {
				p.writeLeadingComments(child, childIndent)
				p.writeBlankLinesAfterComments(child)
			}
			p.writeIndent(childIndent)
		} else {
			p.writeSameLineSpacing(child)
		}
		p.writeExpr(child, childIndent)
		p.writeTrailingComment(child)
	}

	// Write comments between last child and closing bracket
	commentIndent := p.computeChildIndent(rule, len(v.Cells), firstArgCol, bracketCol, true)
	p.writeInnerTrailingComments(v, commentIndent)

	closingOnNewLine := v.Meta != nil && len(v.Meta.InnerTrailingComments) > 0
	if !closingOnNewLine && v.Meta != nil && v.Meta.ClosingBracketNewline {
		closingOnNewLine = true
	}
	if closingOnNewLine {
		p.newline()
		p.writeIndent(commentIndent)
	}
	p.writeString(string(bracketClose(bracket)))
}

// writeListInner writes the brackets and contents of a list expression.
func (p *printer) writeListInner(v *lisp.LVal, indent int) {
	bracket := bracketOpen(v)
	p.writeString(string(bracket))
	openCol := p.col

	for i, child := range v.Cells {
		if i > 0 {
			childIndent := openCol
			if hasNewlineBefore(child) {
				p.newline()
				for j := 0; j < p.blankLinesBefore(child); j++ {
					p.newline()
				}
				if child.Meta != nil && len(child.Meta.LeadingComments) > 0 {
					p.writeLeadingComments(child, childIndent)
					p.writeBlankLinesAfterComments(child)
				}
				p.writeIndent(childIndent)
			} else {
				p.writeSameLineSpacing(child)
			}
		} else if hasNewlineBefore(child) {
			// First child on a new line — preserve bracket-on-its-own-line style
			p.newline()
			for j := 0; j < p.blankLinesBefore(child); j++ {
				p.newline()
			}
			if child.Meta != nil && len(child.Meta.LeadingComments) > 0 {
				p.writeLeadingComments(child, openCol)
				p.writeBlankLinesAfterComments(child)
			}
			p.writeIndent(openCol)
		}
		p.writeExpr(child, openCol)
		p.writeTrailingComment(child)
	}

	// Write comments between last child and closing bracket
	p.writeInnerTrailingComments(v, openCol)

	closingOnNewLine := v.Meta != nil && len(v.Meta.InnerTrailingComments) > 0
	if !closingOnNewLine && v.Meta != nil && v.Meta.ClosingBracketNewline {
		closingOnNewLine = true
	}
	if closingOnNewLine {
		p.newline()
		p.writeIndent(openCol)
	}
	p.writeString(string(bracketClose(bracket)))
}

// tryPrefixForm checks if this s-expr is a known prefix shorthand and renders it.
func (p *printer) tryPrefixForm(v *lisp.LVal, indent int) bool {
	if len(v.Cells) != 2 || v.Cells[0].Type != lisp.LSymbol {
		return false
	}
	switch v.Cells[0].Str {
	case "lisp:function":
		p.writeString("#'")
		p.writeExpr(v.Cells[1], indent)
		return true
	case "lisp:expr":
		p.writeString("#^")
		p.writeExpr(v.Cells[1], indent)
		return true
	}
	return false
}

// computeChildIndent determines the indentation for child at index i.
// For IndentSpecial header args, if the child wraps to a new line, body indent
// is used instead of first-arg alignment to avoid rightward drift.
func (p *printer) computeChildIndent(rule *IndentRule, childIdx int, firstArgCol int, bracketCol int, onNewLine bool) int {
	switch rule.Style {
	case IndentBody:
		return bracketCol + p.cfg.IndentSize
	case IndentSpecial:
		if childIdx <= rule.HeaderArgs {
			if onNewLine {
				return bracketCol + p.cfg.IndentSize
			}
			return firstArgCol
		}
		return bracketCol + p.cfg.IndentSize
	default: // IndentAlign
		return firstArgCol
	}
}

// writeSameLineSpacing writes the spacing between tokens on the same line.
// If the source had extra spaces (e.g., for column alignment), they are preserved.
func (p *printer) writeSameLineSpacing(v *lisp.LVal) {
	spaces := 1
	if v.Meta != nil && v.Meta.PrecedingSpaces > 1 {
		spaces = v.Meta.PrecedingSpaces
	}
	for i := 0; i < spaces; i++ {
		p.buf.WriteByte(' ')
	}
	p.col += spaces
}

// writeIndent writes spaces to reach the desired column.
func (p *printer) writeIndent(col int) {
	if !p.atBOL {
		return
	}
	for i := 0; i < col; i++ {
		p.buf.WriteByte(' ')
	}
	p.col = col
	p.atBOL = false
}

// writeString writes a string, updating column tracking.
func (p *printer) writeString(s string) {
	if p.atBOL && s != "" {
		p.atBOL = false
	}
	p.buf.WriteString(s)
	if idx := strings.LastIndex(s, "\n"); idx >= 0 {
		p.col = len(s) - idx - 1
	} else {
		p.col += len(s)
	}
}

// newline writes a newline and marks beginning of line.
func (p *printer) newline() {
	p.buf.WriteByte('\n')
	p.col = 0
	p.atBOL = true
}

// bracketOpen returns the opening bracket for an s-expression.
func bracketOpen(v *lisp.LVal) rune {
	if v.Meta != nil && v.Meta.BracketType != 0 {
		return v.Meta.BracketType
	}
	if v.Quoted {
		return '['
	}
	return '('
}

// bracketClose returns the matching close bracket.
func bracketClose(open rune) rune {
	if open == '[' {
		return ']'
	}
	return ')'
}
