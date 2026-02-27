// Copyright © 2018 The ELPS authors

package rdparser

import (
	"errors"
	"io"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

type reader struct {
}

// NewReader returns a lisp.Reader to use in a lisp.Runtime.
func NewReader() lisp.Reader {
	return &reader{}
}

// Read implements lisp.Reader.
func (*reader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	p := New(s)
	return p.ParseProgram()
}

// ReadLocation implements lisp.LocationReader.
func (*reader) ReadLocation(name string, loc string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	s.SetPath(loc)
	p := New(s)
	return p.ParseProgram()
}

// DefaultMaxParseDepth is the maximum nesting depth allowed by the parser.
// Deeply nested input (e.g. 50K+ unmatched parens) can overflow the Go
// goroutine stack with a fatal crash that recover() cannot catch.  This
// limit converts the fatal crash into a parse error.
const DefaultMaxParseDepth = 10000

// Parser is a lisp parser.
type Parser struct {
	parsing         bool
	src             *TokenSource
	preserveFormat  bool
	pendingComments []*token.Token
	depth           int
	maxDepth        int
}

// NewFromSource initializes and returns a Parser that reads tokens from src.
func NewFromSource(src *TokenSource) *Parser {
	return &Parser{
		src:      src,
		maxDepth: DefaultMaxParseDepth,
	}
}

// New initializes and returns a new Parser that reads tokens from scanner.
func New(scanner *token.Scanner) *Parser {
	return NewFromSource(NewTokenSource(scanner))
}

// NewFormatting initializes a Parser in format-preserving mode.
// Comments, bracket types, original literal text, and blank lines are
// attached to LVal.Meta fields on the returned AST.
func NewFormatting(scanner *token.Scanner) *Parser {
	p := New(scanner)
	p.preserveFormat = true
	return p
}

// Parse is a generic entry point that is similar to ParseExpression but is
// capable of handling EOF before reading an expression.
func (p *Parser) Parse() (*lisp.LVal, error) {
	p.ignoreComments()
	if p.src.IsEOF() {
		return nil, io.EOF
	}
	expr := p.ParseExpression()
	if expr.Type == lisp.LError {
		return nil, lisp.GoError(expr)
	}
	// In formatting mode, check for a trailing comment on the same line
	// as the expression's closing bracket (e.g., (foo)) ;; comment).
	if p.preserveFormat {
		p.ignoreComments()
		if len(p.pendingComments) > 0 && p.pendingComments[0].PrecedingNewlines == 0 {
			if expr.Meta == nil {
				expr.Meta = &lisp.SourceMeta{}
			}
			expr.Meta.TrailingComment = p.pendingComments[0]
			p.pendingComments = p.pendingComments[1:]
		}
	}
	return expr, nil
}

// ParseProgram parses a series of expressions potentially preceded by a
// hash-bang, `#!`.
func (p *Parser) ParseProgram() ([]*lisp.LVal, error) {
	var exprs []*lisp.LVal

	p.ignoreHashBang()

	for {
		expr, err := p.Parse()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}

	return exprs, nil
}

// ParseExpression parses a single expression.  Unlike Parse, ParseExpression
// requires an expression to be present in the input stream and will report
// unexpected EOF tokens encountered.
func (p *Parser) ParseExpression() *lisp.LVal {
	p.depth++
	defer func() { p.depth-- }()
	if p.depth > p.maxDepth {
		return p.errorf("parse-error", "expression nesting exceeds maximum depth (%d)", p.maxDepth)
	}

	fn := p.parseExpression()

	// We have a token marking the beginning of an expression.  Flag that we
	// are currently in the middle of an expression while we finish parsing the
	// expression so that an Interactive parser can determine what state we are
	// in (and thus imply what the REPL prompt should be).
	if !p.parsing {
		p.parsing = true
		defer func() { p.parsing = false }()
	}

	return fn(p)
}

func (p *Parser) ignoreHashBang() {
	if p.PeekType() != token.HASH_BANG {
		return
	}
	p.src.Scan()
	if p.preserveFormat {
		// Preserve the hashbang + comment as a pending comment
		tok := *p.src.Token
		p.pendingComments = append(p.pendingComments, &tok)
	}
	if p.src.AcceptType(token.COMMENT) {
		if p.preserveFormat {
			tok := *p.src.Token
			// Merge hashbang text with the comment text
			if len(p.pendingComments) > 0 {
				p.pendingComments[len(p.pendingComments)-1].Text += tok.Text
			}
		}
	}
}

func (p *Parser) parseExpression() func(p *Parser) *lisp.LVal {
	p.ignoreComments()
	switch p.PeekType() {
	case token.INT:
		return (*Parser).ParseLiteralInt
	case token.INT_OCTAL_MACRO:
		return (*Parser).ParseLiteralIntOctal
	case token.INT_HEX_MACRO:
		return (*Parser).ParseLiteralIntHex
	case token.FLOAT:
		return (*Parser).ParseLiteralFloat
	case token.STRING:
		return (*Parser).ParseLiteralString
	case token.STRING_RAW:
		return (*Parser).ParseLiteralStringRaw
	case token.NEGATIVE:
		return (*Parser).ParseNegative
	case token.QUOTE:
		return (*Parser).ParseQuote
	case token.UNBOUND:
		return (*Parser).ParseUnbound
	case token.FUN_REF:
		return (*Parser).ParseFunRef
	case token.SYMBOL:
		return (*Parser).ParseSymbol
	case token.PAREN_L:
		return (*Parser).ParseConsExpression
	case token.BRACE_L:
		return (*Parser).ParseList
	case token.ERROR, token.INVALID:
		return func(p *Parser) *lisp.LVal {
			p.ReadToken()
			return p.errorf("scan-error", "%s", p.TokenText())
		}
	default:
		return func(p *Parser) *lisp.LVal {
			p.ReadToken()
			return p.errorf("parse-error", "unexpected token: %v", p.TokenType())
		}
	}
}

func (p *Parser) ParseLiteralInt() *lisp.LVal {
	if !p.Accept(token.INT) {
		return p.errorf("parse-error", "invalid integer literal: %v", p.PeekType())
	}
	text := p.TokenText()
	x, err := strconv.Atoi(text)
	if err != nil {
		return p.errorf("integer-overflow-error", "integer literal overflows int: %v", text)
	}
	v := p.Int(x)
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = text
	}
	return v
}

func (p *Parser) ParseLiteralIntOctal() *lisp.LVal {
	if !p.Accept(token.INT_OCTAL_MACRO) {
		return p.errorf("parse-error", "unexpected token: %v", p.PeekType())
	}
	macroText := p.TokenText()
	if !p.Accept(token.INT_OCTAL) {
		if p.Accept(token.ERROR, token.INVALID) {
			return p.scanError("invalid-octal-literal")
		}
		return p.errorf("invalid-octal-literal", "unexpected token: %v", p.PeekType())
	}
	text := p.TokenText()
	x, err := strconv.ParseInt(text, 8, 0)
	if err != nil {
		return p.errorf("integer-overflow-error", "octal literal overflows int: %v", text)
	}
	v := p.Int(int(x))
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = macroText + text
	}
	return v
}

func (p *Parser) ParseLiteralIntHex() *lisp.LVal {
	if !p.Accept(token.INT_HEX_MACRO) {
		return p.errorf("parse-error", "unexpected token: %v", p.PeekType())
	}
	macroText := p.TokenText()
	if !p.Accept(token.INT_HEX) {
		if p.Accept(token.ERROR, token.INVALID) {
			return p.scanError("invalid-hex-literal")
		}
		return p.errorf("invalid-hex-literal", "unexpected token: %v", p.PeekType())
	}
	text := p.TokenText()
	x, err := strconv.ParseInt(text, 16, 0)
	if err != nil {
		return p.errorf("integer-overflow-error", "hex literal overflows int: %v", text)
	}
	v := p.Int(int(x))
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = macroText + text
	}
	return v
}

func (p *Parser) ParseLiteralFloat() *lisp.LVal {
	if !p.Accept(token.FLOAT) {
		return p.errorf("parse-error", "invalid float literal: %v", p.PeekType())
	}
	text := p.TokenText()
	x, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return p.errorf("invalid-float", "invalid floating point literal: %v", text)
	}
	v := p.Float(x)
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = text
	}
	return v
}

func (p *Parser) ParseLiteralString() *lisp.LVal {
	if !p.Accept(token.STRING) {
		return p.errorf("parse-error", "invalid string literal: %v", p.PeekType())
	}
	text := p.TokenText()
	s, err := strconv.Unquote(text)
	if err != nil {
		return p.errorf("invalid-float", "invalid string literal: %v", text)
	}
	v := p.String(s)
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = text
	}
	return v
}

func (p *Parser) ParseLiteralStringRaw() *lisp.LVal {
	if !p.Accept(token.STRING_RAW) {
		return p.errorf("parse-error", "invalid raw string literal: %v", p.PeekType())
	}
	text := p.TokenText()
	if len(text) < 6 {
		return p.errorf("parse-error", "invalid raw string literal: too short")
	}
	v := p.String(text[3 : len(text)-3])
	if p.preserveFormat && v.Meta != nil {
		v.Meta.OriginalText = text
	}
	return v
}

func (p *Parser) ParseQuote() *lisp.LVal {
	if !p.Accept(token.QUOTE) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	inner := p.ParseExpression()
	result := p.Quote(inner)
	inheritEndPos(result, inner)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

func (p *Parser) ParseUnbound() *lisp.LVal {
	if !p.Accept(token.UNBOUND) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	expr := p.ParseExpression()
	if expr.Type == lisp.LError {
		return expr
	}
	sym := lisp.Symbol("lisp:expr")
	sym.Source = expr.Source
	// Ensure that the expression doesn't contain nested cons expressions.
	for _, c := range expr.Cells {
		if c.Type == lisp.LSExpr && !c.Quoted {
			return p.errorf("unbound-expression-error", "unbound expression cannot contain nested expressions")
		}
	}
	result := p.SExpr([]*lisp.LVal{sym, expr})
	inheritEndPos(result, expr)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

func (p *Parser) ParseFunRef() *lisp.LVal {
	op := lisp.Symbol("lisp:function")
	if !p.Accept(token.FUN_REF) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	name := p.ParseSymbol()
	if name.Type == lisp.LError {
		return name
	}
	result := p.SExpr([]*lisp.LVal{op, name})
	inheritEndPos(result, name)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

// inheritEndPos copies end position from inner to outer, for prefix forms
// where the outer node starts at the prefix token but ends at the inner
// expression's end.
func inheritEndPos(outer, inner *lisp.LVal) {
	if outer.Source != nil && inner.Source != nil {
		outer.Source.EndPos = inner.Source.EndPos
		outer.Source.EndLine = inner.Source.EndLine
		outer.Source.EndCol = inner.Source.EndCol
	}
}

// applyPrefixNewlines sets the newline and spacing metadata on a prefix form
// (quote, #', #^) using the prefix token's values, which would otherwise be
// lost because tokenLVal reads from the inner expression's token.
func (p *Parser) applyPrefixNewlines(v *lisp.LVal, newlines int, spaces int) {
	if !p.preserveFormat || v.Type == lisp.LError {
		return
	}
	if v.Meta == nil {
		v.Meta = &lisp.SourceMeta{}
	}
	if newlines >= 1 {
		v.Meta.NewlineBefore = true
	}
	if newlines > 1 {
		v.Meta.BlankLinesBefore = newlines - 1
	}
	v.Meta.PrecedingSpaces = spaces
}

func (p *Parser) ParseNegative() *lisp.LVal {
	if !p.Accept(token.NEGATIVE) {
		return p.errorf("parse-error", "invalid negative: %v", p.PeekType())
	}
	switch p.PeekType() {
	case token.INT, token.FLOAT, token.SYMBOL:
		p.src.Peek().Source = p.Location()
		p.src.Peek().Text = p.TokenText() + p.src.Peek().Text
	default:
		return p.Symbol(p.TokenText())
	}
	return p.ParseExpression()
}

func (p *Parser) ParseSymbol() *lisp.LVal {
	if !p.Accept(token.SYMBOL) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	tok := p.src.Token
	pieces := strings.Split(tok.Text, ":")
	if len(pieces) > 2 {
		return p.errorf("invalid-symbol", "invalid symbol %q", tok.Text)
	}
	if len(pieces) == 2 && pieces[1] == "" {
		return p.errorf("invalid-symbol", "invalid symbol %q", tok.Text)
	}
	return p.Symbol(tok.Text)
}

func (p *Parser) ParseConsExpression() *lisp.LVal {
	if !p.Accept(token.PAREN_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.SExpr(nil)
	if p.preserveFormat && expr.Meta != nil {
		expr.Meta.BracketType = '('
	}
	for {
		p.ignoreComments()
		p.attachTrailingComment(expr)
		if p.src.IsEOF() {
			return p.errorAtf(open.Source, "unmatched-syntax", "unclosed %s opened at %s", open.Text, open.Source)
		}
		if p.PeekType() == token.BRACE_R {
			p.ReadToken()
			return p.errorf("mismatched-syntax", "expected ) to close %s opened at %s, but found ]", open.Text, open.Source)
		}
		if p.Accept(token.PAREN_R) {
			// Set end position from closing bracket.
			if p.src.Token.Source != nil && expr.Source != nil {
				expr.Source.EndPos = p.src.Token.Source.Pos + 1
				expr.Source.EndLine = p.src.Token.Source.Line
				expr.Source.EndCol = p.src.Token.Source.Col + 1
			}
			p.recordClosingBracketNewline(expr)
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	p.captureInnerTrailingComments(expr)
	return expr
}

func (p *Parser) ParseList() *lisp.LVal {
	if !p.Accept(token.BRACE_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.QExpr(nil)
	if p.preserveFormat && expr.Meta != nil {
		expr.Meta.BracketType = '['
	}
	for {
		p.ignoreComments()
		p.attachTrailingComment(expr)
		if p.src.IsEOF() {
			return p.errorAtf(open.Source, "unmatched-syntax", "unclosed %s opened at %s", open.Text, open.Source)
		}
		if p.PeekType() == token.PAREN_R {
			p.ReadToken()
			return p.errorf("mismatched-syntax", "expected ] to close %s opened at %s, but found )", open.Text, open.Source)
		}
		if p.Accept(token.BRACE_R) {
			// Set end position from closing bracket.
			if p.src.Token.Source != nil && expr.Source != nil {
				expr.Source.EndPos = p.src.Token.Source.Pos + 1
				expr.Source.EndLine = p.src.Token.Source.Line
				expr.Source.EndCol = p.src.Token.Source.Col + 1
			}
			p.recordClosingBracketNewline(expr)
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	p.captureInnerTrailingComments(expr)
	return expr
}

func (p *Parser) ignoreComments() {
	for p.Accept(token.COMMENT) {
		if p.preserveFormat {
			tok := *p.src.Token // copy the token
			p.pendingComments = append(p.pendingComments, &tok)
		}
	}
}

// attachTrailingComment checks if the first pending comment is an inline
// trailing comment (PrecedingNewlines == 0) on the last child of parent.
// If so, it moves the comment from pendingComments to the child's TrailingComment.
func (p *Parser) attachTrailingComment(parent *lisp.LVal) {
	if !p.preserveFormat || len(parent.Cells) == 0 || len(p.pendingComments) == 0 {
		return
	}
	if p.pendingComments[0].PrecedingNewlines == 0 {
		last := parent.Cells[len(parent.Cells)-1]
		if last.Meta == nil {
			last.Meta = &lisp.SourceMeta{}
		}
		last.Meta.TrailingComment = p.pendingComments[0]
		p.pendingComments = p.pendingComments[1:]
	}
}

// recordClosingBracketNewline records whether the closing bracket was on
// its own line in the source.
func (p *Parser) recordClosingBracketNewline(expr *lisp.LVal) {
	if !p.preserveFormat {
		return
	}
	if p.src.Token.PrecedingNewlines > 0 {
		if expr.Meta == nil {
			expr.Meta = &lisp.SourceMeta{}
		}
		expr.Meta.ClosingBracketNewline = true
	}
}

// captureInnerTrailingComments moves any remaining pending comments to the
// expression's InnerTrailingComments. These are comments that appear between
// the last child and the closing bracket of an s-expression or list.
func (p *Parser) captureInnerTrailingComments(expr *lisp.LVal) {
	if !p.preserveFormat || len(p.pendingComments) == 0 {
		return
	}
	if expr.Meta == nil {
		expr.Meta = &lisp.SourceMeta{}
	}
	expr.Meta.InnerTrailingComments = p.pendingComments
	p.pendingComments = nil
}

func (p *Parser) ReadToken() *token.Token {
	p.src.Scan()
	return p.src.Token
}

func (p *Parser) TokenText() string {
	return p.src.Token.Text
}

func (p *Parser) TokenType() token.Type {
	return p.src.Token.Type
}

func (p *Parser) Location() *token.Location {
	return p.src.Token.Source
}

func (p *Parser) PeekType() token.Type {
	return p.src.Peek().Type
}

func (p *Parser) PeekLocation() *token.Location {
	return p.src.Peek().Source
}

func (p *Parser) String(s string) *lisp.LVal {
	return p.tokenLVal(lisp.String(s))
}

func (p *Parser) Symbol(sym string) *lisp.LVal {
	return p.tokenLVal(lisp.Symbol(sym))
}

func (p *Parser) Int(x int) *lisp.LVal {
	return p.tokenLVal(lisp.Int(x))
}

func (p *Parser) Float(x float64) *lisp.LVal {
	return p.tokenLVal(lisp.Float(x))
}

func (p *Parser) Quote(v *lisp.LVal) *lisp.LVal {
	if v.Type == lisp.LError {
		return v
	}
	return p.tokenLVal(lisp.Quote(v))
}

func (p *Parser) SExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.SExpr(cells))
}

func (p *Parser) QExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.QExpr(cells))
}

func (p *Parser) tokenLVal(v *lisp.LVal) *lisp.LVal {
	v.Source = p.Location()
	// Set end position from the current token.
	if v.Source != nil && p.src.Token != nil {
		endLine, endCol, endPos := token.TokenEnd(p.src.Token)
		v.Source.EndLine = endLine
		v.Source.EndCol = endCol
		v.Source.EndPos = endPos
	}
	if p.preserveFormat {
		if v.Meta == nil {
			v.Meta = &lisp.SourceMeta{}
		}
		if len(p.pendingComments) > 0 {
			v.Meta.LeadingComments = p.pendingComments
			p.pendingComments = nil
		}
		// Compute newline info from the first pending comment or the current token.
		// When there are leading comments, also preserve the gap between the
		// last comment and the expression itself.
		tokenNewlines := p.src.Token.PrecedingNewlines
		newlines := tokenNewlines
		if len(v.Meta.LeadingComments) > 0 {
			newlines = v.Meta.LeadingComments[0].PrecedingNewlines
			if tokenNewlines > 1 {
				v.Meta.BlankLinesAfterComments = tokenNewlines - 1
			}
		}
		if newlines >= 1 {
			v.Meta.NewlineBefore = true
		}
		if newlines > 1 {
			v.Meta.BlankLinesBefore = newlines - 1
		}
		v.Meta.PrecedingSpaces = p.src.Token.PrecedingSpaces
	}
	return v
}

func (p *Parser) Accept(typ ...token.Type) bool {
	return p.src.AcceptType(typ...)
}

func (p *Parser) errorf(condition string, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	err.Source = p.Location()
	return err
}

func (p *Parser) errorAtf(source *token.Location, condition, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	err.Source = source
	return err
}

func (p *Parser) scanError(condition string) *lisp.LVal {
	err := lisp.ErrorCondition(condition, errors.New(p.TokenText()))
	err.Source = p.Location()
	return err
}

// ParseResult holds the result of a fault-tolerant parse: a partial AST
// and any errors encountered during parsing.
type ParseResult struct {
	Exprs  []*lisp.LVal
	Errors []error
}

// maxRecoveryErrors is the maximum number of errors collected before
// ParseProgramFaultTolerant stops attempting to recover. This prevents
// runaway recovery on pathological input.
const maxRecoveryErrors = 50

// ParseProgramFaultTolerant parses a series of expressions like
// ParseProgram, but recovers from errors and continues parsing. It returns
// all successfully parsed expressions alongside all collected errors.
// This is useful for IDE/LSP scenarios where partial ASTs are valuable.
func (p *Parser) ParseProgramFaultTolerant() ParseResult {
	var result ParseResult

	p.ignoreHashBang()

	for {
		expr, err := p.Parse()
		if err == io.EOF {
			break
		}
		if err != nil {
			result.Errors = append(result.Errors, err)
			if len(result.Errors) >= maxRecoveryErrors {
				break
			}
			p.skipToNextExpression()
			continue
		}
		result.Exprs = append(result.Exprs, expr)
	}

	return result
}

// skipToNextExpression advances the token stream past the current error
// region to find the start of the next top-level expression. It consumes
// stray closing brackets and skips over any remaining tokens inside
// unbalanced brackets.
func (p *Parser) skipToNextExpression() {
	// Clear pending comments to prevent comment leakage across error boundaries.
	p.pendingComments = nil

	bracketDepth := 0
	for {
		if p.src.IsEOF() {
			return
		}
		switch p.PeekType() {
		case token.PAREN_L, token.BRACE_L:
			if bracketDepth == 0 {
				// Found the start of a new expression.
				return
			}
			bracketDepth++
			p.ReadToken()
		case token.PAREN_R, token.BRACE_R:
			p.ReadToken()
			if bracketDepth > 0 {
				bracketDepth--
			}
			// At depth 0, stray closers are consumed as orphans; continue.
		default:
			if bracketDepth == 0 {
				// Found a non-bracket token at top level — this is the
				// start of a new expression (e.g., a symbol or literal).
				return
			}
			p.ReadToken()
		}
	}
}

// PendingComments returns any comments collected but not yet attached to an
// LVal (e.g., trailing comments at end of file). Only useful in formatting mode.
func (p *Parser) PendingComments() []*token.Token {
	return p.pendingComments
}
