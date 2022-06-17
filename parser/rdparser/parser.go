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

// Parser is a lisp parser.
type Parser struct {
	parsing bool
	src     *TokenSource
}

// NewFromSource initializes and returns a Parser that reads tokens from src.
func NewFromSource(src *TokenSource) *Parser {
	return &Parser{
		src: src,
	}
}

// New initializes and returns a new Parser that reads tokens from scanner.
func New(scanner *token.Scanner) *Parser {
	return NewFromSource(NewTokenSource(scanner))
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
	p.src.AcceptType(token.COMMENT)
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
			return p.errorf("scan-error", p.TokenText())
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
	return p.Int(x)
}

func (p *Parser) ParseLiteralIntOctal() *lisp.LVal {
	if !p.Accept(token.INT_OCTAL_MACRO) {
		return p.errorf("parse-error", "unexpected token: %v", p.PeekType())
	}
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
	return p.Int(int(x))
}

func (p *Parser) ParseLiteralIntHex() *lisp.LVal {
	if !p.Accept(token.INT_HEX_MACRO) {
		return p.errorf("parse-error", "unexpected token: %v", p.PeekType())
	}
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
	return p.Int(int(x))
}

func (p *Parser) ParseLiteralFloat() *lisp.LVal {
	if !p.Accept(token.FLOAT) {
		return p.errorf("parse-error", "invalid float literal: %v", p.PeekType())
	}
	x, err := strconv.ParseFloat(p.TokenText(), 64)
	if err != nil {
		return p.errorf("invalid-float", "invalid floating point literal: %v", p.TokenText())
	}
	return p.Float(x)
}

func (p *Parser) ParseLiteralString() *lisp.LVal {
	if !p.Accept(token.STRING) {
		return p.errorf("parse-error", "invalid string literal: %v", p.PeekType())
	}
	s, err := strconv.Unquote(p.TokenText())
	if err != nil {
		return p.errorf("invalid-float", "invalid string literal: %v", p.TokenText())
	}
	return p.String(s)
}

func (p *Parser) ParseLiteralStringRaw() *lisp.LVal {
	if !p.Accept(token.STRING_RAW) {
		return p.errorf("parse-error", "invalid raw string literal: %v", p.PeekType())
	}
	text := p.TokenText()
	if len(text) < 6 {
		panic("short raw string")
	}
	return p.String(text[3 : len(text)-3])
}

func (p *Parser) ParseQuote() *lisp.LVal {
	if !p.Accept(token.QUOTE) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	return p.Quote(p.ParseExpression())
}

func (p *Parser) ParseUnbound() *lisp.LVal {
	if !p.Accept(token.UNBOUND) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
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
	return p.SExpr([]*lisp.LVal{sym, expr})
}

func (p *Parser) ParseFunRef() *lisp.LVal {
	op := lisp.Symbol("lisp:function")
	if !p.Accept(token.FUN_REF) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	name := p.ParseSymbol()
	if name.Type == lisp.LError {
		return name
	}
	return p.SExpr([]*lisp.LVal{op, name})
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
	for {
		p.ignoreComments()
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.Accept(token.PAREN_R) {
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	return expr
}

func (p *Parser) ParseList() *lisp.LVal {
	if !p.Accept(token.BRACE_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.QExpr(nil)
	for {
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		if p.Accept(token.BRACE_R) {
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x)
	}
	return expr
}

func (p *Parser) ignoreComments() {
	for p.Accept(token.COMMENT) {
	}
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

func (p *Parser) scanError(condition string) *lisp.LVal {
	err := lisp.ErrorCondition(condition, errors.New(p.TokenText()))
	err.Source = p.Location()
	return err
}
