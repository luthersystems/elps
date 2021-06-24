// Copyright © 2018 The ELPS authors

package rdparser

import (
	"bytes"
	"fmt"
	"io"
	"strconv"

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
func (_ *reader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	p := New(s)
	return p.ParseProgram()
}

// ReadLocation implements lisp.LocationReader.
func (_ *reader) ReadLocation(name string, loc string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	s.SetPath(loc)
	p := New(s)
	return p.ParseProgram()
}

// hopefully this can be inlined
func clearStack(s []*lisp.LVal) {
	for i := range s {
		s[i] = nil
	}
}

type ParseError struct {
	loc       *token.Location
	condition string
	msg       string
}

func (err *ParseError) Error() string {
	return fmt.Sprintf("%v: %v: %v", err.loc, err.condition, err.msg)
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

func (p *Parser) ParseAST() ([]*lisp.LVal, error) {
	astp := astparser{ctor: p.parseVal, src: p.src}
	return astp.ParseProgram(true)
}

// Parse is a generic entry point that is similar to ParseExpression but is
// capable of handling EOF before reading an expression.
func (p *Parser) Parse() (*lisp.LVal, error) {
	astp := astparser{ctor: p.parseVal, src: p.src}
	astp.parseComments()
	if p.src.IsEOF() {
		return lisp.Nil(), nil
	}
	return p.ParseExpression()
}

// ParseProgram parses a series of expressions potentially preceded by a
// hash-bang, `#!`.
func (p *Parser) ParseProgram() ([]*lisp.LVal, error) {
	return ParseProgram(p.src, ParseConfig{HashBang: true, Ctor: p.parseVal})
}

// ParseExpression parses a single expression.  Unlike Parse, ParseExpression
// requires an expression to be present in the input stream and will report
// unexpected EOF tokens encountered.
func (p *Parser) ParseExpression() (*lisp.LVal, error) {
	astp := astparser{ctor: p.parseVal, src: p.src}
	fn := astp.parseExpression()

	// We have a token marking the beginning of an expression.  Flag that we
	// are currently in the middle of an expression while we finish parsing the
	// expression so that an Interactive parser can determine what state we are
	// in (and thus imply what the REPL prompt should be).
	if !p.parsing {
		p.parsing = true
		defer func() { p.parsing = false }()
	}

	err := fn(&astp)
	if err != nil {
		return lisp.Nil(), err
	}
	return astp.top(), nil
}

func (p *Parser) parseVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	switch tok.Type {
	case token.SYMBOL:
		return p.symbolVal(tok)
	case token.STRING:
		return p.stringVal(tok)
	case token.STRING_RAW:
		return p.stringRawVal(tok)
	case token.FLOAT:
		return p.floatVal(tok)
	case token.INT:
		return p.intDVal(tok)
	case token.INT_OCTAL_MACRO:
		tok, ok := GetTokenOK(c[0])
		if !ok {
			return lisp.Nil(), &invalidNodeError{
				tok: tok,
				msg: "expected octal literal token",
			}
		}
		return p.intOVal(tok)
	case token.INT_HEX_MACRO:
		tok, ok := GetTokenOK(c[0])
		if !ok {
			return lisp.Nil(), &invalidNodeError{
				tok: tok,
				msg: "expected hex literal token",
			}
		}
		return p.intXVal(tok)
	case token.QUOTE:
		return p.quoteVal(tok, c)
	case token.UNBOUND:
		return p.unboundVal(tok, c)
	case token.FUN_REF:
		return p.funRefVal(tok, c)
	case token.BRACE_L:
		return p.qexprVal(tok, c)
	case token.PAREN_L:
		return p.sexprVal(tok, c)
	case token.BRACE_R, token.PAREN_R:
		return lisp.Nil(), nil
	default:
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: "unexpected node token",
		}
	}
}

/*
func (p *Parser) parseNode(v *lisp.LVal) (*lisp.LVal, error) {
	if tok, ok := GetTokenOK(v); ok {
		switch tok.Type {
		case token.COMMENT:
			return lisp.Nil(), parseSkip
		default:
			return p.parseTokenLiteral(tok)
		}
	}
	if node, ok := GetNodeOK(v); ok {
		return p.parseChild(node)
	}
	// this is a literal lisp value
	return v, nil
}

func (p *Parser) parseTokenLiteral(tok *token.Token) (*lisp.LVal, error) {
	switch tok.Type {
	case token.SYMBOL:
		return p.symbolVal(tok)
	case token.STRING:
		return p.stringVal(tok)
	case token.STRING_RAW:
		return p.stringRawVal(tok)
	case token.FLOAT:
		return p.floatVal(tok)
	case token.INT:
		return p.intDVal(tok)
	case token.INT_OCTAL_MACRO:
		return p.intOVal(tok)
	case token.INT_HEX_MACRO:
		return p.intXVal(tok)
	case token.HASH_BANG:
		return lisp.Nil(), parseSkip
	default:
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: "expected token literal",
		}
	}
}
*/

func (p *Parser) stringVal(tok *token.Token) (*lisp.LVal, error) {
	s, err := strconv.Unquote(tok.Text)
	if err != nil {
		return nil, &invalidNodeError{
			tok: tok,
			msg: fmt.Sprintf("invalid string literal: %v", err),
		}
	}
	return String(tok, s), nil
}

func (p *Parser) stringRawVal(tok *token.Token) (*lisp.LVal, error) {
	inner := tok.Text[3 : len(tok.Text)-3]
	return String(tok, inner), nil
}

func (p *Parser) symbolVal(tok *token.Token) (*lisp.LVal, error) {
	return Symbol(tok, tok.Text), nil
}

func (p *Parser) intDVal(tok *token.Token) (*lisp.LVal, error) {
	return p.intVal(tok, 10)
}

func (p *Parser) intXVal(tok *token.Token) (*lisp.LVal, error) {
	return p.intVal(tok, 16)
}

func (p *Parser) intOVal(tok *token.Token) (*lisp.LVal, error) {
	return p.intVal(tok, 8)
}

func (p *Parser) intVal(tok *token.Token, base int) (*lisp.LVal, error) {
	x, err := strconv.ParseInt(tok.Text, base, 0)
	if err != nil {
		return lisp.Nil(), err
	}
	v := lisp.Int(int(x))
	v.Source = tok.Source
	return v, nil
}

func (p *Parser) floatVal(tok *token.Token) (*lisp.LVal, error) {
	x, err := strconv.ParseFloat(tok.Text, 64)
	if err != nil {
		return lisp.Nil(), err
	}
	v := lisp.Float(x)
	v.Source = tok.Source
	return v, nil
}

func String(tok *token.Token, s string) *lisp.LVal {
	v := lisp.String(s)
	v.Source = tok.Source
	return v
}

func Symbol(tok *token.Token, s string) *lisp.LVal {
	v := lisp.Symbol(s)
	v.Source = tok.Source
	return v
}

func Int(tok *token.Token, x int) *lisp.LVal {
	v := lisp.Int(x)
	v.Source = tok.Source
	return v
}

/*
func (p *Parser) parseChild(v *lisp.LVal) (*lisp.LVal, error) {
	ltok := v.Cells[0]
	children := v.Cells[1:]
	tok, ok := GetTokenOK(ltok)
	if !ok {
		return lisp.Nil(), &invalidNodeError{
			tok: nil,
			msg: fmt.Sprintf("unexpected node type: %v", lisp.GetType(ltok)),
		}
	}
	switch tok.Type {
	case token.HASH_BANG:
		return lisp.Nil(), parseSkip
	case token.QUOTE:
		return p.quoteVal(tok, children)
	case token.UNBOUND:
		return p.unboundVal(tok, children)
	case token.FUN_REF:
		return p.funRefVal(tok, children)
	case token.BRACE_L:
		return p.qexprVal(tok, children)
	case token.PAREN_L:
		return p.sexprVal(tok, children)
	default:
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: "unexpected compound expression",
		}
	}
}
*/

func (p *Parser) quoteVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if len(c) != 1 {
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: fmt.Sprintf("quoted expression has %d children: %v", len(c), c),
		}
	}
	return lisp.Quote(c[0]), nil
}

func (p *Parser) unboundVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if len(c) != 1 {
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: fmt.Sprintf("unbound expression has %d children", len(c)),
		}
	}
	expr := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:expr"), c[0]})
	expr.Source = tok.Source
	return expr, nil
}

func (p *Parser) funRefVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if len(c) != 1 {
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: fmt.Sprintf("unbound expression has %d children", len(c)),
		}
	}
	if c[0].Type != lisp.LSymbol {
		return lisp.Nil(), &invalidNodeError{
			tok: tok,
			msg: fmt.Sprintf("invalid function reference with type %v", lisp.GetType(c[0])),
		}
	}
	expr := lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:function"), c[0]})
	expr.Source = tok.Source
	return expr, nil
}

func (p *Parser) sexprVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if len(c) < 1 {
		panic("expected at least one child for s-expression")
	}
	cells, err := copyCells(tok, c[:len(c)-1])
	v := lisp.SExpr(cells)
	v.Source = tok.Source
	return v, err
}

func (p *Parser) qexprVal(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if len(c) < 1 {
		panic("expected at least one child for quoted s-expression")
	}
	cells, err := copyCells(tok, c[:len(c)-1])
	v := lisp.QExpr(cells)
	v.Source = tok.Source
	return v, err
}

func copyCells(tok *token.Token, c []*lisp.LVal) (cells []*lisp.LVal, err error) {
	cells = make([]*lisp.LVal, len(c))
	copy(cells, c)
	return cells, nil
}

var parseSkip = fmt.Errorf("skip node")

type invalidNodeError struct {
	tok *token.Token
	msg string
}

func (err invalidNodeError) Error() string {
	if err.tok == nil {
		return fmt.Sprintf("%v: invalid node %v: %v", "???", "UNKNOWN", err.msg)
	}
	return fmt.Sprintf("%v: invalid node %v: %v", err.tok.Source, err.tok.Type, err.msg)
}

func nodeString(node *AST) string {
	var buf bytes.Buffer
	fmtNode(&buf, node)
	return buf.String()
}

func fmtNode(buf *bytes.Buffer, node *AST) {
	buf.WriteByte('<')
	fmt.Fprint(buf, node.Token)
	for i := range node.Nodes {
		if i > 0 {
			buf.WriteByte(' ')
		}
		fmtNode(buf, node.Nodes[i])
	}
	buf.WriteByte('>')
	fmt.Fprint(buf, node)
}
