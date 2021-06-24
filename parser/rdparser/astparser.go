package rdparser

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// ASTConstructor parses a node in a lisp abstract syntax tree.  Elements of
// children may be retained by an ASTConstructor but (subslices of) the slice
// children must not be retained by the ASTConstructor as it will be zeroed out
// for reuse immediately following the return of the constructor.
type ASTConstructor = func(token *token.Token, children []*lisp.LVal) (*lisp.LVal, error)

// ParseConfig configures a generic parser
type ParseConfig struct {
	// HashBang enables the parser to handle a token.HASH_BANG leading a file.
	HashBang bool
	// Comments enables the parser to capture comment nodes.
	Comments bool
	// Ctor builds an AST node given the token rooting a subtree and the
	// chidren which make up the subtree.
	Ctor ASTConstructor
}

// ParseProgram reads source code from
func ParseProgram(src *TokenSource, config ParseConfig) ([]*lisp.LVal, error) {
	if config.Ctor == nil {
		return nil, fmt.Errorf("nil constructor")
	}
	p := astparser{
		children: make([]*lisp.LVal, 0, 32),
		ctor:     config.Ctor,
		comments: config.Comments,
		src:      src,
	}
	return p.ParseProgram(config.HashBang)
}

const astPackage = "elps"

type parseFunc func(p *astparser) (next parseFunc, err error)

type astparser struct {
	// children is a stack of nodes
	children []*lisp.LVal
	ctor     func(*token.Token, []*lisp.LVal) (*lisp.LVal, error)
	comments bool
	src      *TokenSource
}

func (p *astparser) mark() int {
	return len(p.children) - 1
}

func (p *astparser) top() *lisp.LVal {
	return p.children[p.mark()]
}

func (p *astparser) pushChild(v *lisp.LVal) {
	p.children = append(p.children, v)
}

func (p *astparser) discardFrom(mark int) error {
	m := len(p.children)
	if 0 <= mark && mark < m {
		clearStack(p.children[mark:])
		p.children = p.children[:mark]
		return nil
	}
	return fmt.Errorf("mark is invalid: %d", mark)
}

func (p *astparser) popFrom(mark int) (*lisp.LVal, error) {
	m := len(p.children)
	if 0 <= mark && mark < m {
		tok, ok := p.children[mark].Native.(*markToken)
		if !ok {
			return lisp.Nil(), fmt.Errorf("mark is not a token: %v", lisp.GetType(p.children[mark]))
		}
		node, err := p.mkval((*token.Token)(tok), p.children[mark+1:])
		if err != nil {
			return lisp.Nil(), err
		}
		clearStack(p.children[mark:])
		p.children = p.children[:mark]
		return node, nil
	}
	return lisp.Nil(), fmt.Errorf("mark is invalid: %d", mark)
}

func (p *astparser) pushFrom(mark int) error {
	v, err := p.popFrom(mark)
	if err == nil {
		p.pushChild(v)
	}
	return err
}

func (p *astparser) pushTokenType(typ token.Type) bool {
	if p.PeekType() != typ || !p.src.Scan() {
		return false
	}
	p.pushChild(Token(p.src.Token))
	return true
}

type markToken token.Token

func (p *astparser) pushMark() (int, error) {
	if !p.src.Scan() {
		return -1, p.scanError("scan failed")
	}
	p.pushChild(lisp.Native((*markToken)(p.src.Token)))
	return p.mark(), nil
}

func (p *astparser) pushMarkType(typ token.Type) (int, error) {
	if p.PeekType() != typ {
		return -1, nil
	}
	return p.pushMark()
}

// push the current token as a literal node and scan the next token
func (p *astparser) pushLiteral() error {
	if !p.src.Scan() {
		return p.scanError("scan failed")
	}
	v, err := p.mkval(p.src.Token, nil)
	if err != nil {
		return err
	}
	p.pushChild(v)
	return nil
}

// push a literal node if it matches the given type
func (p *astparser) pushLiteralType(typ token.Type) (bool, error) {
	if p.PeekType() != typ {
		return false, nil
	}
	return true, p.pushLiteral()
}

func (p *astparser) mkval(typ *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	if p.ctor != nil {
		return p.ctor(typ, c)
	}
	if c == nil {
		return Token(typ), nil
	}
	return Node(typ, c)
}

// ParseProgram parses a series of expressions potentially preceded by a
// hash-bang, `#!`.
func (p *astparser) ParseProgram(hashbang bool) ([]*lisp.LVal, error) {
	if hashbang {
		if err := p.parseHashBang(); err != nil {
			return nil, fmt.Errorf("failed to parse hash-bang: %w", err)
		}
	}
	for !p.src.IsEOF() {
		found, err := p.parseComments()
		if err != nil {
			return nil, err
		}
		if found {
			continue
		}
		err = p.parseExpression()(p)
		if err != nil {
			return nil, err
		}
	}
	return p.children, nil
}

func (p *astparser) parseHashBang() error {
	mark, err := p.pushMarkType(token.HASH_BANG)
	if err != nil {
		return err
	}
	if mark < 0 {
		// optional so this is fine
		return nil
	}
	if !p.pushTokenType(token.COMMENT) {
		return &invalidNodeError{
			tok: p.src.Token,
			msg: fmt.Sprintf("unexpected token %v", p.PeekType()),
		}
	}
	if p.comments {
		return p.pushFrom(mark)
	} else {
		return p.discardFrom(mark)
	}
}

func (p *astparser) parseExpression() func(p *astparser) error {
	p.parseComments()
	switch p.PeekType() {
	case token.INT:
		return (*astparser).pushLiteral
	case token.INT_OCTAL_MACRO:
		return parseMacroChar("o", token.INT_OCTAL)
	case token.INT_HEX_MACRO:
		return parseMacroChar("x", token.INT_HEX)
	case token.FLOAT:
		return (*astparser).pushLiteral
	case token.STRING:
		return (*astparser).pushLiteral
	case token.STRING_RAW:
		return (*astparser).pushLiteral
	case token.NEGATIVE:
		return (*astparser).ParseNegative
	case token.QUOTE:
		return (*astparser).parseQuote
	case token.UNBOUND:
		return (*astparser).parseUnbound
	case token.FUN_REF:
		return (*astparser).ParseFunRef
	case token.SYMBOL:
		return (*astparser).ParseSymbol
	case token.PAREN_L:
		return (*astparser).parseConsExpression
	case token.BRACE_L:
		return (*astparser).parseList
	case token.ERROR, token.INVALID:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("scan-error", p.TokenText())
		}
	default:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("parse-error", "unexpected expression token: %v", p.TokenType())
		}
	}
}

// parseUnboundExpression parses the top-level unbound expression.
func (p *astparser) parseExpressionUnbound() func(p *astparser) error {
	//p.parseComments()
	switch p.PeekType() {
	case token.INT:
		return (*astparser).pushLiteral
	case token.INT_OCTAL_MACRO:
		return parseMacroChar("o", token.INT_OCTAL)
	case token.INT_HEX_MACRO:
		return parseMacroChar("x", token.INT_HEX)
	case token.FLOAT:
		return (*astparser).pushLiteral
	case token.STRING:
		return (*astparser).pushLiteral
	case token.STRING_RAW:
		return (*astparser).pushLiteral
	case token.NEGATIVE:
		return (*astparser).ParseNegative
	case token.QUOTE:
		return (*astparser).parseQuoteUnbound
	// case token.Unbound is removed -- nested unbounds are not allowed
	case token.FUN_REF:
		return (*astparser).ParseFunRef
	case token.SYMBOL:
		return (*astparser).ParseSymbol
	case token.PAREN_L:
		// must be flat
		return (*astparser).parseConsExpressionFlat
	case token.BRACE_L:
		// must be flat
		return (*astparser).parseListFlat
	case token.ERROR, token.INVALID:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("scan-error", p.TokenText())
		}
	default:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("parse-error", "unexpected token parsing unbound expression: %v", p.TokenType())
		}
	}
}

// parseExpressionFlat allows literals and quoted literals for ubound subexpressions
func (p *astparser) parseExpressionFlat() func(p *astparser) error {
	//p.parseComments()
	switch p.PeekType() {
	case token.INT:
		return (*astparser).pushLiteral
	case token.INT_OCTAL_MACRO:
		return parseMacroChar("o", token.INT_OCTAL)
	case token.INT_HEX_MACRO:
		return parseMacroChar("x", token.INT_HEX)
	case token.FLOAT:
		return (*astparser).pushLiteral
	case token.STRING:
		return (*astparser).pushLiteral
	case token.STRING_RAW:
		return (*astparser).pushLiteral
	case token.NEGATIVE:
		return (*astparser).ParseNegative
	case token.QUOTE:
		return (*astparser).parseQuoteLiteral
	case token.FUN_REF:
		return (*astparser).ParseFunRef
	case token.SYMBOL:
		return (*astparser).ParseSymbol
	case token.ERROR, token.INVALID:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("scan-error", p.TokenText())
		}
	default:
		return func(p *astparser) error {
			p.ReadToken()
			return p.errorf("parse-error", "unexpected token for flat expression: %v", p.TokenType())
		}
	}
}

func parseMacroChar(char string, next token.Type) func(*astparser) error {
	return func(p *astparser) error {
		mark, err := p.pushMark()
		if err != nil {
			return err
		}
		if !p.pushTokenType(next) {
			if p.Accept(token.ERROR, token.INVALID) {
				return p.scanError(fmt.Sprintf("invalid-%s-literal", char))
			}
			return p.errorf(fmt.Sprintf("invalid-%s-literal", char), "unexpected token: %v", p.PeekType())
		}
		err = p.pushFrom(mark)
		if err != nil {
			return err
		}
		return nil
	}
}

func (p *astparser) parseQuote() error {
	mark, err := p.pushMarkType(token.QUOTE)
	if err != nil {
		return err
	}
	// FIXME it looks like this allows comments between the quote and the
	// quoted thing which would not be good
	err = p.parseExpression()(p)
	if err != nil {
		return err
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseQuoteUnbound() error {
	mark, err := p.pushMarkType(token.QUOTE)
	if err != nil {
		return err
	}
	err = p.parseExpressionUnbound()(p)
	if err != nil {
		return err
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseQuoteLiteral() error {
	mark, err := p.pushMarkType(token.QUOTE)
	if err != nil {
		return err
	}
	err = p.parseExpressionFlat()(p)
	if err != nil {
		return err
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseUnbound() error {
	mark, err := p.pushMarkType(token.UNBOUND)
	if err != nil {
		return err
	}
	err = p.parseExpressionUnbound()(p)
	if err != nil {
		return err
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

/*
func (p *astparser) checkUnbound(v *lisp.LVal) error {
	switch lisp.GetType(v).Str {
	case astType("token"):
		return nil
	case astType("node"):
	default:
		return p.errorf("invalid-unbound-expression", "invalid expression %v", nodeString(v))
	}
	node, ok := GetNodeOK(v)
	if !ok {
		return p.errorf("invalid-unbound-expression", "unable to unpack node: %v", nodeString(node))
	}
	tok, ok := GetTokenOK(node.Cells[0])
	if !ok {
		return p.errorf("invalid-unbound-expression", "unable to unpack node token: %v", nodeString(node))
	}
	switch tok.Type {
	case token.BRACE_L, token.PAREN_L:
	default:
		defer func() {
			if recover() != nil {
				log.Panicf("failed to check %v", tok)
			}
		}()
		return p.checkUnbound(node.Cells[1])
	}
	for _, child := range node.Cells[1:] {
		err := p.checkUnboundChild(child)
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *astparser) checkUnboundChild(v *lisp.LVal) error {
	switch lisp.GetType(v).Str {
	case astType("token"):
		return nil
	case astType("node"):
	default:
		return p.errorf("invalid-unbound-expression", "invalid child expression %v", lisp.GetType(v))
	}
	node, ok := GetNodeOK(v)
	if !ok {
		return p.errorf("invalid-unbound-expression", "unable to unpack child node: %v", nodeString(node))
	}
	tok, ok := GetTokenOK(node.Cells[0])
	if !ok {
		return p.errorf("invalid-unbound-expression", "unable to unpack child node token: %v", nodeString(node))
	}
	switch tok.Type {
	case token.BRACE_L, token.PAREN_L:
		return p.errorf("invalid-unbound-expression", "contains nested compound expression(s)")
	default:
		return p.checkUnboundChild(node.Cells[1])
	}
}
*/

func (p *astparser) ParseFunRef() error {
	mark, err := p.pushMarkType(token.FUN_REF)
	if err != nil {
		return err
	}
	err = p.ParseSymbol()
	if err != nil {
		return err
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) ParseNegative() error {
	if !p.Accept(token.NEGATIVE) {
		return p.errorf("parse-error", "invalid negative: %v", p.PeekType())
	}
	save := p.src.Token
	switch p.PeekType() {
	case token.INT, token.FLOAT, token.SYMBOL:
		p.src.Peek().Source = save.Source
		p.src.Peek().Text = save.Text + p.src.Peek().Text
		return p.parseExpression()(p)
	default:
		save.Type = token.SYMBOL
		v, err := p.mkval(save, nil)
		if err != nil {
			return err
		}
		p.pushChild(v)
		return nil
	}
}

func (p *astparser) ParseSymbol() error {
	if p.PeekType() != token.SYMBOL {
		return p.errorf("parse-error", "expected symbol: %v", p.PeekType())
	}
	tok := p.src.Peek()
	pieces := strings.Split(tok.Text, ":")
	if len(pieces) > 2 {
		return p.errorf("invalid-symbol", "invalid symbol %q", tok.Text)
	}
	if len(pieces) == 2 && pieces[1] == "" {
		return p.errorf("invalid-symbol", "invalid symbol %q", tok.Text)
	}
	return p.pushLiteral()
}

func (p *astparser) parseConsExpression() error {
	open := p.src.Peek()
	mark, err := p.pushMarkType(token.PAREN_L)
	if err != nil {
		return err
	}
	if mark < 0 {
		return p.errorf("parse-error", "invalid expression: %v", p.PeekType())
	}
	for {
		_, err := p.parseComments()
		if err != nil {
			return err
		}
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		ok, err := p.pushLiteralType(token.PAREN_R)
		if err != nil {
			return err
		}
		if ok {
			break
		}
		err = p.parseExpression()(p)
		if err != nil {
			return err
		}
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseConsExpressionFlat() error {
	open := p.src.Peek()
	mark, err := p.pushMarkType(token.PAREN_L)
	if err != nil {
		return err
	}
	if mark < 0 {
		return p.errorf("parse-error", "invalid expression: %v", p.PeekType())
	}
	for {
		_, err := p.parseComments()
		if err != nil {
			return err
		}
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		ok, err := p.pushLiteralType(token.PAREN_R)
		if err != nil {
			return err
		}
		if ok {
			break
		}
		err = p.parseExpressionFlat()(p)
		if err != nil {
			return err
		}
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseList() error {
	open := p.src.Peek()
	mark, err := p.pushMarkType(token.BRACE_L)
	if err != nil {
		return err
	}
	if mark < 0 {
		return p.errorf("parse-error", "invalid expression: %v", p.PeekType())
	}
	mark = p.mark()
	for {
		_, err := p.parseComments()
		if err != nil {
			return err
		}
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		ok, err := p.pushLiteralType(token.BRACE_R)
		if err != nil {
			return err
		}
		if ok {
			break
		}
		err = p.parseExpression()(p)
		if err != nil {
			return err
		}
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseListFlat() error {
	open := p.src.Peek()
	mark, err := p.pushMarkType(token.BRACE_L)
	if err != nil {
		return err
	}
	if mark < 0 {
		return p.errorf("parse-error", "invalid expression: %v", p.PeekType())
	}
	mark = p.mark()
	for {
		_, err := p.parseComments()
		if err != nil {
			return err
		}
		if p.src.IsEOF() {
			return p.errorf("unmatched-syntax", "unmatched %s", open.Text)
		}
		ok, err := p.pushLiteralType(token.BRACE_R)
		if err != nil {
			return err
		}
		if ok {
			break
		}
		err = p.parseExpressionFlat()(p)
		if err != nil {
			return err
		}
	}
	err = p.pushFrom(mark)
	if err != nil {
		return err
	}
	return nil
}

func (p *astparser) parseComments() (bool, error) {
	if p.comments {
		return p.handleComments((*astparser).pushLiteralType)
	}
	return p.handleComments(func(p *astparser, typ token.Type) (bool, error) {
		return p.src.AcceptType(typ), nil
	})
}

func (p *astparser) handleComments(fn func(p *astparser, typ token.Type) (bool, error)) (bool, error) {
	ok := false
	for {
		found, err := fn(p, token.COMMENT)
		if err != nil {
			return ok, err
		}
		if !found {
			break
		}
		ok = true
	}
	return ok, nil
}

func (p *astparser) ReadToken() *token.Token {
	p.src.Scan()
	return p.src.Token
}

func (p *astparser) TokenText() string {
	return p.src.Token.Text
}

func (p *astparser) TokenType() token.Type {
	return p.src.Token.Type
}

func (p *astparser) Location() *token.Location {
	return p.src.Token.Source
}

func (p *astparser) PeekType() token.Type {
	return p.src.Peek().Type
}

func (p *astparser) PeekLocation() *token.Location {
	return p.src.Peek().Source
}

func (p *astparser) Quote(v *lisp.LVal) *lisp.LVal {
	if v.Type == lisp.LError {
		return v
	}
	return p.tokenLVal(lisp.Quote(v))
}

func (p *astparser) SExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.SExpr(cells))
}

func (p *astparser) QExpr(cells []*lisp.LVal) *lisp.LVal {
	return p.tokenLVal(lisp.QExpr(cells))
}

func (p *astparser) tokenLVal(v *lisp.LVal) *lisp.LVal {
	v.Source = p.Location()
	return v
}

func (p *astparser) Accept(typ ...token.Type) bool {
	return p.src.AcceptType(typ...)
}

func (p *astparser) markError(mark int) error {
	return p.errorf("parse-error", "bad stack mark %d %v", mark, p.children)
}

func (p *astparser) errorf(condition string, format string, v ...interface{}) error {
	return &ParseError{
		loc:       p.Location(),
		condition: condition,
		msg:       fmt.Sprintf(format, v...),
	}
}

func (p *astparser) scanError(condition string) error {
	return &ParseError{
		loc:       p.Location(),
		condition: condition,
		msg:       p.TokenText(),
	}
}

func astType(t string) string {
	return astPackage + ":" + t
}

func Token(t *token.Token) *lisp.LVal {
	return lisp.TaggedValue(astType("token"), lisp.Native(t))
}

func GetTokenOK(v *lisp.LVal) (*token.Token, bool) {
	if v.Type != lisp.LTaggedVal {
		return nil, false
	}
	data := v.UserData()
	tok, ok := data.Native.(*token.Token)
	if !ok {
		return nil, false
	}
	return tok, true
}
