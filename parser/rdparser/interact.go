// Copyright © 2018 The ELPS authors

package rdparser

import (
	"sync"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// Interactive implements a parser that parses a single expression at a time
// and defers to a TokenGenerator function when it is necessary to read more
// tokens.
type Interactive struct {
	prompt     string
	promptCont string
	Read       TokenGenerator
	buf        []*token.Token
	mut        sync.RWMutex
	p          *Parser
}

// NewInteractive initializes and returns a new Interactive parser.
func NewInteractive(read TokenGenerator) *Interactive {
	p := &Interactive{
		Read: read,
	}
	src := NewTokenStreamSource(TokenGenerator(p.read))
	p.p = NewFromSource(src)
	return p
}

// SetPrompts configures the string prompts reutrned by p.Prompt().  The cont
// string is used to prompt the user when the parser is in the middle of
// parsing an expression at the start of a line.
func (p *Interactive) SetPrompts(prompt, cont string) {
	p.prompt = prompt
	p.promptCont = cont
}

// Prompt returns a simple prompt that can be used by a REPL token generator.
func (p *Interactive) Prompt() string {
	if p.IsParsing() {
		return p.promptCont
	}
	return p.prompt

}

// IsParsing returns true if p is in the middle of parsing an expression.
// IsParsing can be called at any time, potentially by concurrent goroutines or
// when p is nil.
func (p *Interactive) IsParsing() bool {
	if p == nil {
		// definitely not parsing right now
		return false
	}
	p.mut.RLock()
	defer p.mut.RUnlock()
	return p.p.parsing
}

func (p *Interactive) read() []*token.Token {
	// Try to read tokens from the buffer before wasting time trying to re-acquire
	// the write-lock
	tok := p.readBuffer()
	if len(tok) != 0 {
		return tok
	}

	p.mut.Unlock()
	defer p.mut.Lock()
	if p.Read == nil {
		panic("nil read func")
	}

	p.buf = p.Read()
	if len(p.buf) == 0 {
		panic("no tokens read")
	}

	return p.readBuffer()
}

// readBuffer may return an empty list.
func (p *Interactive) readBuffer() []*token.Token {
	if len(p.buf) > 0 {
		tok := p.buf[0]
		p.buf = p.buf[1:]
		return []*token.Token{tok}
	}
	return nil
}

// Parse parses one expression from the interactive token stream and returns
// it, or any error encountered.  A REPL would call this function in its main
// runloop.  If a parse error is encountered, any buffered tokens (presumably
// from the current tty line) are discarded so corrected source can be re-read.
func (p *Interactive) Parse() (*lisp.LVal, error) {
	p.mut.Lock()
	defer p.mut.Unlock()
	lval, err := p.p.Parse()
	if err != nil {
		p.buf = nil
		return nil, err
	}
	return lval, nil
}

// ParseExpression parses one expression from the interactive token stream and
// returns it, or any error encountered.
//
// NOTE:  ParseExpression is deprecated and Parse should be used instead.
func (p *Interactive) ParseExpression() (*lisp.LVal, error) {
	p.mut.Lock()
	defer p.mut.Unlock()
	lval, err := p.p.ParseExpression()
	if err != nil {
		p.buf = nil
		return nil, err
	}
	return lval, nil
}
