// Copyright © 2018 The ELPS authors

package rdparser

import (
	"errors"
	"io"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/internal/fmtraw"
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
//
// It bounds the PARSER's recursion only.  It used to be, incidentally, the
// only thing bounding the EVALUATOR's recursion as well -- nesting had to be
// parsed before it could be evaluated -- but that composition was never
// sound: a recursive macro generates nesting at expansion time and never
// passes through here, as does any embedder building an AST through the Go
// API.  lisp.DefaultMaxEvalNesting is the limit that bounds evaluation depth
// (issue #316); this one is not load-bearing for it.
const DefaultMaxParseDepth = 10000

// Parser is a lisp parser.
type Parser struct {
	src             *TokenSource
	nameReadback    map[string]bool
	pendingComments []*token.Token
	depth           int
	maxDepth        int
	parsing         bool
	preserveFormat  bool
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
// attached as formatting metadata (internal/fmtmeta) on the returned AST.
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
			fmtraw.EnsureMeta(expr).TrailingComment = p.pendingComments[0]
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
		if errors.Is(err, io.EOF) {
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

	expr := fn(p)
	if p.depth == 1 && !p.preserveFormat {
		// Seal each completed top-level expression: from here the tree may
		// be cached and shared by every environment that evaluates this
		// parse, and the sealed flag is what stops kernel mutation sites
		// from writing it in place (lisp/seal.go).  Sealing happens at
		// depth 1 — after every nested ParseExpression call has finished
		// its construction-time fixups — so the parser never writes a
		// sealed node's fields.  SealAST ignores error values.
		//
		// Format-preserving parses are excluded: they are the one path where
		// construction of a top-level node is NOT finished here — Parse()
		// attaches a same-line trailing comment to the expression's formatting metadata after this
		// point (`(foo) ;; c`), which would be a write to an already-sealed
		// node and would stale the fingerprint recorded at seal time.
		// Format trees are never evaluated or shared across environments
		// (the formatter only reads Meta), so they need no seal, and the
		// documented seal scope (Reader.Read, LoadString, ParseProgram, the
		// REPL — all non-format) already excludes them.
		expr.SealAST()
	}
	return expr
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
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = text
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
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = macroText + text
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
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = macroText + text
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
		return p.errorf(lisp.CondInvalidFloat, "invalid floating point literal: %v", text)
	}
	v := p.Float(x)
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = text
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
		return p.errorf(lisp.CondInvalidString, "invalid string literal: %v", text)
	}
	v := p.String(s)
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = text
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
	if m := fmtraw.Meta(v); p.preserveFormat && m != nil {
		m.OriginalText = text
	}
	return v
}

func (p *Parser) ParseQuote() *lisp.LVal {
	if !p.Accept(token.QUOTE) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	quoteLoc := p.Location() // save ' location before parsing inner expression
	inner := p.ParseExpression()
	result := p.Quote(inner)
	if inner.Type == lisp.LError {
		// p.Quote returns errors untouched, so the historical in-place
		// position fixups rewrote the error's own location to start at the
		// quote token.  Replicate that on a private copy of the location —
		// LVal locations are read-only outside package lisp (issue #362).
		if src, ok := inner.Source(); ok {
			applyPrefixLocation(&src, quoteLoc)
			inner.SetSource(&src)
		}
	} else {
		// p.Quote stamped result with the current token's Location and
		// tokenLVal consumes no tokens, so p.Location() is the same
		// *token.Location the result now holds; fix it up through the
		// parser's own reference.
		resultLoc := p.Location()
		inheritEndPos(resultLoc, inner)
		applyPrefixLocation(resultLoc, quoteLoc)
	}
	p.hoistOperandComments(result, inner)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

func (p *Parser) ParseUnbound() *lisp.LVal {
	if !p.Accept(token.UNBOUND) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	prefixLoc := p.Location() // save #^ location before parsing inner expression
	expr := p.ParseExpression()
	if expr.Type == lisp.LError {
		return expr
	}
	sym := lisp.Symbol("lisp:expr")
	// Ensure that the expression doesn't contain nested cons expressions.
	for _, c := range expr.Cells {
		if c.Type == lisp.LSExpr && !c.IsQuoted() {
			return p.errorf("unbound-expression-error", "unbound expression cannot contain nested expressions")
		}
	}
	result := p.SExpr([]*lisp.LVal{sym, expr})
	// p.SExpr stamped result with the current token's Location; keep the
	// parser's own reference to it for the fixups below (issue #362).
	resultLoc := p.Location()
	p.recordSynthesizedBrackets(result)
	inheritEndPos(resultLoc, expr)
	applyPrefixLocation(resultLoc, prefixLoc)
	// sym mirrors expr's location.  It historically shared expr's *Location
	// pointer (assigned before the fixups above, which mutate expr's
	// location in place when expr is an atom); copying the location after
	// the fixups yields the same observable positions.
	if loc, ok := expr.Source(); ok {
		sym.SetSource(&loc)
	} else {
		sym.SetSource(nil)
	}
	p.hoistOperandComments(result, expr)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

func (p *Parser) ParseFunRef() *lisp.LVal {
	op := lisp.Symbol("lisp:function")
	// The synthesized head symbol has no token of its own.  It has always
	// reported the "<native code>" location; constructors no longer stamp
	// one (issue #362), so give it a private copy explicitly.
	opLoc := token.NativeLocation()
	op.SetSource(&opLoc)
	if !p.Accept(token.FUN_REF) {
		return p.errorf("parse-error", "invalid quote: %v", p.PeekType())
	}
	prefixNewlines := p.src.Token.PrecedingNewlines
	prefixSpaces := p.src.Token.PrecedingSpaces
	prefixLoc := p.Location() // save #' location before parsing inner expression
	name := p.ParseSymbol()
	if name.Type == lisp.LError {
		return name
	}
	// The operand has to be a name the reader gives back as a symbol. #' is
	// shorthand, printed in longhand as "(lisp:function <name>)", so a name
	// that reads back as anything else means the printer changed the program.
	//
	// "#'0" was rejected earlier by requiring a symbol-START rune in the
	// lexer, but '-' and '+' ARE symbol-start runes, so "#'-0", "#'-1",
	// "#'+0", "#'1.5" and "#'-1abc" all sailed past it and produced symbols
	// that turn back into INTs in longhand -- `elps fmt` silently rewriting a
	// function reference as a number. Found by FuzzFormatCompact on "#'-0".
	//
	// The check lives here and not in the lexer because the lexer is the
	// wrong layer to ask: "--" and "-a" lex as NEGATIVE, and only become
	// symbols when ParseNegative merges them. A token-level guard rejects
	// those valid names; re-reading through the parser gets them right.
	if !p.readsBackAsSymbol(name.Str) {
		return p.errorf("parse-error",
			"invalid symbol following #': %q does not read back as a symbol", name.Str)
	}
	result := p.SExpr([]*lisp.LVal{op, name})
	// p.SExpr stamped result with the current token's Location; keep the
	// parser's own reference to it for the fixups below (issue #362).
	resultLoc := p.Location()
	p.recordSynthesizedBrackets(result)
	inheritEndPos(resultLoc, name)
	applyPrefixLocation(resultLoc, prefixLoc)
	p.hoistOperandComments(result, name)
	p.applyPrefixNewlines(result, prefixNewlines, prefixSpaces)
	return result
}

// recordSynthesizedBrackets stamps the paren bracket kind onto an s-expression
// the parser BUILT rather than read -- the desugared forms behind #^ and #',
// which have no bracket of their own in the source.
//
// Without it those nodes reach the formatter with BracketType unset, and
// printer.bracketOpen falls back to '[' for any quoted value.  A quoted
// shorthand therefore printed as '[lisp:expr 0] -- the bracket quotes the
// value a second time, so it re-parses with a level of quoting the source
// never had.  Found by FuzzFormat on the input "'#^0".
func (p *Parser) recordSynthesizedBrackets(v *lisp.LVal) {
	m := fmtraw.Meta(v)
	if !p.preserveFormat || m == nil {
		return
	}
	m.BracketType = '('
}

// inheritEndPos copies the end position from inner's location onto outerLoc,
// for prefix forms where the outer node starts at the prefix token but ends
// at the inner expression's end.  outerLoc is the parser-owned Location most
// recently stamped onto the outer node by tokenLVal.
func inheritEndPos(outerLoc *token.Location, inner *lisp.LVal) {
	innerLoc, ok := inner.Source()
	if outerLoc != nil && ok {
		outerLoc.EndPos = innerLoc.EndPos
		outerLoc.EndLine = innerLoc.EndLine
		outerLoc.EndCol = innerLoc.EndCol
	}
}

// applyPrefixLocation overwrites the start position of a prefix form's
// location dst (quote, #', #^) with the prefix token's location. tokenLVal
// sets the node's source from the last consumed token, which for prefix
// forms is the inner expression — not the prefix. This corrects it so that
// e.g. '() has its location at the ' rather than at the (.
func applyPrefixLocation(dst, loc *token.Location) {
	if dst != nil && loc != nil {
		dst.File = loc.File
		dst.Path = loc.Path
		dst.Line = loc.Line
		dst.Col = loc.Col
		dst.Pos = loc.Pos
	}
}

// hoistOperandComments moves comments that landed on a prefix form's OPERAND
// up onto the prefix node itself.
//
// tokenLVal drains p.pendingComments onto whatever LVal it is building, and
// for a prefix form the first LVal built is the operand.  Every comment
// written anywhere in front of the operand therefore ends up attached to it --
// the ones before the prefix token ("; c\n#\'a") and the ones in the gap after
// it ("#^; c\na") alike.
//
// The printer reaches an operand through writeQuote or tryPrefixForm, and
// neither writes a child's LeadingComments; the s-expression and list printers
// do, but a prefix form is printed as neither.  Those comments were therefore
// DELETED: `elps fmt` turned
//
//	; c
//	#'a
//
// into "#'a", and "'\n; c\n[a]" into "'[a]".  Worse, when the operand could
// not be re-sugared the longhand path printed the comment INSIDE the form, so
// "#!/usr/bin/env elps\n#':%" formatted to "(lisp:function\n  #!/usr/bin/env
// elps\n  :%)" -- output the parser then rejected, because a hash-bang is only
// legal at the start of a program.  Found by FuzzGeneratedPipeline.
//
// It only bites when the prefix node has a Meta of its OWN.  lisp.Quote
// shallow-copies an unquoted operand, Meta pointer included, so "'\n; c\na"
// happened to survive -- the outer node saw the very same comments.  Quoting
// an ALREADY-quoted operand builds an LQuote node with a fresh Meta, and #'
// and #^ build a fresh two-cell s-expression, and those three lost them.
//
// Must run BEFORE applyPrefixNewlines, which reads LeadingComments to decide
// which gap the node's newline metadata describes.
func (p *Parser) hoistOperandComments(outer, inner *lisp.LVal) {
	if !p.preserveFormat || outer.Type == lisp.LError || inner == nil {
		return
	}
	im := fmtraw.Meta(inner)
	if im == nil || len(im.LeadingComments) == 0 {
		return
	}
	if fmtraw.Meta(outer) == im {
		// Shared Meta: the comments are already on the node the printer
		// writes.  Moving them would move them onto themselves.
		return
	}
	om := fmtraw.EnsureMeta(outer)
	om.LeadingComments = append(om.LeadingComments, im.LeadingComments...)
	im.LeadingComments = nil
}

// applyPrefixNewlines sets the newline and spacing metadata on a prefix form
// (quote, #', #^) using the prefix token's values, which would otherwise be
// lost because tokenLVal reads from the inner expression's token.
func (p *Parser) applyPrefixNewlines(v *lisp.LVal, newlines int, spaces int) {
	if !p.preserveFormat || v.Type == lisp.LError {
		return
	}
	m := fmtraw.EnsureMeta(v)
	if len(m.LeadingComments) > 0 {
		// Two independent gaps have to be recorded here, and NEITHER can be
		// left to tokenLVal.
		//
		// BlankLinesAfterComments is the gap between the last comment and
		// this node, which the prefix token measures because the prefix IS
		// this node's first token.  tokenLVal derived it from the INNER
		// expression's token, which sits after the prefix and so measures the
		// wrong whitespace: for ";\n'\n\n0" it recorded the blank line
		// between ' and 0 instead of the (none) between ; and ', so the blank
		// line moved on every re-format and Format stopped being idempotent.
		// Found by FuzzFormat.
		//
		// NewlineBefore / BlankLinesBefore describe the gap before the FIRST
		// COMMENT.  These used to be left alone on the premise that tokenLVal
		// had already taken them from that comment.  That held only while this
		// branch was reachable ONLY for a node sharing its operand's Meta --
		// the quote-a-plain-symbol case, where tokenLVal really did see the
		// comments.  hoistOperandComments made it reachable for #', #^ and
		// LQuote too, and those have a Meta of their OWN that tokenLVal filled
		// in BEFORE the hoist, from the operand token.  For #' that operand
		// inherits the prefix's own PrecedingNewlines -- readFunRef emits
		// FUN_REF and its symbol from a single readToken, with no
		// skipWhitespace between them -- so "(a)\n; c\n\n#'f\n" reported one
		// blank line before the comment and `elps fmt` INSERTED one, reflowing
		// a comment that describes the line above it.  It reaches real source:
		// the `; <-` markers in editors/vscode/test/grammar/basics.lisp are
		// syntax-test assertions ABOUT the preceding line, so moving them
		// changes what they assert.
		m.BlankLinesAfterComments = 0
		if newlines > 1 {
			m.BlankLinesAfterComments = newlines - 1
		}
		m.NewlineBefore = true
		m.BlankLinesBefore = 0
		if n := m.LeadingComments[0].PrecedingNewlines; n > 1 {
			m.BlankLinesBefore = n - 1
		}
	} else {
		if newlines >= 1 {
			m.NewlineBefore = true
		}
		if newlines > 1 {
			m.BlankLinesBefore = newlines - 1
		}
	}
	m.PrecedingSpaces = spaces
}

func (p *Parser) ParseNegative() *lisp.LVal {
	if !p.Accept(token.NEGATIVE) {
		return p.errorf("parse-error", "invalid negative: %v", p.PeekType())
	}
	switch p.PeekType() {
	case token.INT, token.FLOAT, token.SYMBOL:
		// The sign and the digits are two tokens that become one literal, so
		// the merged token has to inherit the SIGN's position AND its
		// whitespace bookkeeping -- the sign is the literal's first character,
		// so it is the sign that measures the gap in front of it.  The digits
		// are glued to the sign and always report a gap of zero.
		//
		// Only Source was carried over.  PrecedingNewlines therefore collapsed
		// to zero, tokenLVal cleared NewlineBefore, and the formatter put the
		// literal back on the previous line -- which, when that line ended in
		// a comment, meant "(a ; c\n-1)" formatted to "(a ; c -1)" and the
		// -1 was swallowed by the comment.  `elps fmt` silently deleting a
		// term from the program it was tidying.  Found by
		// FuzzGeneratedPipeline.
		p.src.Peek().Source = p.Location()
		p.src.Peek().PrecedingNewlines = p.src.Token.PrecedingNewlines
		p.src.Peek().PrecedingSpaces = p.src.Token.PrecedingSpaces
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
	// A package-qualified symbol NAMES A BINDING: "pkg:name" is the qualified
	// spelling of the symbol "name" inside package "pkg", so both halves have
	// to be names the reader gives back as symbols on their own.
	//
	// They were not checked, and "a:1" was the result -- the symbol named "1"
	// in package "a". Nothing else in the language can write that name: a bare
	// 1 is an INT, and docs/lang.md has said since the beginning that
	// identifiers cannot start with a number. The binding it creates is
	// reachable only through the qualified spelling and can never be imported
	// usefully by use-package, so it is a trap rather than a capability.
	//
	// This is the same defect as "#'0", recorded in lexer.readFunRef: isWord
	// is the CONTINUATION rune class and admits digits, and readSymbol gets
	// away with using it only because it is entered after isWordStart or ':'
	// has already matched. Matching ':' is what admits the digit here. The
	// fix is deliberately the same one ParseFunRef got -- re-read the name and
	// require a symbol back -- rather than a second, differently-worded rune
	// test. Re-reading also states the requirement exactly: it accepts "+1"
	// and ".1", which ARE writable as bare symbols, and it keeps working if
	// the number syntax changes. Issue #319.
	//
	// A LEADING ':' IS NOT COVERED, deliberately: ":1" is a keyword, and a
	// keyword names no binding. It is a self-evaluating literal that PutGlobal
	// explicitly refuses to bind, so the rule above has nothing to say about
	// it. See docs/lang.md, "Keywords".
	if len(pieces) == 2 && pieces[0] != "" {
		for _, piece := range pieces {
			if !p.readsBackAsSymbol(piece) {
				return p.errorf("invalid-symbol",
					"invalid symbol %q: %q does not read back as a symbol", tok.Text, piece)
			}
		}
	}
	return p.Symbol(tok.Text)
}

func (p *Parser) ParseConsExpression() *lisp.LVal {
	if !p.Accept(token.PAREN_L) {
		return p.errorf("parse-error", "invalid symbol: %v", p.PeekType())
	}
	open := p.src.Token
	expr := p.SExpr(nil)
	if m := fmtraw.Meta(expr); p.preserveFormat && m != nil {
		m.BracketType = '('
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
			// Set end position from closing bracket.  p.SExpr stamped expr
			// with the opening bracket token's Location, so open.Source is
			// the parser's own reference to expr's location (issue #362).
			if p.src.Token.Source != nil && open.Source != nil {
				open.Source.EndPos = p.src.Token.Source.Pos + 1
				open.Source.EndLine = p.src.Token.Source.Line
				open.Source.EndCol = p.src.Token.Source.Col + 1
			}
			p.recordClosingBracketNewline(expr)
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x) //elps:mutates children accumulate onto this parse's own node (p.SExpr/p.QExpr above) while the tree is parser-owned
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
	if m := fmtraw.Meta(expr); p.preserveFormat && m != nil {
		m.BracketType = '['
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
			// Set end position from closing bracket.  p.QExpr stamped expr
			// with the opening bracket token's Location, so open.Source is
			// the parser's own reference to expr's location (issue #362).
			if p.src.Token.Source != nil && open.Source != nil {
				open.Source.EndPos = p.src.Token.Source.Pos + 1
				open.Source.EndLine = p.src.Token.Source.Line
				open.Source.EndCol = p.src.Token.Source.Col + 1
			}
			p.recordClosingBracketNewline(expr)
			break
		}
		x := p.ParseExpression()
		if x.Type == lisp.LError {
			return x
		}
		expr.Cells = append(expr.Cells, x) //elps:mutates children accumulate onto this parse's own node (p.SExpr/p.QExpr above) while the tree is parser-owned
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
		fmtraw.EnsureMeta(last).TrailingComment = p.pendingComments[0]
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
		fmtraw.EnsureMeta(expr).ClosingBracketNewline = true
	}
}

// captureInnerTrailingComments moves any remaining pending comments to the
// expression's InnerTrailingComments. These are comments that appear between
// the last child and the closing bracket of an s-expression or list.
func (p *Parser) captureInnerTrailingComments(expr *lisp.LVal) {
	if !p.preserveFormat || len(p.pendingComments) == 0 {
		return
	}
	fmtraw.EnsureMeta(expr).InnerTrailingComments = p.pendingComments
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
	// The parser owns the current token's Location, so it mutates the
	// end-position fields through its own reference before handing the same
	// reference to the LVal.  Callers that need to fix up positions after
	// tokenLVal returns (prefix forms, bracket-close fixups) likewise keep
	// their own *token.Location references instead of reading them back
	// through the LVal — LVal locations are read-only outside package lisp
	// (issue #362).
	loc := p.Location()
	// Set end position from the current token.
	if loc != nil && p.src.Token != nil {
		endLine, endCol, endPos := token.TokenEnd(p.src.Token)
		loc.EndLine = endLine
		loc.EndCol = endCol
		loc.EndPos = endPos
	}
	//elps:aliases the documented producer-fixup contract (see the comment above and SetSource's doc): the parser owns this token's Location and keeps fixing up its end-position fields until the parse completes, after which sealing freezes it
	v.SetSource(loc)
	if p.preserveFormat {
		m := fmtraw.EnsureMeta(v)
		if len(p.pendingComments) > 0 {
			m.LeadingComments = p.pendingComments
			p.pendingComments = nil
		}
		// Compute newline info from the first pending comment or the current token.
		// When there are leading comments, also preserve the gap between the
		// last comment and the expression itself.
		tokenNewlines := p.src.Token.PrecedingNewlines
		newlines := tokenNewlines
		if len(m.LeadingComments) > 0 {
			newlines = m.LeadingComments[0].PrecedingNewlines
			if tokenNewlines > 1 {
				m.BlankLinesAfterComments = tokenNewlines - 1
			}
		}
		if newlines >= 1 {
			m.NewlineBefore = true
		}
		if newlines > 1 {
			m.BlankLinesBefore = newlines - 1
		}
		m.PrecedingSpaces = p.src.Token.PrecedingSpaces
	}
	return v
}

func (p *Parser) Accept(typ ...token.Type) bool {
	return p.src.AcceptType(typ...)
}

func (p *Parser) errorf(condition string, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	//elps:aliases producer-fixup contract (SetSource's documented convention): the parser owns the current token's Location and may still fix up its end-position fields; a parse error's position moving with those fixups is the intended behavior
	err.SetSource(p.Location())
	return err
}

func (p *Parser) errorAtf(source *token.Location, condition, format string, v ...interface{}) *lisp.LVal {
	err := lisp.ErrorConditionf(condition, format, v...)
	err.SetSource(source)
	return err
}

func (p *Parser) scanError(condition string) *lisp.LVal {
	err := lisp.ErrorCondition(condition, errors.New(p.TokenText()))
	//elps:aliases producer-fixup contract (SetSource's documented convention) — see errorf above
	err.SetSource(p.Location())
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
		if errors.Is(err, io.EOF) {
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

// readsBackAsSymbol memoizes readsBackAsSymbol over the lifetime of p.
//
// The predicate is a pure function of the string, and source repeats names:
// substrate's phylum sources hold thousands of qualified-symbol occurrences
// drawn from ~200 distinct names, and ParseSymbol asks about both halves of
// every one of them. Without the cache the check costs +18% on the wall clock
// of parsing that corpus; with it the sub-parse runs once per distinct name
// per file.
func (p *Parser) readsBackAsSymbol(s string) bool {
	if ok, seen := p.nameReadback[s]; seen {
		return ok
	}
	ok := readsBackAsSymbol(s)
	if p.nameReadback == nil {
		p.nameReadback = make(map[string]bool)
	}
	p.nameReadback[s] = ok
	return ok
}

// readsBackAsSymbol reports whether s, parsed on its own, yields exactly one
// expression and that expression is a symbol named s.
//
// Used by ParseFunRef to reject #' operands that cannot survive the round trip
// through the printer, and by ParseSymbol to reject either half of a
// package-qualified symbol that is not a name (issue #319). Re-parsing rather
// than pattern-matching the name states the requirement exactly: an earlier
// attempt used strconv.ParseInt/ParseFloat and was wrong, because "-1abc" is
// not a number by either yet still reads back as an INT. Re-parsing also stays
// correct if the number syntax ever changes.
//
// s never contains a ':' at either call site -- ParseSymbol has already split
// on it and rejected any symbol with more than one -- so the ParseSymbol call
// below cannot recurse back into here.
func readsBackAsSymbol(s string) bool {
	if s == "" {
		return false
	}
	// NewScannerString, not NewScanner: this runs once per #' operand and
	// twice per package-qualified symbol, and NewScanner charges a 128KiB
	// sliding window per call regardless of how few bytes it scans.
	exprs, err := New(token.NewScannerString("", s)).ParseProgram()
	if err != nil || len(exprs) != 1 {
		return false
	}
	return exprs[0].Type == lisp.LSymbol && exprs[0].Str == s
}
