// Copyright © 2024 The ELPS authors

package lsp

import (
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Semantic token type indices — must match the order in semanticTokenLegend().
const (
	semTokenNamespace = iota
	semTokenType
	semTokenParameter
	semTokenVariable
	semTokenFunction
	semTokenMacro
	semTokenKeyword
	semTokenComment
	semTokenString
	semTokenNumber
	semTokenOperator
)

// Semantic token modifier bit flags — must match the order in semanticTokenLegend().
const (
	semModDefinition     = 1 << iota
	semModDefaultLibrary // reserved for future use
)

// semanticTokenLegend returns the legend that the client uses to decode tokens.
func semanticTokenLegend() protocol.SemanticTokensLegend {
	return protocol.SemanticTokensLegend{
		TokenTypes: []string{
			"namespace", // 0
			"type",      // 1
			"parameter", // 2
			"variable",  // 3
			"function",  // 4
			"macro",     // 5
			"keyword",   // 6
			"comment",   // 7
			"string",    // 8
			"number",    // 9
			"operator",  // 10
		},
		TokenModifiers: []string{
			"definition",     // bit 0
			"defaultLibrary", // bit 1
		},
	}
}

// rawToken is an intermediate representation before delta encoding.
type rawToken struct {
	line      int // 0-based
	startChar int // 0-based
	length    int
	tokenType int
	modifiers int
}

// textDocumentSemanticTokensFull handles the textDocument/semanticTokens/full request.
func (s *Server) textDocumentSemanticTokensFull(_ *glsp.Context, params *protocol.SemanticTokensParams) (*protocol.SemanticTokens, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil
	}
	s.ensureAnalysis(doc)

	doc.mu.Lock()
	ast := doc.ast
	analysisResult := doc.analysis
	content := doc.Content
	doc.mu.Unlock()

	if ast == nil {
		return nil, nil
	}

	// Build lookup maps from the analysis result for fast symbol classification.
	symbolDefs := buildSymbolDefsMap(analysisResult)
	symbolRefs := buildSymbolRefsMap(analysisResult)

	var tokens []rawToken
	src := &sourceText{content: content}
	for _, expr := range ast {
		collectSemanticTokens(expr, symbolDefs, symbolRefs, src, &tokens)
	}

	// Sort by position (line, then character).
	sort.Slice(tokens, func(i, j int) bool {
		if tokens[i].line != tokens[j].line {
			return tokens[i].line < tokens[j].line
		}
		return tokens[i].startChar < tokens[j].startChar
	})

	// Delta-encode.
	data := deltaEncode(tokens)

	return &protocol.SemanticTokens{Data: data}, nil
}

// collectSemanticTokens recursively walks the AST and collects semantic tokens.
func collectSemanticTokens(
	v *lisp.LVal,
	defs map[symbolKey]*analysis.Symbol,
	refs map[symbolKey]*analysis.Symbol,
	src *sourceText,
	tokens *[]rawToken,
) {
	vLoc, ok := v.Source()
	if !ok || vLoc.Line == 0 {
		return
	}

	switch v.Type {
	case lisp.LInt, lisp.LFloat:
		tokLine, tokCol, length := atomSpan(v, src, 1)
		*tokens = append(*tokens, rawToken{
			line: tokLine, startChar: tokCol, length: length,
			tokenType: semTokenNumber,
		})

	case lisp.LString:
		// The length comes from the SPAN, not from v.Str.  v.Str is the string
		// after escape processing, so len(v.Str)+2 measured the decoded value:
		// "x\ty" is six characters of source and three of value, and the token
		// came out one short (elps#449).  The span is the source, escapes and
		// all, and it is also what makes a raw literal's """ delimiters count.
		tokLine, tokCol, length := atomSpan(v, src, len(v.Str)+2)
		*tokens = append(*tokens, rawToken{
			line: tokLine, startChar: tokCol, length: length,
			tokenType: semTokenString,
		})

	case lisp.LSymbol:
		if isSynthesizedPrefixHead(v) {
			// No token: this symbol is not text the user wrote.  See
			// isSynthesizedPrefixHead.
			return
		}
		name := v.Str
		tokLine, tokCol, length := atomSpan(v, src, len(name))
		// The ATOM's position, which is both where this token starts and what
		// the analysis result is keyed by: buildSymbolDefsMap and
		// buildSymbolRefsMap index analysis.Symbol.Source, and astutil.SymbolLoc
		// puts that on the NAME rather than on the reader quote in front of it
		// (elps#577).  It used to be keyed by the NODE's position -- the quote
		// -- so this call had to be passed a different position from the token
		// it classifies; the two agree now, and a quoted symbol matches its own
		// analysis entry at the column its token occupies.
		tokType, mods := classifySymbol(name, tokLine, tokCol, defs, refs)
		*tokens = append(*tokens, rawToken{
			line: tokLine, startChar: tokCol, length: length,
			tokenType: tokType, modifiers: mods,
		})

	case lisp.LSExpr:
		// For quoted lists like '(a b c), just recurse into children.
		for _, child := range v.Cells {
			collectSemanticTokens(child, defs, refs, src, tokens)
		}
		return

	case lisp.LInvalid, lisp.LError, lisp.LQSymbol, lisp.LFun, lisp.LQuote,
		lisp.LBytes, lisp.LSortMap, lisp.LArray, lisp.LNative,
		lisp.LTaggedVal, lisp.LMarkTerminal, lisp.LMarkTailRec,
		lisp.LMarkMacExpand, lisp.LTypeMax:
		// No token.  All but LQuote are runtime-only values that a parsed
		// document never contains.  LQuote does occur -- it is how the reader
		// represents two or more levels of quoting (''x) -- so ''x and its
		// contents currently go unhighlighted.  Cosmetic, and left as-is
		// rather than changed under a lint fix.
	}
}

// readerPrefixHeads maps the head symbol the READER synthesizes for a prefix
// form to the prefix it was synthesized from.  The reader desugars #^e to
// (lisp:expr e) and #'f to (lisp:function f); rdparser.locateSynthesized
// (elps#419) then gives the manufactured head the prefix TOKEN's own location,
// so the head stands for those two characters of source and nothing else.
var readerPrefixHeads = map[string]string{
	"lisp:expr":     "#^",
	"lisp:function": "#'",
}

// isSynthesizedPrefixHead reports whether v is a head symbol the reader
// manufactured for a #^ or #' prefix, rather than a symbol the user typed.
//
// Written out in longhand -- "(lisp:function f)", which is legal and is what
// the printer emits when it cannot re-sugar -- the head is ordinary source text
// and spans its whole name.  Synthesized from a prefix it spans exactly the
// prefix, two columns, while carrying a 9- or 13-byte name that appears nowhere
// in the document.  That is the difference, and it is the whole test.
//
// WHY NO TOKEN, RATHER THAN A TWO-CHARACTER ONE (elps#428).  A semantic token
// is a claim about the meaning of the text it covers, and the only claim
// available here is the one classifySymbol derives from the DESUGARED NAME --
// a name that is not in the document.  What that yields is an accident of the
// standard library: "lisp:function" resolves to the special operator `function`
// and comes back a KEYWORD, while "lisp:expr" resolves to nothing and comes
// back a VARIABLE.  So a length-only fix ships "#^" painted in the same colour
// as the identifier beside it and "#'" painted in another, with the difference
// determined by which desugared name happens to be bound.  Neither colour says
// anything true about the two characters on screen.
//
// Suppressing instead leaves those characters to the client's syntax grammar,
// which is the right owner for fixed punctuation the server knows nothing extra
// about -- and it makes the three reader prefixes agree, since ' already
// produces no token of its own (ParseQuote sets Quoted rather than synthesizing
// a head, so there is no node at the quote to tokenize).  It is also what other
// language servers do with nodes that stand for desugaring rather than for
// text: rust-analyzer highlights the syntax tree of real tokens and never the
// desugared HIR, and clangd drops highlightings whose range is not a plain
// source range.  The operand keeps its token either way; only the prefix
// changes hands.
func isSynthesizedPrefixHead(v *lisp.LVal) bool {
	prefix, ok := readerPrefixHeads[v.Str]
	if !ok {
		return false
	}
	loc, ok := v.Source()
	if !ok || loc.EndLine != loc.Line {
		return false
	}
	return loc.EndCol-loc.Col == len(prefix)
}

// symbolKey uniquely identifies a symbol occurrence by position.
type symbolKey struct {
	line int // 1-based (ELPS convention)
	col  int // 1-based
}

// buildSymbolDefsMap creates a lookup from position to symbol definition.
func buildSymbolDefsMap(res *analysis.Result) map[symbolKey]*analysis.Symbol {
	m := make(map[symbolKey]*analysis.Symbol)
	if res == nil {
		return m
	}
	for _, sym := range res.Symbols {
		if sym.Source != nil && sym.Source.Line > 0 {
			m[symbolKey{sym.Source.Line, sym.Source.Col}] = sym
		}
	}
	return m
}

// buildSymbolRefsMap creates a lookup from position to the referenced symbol.
func buildSymbolRefsMap(res *analysis.Result) map[symbolKey]*analysis.Symbol {
	m := make(map[symbolKey]*analysis.Symbol)
	if res == nil {
		return m
	}
	for _, ref := range res.References {
		if ref.Source != nil && ref.Source.Line > 0 {
			m[symbolKey{ref.Source.Line, ref.Source.Col}] = ref.Symbol
		}
	}
	return m
}

// specialOps is the set of ELPS special operators and core forms that should
// be highlighted as keywords.
var specialOps = map[string]bool{
	"defun": true, "defmacro": true, "deftype": true, "defmethod": true,
	"lambda": true, "let": true, "let*": true, "flet": true, "labels": true,
	"if": true, "cond": true, "or": true, "and": true, "not": true,
	"set": true, "set!": true,
	"progn": true, "loop": true, "dotimes": true,
	"handler-bind": true, "ignore-errors": true, "rethrow": true,
	"with-cleanup": true,
	"in-package":   true, "use-package": true, "export": true,
	"quote": true, "quasiquote": true, "unquote": true,
	"funcall": true, "apply": true,
	"debug-print":  true,
	"assert-equal": true, "assert-nil": true, "assert-not-nil": true,
	"test": true, "test-let": true,
	"thread-first": true, "thread-last": true,
	"function": true, "expr": true, "qualified-symbol": true,
	"macrolet": true, "assert": true,
}

// classifySymbol determines the semantic token type for a symbol based on
// analysis results and built-in knowledge.
func classifySymbol(
	name string,
	line, col int,
	defs map[symbolKey]*analysis.Symbol,
	refs map[symbolKey]*analysis.Symbol,
) (tokenType int, modifiers int) {
	// Check if this is a keyword symbol (starts with :).
	if strings.HasPrefix(name, ":") {
		return semTokenVariable, 0
	}

	// Check analysis result — look up by 1-based position.
	key := symbolKey{line + 1, col + 1}

	// Check if this position is a definition.
	if sym, ok := defs[key]; ok {
		return symbolKindToTokenType(sym.Kind), semModDefinition
	}

	// Check if this position is a reference.
	if sym, ok := refs[key]; ok {
		return symbolKindToTokenType(sym.Kind), 0
	}

	// Fall back to name-based classification.
	if specialOps[name] {
		return semTokenKeyword, 0
	}
	if name == "true" || name == "false" {
		return semTokenKeyword, 0
	}

	return semTokenVariable, 0
}

// symbolKindToTokenType converts an analysis.SymbolKind to a semantic token type index.
func symbolKindToTokenType(kind analysis.SymbolKind) int {
	switch kind {
	case analysis.SymFunction, analysis.SymBuiltin:
		return semTokenFunction
	case analysis.SymMacro:
		return semTokenMacro
	case analysis.SymSpecialOp:
		return semTokenKeyword
	case analysis.SymParameter:
		return semTokenParameter
	case analysis.SymType:
		return semTokenType
	case analysis.SymVariable:
		return semTokenVariable
	default:
		return semTokenVariable
	}
}

// sourceText is a document, split into lines only if something asks for them.
//
// Almost every atom is located from its span alone and never touches the text;
// the lines are wanted only for a reader prefix and for the first line of a
// multi-line literal.  Splitting lazily keeps a document with neither at the
// cost it had before this file started consulting the source at all.
type sourceText struct {
	content string
	lines   []string
	split   bool
}

func (s *sourceText) Lines() []string {
	if !s.split {
		s.lines = strings.Split(s.content, "\n")
		s.split = true
	}
	return s.lines
}

// lineLen is the length of line l IN BYTES, or -1 if l is not a line of s.
//
// Bytes, because that is the unit every position in this package is counted
// in: token.Location.Col is "byte offset within the line, plus one" (see
// Scanner.LocStart), elpsToLSPPosition passes it through untouched, and
// position.go slices lines by byte throughout.  See atomSpan on why this is
// not the unit LSP asks for.
func (s *sourceText) lineLen(l int) int {
	lines := s.Lines()
	if l < 0 || l >= len(lines) {
		return -1
	}
	return len(lines[l])
}

// byteAt returns the byte at 0-based column c of line l, and whether there is
// one.
func (s *sourceText) byteAt(l, c int) (byte, bool) {
	lines := s.Lines()
	if l < 0 || l >= len(lines) || c < 0 || c >= len(lines[l]) {
		return 0, false
	}
	return lines[l][c], true
}

// atomSpan locates the source text an ATOM occupies: its 0-based line, its
// 0-based start column, and its length in bytes.
//
// It is the answer to elps#449, and the two things it does that taking a
// length from a name or a decoded value does not are:
//
// THE LENGTH COMES FROM THE SOURCE SPAN.  Location.EndPos-Pos is the width of
// the atom as the scanner measured it, in the same byte unit Col is counted
// in.  So escape sequences count as the characters they are written with --
// "x\ty" is six bytes of source and three of value, and len(v.Str)+2 gave it a
// five-character token (elps#449) -- a raw literal's """ delimiters count, and
// a \U escape counts as its twelve characters rather than the four bytes it
// decodes to.
//
// Note that EndCol is NOT used, deliberately: TokenEnd derives it by counting
// RUNES onto a Col that counts BYTES, so on a line with any multi-byte text it
// is in neither unit.  EndPos and Pos are both byte offsets and their
// difference is exact.
//
// A multi-line literal keeps the existing behaviour of being highlighted on its
// first line only, but whether a literal IS multi-line is now decided by the
// span rather than by looking for a newline in the DECODED value.  The old test
// took a single-line "a\nb" -- one line of source, an escape, not a newline --
// for a multi-line literal and gave it "the rest of the line", so the token ran
// past the closing quote and over whatever followed.
//
// THE READER'S ' IS NOT PART OF THE ATOM.  rdparser.applyPrefixLocation moves a
// quoted atom's Col back onto the quote so that 'a reports the position a
// reader would point at, which leaves the atom starting one column inside its
// own span.  PR #448 settled what a reader prefix gets -- no token, with those
// characters left to the client's syntax grammar, which is the right owner for
// punctuation the server has nothing to add about -- and cited ' as the
// precedent #' and #^ were being made to match.  Skipping the quote here is
// that same decision applied to the prefix that occasioned it: afterwards none
// of ' , #' and #^ is inside any semantic token, and the atom after the prefix
// gets exactly its own.
//
// ON UNITS AND LSP.  LSP 3.16 counts a position in UTF-16 code units unless
// client and server negotiate otherwise, and this server neither offers
// positionEncoding nor converts anything: every column it emits, from every
// handler, is a byte offset.  That is a server-wide gap, filed separately; it
// is not something a length can fix locally, and a length in some other unit
// would only be inconsistent with the column it is added to.  What this
// function guarantees is that a length is in the SAME unit as its start.
//
// fallbackLen is used when the node has no usable end position, which the
// fault-tolerant parser can in principle produce; it is the length the case in
// question computed before this function existed.
func atomSpan(v *lisp.LVal, src *sourceText, fallbackLen int) (line, col, length int) {
	loc, _ := v.Source()
	line = loc.Line - 1
	col = max(loc.Col-1, 0)

	skipped := 0
	if v.IsQuoted() {
		line, col, skipped = skipReaderQuote(src, line, col)
	}

	switch {
	case loc.EndPos > loc.Pos && loc.EndLine-1 == line:
		if n := loc.EndPos - loc.Pos - skipped; n > 0 {
			return line, col, n
		}
	case loc.EndLine-1 > line:
		// The atom continues onto a later line, which only a multi-line
		// literal does.  Highlight its first line.
		if n := src.lineLen(line) - col; n > 0 {
			return line, col, n
		}
	}
	return line, col, fallbackLen
}

// skipReaderQuote advances past a ' prefix that applyPrefixLocation folded into
// a quoted atom's location, together with any whitespace or comment between the
// quote and the atom -- "' a" and "'\na" are both legal, and with comments
// preserved the reader allows one in the gap too.  It returns the atom's
// position and the number of BYTES skipped, which is what the prefix costs the
// span.
//
// It moves nothing unless the position really is a quote, so a node that is
// Quoted for some other reason, or whose column does not line up with the text
// the request was computed against, is left exactly where it was.
func skipReaderQuote(src *sourceText, line, col int) (int, int, int) {
	if c, ok := src.byteAt(line, col); !ok || c != '\'' {
		return line, col, 0
	}
	startLine, startCol := line, col
	skipped := 1
	col++
	for line < len(src.Lines()) {
		c, ok := src.byteAt(line, col)
		switch {
		case !ok: // end of line: the newline is one byte too
			line, col, skipped = line+1, 0, skipped+1
		case c == ';': // a comment runs to the end of the line
			skipped += src.lineLen(line) - col + 1
			line, col = line+1, 0
		case c == ' ' || c == '\t' || c == '\r' || c == '\f' || c == '\v':
			col, skipped = col+1, skipped+1
		default:
			return line, col, skipped
		}
	}
	// Nothing but space after the quote, so there is no atom to point at.
	// Leave the caller where it started rather than off the end of the file.
	return startLine, startCol, 0
}

// deltaEncode converts sorted raw tokens into the LSP delta-encoded format.
// Each token is 5 integers: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers].
func deltaEncode(tokens []rawToken) []protocol.UInteger {
	data := make([]protocol.UInteger, 0, len(tokens)*5)
	prevLine := 0
	prevChar := 0
	for _, tok := range tokens {
		deltaLine := tok.line - prevLine
		deltaChar := tok.startChar
		if deltaLine == 0 {
			deltaChar = tok.startChar - prevChar
		}
		data = append(data,
			safeUint(deltaLine),
			safeUint(deltaChar),
			safeUint(tok.length),
			safeUint(tok.tokenType),
			safeUint(tok.modifiers),
		)
		prevLine = tok.line
		prevChar = tok.startChar
	}
	return data
}
