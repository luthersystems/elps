// Copyright © 2018 The ELPS authors

package token

import "fmt"

// Source is an abstract stream of tokens which allows one token lookahead.
type Source interface {
	// Token returns the current token.  Token returns nil if Scan has not been
	// called.
	Token() *Token
	// Peek returns the next token in the stream.  At the end of the stream
	// Peek should return a value to indicate the lack of a token (EOF).
	Peek() *Token
	// Scan advances the token stream if possible.  If there are no tokens
	// remaining Scan returns false.
	Scan() bool
}

type Token struct {
	Source            *Location
	Text              string
	Type              Type
	PrecedingNewlines int // newlines in whitespace before this token
	PrecedingSpaces   int // spaces in whitespace before this token (same-line only)
}

type Type uint

// Type constants used for the elps lexer/parser.  These constants aren't
// necessary to use the package.
const (
	INVALID Type = iota
	ERROR
	EOF

	HASH_BANG

	// Atomic expressions & literals
	SYMBOL
	INT
	INT_OCTAL_MACRO
	INT_OCTAL
	INT_HEX_MACRO
	INT_HEX
	FLOAT
	STRING
	STRING_RAW

	COMMENT

	// Operators
	NEGATIVE // arithmetic negation is parsed specially
	QUOTE
	UNBOUND
	FUN_REF

	// Delimiters
	PAREN_L
	PAREN_R
	BRACE_L
	BRACE_R

	numTokenTypes
)

func (typ Type) String() string {
	typeStrings := [numTokenTypes]string{
		INVALID:         "invalid",
		ERROR:           "error",
		EOF:             "EOF",
		HASH_BANG:       "#!",
		SYMBOL:          "symbol",
		INT:             "int",
		INT_OCTAL_MACRO: "#o",
		INT_OCTAL:       "octal",
		INT_HEX_MACRO:   "#x",
		INT_HEX:         "hex",
		FLOAT:           "float",
		STRING:          "string",
		STRING_RAW:      "raw-string",
		COMMENT:         ";",
		NEGATIVE:        "-",
		QUOTE:           "'",
		UNBOUND:         "#^",
		FUN_REF:         "#'",
		PAREN_L:         "(",
		PAREN_R:         ")",
		BRACE_L:         "[",
		BRACE_R:         "]",
	}
	if typ >= numTokenTypes {
		return typeStrings[INVALID] //nolint:gosec // INVALID is 0, always valid
	}
	return typeStrings[typ] //nolint:gosec // bounds checked above
}

// Location is a span in a source stream.
//
// UNITS.  Every offset and column in this struct is counted in BYTES, and
// says so on its own line below.  The unit is not a detail: Col and EndCol
// are subtracted from and added to each other by consumers all over the tree
// (analysis.scopeContainingAnalysis, lsp.locContainsCol, lsp.elpsToLSPRange,
// lint.endPosFromNode), and a value in one unit compared against a value in
// another is wrong without being obviously wrong.  Whatever unit is chosen,
// the four position fields have to agree on it.
//
// They did not.  TokenEnd derived EndCol by counting RUNES onto Scanner's
// byte-valued Col, so on any token containing a multi-byte rune EndCol was
// short by len(text)-utf8.RuneCountInString(text) and was in neither unit.
// Every LSP range built from EndCol was correspondingly short, and
// textDocument/rename -- which builds its TextEdit ranges from the same
// helper -- therefore replaced fewer bytes than the name occupied and left
// the tail behind: renaming "éx" to "zz" produced "zzx", a different program,
// silently (elps#463).  The absence of a stated unit is what allowed that, so
// the unit is stated here.
//
// BYTES is an INTERNAL convention, not what LSP asks for: LSP 3.16 counts a
// position in UTF-16 code units unless client and server negotiate otherwise,
// and this server neither offers positionEncoding nor converts anything.
// That server-wide gap is elps#464 and is deliberately not what this comment
// settles.  #464 is about which unit crosses the wire; the requirement here
// is only that these four fields agree with EACH OTHER, which they must under
// any choice #464 goes on to make.
type Location struct {
	File    string // a name representing the source stream
	Path    string // a physical location which may differ from File
	Pos     int    // BYTE offset of the first byte of the token/expr
	Line    int    // line number (starting at 1 when tracked)
	Col     int    // BYTE column within the line (1-based; 0 = not tracked)
	EndPos  int    // BYTE offset one past the last byte (0 = not tracked)
	EndLine int    // end line (1-based, 0 = not tracked)
	EndCol  int    // BYTE column one past the last byte (1-based, exclusive; 0 = not tracked)
}

func (loc *Location) String() string {
	switch {
	case loc.Pos < 0:
		return loc.File
	case loc.Line == 0:
		return fmt.Sprintf("%s[%d]", loc.File, loc.Pos)
	case loc.Col == 0:
		return fmt.Sprintf("%s:%d", loc.File, loc.Line)
	default:
		return fmt.Sprintf("%s:%d:%d", loc.File, loc.Line, loc.Col)
	}
}

// NativeFile is the File reported by a Location that does not come from a
// source stream -- a value constructed by Go code rather than read from a
// file.  Paired with Pos == -1, which is what Location.String and the
// "is this a real position?" checks throughout the tree test for.
const NativeFile = "<native code>"

// NativeLocation returns the Location describing code that has no source
// stream: values constructed by Go rather than read by the parser.
//
// It returns a VALUE, deliberately, and it is the only definition of that
// location in the tree.  A function handing out a *Location here would hand
// every caller a pointer into shared state that any one of them could write
// through, which is issue #362 -- and lisp.nativeSource is exactly that
// function.  Callers that need a *Location for a node they own take the
// address of their own copy:
//
//	loc := token.NativeLocation()
//	v.Source = &loc
func NativeLocation() Location {
	return Location{
		File: NativeFile,
		Pos:  -1,
	}
}

// Copy returns a pointer to an independent copy of loc, or nil if loc is nil.
//
// Location holds no reference-typed fields, so the shallow copy is fully
// independent: a later write through either pointer is invisible to the
// other.  Nil is preserved rather than materialised into a zero Location
// because a nil Source is meaningful throughout the tree ("no position
// recorded") and distinct from a zero one ("position 0").
//
// Use it at every point where a Location owned by one object is stored into
// another that outlives, or is mutated independently of, the first --
// see issues #362 and #366.
func (loc *Location) Copy() *Location {
	if loc == nil {
		return nil
	}
	cp := *loc
	return &cp
}

// TokenEnd computes the end position of a token from its start position and
// text.  All three results are in the units Location documents: endCol is an
// exclusive BYTE column, endPos an absolute BYTE offset.  For a multi-line
// token (a raw string) the line and column are adjusted accordingly.
//
// THE DEFECT (elps#463).  This used to advance col by ONE PER RUNE:
//
//	for _, ch := range tok.Text {
//		if ch == '\n' { line++; col = 1 } else { col++ }
//	}
//
// `range` over a string iterates runes, so that produced Col + runeCount --
// a rune width added to the byte base Scanner.LocStart computes as
// `startPos - s.startLinePos + 1`.  On a pure-ASCII token the two units
// coincide and it was right by accident; on a token holding any multi-byte
// rune it was short by len(text)-runeCount(text) and was in neither unit,
// while endPos beside it was already byte-exact.  A field that is correct for
// most inputs and quietly wrong for the rest is the shape of bug that reaches
// users, and this one reached them through textDocument/rename: the edit
// range was narrower than the identifier, so the rename replaced a prefix and
// left the tail, turning "éx" into "zzx" rather than "zz" with no diagnostic.
// TestRenameNonASCIIIdentifierRewritesWholeName is the end-to-end pin.
//
// Counting BYTES rather than runes here is the whole fix, and it is a fix
// under either answer to elps#464 (whether the SERVER should be emitting
// UTF-16 code units on the wire): the invariant this restores is that endCol
// agrees with the Col it is measured from, which any wire encoding needs
// before it can convert anything.
//
// The loop scans bytes rather than runes deliberately.  A byte scan is exact
// on invalid UTF-8, where `range` yields RuneError with a width that does not
// describe the input; '\n' cannot occur as a UTF-8 continuation byte, so
// looking for it bytewise finds exactly the newlines a rune scan would.
func TokenEnd(tok *Token) (endLine, endCol, endPos int) {
	if tok == nil || tok.Source == nil {
		return 0, 0, 0
	}
	line := tok.Source.Line
	// lineStart is the index in tok.Text of the first byte of the last line
	// the token covers, or -1 while the token has not crossed a newline and
	// the end column is therefore still measured from tok.Source.Col.
	lineStart := -1
	for i := 0; i < len(tok.Text); i++ {
		if tok.Text[i] == '\n' {
			line++
			lineStart = i + 1
		}
	}
	col := tok.Source.Col + len(tok.Text)
	if lineStart >= 0 {
		col = len(tok.Text) - lineStart + 1
	}
	return line, col, tok.Source.Pos + len(tok.Text)
}

type LocationError struct {
	Err    error
	Source *Location
	Code   string // error classification (empty = unclassified)
}

func (err *LocationError) Error() string {
	return fmt.Sprintf("%s: %s", err.Source, err.Err)
}

// Unwrap returns the underlying error for use with errors.Is/errors.As.
func (err *LocationError) Unwrap() error {
	return err.Err
}
