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
	Type   Type
	Text   string
	Source *Location
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
		return typeStrings[INVALID]
	}
	return typeStrings[typ]
}

type Location struct {
	File string // a name representing the source stream
	Path string // a physical location which may differ from File
	Pos  int
	Line int // line number (starting at 1 when tracked)
	Col  int // line column number (starting at 1 when tracked)
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

type LocationError struct {
	Err    error
	Source *Location
}

func (err *LocationError) Error() string {
	return fmt.Sprintf("%s: %s", err.Source, err.Err)
}
