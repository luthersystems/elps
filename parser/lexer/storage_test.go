// Copyright © 2026 The ELPS authors

package lexer

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

func TestSingleTokenSliceAllocations(t *testing.T) {
	for _, tc := range []struct {
		name string
		emit func(*Lexer, token.Type) []*token.Token
		want int
	}{
		{"character", (*Lexer).charToken, 2},
		{"text", (*Lexer).emitText, 2},
		{"explicit", func(lex *Lexer, typ token.Type) []*token.Token {
			return lex.emit(typ, "")
		}, 65},
	} {
		t.Run(tc.name, func(t *testing.T) {
			lex := &Lexer{scanner: &token.Scanner{}}
			var tok *token.Token
			// Exactly one chunk per run, with no text allocation. Character
			// and text emissions allocate two slabs; explicit emissions still
			// allocate each Token plus one Location slab. None allocates slices.
			allocs := testing.AllocsPerRun(200, func() {
				for range 64 {
					toks := tc.emit(lex, token.PAREN_L)
					if len(toks) != 1 {
						t.Fatalf("got %d tokens, want 1", len(toks))
					}
					tok = toks[0]
				}
			})
			if tok.Type != token.PAREN_L || tok.Text != "" || tok.Source == nil {
				t.Fatalf("unexpected token: %+v", tok)
			}
			if allocs != float64(tc.want) {
				t.Errorf("64 tokens allocated %v times, want %d", allocs, tc.want)
			}
		})
	}
}

func TestReadTokenRetainedPointers(t *testing.T) {
	// Cross several slab boundaries through character, text, comment and EOF
	// emission. Retain individual pointers, as the parser and REPL do.
	lex := New(token.NewScannerString("test", strings.Repeat("(ab) ;comment\n", 70)))
	var tokens []*token.Token
	var values []token.Token
	var locations []token.Location
	seenTokens := make(map[*token.Token]bool)
	seenLocations := make(map[*token.Location]bool)
	for {
		toks := lex.ReadToken()
		if len(toks) != 1 {
			t.Fatalf("got %d tokens, want 1", len(toks))
		}
		tok := toks[0]
		if seenTokens[tok] || seenLocations[tok.Source] {
			t.Fatal("token or location storage was reused")
		}
		seenTokens[tok], seenLocations[tok.Source] = true, true
		tokens = append(tokens, tok)
		values = append(values, *tok)
		locations = append(locations, *tok.Source)
		if tok.Type == token.EOF {
			break
		}
		if len(tokens) > 280 {
			t.Fatal("lexer did not terminate")
		}
	}
	if len(tokens) != 281 {
		t.Fatalf("got %d tokens, want 281", len(tokens))
	}
	for i, tok := range tokens {
		if *tok != values[i] || *tok.Source != locations[i] {
			t.Fatalf("token %d changed after a later read", i)
		}
	}
	// Parser end-span writes must remain private to the selected location.
	tokens[0].Source.EndPos = 1234
	for i, tok := range tokens[1:] {
		if *tok.Source != locations[i+1] {
			t.Fatalf("token %d aliases the first location", i+1)
		}
	}
}
