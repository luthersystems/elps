// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/token"
)

// lexerPackageName is validPackageName's slow path on its own: the lexer's
// verdict with no fast path in front of it.  It is what the fast path must
// agree with, byte for byte.
func lexerPackageName(name string) bool {
	lex := lexer.New(token.NewScannerString("", name))
	tok := lex.ReadToken()[0]
	if tok.Type == token.NEGATIVE {
		tok = lex.ReadToken()[0]
		if tok.Text != name[1:] {
			return false
		}
	} else if tok.Text != name {
		return false
	}
	return tok.Type == token.SYMBOL && lex.ReadToken()[0].Type == token.EOF
}

// TestValidPackageNameFastPathAgreesWithLexer pins that isPlainASCIIName
// accepts only strings the lexer accepts too: the fast path may say true only
// where the lexer would, and on everything else validPackageName's verdict
// is the lexer's.  Exhaustive over every string of length one to three drawn
// from an alphabet that covers each character class the lexer dispatches on.
func TestValidPackageNameFastPathAgreesWithLexer(t *testing.T) {
	const alphabet = "aZ_09-+.:'\"#;( )~%$?!<>=*/&\\\t\né"
	var runes []rune
	for _, r := range alphabet {
		runes = append(runes, r)
	}
	var walk func(prefix []rune, depth int)
	checked := 0
	walk = func(prefix []rune, depth int) {
		if len(prefix) > 0 {
			name := string(prefix)
			checked++
			fast := isPlainASCIIName(name)
			slow := lexerPackageName(name)
			if fast && !slow {
				t.Fatalf("fast path accepts %q but the lexer rejects it", name)
			}
			if got := validPackageName(name); got != (slow && name != "" && !containsColon(name)) {
				t.Fatalf("validPackageName(%q) = %v, lexer says %v", name, got, slow)
			}
		}
		if depth == 0 {
			return
		}
		for _, r := range runes {
			walk(append(prefix, r), depth-1)
		}
	}
	walk(nil, 3)
	if checked < 30000 {
		t.Fatalf("checked only %d strings", checked)
	}
	for _, name := range []string{"lisp", "my-service", "utils_v2", "A1", "_x", "x-"} {
		if !isPlainASCIIName(name) {
			t.Errorf("fast path should accept %q", name)
		}
	}
	for _, name := range []string{"", "-x", "1x", "x.y", "x:y", "é", "x y", "x+"} {
		if isPlainASCIIName(name) {
			t.Errorf("fast path must not accept %q", name)
		}
	}
}

func containsColon(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] == ':' {
			return true
		}
	}
	return false
}
