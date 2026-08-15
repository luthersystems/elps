// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// semanticTokenOverruns decodes a semanticTokens/full response back to absolute
// (line, startChar, length) triples and returns a description of every token
// whose range does not lie inside the document.
//
// LSP 3.16 §textDocument/semanticTokens defines a token as (deltaLine,
// deltaStartChar, length) over the document text, so a token that runs past the
// end of its line names no text at all.  Clients disagree about what to do with
// one -- decorate into the next line, drop the token, or drop the whole
// response -- so the practical symptom is "highlighting in this file is wrong
// or missing".
func semanticTokenOverruns(content string, data []protocol.UInteger) []string {
	lines := strings.Split(content, "\n")
	var bad []string
	for _, tok := range decodeTokens(data) {
		if tok.line < 0 || tok.line >= len(lines) {
			bad = append(bad, fmt.Sprintf("token [%d:%d,+%d) is on line %d, but the document has %d lines",
				tok.line, tok.startChar, tok.length, tok.line, len(lines)))
			continue
		}
		// Lines are indexed by byte everywhere else in this package (see
		// position.go), so the bound is a byte count, not a rune count.
		text := strings.TrimSuffix(lines[tok.line], "\r")
		if tok.startChar+tok.length > len(text) {
			bad = append(bad, fmt.Sprintf("token [%d:%d,+%d) overruns line %d (%d chars: %q)",
				tok.line, tok.startChar, tok.length, tok.line, len(text), text))
		}
	}
	return bad
}

// tokensFor opens content as a document and returns the decoded token stream.
func tokensFor(t *testing.T, s *Server, uri, content string) []protocol.UInteger {
	t.Helper()
	doc := openDoc(s, uri, content)
	s.ensureAnalysis(doc)
	res, err := s.textDocumentSemanticTokensFull(mockContext(), &protocol.SemanticTokensParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: doc.URI},
	})
	if err != nil {
		t.Fatalf("semanticTokens/full(%q): %v", content, err)
	}
	if res == nil {
		return nil
	}
	return res.Data
}

// TestSemanticTokensStayInsideTheDocument is the RED test for elps#428: the
// reader desugars #^e to (lisp:expr e) and #'f to (lisp:function f), and the
// synthesized head symbol it builds carries the DESUGARED NAME (9 and 13 bytes)
// while occupying 2 bytes of source.  collectSemanticTokens took the LSymbol
// case's length from len(v.Str), so #'foo -- five characters -- was reported to
// the editor as a 13-character token starting at char 0.
//
// Every case below FAILED on bfb6ee8 (the commit this fix is based on); none of
// them is a guard.
func TestSemanticTokensStayInsideTheDocument(t *testing.T) {
	s := testServer()
	for i, content := range []string{
		"#^a",
		"#'foo",
		"(f #^a)",
		"(defun g () #'g)",
		"#^(+ % 1)", // fuzzseed.All() seed 95, committed to this repository
		"'#^0",
		"(list #'car #'cdr)",
		"#^(+ #^(+ % 1) 1)",
		"(defun g ()\n  #'g)", // head not on the first line
	} {
		t.Run(fmt.Sprintf("%d/%s", i, content), func(t *testing.T) {
			data := tokensFor(t, s, fmt.Sprintf("file:///test/bounds%d.lisp", i), content)
			for _, msg := range semanticTokenOverruns(content, data) {
				t.Errorf("%s", msg)
			}
		})
	}
}

// TestSemanticTokensStayInsideTheSeedCorpus runs the same property over every
// .lisp source in this repository plus the hand-written adversarial seeds.
//
// This is the shape the property was found in (elps#428 was reported from a run
// over fuzzseed.All()), and it is the cheap deterministic half of the assertion
// FuzzLSPSession now makes on every generated document.
func TestSemanticTokensStayInsideTheSeedCorpus(t *testing.T) {
	s := testServer()
	for i, seed := range fuzzseed.All() {
		content := string(seed)
		if !utf8Valid(content) {
			// Byte-indexed line lengths are still the right bound, but a seed
			// that is not text is not what this property is about.
			continue
		}
		data := tokensFor(t, s, fmt.Sprintf("file:///test/seed%d.lisp", i), content)
		for _, msg := range semanticTokenOverruns(content, data) {
			t.Errorf("seed %d: %s", i, msg)
		}
	}
}

// TestSemanticTokensSuppressSynthesizedPrefixHeads pins the DECISION taken for
// elps#428 -- suppress the synthesized head rather than shorten it -- and the
// two things that decision must not break: the operand still gets its token,
// and a longhand (lisp:function f) the user actually typed is untouched.
//
// Without the fix the first four cases each carry an extra 9- or 13-byte token
// at the prefix; with it they do not.
func TestSemanticTokensSuppressSynthesizedPrefixHeads(t *testing.T) {
	s := testServer()
	for i, tc := range []struct {
		content string
		want    []rawToken
	}{
		// #^ and #' contribute no token; the operand keeps its own.
		{"#^a", []rawToken{{line: 0, startChar: 2, length: 1, tokenType: semTokenVariable}}},
		{"#'foo", []rawToken{{line: 0, startChar: 2, length: 3, tokenType: semTokenVariable}}},
		{"(f #^a)", []rawToken{
			{line: 0, startChar: 1, length: 1, tokenType: semTokenVariable},
			{line: 0, startChar: 5, length: 1, tokenType: semTokenVariable},
		}},
		{"(list #'car)", []rawToken{
			{line: 0, startChar: 1, length: 4, tokenType: semTokenFunction},
			{line: 0, startChar: 8, length: 3, tokenType: semTokenFunction},
		}},
		// Longhand is ordinary source text and keeps its full-width token.
		// This is the case a name-only test would have broken.
		{"(lisp:function foo)", []rawToken{
			{line: 0, startChar: 1, length: 13, tokenType: semTokenKeyword},
			{line: 0, startChar: 15, length: 3, tokenType: semTokenVariable},
		}},
		{"(lisp:expr foo)", []rawToken{
			{line: 0, startChar: 1, length: 9, tokenType: semTokenVariable},
			{line: 0, startChar: 11, length: 3, tokenType: semTokenVariable},
		}},
	} {
		t.Run(fmt.Sprintf("%d/%s", i, tc.content), func(t *testing.T) {
			data := tokensFor(t, s, fmt.Sprintf("file:///test/synth%d.lisp", i), tc.content)
			got := decodeTokens(data)
			if len(got) != len(tc.want) {
				t.Fatalf("got %d tokens %v, want %d %v", len(got), got, len(tc.want), tc.want)
			}
			for j := range got {
				if got[j].line != tc.want[j].line || got[j].startChar != tc.want[j].startChar ||
					got[j].length != tc.want[j].length || got[j].tokenType != tc.want[j].tokenType {
					t.Errorf("token %d: got %+v, want %+v", j, got[j], tc.want[j])
				}
			}
		})
	}
}

// TestSemanticTokensQuotePrefixEmitsNoToken is a GUARD, not a catch: it passed
// on bfb6ee8 as well.  It records the behaviour elps#428's fix was made
// consistent with -- ' produces no token of its own, because ParseQuote sets
// Quoted on the node instead of synthesizing a head for the reader to place.
//
// If a later change starts emitting a token for ' this will fail, and whoever
// sees it should revisit whether #^ and #' should stay suppressed.
func TestSemanticTokensQuotePrefixEmitsNoToken(t *testing.T) {
	s := testServer()
	data := tokensFor(t, s, "file:///test/quoteprefix.lisp", "'foo")
	for _, tok := range decodeTokens(data) {
		if tok.startChar == 0 && tok.length == 1 {
			t.Errorf("' produced a token of its own: %+v", tok)
		}
	}
}

func utf8Valid(s string) bool {
	for _, r := range s {
		if r == 0xFFFD {
			return false
		}
	}
	return true
}
